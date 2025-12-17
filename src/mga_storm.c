#define PSZ 8

#include "config.h"

/* All drivers should typically include these */
#include "xf86.h"
#include "xf86_OSproc.h"

/* For correct __inline__ usage */
#include "compiler.h"

/* Drivers that need to access the PCI config space directly need this */
#include "xf86Pci.h"

#include "xf86fbman.h"
#include "miline.h"
#include "servermd.h"

#include "mga.h"
#include "mga_reg.h"
#include "mga_macros.h"

#ifdef MGADRI
#include "mga_dri.h"
#endif

#define REPLICATE_8(r)  (((r) & 0x0ff) | (((r) & 0x0ff) << 8) \
			 | (((r) & 0x0ff) << 16) | (((r) & 0x0ff) << 24))
#define REPLICATE_16(r) (((r) & 0x0000ffff) | (((r) & 0x0000ffff) << 16))
#define REPLICATE_24(r) (((r) & 0x00ffffff) | (((r) & 0x00ffffff) << 24))
#define REPLICATE_32(r) (r)


#define SET_FOREGROUND_REPLICATED(c, rep_c) \
	if((c) != pMga->FgColor) { \
	   pMga->FgColor = (c); \
	   OUTREG(MGAREG_FCOL,(rep_c)); \
	}

#define SET_BACKGROUND_REPLICATED(c, rep_c) \
	if((c) != pMga->BgColor) { \
	   pMga->BgColor = (c); \
	   OUTREG(MGAREG_BCOL,(rep_c)); \
	}


Bool mgaAccelInit( ScreenPtr pScreen )
{
    ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
    MGAPtr pMga = MGAPTR(pScrn);
//  int maxFastBlitMem, maxlines;
    int maxlines;

    BoxRec AvailFBArea;

    pMga->ScratchBuffer = malloc(((pScrn->displayWidth * pMga->CurrentLayout.bitsPerPixel) + 127) >> 3);
    if(!pMga->ScratchBuffer) return FALSE;


    pMga->RenderTime = 0;
    pMga->LinearScratch = 0;

    pMga->MaxFastBlitY = 0;
    pMga->MaxBlitDWORDS = 0x40000 >> 5;


    /* Set initial acceleration flags.
     */
    pMga->AccelFlags = pMga->chip_attribs->accel_flags;

    if ((pMga->FbMapSize > 8*1024*1024) && (pScrn->depth == 8)) {
	pMga->AccelFlags |= LARGE_ADDRESSES;
    }

    if (pMga->CurrentLayout.bitsPerPixel == 24) {
	pMga->AccelFlags |= MGA_NO_PLANEMASK;
    }

    if (pMga->SecondCrtc) {
	pMga->HasFBitBlt = FALSE;
    }

    if(pMga->HasSDRAM) {
	pMga->Atype = pMga->AtypeNoBLK = MGAAtypeNoBLK;
	pMga->AccelFlags &= ~TWO_PASS_COLOR_EXPAND;
    } else {
	pMga->Atype = MGAAtype;
	pMga->AtypeNoBLK = MGAAtypeNoBLK;
    }

    switch (pMga->Chipset) {
    case PCI_CHIP_MGAG200_SE_A_PCI:
    case PCI_CHIP_MGAG200_SE_B_PCI:
	maxlines = (min(pMga->FbUsableSize, 1*1024*1024)) /
		   (pScrn->displayWidth * pMga->CurrentLayout.bitsPerPixel / 8);
	break;
    default:
	maxlines = (min(pMga->FbUsableSize, 16*1024*1024)) /
		   (pScrn->displayWidth * pMga->CurrentLayout.bitsPerPixel / 8);
	break;
    }

#ifdef MGADRI
    if ( pMga->directRenderingEnabled ) {
       MGADRIServerPrivatePtr pMGADRIServer = pMga->DRIServerInfo;
       BoxRec MemBox;
       int cpp = pScrn->bitsPerPixel / 8;
       int widthBytes = pScrn->displayWidth * cpp;
       int bufferSize = ((pScrn->virtualY * widthBytes + MGA_BUFFER_ALIGN)
			 & ~MGA_BUFFER_ALIGN);
       int scanlines;

       pMGADRIServer->frontOffset = 0;
       pMGADRIServer->frontPitch = widthBytes;

       /* Try for front, back, depth, and two framebuffers worth of
	* pixmap cache.  Should be enough for a fullscreen background
	* image plus some leftovers.
	*/
       pMGADRIServer->textureSize = pMga->FbMapSize - 5 * bufferSize;

       /* If that gives us less than half the available memory, let's
	* be greedy and grab some more.  Sorry, I care more about 3D
	* performance than playing nicely, and you'll get around a full
	* framebuffer's worth of pixmap cache anyway.
	*/
       if ( pMGADRIServer->textureSize < (int)pMga->FbMapSize / 2 ) {
	  pMGADRIServer->textureSize = pMga->FbMapSize - 4 * bufferSize;
       }

       /* Check to see if there is more room available after the maximum
	* scanline for textures.
	*/
       if ( (int)pMga->FbMapSize - maxlines * widthBytes - bufferSize * 2
	    > pMGADRIServer->textureSize ) {
	  pMGADRIServer->textureSize = (pMga->FbMapSize -
					maxlines * widthBytes -
					bufferSize * 2);
       }

       /* Set a minimum usable local texture heap size.  This will fit
	* two 256x256x32bpp textures.
	*/
       if ( pMGADRIServer->textureSize < 512 * 1024 ) {
	  pMGADRIServer->textureOffset = 0;
	  pMGADRIServer->textureSize = 0;
       }

       /* Reserve space for textures */
       pMGADRIServer->textureOffset = (pMga->FbMapSize -
				       pMGADRIServer->textureSize +
				       MGA_BUFFER_ALIGN) & ~MGA_BUFFER_ALIGN;

       /* Reserve space for the shared depth buffer */
       pMGADRIServer->depthOffset = (pMGADRIServer->textureOffset -
				     bufferSize +
				     MGA_BUFFER_ALIGN) & ~MGA_BUFFER_ALIGN;
       pMGADRIServer->depthPitch = widthBytes;

       /* Reserve space for the shared back buffer */
       pMGADRIServer->backOffset = (pMGADRIServer->depthOffset - bufferSize +
				    MGA_BUFFER_ALIGN) & ~MGA_BUFFER_ALIGN;
       pMGADRIServer->backPitch = widthBytes;

       scanlines = pMGADRIServer->backOffset / widthBytes - 1;
       if ( scanlines > maxlines ) scanlines = maxlines;

       MemBox.x1 = 0;
       MemBox.y1 = 0;
       MemBox.x2 = pScrn->displayWidth;
       MemBox.y2 = scanlines;

       if ( !xf86InitFBManager( pScreen, &MemBox ) ) {
	  xf86DrvMsg( pScrn->scrnIndex, X_ERROR,
		      "Memory manager initialization to (%d,%d) (%d,%d) failed\n",
		      MemBox.x1, MemBox.y1, MemBox.x2, MemBox.y2 );
	  return FALSE;
       } else {
	  int width, height;

	  xf86DrvMsg( pScrn->scrnIndex, X_INFO,
		      "Memory manager initialized to (%d,%d) (%d,%d)\n",
		      MemBox.x1, MemBox.y1, MemBox.x2, MemBox.y2 );

	  if ( xf86QueryLargestOffscreenArea( pScreen, &width,
					      &height, 0, 0, 0 ) ) {
	     xf86DrvMsg( pScrn->scrnIndex, X_INFO,
			 "Largest offscreen area available: %d x %d\n",
			 width, height );
	  }
       }

       xf86DrvMsg( pScrn->scrnIndex, X_INFO,
		   "Reserved back buffer at offset 0x%x\n",
		   pMGADRIServer->backOffset );
       xf86DrvMsg( pScrn->scrnIndex, X_INFO,
		   "Reserved depth buffer at offset 0x%x\n",
		   pMGADRIServer->depthOffset );
       xf86DrvMsg( pScrn->scrnIndex, X_INFO,
		   "Reserved %d kb for textures at offset 0x%x\n",
		   pMGADRIServer->textureSize/1024,
		   pMGADRIServer->textureOffset );
    }
    else
#endif /* defined(MGADRI) */
    {
       AvailFBArea.x1 = 0;
       AvailFBArea.x2 = pScrn->displayWidth;
       AvailFBArea.y1 = 0;
       AvailFBArea.y2 = maxlines;

       /*
	* Need to keep a strip of memory to the right of screen to workaround
	* a display problem with the second CRTC.
	*/
       if (pMga->SecondCrtc)
	  AvailFBArea.x2 = pScrn->virtualX;

       xf86InitFBManager(pScreen, &AvailFBArea);
       xf86DrvMsg(pScrn->scrnIndex, X_INFO, "Using %d lines for offscreen "
		  "memory.\n",
		  maxlines - pScrn->virtualY);

    }

    return TRUE;
}


CARD32 MGAAtype[16] = {
   MGADWG_RPL  | 0x00000000, MGADWG_RSTR | 0x00080000,
   MGADWG_RSTR | 0x00040000, MGADWG_BLK  | 0x000c0000,
   MGADWG_RSTR | 0x00020000, MGADWG_RSTR | 0x000a0000,
   MGADWG_RSTR | 0x00060000, MGADWG_RSTR | 0x000e0000,
   MGADWG_RSTR | 0x00010000, MGADWG_RSTR | 0x00090000,
   MGADWG_RSTR | 0x00050000, MGADWG_RSTR | 0x000d0000,
   MGADWG_RPL  | 0x00030000, MGADWG_RSTR | 0x000b0000,
   MGADWG_RSTR | 0x00070000, MGADWG_RPL  | 0x000f0000
};


CARD32 MGAAtypeNoBLK[16] = {
   MGADWG_RPL  | 0x00000000, MGADWG_RSTR | 0x00080000,
   MGADWG_RSTR | 0x00040000, MGADWG_RPL  | 0x000c0000,
   MGADWG_RSTR | 0x00020000, MGADWG_RSTR | 0x000a0000,
   MGADWG_RSTR | 0x00060000, MGADWG_RSTR | 0x000e0000,
   MGADWG_RSTR | 0x00010000, MGADWG_RSTR | 0x00090000,
   MGADWG_RSTR | 0x00050000, MGADWG_RSTR | 0x000d0000,
   MGADWG_RPL  | 0x00030000, MGADWG_RSTR | 0x000b0000,
   MGADWG_RSTR | 0x00070000, MGADWG_RPL  | 0x000f0000
};


Bool
MGAStormAccelInit(ScreenPtr pScreen)
{
    return mgaAccelInit( pScreen );
}



void
MGAStormSync(ScrnInfoPtr pScrn)
{
    MGAPtr pMga = MGAPTR(pScrn);

    CHECK_DMA_QUIESCENT(pMga, pScrn);

    /* MGAISBUSY() reportedly causes a freeze for Mystique revisions 0 and 1 */
    if (!(pMga->Chipset == PCI_CHIP_MGA1064 && (pMga->ChipRev >= 0 && pMga->ChipRev <= 1)))
	while(MGAISBUSY());
    /* flush cache before a read (mga-1064g 5.1.6) */
    OUTREG8(MGAREG_CRTC_INDEX, 0);
    if(pMga->AccelFlags & CLIPPER_ON) {
        pMga->AccelFlags &= ~CLIPPER_ON;
        OUTREG(MGAREG_CXBNDRY, 0xFFFF0000);
    }
}


void MGAStormEngineInit( ScrnInfoPtr pScrn )
{
    long maccess = 0;
    MGAPtr pMga = MGAPTR(pScrn);
    MGAFBLayout *pLayout = &pMga->CurrentLayout;
    CARD32 opmode;
    static const unsigned int maccess_table[5] = {
   /* bpp:  8  16  24  32 */
	0,  0,  1,  3,  2
    };
    static const unsigned int opmode_table[5] = {
        /* bpp:        8       16       24       32 */
	0x00000, 0x00000, 0x10000, 0x20000, 0x20000
    };

    CHECK_DMA_QUIESCENT(pMga, pScrn);

    if ((pMga->Chipset == PCI_CHIP_MGAG100)
	|| (pMga->Chipset == PCI_CHIP_MGAG100_PCI))
    	maccess = 1 << 14;

    opmode = INREG(MGAREG_OPMODE);

    maccess |= maccess_table[ pLayout->bitsPerPixel / 8 ];
    if ( pLayout->depth == 15 ) {
        maccess |= (1 << 31);
    }

    opmode |= opmode_table[ pLayout->bitsPerPixel / 8 ];
#if X_BYTE_ORDER == X_LITTLE_ENDIAN
    opmode &= ~0x30000;
#endif

    pMga->fifoCount = 0;

    while(MGAISBUSY());

    if(!pMga->FifoSize) {
	pMga->FifoSize = INREG8(MGAREG_FIFOSTATUS);
	xf86DrvMsg(pScrn->scrnIndex, X_PROBED, "%i DWORD fifo\n",
						pMga->FifoSize);
    }

    OUTREG(MGAREG_PITCH, pLayout->displayWidth);
    OUTREG(MGAREG_YDSTORG, pMga->YDstOrg);
    OUTREG(MGAREG_MACCESS, maccess);
    pMga->MAccess = maccess;
    pMga->PlaneMask = ~0;
    /* looks like this doesn't apply to mga g100 pci */

    if ((pMga->Chipset != PCI_CHIP_MGAG100)
	&& (pMga->Chipset != PCI_CHIP_MGAG100_PCI))
        OUTREG(MGAREG_PLNWT, pMga->PlaneMask);

    pMga->FgColor = 0;
    OUTREG(MGAREG_FCOL, pMga->FgColor);
    pMga->BgColor = 0;
    OUTREG(MGAREG_BCOL, pMga->BgColor);
    OUTREG(MGAREG_OPMODE, MGAOPM_DMA_BLIT | opmode);

    /* put clipping in a known state */
    OUTREG(MGAREG_CXBNDRY, 0xFFFF0000);	/* (maxX << 16) | minX */
    OUTREG(MGAREG_YTOP, 0x00000000);	/* minPixelPointer */
    OUTREG(MGAREG_YBOT, 0x007FFFFF);	/* maxPixelPointer */
    pMga->AccelFlags &= ~CLIPPER_ON;

    switch(pMga->Chipset) {
    case PCI_CHIP_MGAG550:
    case PCI_CHIP_MGAG400:
    case PCI_CHIP_MGAG200:
    case PCI_CHIP_MGAG200_PCI:
    case PCI_CHIP_MGAG200_SE_A_PCI:
    case PCI_CHIP_MGAG200_SE_B_PCI:
    case PCI_CHIP_MGAG200_WINBOND_PCI:
    case PCI_CHIP_MGAG200_EW3_PCI:
    case PCI_CHIP_MGAG200_EV_PCI:
    case PCI_CHIP_MGAG200_EH_PCI:
    case PCI_CHIP_MGAG200_ER_PCI:
    case PCI_CHIP_MGAG200_EH3_PCI:
	pMga->SrcOrg = 0;
	OUTREG(MGAREG_SRCORG, pMga->realSrcOrg);
	OUTREG(MGAREG_DSTORG, pMga->DstOrg);
	break;
    default:
	break;
    }

    if (pMga->is_G200WB)
    {
        CARD32 dwgctl = MGADWG_RSTR | 0x00060000 | MGADWG_SHIFTZERO |
			MGADWG_BITBLT | MGADWG_BFCOL;
        WAITFIFO(7);
        OUTREG(MGAREG_DWGCTL, dwgctl);
        OUTREG(MGAREG_SGN, 0);
        OUTREG(MGAREG_AR5, 1);
        OUTREG(MGAREG_AR0, 1);
        OUTREG(MGAREG_AR3, 0);
        OUTREG(MGAREG_FXBNDRY, (1 << 16) | (1 & 0xffff));
        OUTREG(MGAREG_YDSTLEN + MGAREG_EXEC, (1 << 16) | 1);
    }

    xf86SetLastScrnFlag(pScrn->entityList[0], pScrn->scrnIndex);
}
