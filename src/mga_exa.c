/*
 * Copyright 2006 Red Hat, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software")
 * to deal in the software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * them Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTIBILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT, OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Authors:
 *    Adam Jackson <ajax@nwnk.net>
 */

/*
 * Sources:
 * - mga kdrive accel by Anders Carlsson
 * - mga g400 Render accel by Damien Ciabrini
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "xf86.h"

#include "mga.h"
#include "mga_reg.h"
#include "mga_macros.h"

#include "exa.h"
#ifdef XF86DRI
#include "mga_dri.h"
#endif

#if 0
#define DEBUG_MSG(x)  ErrorF x
#else
#define DEBUG_MSG(x)
#endif

#define PMGA(x) \
    MGAPtr pMga = xf86Screens[x->drawable.pScreen->myNum]->driverPrivate;

/* FIXME: is this correct? */
#define QUIESCE_DMA(x) \
    CHECK_DMA_QUIESCENT(pMga, xf86Screens[x->drawable.pScreen->myNum]);

/* stuff stolen from mga_storm.c */
#define BLIT_LEFT   1
#define BLIT_UP        4

static const CARD32 mgaRop[16] = {
    /* GXclear        */  MGADWG_RPL  | 0x00000000,    /* 0 */
    /* GXand          */  MGADWG_RSTR | 0x00080000,    /* src AND dst */
    /* GXandReverse   */  MGADWG_RSTR | 0x00040000,    /* src AND NOT dst */
    /* GXcopy         */  MGADWG_RSTR | 0x000c0000,    /* src */
    /* GXandInverted  */  MGADWG_RSTR | 0x00020000,    /* NOT src AND dst */
    /* GXnoop         */  MGADWG_RSTR | 0x000a0000,    /* dst */
    /* GXxor          */  MGADWG_RSTR | 0x00060000,    /* src XOR dst */
    /* GXor           */  MGADWG_RSTR | 0x000e0000,    /* src OR dst */
    /* GXnor          */  MGADWG_RSTR | 0x00010000,    /* NOT src AND NOT dst */
    /* GXequiv        */  MGADWG_RSTR | 0x00090000,    /* NOT src XOR dst */
    /* GXinvert       */  MGADWG_RSTR | 0x00050000,    /* NOT dst */
    /* GXorReverse    */  MGADWG_RSTR | 0x000d0000,    /* src OR NOT dst */
    /* GXcopyInverted */  MGADWG_RPL  | 0x00030000,    /* NOT src */
    /* GXorInverted   */  MGADWG_RSTR | 0x000b0000,    /* NOT src OR dst */
    /* GXnand         */  MGADWG_RSTR | 0x00070000,    /* NOT src OR NOT dst */
    /* GXset          */  MGADWG_RPL  | 0x000f0000    /* 1 */
};

static const CARD32 atype[16] = {
   MGADWG_RPL  | 0x00000000, MGADWG_RSTR | 0x00080000,
   MGADWG_RSTR | 0x00040000, MGADWG_BLK  | 0x000c0000,
   MGADWG_RSTR | 0x00020000, MGADWG_RSTR | 0x000a0000,
   MGADWG_RSTR | 0x00060000, MGADWG_RSTR | 0x000e0000,
   MGADWG_RSTR | 0x00010000, MGADWG_RSTR | 0x00090000,
   MGADWG_RSTR | 0x00050000, MGADWG_RSTR | 0x000d0000,
   MGADWG_RPL  | 0x00030000, MGADWG_RSTR | 0x000b0000,
   MGADWG_RSTR | 0x00070000, MGADWG_RPL  | 0x000f0000
};

static const CARD32 atype_noblk[16] = {
   MGADWG_RPL  | 0x00000000, MGADWG_RSTR | 0x00080000,
   MGADWG_RSTR | 0x00040000, MGADWG_RPL  | 0x000c0000,
   MGADWG_RSTR | 0x00020000, MGADWG_RSTR | 0x000a0000,
   MGADWG_RSTR | 0x00060000, MGADWG_RSTR | 0x000e0000,
   MGADWG_RSTR | 0x00010000, MGADWG_RSTR | 0x00090000,
   MGADWG_RSTR | 0x00050000, MGADWG_RSTR | 0x000d0000,
   MGADWG_RPL  | 0x00030000, MGADWG_RSTR | 0x000b0000,
   MGADWG_RSTR | 0x00070000, MGADWG_RPL  | 0x000f0000
};

static const struct {
    Bool dst_alpha;
    Bool src_alpha;
    CARD32 blend_cntl;
} mgaBlendOp[] = {
    {0, 0, MGA_SRC_ZERO                     | MGA_DST_ZERO},
    {0, 0, MGA_SRC_ONE                      | MGA_DST_ZERO},
    {0, 0, MGA_SRC_ZERO                     | MGA_DST_ONE},
    {0, 1, MGA_SRC_ONE                      | MGA_DST_ONE_MINUS_SRC_ALPHA},
    {1, 0, MGA_SRC_ONE_MINUS_DST_ALPHA      | MGA_DST_ONE},
    {1, 0, MGA_SRC_DST_ALPHA                | MGA_DST_ZERO},
    {0, 1, MGA_SRC_ZERO                     | MGA_DST_SRC_ALPHA},
    {1, 0, MGA_SRC_ONE_MINUS_DST_ALPHA      | MGA_DST_ZERO},
    {0, 1, MGA_SRC_ZERO                     | MGA_DST_ONE_MINUS_SRC_ALPHA},
    {1, 1, MGA_SRC_DST_ALPHA                | MGA_DST_ONE_MINUS_SRC_ALPHA},
    {1, 1, MGA_SRC_ONE_MINUS_DST_ALPHA      | MGA_DST_SRC_ALPHA},
    {1, 1, MGA_SRC_ONE_MINUS_DST_ALPHA      | MGA_DST_ONE_MINUS_SRC_ALPHA},
    {0, 0, MGA_SRC_ONE                      | MGA_DST_ONE},
};

static const struct {
    int fmt;
    CARD32 card_fmt;
} texformats[] = {
    { PICT_a8r8g8b8, MGA_TW32 },
    { PICT_x8r8g8b8, MGA_TW32 },
    { PICT_r5g6b5, MGA_TW16 },
    { PICT_a1r5g5b5, MGA_TW15 },
    { PICT_x1r5g5b5, MGA_TW15 },
    { PICT_a4r4g4b4, MGA_TW12 },
    { PICT_x4r4g4b4, MGA_TW12 },
    { PICT_a8, MGA_TW8A },
};

static CARD32
mgaGetPixmapPitch(PixmapPtr pPix)
{
    return exaGetPixmapPitch(pPix) / (pPix->drawable.bitsPerPixel >> 3);
}

static Bool
mgaSetup(ScreenPtr pScreen, int dest_bpp, int wait)
{
    ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
    MGAPtr pMga = pScrn->driverPrivate;
    unsigned int maccess = 0;
    static const unsigned int maccess_table[5] = {
        0, /* dummy */
        0, /*  8 bpp, PW8 */
        1, /* 16 bpp, PW16 */
        3, /* 24 bpp, PW24 */
        2, /* 32 bpp, PW32 */
    };

    WAITFIFO(wait + 4);

    /* Set the format of the destination pixmap.
     * Taken from MGAStormEngineInit().
     */
    maccess |= maccess_table[dest_bpp / 8];
    OUTREG(MGAREG_MACCESS, maccess);

    OUTREG(MGAREG_CXBNDRY, 0xffff0000);
    OUTREG(MGAREG_YTOP, 0x00000000);
    OUTREG(MGAREG_YBOT, 0x007fffff);

    return TRUE;
}

static void
mgaNoopDone(PixmapPtr pPixmap)
{
}

static Bool
mgaPrepareSolid(PixmapPtr pPixmap, int alu, Pixel planemask, Pixel fg)
{
    PMGA(pPixmap);
    QUIESCE_DMA(pPixmap);

    /* We must pad planemask and fg depending on the format of the
     * destination pixmap
     */
    switch (pPixmap->drawable.bitsPerPixel) {
    case 8:
        fg |= fg << 8;
        planemask |= planemask << 8;
        /* fall through */
    case 16:
        fg |= fg << 16;
        planemask |= planemask << 16;
        break;
    }

    pMga->FilledRectCMD = MGADWG_TRAP | MGADWG_SOLID | MGADWG_ARZERO |
                          MGADWG_SGNZERO | MGADWG_SHIFTZERO | mgaRop[alu];

    mgaSetup(pPixmap->drawable.pScreen, pPixmap->drawable.bitsPerPixel, 5);

    OUTREG(MGAREG_PITCH, mgaGetPixmapPitch(pPixmap));
    OUTREG(MGAREG_DSTORG, exaGetPixmapOffset(pPixmap));
    OUTREG(MGAREG_FCOL, fg);
    OUTREG(MGAREG_PLNWT, planemask);
    OUTREG(MGAREG_DWGCTL, pMga->FilledRectCMD);

    return TRUE;
}

static void
mgaSolid(PixmapPtr pPixmap, int x1, int y1, int x2, int y2)
{
    PMGA(pPixmap);

    WAITFIFO(2);
    OUTREG(MGAREG_FXBNDRY, (x2 << 16) | (x1 & 0xffff));
    OUTREG(MGAREG_YDSTLEN | MGAREG_EXEC, (y1 << 16) | (y2 - y1));
}

static Bool
mgaPrepareCopy(PixmapPtr pSrc, PixmapPtr pDst, int xdir, int ydir, int alu,
           Pixel planemask)
{
    PMGA(pSrc);
    int blit_direction = 0;
    int dwgctl = 0;

    QUIESCE_DMA(pSrc);

    DEBUG_MSG(("s: %x@%x d: %x@%x xdir %d ydir %d alu %d pm %d\n",
           exaGetPixmapOffset(pSrc), exaGetPixmapPitch(pSrc),
           exaGetPixmapOffset(pDst), exaGetPixmapPitch(pDst),
           xdir, ydir, alu, planemask));

    if (xdir < 0)
        blit_direction |= BLIT_LEFT;

    if (ydir < 0)
        blit_direction |= BLIT_UP;

    pMga->BltScanDirection = blit_direction;

    dwgctl = mgaRop[alu] | MGADWG_SHIFTZERO | MGADWG_BITBLT | MGADWG_BFCOL;
    pMga->src_pitch = mgaGetPixmapPitch(pSrc);

    mgaSetup(pSrc->drawable.pScreen, pDst->drawable.bitsPerPixel, 7);
    OUTREG(MGAREG_PITCH, mgaGetPixmapPitch(pDst));
    OUTREG(MGAREG_SRCORG, exaGetPixmapOffset(pSrc));
    OUTREG(MGAREG_DSTORG, exaGetPixmapOffset(pDst));
    OUTREG(MGAREG_DWGCTL, dwgctl);
    OUTREG(MGAREG_SGN, blit_direction);
    OUTREG(MGAREG_PLNWT, planemask);
    OUTREG(MGAREG_AR5, (ydir < -0 ? -1 : 1) * pMga->src_pitch);

    return TRUE;
}

static void
mgaCopy(PixmapPtr pDst, int srcx, int srcy, int dstx, int dsty, int w, int h)
{
    PMGA(pDst);
    int start, end;

    DEBUG_MSG(("    %d,%d -> %d,%d %dx%d\n", srcx, srcy, dstx,dsty, w, h));

    w--;

    if (pMga->BltScanDirection & BLIT_UP) {
        srcy += h - 1;
        dsty += h - 1;
    }

    start = end = srcy * pMga->src_pitch + srcx;

    if (pMga->BltScanDirection & BLIT_LEFT)
        start += w;
    else
        end += w;

    DEBUG_MSG(("        end %d start %d dstx %d dsty %d w %d h %d\n",
              end, start, dstx, dsty, w, h));
    WAITFIFO(4);
    OUTREG(MGAREG_AR0, end);
    OUTREG(MGAREG_AR3, start);
    OUTREG(MGAREG_FXBNDRY, ((dstx + w) << 16) | (dstx & 0xffff));
    OUTREG(MGAREG_YDSTLEN | MGAREG_EXEC, (dsty << 16) | h);
}

static int
MGA_LOG2(int val)
{
    int ret = 0;

    if (val == 1)
        return 0;

    while (val >> ret)
        ret++;

    return ((1 << (ret - 1)) == val) ? (ret - 1) : ret;
}

static Bool
mgaCheckSourceTexture(int tmu, PicturePtr pPict)
{
    int w = pPict->pDrawable->width;
    int h = pPict->pDrawable->height;
    int i;
    CARD32 texctl = 0;

    if ((w > 2047) || (h > 2047)){
        DEBUG_MSG(("Picture w/h too large (%dx%d)\n", w, h));
        return FALSE;
    }

    for (i = 0; i < sizeof(texformats) / sizeof(texformats[0]); i++) {
        if (texformats[i].fmt == pPict->format) {
            texctl = texformats[i].card_fmt;
            break;
        }
    }

    if (texctl == 0) {
        DEBUG_MSG(("Unsupported picture format 0x%x\n", pPict->format));
        return FALSE;
    }

    if (pPict->repeat && ((w & (w - 1)) != 0 || (h & (h - 1)) != 0)) {
        DEBUG_MSG(("NPOT repeat unsupported (%dx%d)\n", w, h));
        return FALSE;
    }

    if (pPict->filter != PictFilterNearest &&
        pPict->filter != PictFilterBilinear) {
        DEBUG_MSG(("Unsupported filter 0x%x\n", pPict->filter));
        return FALSE;
    }

    return TRUE;
}

static Bool
mgaCheckComposite(int op, PicturePtr pSrcPict, PicturePtr pMaskPict,
                  PicturePtr pDstPict)
{
    if (op >= sizeof(mgaBlendOp) / sizeof(mgaBlendOp[0])) {
        DEBUG_MSG(("unsupported op %x\n", op));
        return FALSE;
    }

    if (!mgaCheckSourceTexture(0, pSrcPict))
        return FALSE;

    if (pMaskPict) {
        if (PICT_FORMAT_A(pMaskPict->format) == 0) {
            DEBUG_MSG(("Mask without alpha unsupported\n"));
            return FALSE;
        }

        if (!mgaCheckSourceTexture(1, pMaskPict))
            return FALSE;

        if (pMaskPict->componentAlpha) {
            DEBUG_MSG(("Component alpha unsupported\n"));
            return FALSE;
        }
    }

    switch (pDstPict->format) {
    case PICT_a8:
        DEBUG_MSG(("render to A8 unsupported\n"));
        return FALSE;
    default:
        break;
    }

    return TRUE;
}

static Bool
PrepareSourceTexture(int tmu, PicturePtr pSrcPicture, PixmapPtr pSrc)
{
    PMGA(pSrc);
    int i;
    int pitch = mgaGetPixmapPitch(pSrc);
    int w = pSrc->drawable.width;
    int h = pSrc->drawable.height;
    int w_log2 = MGA_LOG2(w);
    int h_log2 = MGA_LOG2(h);

    int texctl = MGA_PITCHLIN | ((pitch & (2048 - 1)) << 9) |
                 MGA_CLAMPUV | MGA_NOPERSPECTIVE;
    int flags = 0;
    int texctl2 = MGA_G400_TC2_MAGIC | flags;

    for (i = 0; i < sizeof(texformats) / sizeof(texformats[0]); i++) {
        if (texformats[i].fmt == pSrcPicture->format) {
            texctl |= texformats[i].card_fmt;
            break;
        }
    }

    if (PICT_FORMAT_A(pSrcPicture->format) != 0) {
        texctl |= MGA_TAKEY;
    } else {
        texctl |= MGA_TAMASK | MGA_TAKEY;
    }

    if (pSrcPicture->repeat) {
        texctl &= ~MGA_CLAMPUV;
    }

    if (tmu == 1)
        texctl2 |= MGA_TC2_DUALTEX | MGA_TC2_SELECT_TMU1 | flags;

    WAITFIFO(6);
    OUTREG(MGAREG_TEXCTL2, texctl2);
    OUTREG(MGAREG_TEXCTL, texctl);
    /* Source (texture) address + pitch */
    OUTREG(MGAREG_TEXORG, exaGetPixmapOffset(pSrc));
    OUTREG(MGAREG_TEXWIDTH, (w-1)<<18 | ((8-w_log2)&63)<<9 | w_log2);
    OUTREG(MGAREG_TEXHEIGHT, (h-1)<<18 | ((8-h_log2)&63)<<9 | h_log2);
    /* Set blit filtering flags */
    if (pSrcPicture->filter == PictFilterBilinear)
        OUTREG(MGAREG_TEXFILTER, (0x10<<21) | MGA_MAG_BILIN | MGA_MIN_BILIN);
    else
        OUTREG(MGAREG_TEXFILTER, (0x10<<21) | MGA_MAG_NRST | MGA_MIN_NRST);

    if (tmu == 1) {
        WAITFIFO(1);
        OUTREG(MGAREG_TEXCTL2, MGA_G400_TC2_MAGIC | MGA_TC2_DUALTEX | flags);
    }

    return TRUE;
}

/*
 *  The formals params are the elements of the following matrix:
 *
 *     Dest            Transform             Src
 *    coords                                coords
 *   / Xdst \   / X_incx X_incy X_init \   / Xsrc \
 *   | Ydst | = | Y_incx Y_incy Y_init | x | Ysrc |
 *   \  1   /   \ H_incx H_incy H_init /   \  1   /
 *
 * matrix elements are 32bits fixed points (16.16)
 * mga_fx_* is the size of the fixed point for the TMU
 */
static void
setTMIncrementsRegs(PixmapPtr pPix, int X_incx, int X_incy, int X_init,
                    int Y_incx, int Y_incy, int Y_init,
                    int H_incx, int H_incy, int H_init,
                    int mga_fx_width_size, int mga_fx_height_size)
{
    PMGA(pPix);

    int decalw = mga_fx_width_size - 16;
    int decalh = mga_fx_height_size - 16;

    /* Convert 16 bits fixpoint -> MGA variable size fixpoint */
    if (decalw >= 0) {
        X_incx = X_incx << decalw;
        X_incy = X_incy << decalw;
        X_init = X_init << decalw;
    } else {
        decalw =- decalw;
        X_incx = X_incx >> decalw;
        X_incy = X_incy >> decalw;
        X_init = X_init >> decalw;
    }

    /* Convert 16 bits fixpoint -> MGA variable size fixpoint */
    if (decalh >= 0) {
        Y_incx = Y_incx << decalh;
        Y_incy = Y_incy << decalh;
        Y_init = Y_init << decalh;
    } else {
        decalh =- decalh;
        Y_incx = Y_incx >> decalh;
        Y_incy = Y_incy >> decalh;
        Y_init = Y_init >> decalh;
    }

    /* Set TM registers */
    WAITFIFO(9);
    OUTREG(MGAREG_TMR0, X_incx);
    OUTREG(MGAREG_TMR1, Y_incx);
    OUTREG(MGAREG_TMR2, X_incy);
    OUTREG(MGAREG_TMR3, Y_incy);
    OUTREG(MGAREG_TMR4, H_incx);
    OUTREG(MGAREG_TMR5, H_incy);
    OUTREG(MGAREG_TMR6, X_init);
    OUTREG(MGAREG_TMR7, Y_init);
    OUTREG(MGAREG_TMR8, H_init);
}

/* XXX these look like magic */
#define C_ARG1_CUR              0x0
#define C_ARG1_ALPHA            MGA_TDS_COLOR_ARG1_REPLICATEALPHA
#define C_ARG2_DIFFUSE          MGA_TDS_COLOR_ARG2_DIFFUSE
#define C_ARG2_FCOL             MGA_TDS_COLOR_ARG2_FCOL
#define C_ARG2_PREV             MGA_TDS_COLOR_ARG2_PREVSTAGE
#define C_ARG1_INV              MGA_TDS_COLOR_ARG1_INV
#define C_ARG2_INV              MGA_TDS_COLOR_ARG2_INV
#define COLOR_MUL               MGA_TDS_COLOR_SEL_MUL
#define COLOR_ARG1              MGA_TDS_COLOR_SEL_ARG1
#define COLOR_ARG2              MGA_TDS_COLOR_SEL_ARG2
#define A_ARG1_CUR              0x0
#define A_ARG2_IGN              A_ARG2_DIFFUSE
#define A_ARG2_FCOL             MGA_TDS_ALPHA_ARG2_FCOL
#define A_ARG2_DIFFUSE          MGA_TDS_ALPHA_ARG2_DIFFUSE
#define A_ARG2_PREV             MGA_TDS_ALPHA_ARG2_PREVSTAGE
#define ALPHA_MUL               MGA_TDS_ALPHA_SEL_MUL
#define ALPHA_ARG1              MGA_TDS_ALPHA_SEL_ARG1
#define ALPHA_ARG2              MGA_TDS_ALPHA_SEL_ARG2

static Bool
mgaPrepareComposite(int op, PicturePtr pSrcPict, PicturePtr pMaskPict,
                    PicturePtr pDstPict, PixmapPtr pSrc, PixmapPtr pMask,
                    PixmapPtr pDst)
{
    PMGA(pDst);
    CARD32 ds0 = 0, ds1 = 0, cmd, blendcntl;

    mgaSetup(pSrc->drawable.pScreen, pDst->drawable.bitsPerPixel, 3);
    OUTREG(MGAREG_FCOL, 0xff000000);
    OUTREG(MGAREG_DSTORG, exaGetPixmapOffset(pDst));
    OUTREG(MGAREG_PITCH, mgaGetPixmapPitch(pDst));

    if (!PrepareSourceTexture(0, pSrcPict, pSrc))
        return FALSE;

    if (pMask && !PrepareSourceTexture(1, pMaskPict, pMask))
        return FALSE;

    if (pSrcPict->format == PICT_a8) {
    /* C = 0        A = As */
    /* MGA HW: A8 format makes RGB white. We use FCOL for the black
     * If FCOL was not 0, it would have been be premultiplied (RENDER)
     * color component would have been:
     *   C_ARG1_ALPHA | C_ARG2_FCOL | COLOR_MUL
     */
        ds0 = C_ARG2_FCOL | COLOR_ARG2 |
              A_ARG1_CUR | ALPHA_ARG1;

        /* MGA HW: TMU1 must be enabled when DUALSTAGE0 contains something */
        if (!pMask) {
            if (!PrepareSourceTexture(1, pSrcPict, pSrc))
                return FALSE;
            ds1 = C_ARG2_PREV | COLOR_ARG2 |
                  A_ARG2_PREV | ALPHA_ARG2;
        }
    } else {
        /* C = Cs       A = As */
        ds0 = C_ARG1_CUR | COLOR_ARG1 |
              A_ARG1_CUR | ALPHA_ARG1;
    }

    if (pMask) {
        /* As or Am might be NULL. in this case we don't multiply because,
         * the alpha component holds garbage.
         */
        int color, alpha;

        if (PICT_FORMAT_A(pMaskPict->format) == 0) {
            /* C = Cs */
            color = C_ARG2_PREV | COLOR_ARG2;
        } else {
            /* C = Am * Cs */
            color = C_ARG1_ALPHA | C_ARG2_PREV | COLOR_MUL;
        }

        if (PICT_FORMAT_A(pMaskPict->format) == 0) {
            /* A = As */
            alpha = A_ARG2_PREV | ALPHA_ARG2;
        } else if (PICT_FORMAT_A(pSrcPict->format) == 0) {
            /* A = Am */
            alpha = A_ARG1_CUR | ALPHA_ARG1;
        } else {
            /* A = Am * As */
            alpha = A_ARG1_CUR | A_ARG2_PREV | ALPHA_MUL;
        }

        ds1 = color | alpha;
    }

    cmd = MGADWG_TEXTURE_TRAP | MGADWG_I | 0x000c0000 |
          MGADWG_SHIFTZERO | MGADWG_SGNZERO | MGADWG_ARZERO;
    blendcntl = mgaBlendOp[op].blend_cntl;

    if (PICT_FORMAT_A(pDstPict->format) == 0 && mgaBlendOp[op].dst_alpha) {
        if ((blendcntl & MGA_SRC_BLEND_MASK) == MGA_SRC_DST_ALPHA)
            blendcntl = (blendcntl & ~MGA_SRC_BLEND_MASK) | MGA_SRC_ONE;
        else if ((blendcntl&MGA_SRC_BLEND_MASK) == MGA_SRC_ONE_MINUS_DST_ALPHA)
            blendcntl = (blendcntl & ~MGA_SRC_BLEND_MASK) | MGA_SRC_ZERO;
    }

    WAITFIFO(4);
    OUTREG(MGAREG_TDUALSTAGE0, ds0);
    OUTREG(MGAREG_TDUALSTAGE1, ds1);
    OUTREG(MGAREG_DWGCTL, cmd);
    OUTREG(MGAREG_ALPHACTRL, MGA_ALPHACHANNEL | blendcntl);

    pMga->currentSrcPicture = pSrcPict;
    pMga->currentMaskPicture = pMaskPict;
    pMga->currentSrc = pSrc;
    pMga->currentMask = pMask;

    pMga->src_w2 = MGA_LOG2 (pSrc->drawable.width);
    pMga->src_h2 = MGA_LOG2 (pSrc->drawable.height);

    if (pMask) {
        pMga->mask_w2 = MGA_LOG2 (pMask->drawable.width);
        pMga->mask_h2 = MGA_LOG2 (pMask->drawable.height);
    }

    return TRUE;
}

static void
mgaComposite(PixmapPtr pDst, int srcx, int srcy, int maskx, int masky,
             int dstx, int dsty, int w, int h)
{
    PMGA(pDst);
    PictTransformPtr t;

    srcx %= pMga->currentSrc->drawable.width;
    srcy %= pMga->currentSrc->drawable.height;

    if (pMga->currentMask) {
        maskx %= pMga->currentMask->drawable.width;
        maskx %= pMga->currentMask->drawable.height;
    }

    t = pMga->currentSrcPicture->transform;

    if (t)
        setTMIncrementsRegs(pMga->currentSrc,
                            t->matrix[0][0],
                            t->matrix[0][1],
                            t->matrix[0][2] + (srcx << 16),
                            t->matrix[1][0],
                            t->matrix[1][1],
                            t->matrix[1][2] + (srcy << 16),
                            t->matrix[2][0],
                            t->matrix[2][1],
                            t->matrix[2][2],
                            20 - pMga->src_w2,
                            20 - pMga->src_h2);
    else
        setTMIncrementsRegs(pMga->currentSrc,
                            1 << 16, 0, srcx << 16,
                            0, 1 << 16, srcy << 16,
                            0, 0, 0x10000,
                            20 - pMga->src_w2,
                            20 - pMga->src_h2);

    if (pMga->currentMask) {
        WAITFIFO(1);
        OUTREG(MGAREG_TEXCTL2,
        MGA_G400_TC2_MAGIC | MGA_TC2_DUALTEX | MGA_TC2_SELECT_TMU1);

        t = pMga->currentMaskPicture->transform;

        if (t)
            setTMIncrementsRegs(pMga->currentMask,
                                t->matrix[0][0],
                                t->matrix[0][1],
                                t->matrix[0][2] + (maskx << 16),
                                t->matrix[1][0],
                                t->matrix[1][1],
                                t->matrix[1][2] + (masky << 16),
                                t->matrix[2][0],
                                t->matrix[2][1],
                                t->matrix[2][2],
                                20 - pMga->mask_w2,
                                20 - pMga->mask_h2);
        else
            setTMIncrementsRegs(pMga->currentMask,
                                1 << 16, 0, maskx << 16,
                                0, 1 << 16, masky << 16,
                                0, 0, 0x10000,
                                20 - pMga->mask_w2,
                                20 - pMga->mask_h2);

        WAITFIFO(1);
        OUTREG(MGAREG_TEXCTL2, MGA_G400_TC2_MAGIC | MGA_TC2_DUALTEX);
    }

    WAITFIFO(2);
    OUTREG(MGAREG_FXBNDRY, ((dstx + w) << 16) | (dstx & 0xffff));
    OUTREG(MGAREG_YDSTLEN | MGAREG_EXEC, (dsty << 16) | (h & 0xffff));
}

static Bool
mgaUploadToScreen(PixmapPtr pDst, int x, int y, int w, int h, char *src,
          int src_pitch)
{
    return FALSE;
}

static Bool
mgaDownloadFromScreen(PixmapPtr pSrc, int x, int y, int w, int h, char *dst,
              int dst_pitch)
{
    return FALSE;
}

static void
mgaWaitMarker(ScreenPtr pScreen, int marker)
{
    ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
    MGAPtr pMga = pScrn->driverPrivate;

    WAITFIFO(1);

    OUTREG(MGAREG_CACHEFLUSH, 0);

    /* wait until the "drawing engine busy" bit is unset */
    while (INREG (MGAREG_Status) & 0x10000);
}

Bool
mgaExaInit(ScreenPtr pScreen)
{
    ExaDriverPtr pExa;
    ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
    MGAPtr pMga = pScrn->driverPrivate;

    pExa = exaDriverAlloc();
    if (!pExa) {
        pMga->NoAccel = TRUE;
        return FALSE;
    }

    pMga->ExaDriver = pExa;

    pExa->exa_major = EXA_VERSION_MAJOR;
    pExa->exa_minor = EXA_VERSION_MINOR;

    pExa->flags = EXA_OFFSCREEN_PIXMAPS | EXA_OFFSCREEN_ALIGN_POT;
    pExa->memoryBase = pMga->FbStart;
    pExa->memorySize = pMga->FbUsableSize - 4096;
    pExa->offScreenBase = (pScrn->virtualX * pScrn->virtualY *
                           pScrn->bitsPerPixel/8) + 4096;

    xf86DrvMsg(pScrn->scrnIndex, X_INFO, "X %d Y %d bpp %d\n",
               pScrn->virtualX, pScrn->virtualY, pScrn->bitsPerPixel);
    xf86DrvMsg(pScrn->scrnIndex, X_INFO, "Start at %p, size %x, osb %x\n",
               pExa->memoryBase, pExa->memorySize, pExa->offScreenBase);

    /* In PW24 mode, we need to align to "3 64-bytes" */
    pExa->pixmapOffsetAlign = 192;

    /* Pitch alignment is in sets of 32 pixels, and we need to cover
     * 32bpp, so it's 128 bytes.
     */
    pExa->pixmapPitchAlign = 128;

    pExa->maxX = 2048;
    pExa->maxY = 2048;

    pExa->WaitMarker = mgaWaitMarker;
    pExa->PrepareSolid = mgaPrepareSolid;
    pExa->Solid = mgaSolid;
    pExa->DoneSolid = mgaNoopDone;
    pExa->PrepareCopy = mgaPrepareCopy;
    pExa->Copy = mgaCopy;
    pExa->DoneCopy = mgaNoopDone;

    /* FIXME
     * if (pMga->Chipset >= PCI_CHIP_MGAG400) {
     */
    if (0) {
        pExa->CheckComposite = mgaCheckComposite;
        pExa->PrepareComposite = mgaPrepareComposite;
        pExa->Composite = mgaComposite;
        pExa->DoneComposite = mgaNoopDone;
    }

    if (0) {
        pExa->DownloadFromScreen = mgaDownloadFromScreen;
        pExa->UploadToScreen = mgaUploadToScreen;
    }

    /* XXX fill in the XAA setup code here */
#if 0
    if(pMga->HasSDRAM) {
        pMga->Atype = pMga->AtypeNoBLK = atype_noblk;
    } else {
        pMga->Atype = atype;
        pMga->AtypeNoBLK = atype_noblk;
    }
#endif

    return exaDriverInit(pScreen, pExa);
}
