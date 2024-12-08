/*
* Copyright 2021-2024 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Luon runtime library.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* This file may be used under the terms of the GNU Lesser
* General Public License version 2.1 or version 3 as published by the Free
* Software Foundation and appearing in the file LICENSE.LGPLv21 and
* LICENSE.LGPLv3 included in the packaging of this file. Please review the
* following information to ensure the GNU Lesser General Public License
* requirements will be met: https://www.gnu.org/licenses/lgpl.html and
* http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/
// Adopted from Smalltalk StDisplay.cpp

#include <stdint.h>
#include <assert.h>

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

typedef unsigned char BOOL;

typedef struct Bitmap {
    uint16_t pixWidth, pixHeight;
    uint16_t wordLen;
    uint8_t* buf;
}Bitmap;

typedef struct Context
{
    const Bitmap* sourceBits;
    Bitmap* destBits;
    const Bitmap* halftoneBits;
    int16_t combinationRule;
    int16_t destX, clipX, clipWidth, sourceX, width;
    int16_t destY, clipY, clipHeight, sourceY, height;
} Context;

static Context* ctx;
static int16_t sourceRaster;
static int16_t destRaster;
static int16_t skew, nWords, vDir, hDir;
static uint16_t mask1, mask2, skewMask;
static int16_t sx, sy, dx, dy, w, h; // pixel
static int16_t sourceIndex, destIndex, sourceDelta, destDelta;
BOOL preload;
static int16_t RightMasks[] = {
    0, 0x1 , 0x3 , 0x7 , 0xf ,
       0x1f , 0x3f , 0x7f , 0xff ,
       0x1ff , 0x3ff , 0x7ff , 0xfff ,
       0x1fff , 0x3fff , 0x7fff , (int16_t)0xffff
};
static const int16_t AllOnes = 0xffff;


static void clipRange()
{
    // set sx/y, dx/y, w and h so that dest doesn't exceed clipping range and
    // source only covers what needed by clipped dest
    if( ctx->destX >= ctx->clipX )
    {
        sx = ctx->sourceX;
        dx = ctx->destX;
        w = ctx->width;
    }else
    {
        sx = ctx->sourceX + ( ctx->clipX - ctx->destX );
        w = ctx->width - ( ctx->clipX - ctx->destX );
        dx = ctx->clipX;
    }
    if( ( dx + w ) > ( ctx->clipX + ctx->clipWidth ) )
        w = w - ( ( dx + w ) - ( ctx->clipX + ctx->clipWidth ) );
    if( ctx->destY >= ctx->clipY )
    {
        sy = ctx->sourceY;
        dy = ctx->destY;
        h = ctx->height;
    }else
    {
        sy = ctx->sourceY + ctx->clipY - ctx->destY;
        h = ctx->height - ctx->clipY + ctx->destY;
        dy = ctx->clipY;
    }
    if( ( dy + h ) > ( ctx->clipY + ctx->clipHeight ) )
        h = h - ( ( dy + h ) - ( ctx->clipY + ctx->clipHeight ) );

    if( ctx->sourceBits == 0 )
        return;

    if( sx < 0 )
    {
        dx = dx - sx;
        w = w + sx;
        sx = 0;
    }
    if( ( sx + w ) > ctx->sourceBits->pixWidth )
        w = w - ( sx + w - ctx->sourceBits->pixWidth );
    if( sy < 0 )
    {
        dy = dy - sy;
        h = h + sy;
        sy = 0;
    }
    if( ( sy + h ) > ctx->sourceBits->pixHeight )
        h = h - ( sy + h - ctx->sourceBits->pixHeight );

}

static void computeMasks()
{
    // destBits = destForm bits
    destRaster = ( ( ctx->destBits->pixWidth - 1 ) / 16 ) + 1;
    if( ctx->sourceBits != 0 )
        sourceRaster = ( ( ctx->sourceBits->pixWidth - 1 ) / 16 ) + 1;
    else
        sourceRaster = 0;
    // halftoneBits = halftoneForm bits
    skew = ( sx - dx ) & 15;
    const uint16_t startBits = 16 - ( dx & 15 );
    mask1 = RightMasks[ startBits /* + 1 */ ]; // ST array index starts with 1
    const uint16_t endBits = 15 - ( ( dx + w - 1 ) & 15 );
    mask2 = ~RightMasks[ endBits /* + 1 */ ];
    skewMask = skew == 0 ? 0 : RightMasks[ 16 - skew /* + 1 */ ];
    if( w < startBits )
    {
        mask1 = mask1 & mask2;
        mask2 = 0;
        nWords = 1;
    }else
        // nWords = ( w - startBits - 1 ) / 16 + 2; // BB error, doesn't work
        // fix found in https://github.com/dbanay/Smalltalk/blob/master/src/bitblt.cpp
        // ERROR dbanay : nWords <-  (w - startBits + 15) // 16 + 1 for False case"
        nWords = ( w - startBits + 15) / 16 + 1;
}

static void checkOverlap()
{
    hDir = vDir = 1;
    if( ctx->sourceBits && ctx->destBits && ctx->sourceBits->buf == ctx->destBits->buf && dy >= sy )
    {
        if( dy > sy )
        {
            vDir = -1;
            sy = sy + h - 1;
            dy = dy + h - 1;
        }else if( dx > sx )
        {
            hDir = -1;
            sx = sx + w - 1;
            dx = dx + w - 1;
            skewMask = ~skewMask;
            int16_t t = mask1;
            mask1 = mask2;
            mask2 = t;
        }
    }
}

static void calculateOffsets()
{
    preload = ( ctx->sourceBits != 0 && skew != 0 && skew <= ( sx & 15 ) );
    if( hDir < 0 )
        preload = preload == 0;
    sourceIndex = sy * sourceRaster + ( sx / 16 );
    destIndex = dy * destRaster + ( dx / 16 );
    sourceDelta = ( sourceRaster * vDir ) - ( (nWords + ( preload ? 1 : 0 ) ) * hDir );
    destDelta = ( destRaster * vDir ) - ( nWords * hDir );
}

static inline uint16_t readU16( const uint8_t* data, int off )
{
    return ( ((uint8_t)data[off]) << 8 ) + ((uint8_t)data[off+1] );
}

static inline uint16_t wordAt(const Bitmap* bm, uint16_t i)
{
    i--; // Smalltalk array indexes start with 1
    assert( i < bm->wordLen );
    return readU16( bm->buf, i * 2 );
}

static uint16_t merge(uint16_t combinationRule, uint16_t source, uint16_t destination)
{
    switch( combinationRule )
    {
    case 0:
        return 0;
    case 1:
        return source & destination;
    case 2:
        return source & ~destination;
    case 3:
        return source;
    case 4:
        return ~source & destination;
    case 5:
        return destination;
    case 6:
        return source ^ destination;
    case 7:
        return source | destination;
    case 8:
        return ~source & ~destination;
    case 9:
        return ~source ^ destination;
    case 10:
        return ~destination;
    case 11:
        return source | ~destination;
    case 12:
        return ~source;
    case 13:
        return ~source | destination;
    case 14:
        return ~source | ~destination;
    case 15:
        return AllOnes;
    default:
        assert( 0 );
        break;
    }
    return 0;
}

static inline void writeU16( uint8_t* data, int off, uint16_t val )
{
    data[off] = ( val >> 8 ) & 0xff;
    data[off+1] = val & 0xff;
}

static inline void wordAtPut(Bitmap* bm, uint16_t i, uint16_t v)
{
    assert( i <= bm->wordLen );
    i--;
    writeU16( bm->buf, i * 2, v );
}

static void copyLoop()
{
    uint16_t prevWord, thisWord, skewWord, mergeMask,
            halftoneWord, mergeWord, word;
    int i;
    for( i = 1; i <= h; i++ )
    {
        if( ctx->halftoneBits != 0 )
        {
            halftoneWord = wordAt(ctx->halftoneBits, 1 + ( dy & 15 ) );
            dy = dy + vDir;
        }else
            halftoneWord = AllOnes;
        skewWord = halftoneWord;
        if( preload && ctx->sourceBits != 0 )
        {
            prevWord = wordAt(ctx->sourceBits, sourceIndex + 1 );
            sourceIndex = sourceIndex + hDir;
        }else
            prevWord = 0;
        mergeMask = mask1;
        for( word = 1; word <= nWords; word++ )
        {
            if( ctx->sourceBits != 0 )
            {
                prevWord = prevWord & skewMask;
                if( word <= sourceRaster && sourceIndex >= 0 && sourceIndex < ctx->sourceBits->wordLen )
                    thisWord = wordAt(ctx->sourceBits, sourceIndex + 1 );
                else
                    thisWord = 0;
                skewWord = prevWord | ( thisWord & ~skewMask );
                prevWord = thisWord;
                // does not work:
                // skewWord = ObjectMemory2::bitShift( skewWord, skew ) | ObjectMemory2::bitShift( skewWord, skew - 16 );
                skewWord = ( skewWord << skew ) | ( skewWord >> -( skew - 16 ) );
            }
            if( destIndex >= ctx->destBits->wordLen )
                return;
            const uint16_t destWord =  wordAt(ctx->destBits, destIndex + 1 );
            mergeWord = merge( ctx->combinationRule, skewWord & halftoneWord, destWord );
            wordAtPut(ctx->destBits, destIndex + 1, ( mergeMask & mergeWord ) | ( ~mergeMask & destWord ) );
            sourceIndex = sourceIndex + hDir;
            destIndex = destIndex + hDir;
            if( word == ( nWords - 1 ) )
                mergeMask = mask2;
            else
                mergeMask = AllOnes;
        }
        sourceIndex = sourceIndex + sourceDelta;
        destIndex = destIndex + destDelta;
    }
}

DllExport void BitBlt_copyBits(Context* c)
{
    ctx = c;
    clipRange();
    if( w <= 0 || h <= 0 )
        return;
    computeMasks();
    checkOverlap();
    calculateOffsets();
    copyLoop();
}
