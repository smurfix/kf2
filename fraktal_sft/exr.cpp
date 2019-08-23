/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2019 Claude Heiland-Allen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include <ImfNamespace.h>
#include <ImfOutputFile.h>
#include <ImfIntAttribute.h>
#include <ImfStringAttribute.h>
#include <ImfChannelList.h>
#include <ImfArray.h>
#include <ImfPreviewImage.h>

#include <iostream>

#include "fraktal_sft.h"
#include "exr.h"

namespace IMF = OPENEXR_IMF_NAMESPACE;
using namespace IMF;

extern int SaveEXR
( const std::string &filename
, const unsigned char *Data
, int nWidth
, int nHeight
, int nColors
, const std::string &comment
, int maxiter
, int arrWidth
, int arrHeight
, const int *count
, const float *trans
, const float *de
)
{
  try
  {
    // prepare arrays with proper format
    uint32_t *n = new uint32_t[size_t(arrWidth) * arrHeight];
    float *nf = new float[size_t(arrWidth) * arrHeight];
    // TODO parallelize
    for (size_t k = 0; k < size_t(arrWidth) * arrHeight; ++k)
    {
      if (PIXEL_UNEVALUATED == count[k] || GET_TRANS_GLITCH(trans[k]))
      {
        // not calculated, or glitched
        n[k] = 0;
        nf[k] = 0.0f;
      }
      else if (count[k] == maxiter)
      {
        // unescaped
        n[k] = 0xFFffFFffU;
        nf[k] = 0.0f;
      }
      else
      {
        n[k] = count[k];
        nf[k] = 1.0f - trans[k];
      }
    }
    // prepare preview image
    Header header(arrWidth, arrHeight);
    Array2D<PreviewRgba> preview;
    preview.resizeErase(nHeight, nWidth);
    // TODO parallelize
    for (int j = 0; j < nHeight; ++j)
    {
      for (int i = 0; i < nWidth; ++i)
      {
        size_t k = (j * size_t(nWidth) + i) * 3;
        PreviewRgba &o = preview[j][i];
        o.r = Data[k + 0];
        o.g = Data[k + 1];
        o.b = Data[k + 2];
        o.a = 255;
      }
    }
    header.setPreviewImage(PreviewImage(nWidth, nHeight, &preview[0][0]));
    // insert metadata
    header.insert("KallesFraktaler2+", StringAttribute(comment));
    header.insert("Iterations", IntAttribute(maxiter));
    // write image
    const half *rgb = g_SFT.GetArrayHalfColour();
    if (rgb)
    {
      header.channels().insert("R", Channel(IMF::HALF));
      header.channels().insert("G", Channel(IMF::HALF));
      header.channels().insert("B", Channel(IMF::HALF));
    }
    header.channels().insert("N",  Channel(IMF::UINT));
    header.channels().insert("NF", Channel(IMF::FLOAT));
    if (de) header.channels().insert("DE", Channel(IMF::FLOAT));
    OutputFile of(filename.c_str(), header);
    FrameBuffer fb;
    if (rgb)
    {
      // [y][x]
      size_t row = g_SFT.GetArrayHalfColourStride();
      fb.insert("R", Slice(IMF::HALF, (char *) (rgb + 0), sizeof(*rgb) * 3, sizeof(*rgb) * row));
      fb.insert("G", Slice(IMF::HALF, (char *) (rgb + 1), sizeof(*rgb) * 3, sizeof(*rgb) * row));
      fb.insert("B", Slice(IMF::HALF, (char *) (rgb + 2), sizeof(*rgb) * 3, sizeof(*rgb) * row));
    }
    else
    {
      std::cerr << "no rgb" << std::endl;
    }
    // [x][y]
    fb.insert("N",  Slice(IMF::UINT,  (char *) n,  sizeof(*n ) * arrHeight, sizeof(*n ) * 1));
    fb.insert("NF", Slice(IMF::FLOAT, (char *) nf, sizeof(*nf) * arrHeight, sizeof(*nf) * 1));
    if (de) fb.insert("DE", Slice(IMF::FLOAT, (char *) de, sizeof(*de) * arrHeight, sizeof(*de) * 1));
    of.setFrameBuffer(fb);
    of.writePixels(arrHeight);
    delete [] nf;
    delete [] n;
    return 1;
  }
  catch (...)
  {
    return 0;
  }
}
