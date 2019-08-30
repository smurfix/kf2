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
#include <ImfMultiPartInputFile.h>
#include <ImfInputFile.h>
#include <ImfOutputFile.h>
#include <ImfHeader.h>
#include <ImfIntAttribute.h>
#include <ImfStringAttribute.h>
#include <ImfChannelList.h>
#include <ImfArray.h>
#include <ImfFrameBuffer.h>
#include <ImathBox.h>
#include <ImfPreviewImage.h>

#include <iostream>

#include "fraktal_sft.h"
#include "exr.h"

namespace IMF = OPENEXR_IMF_NAMESPACE;
using namespace IMF;
using namespace IMATH_NAMESPACE;

const char magic[] = "KallesFraktaler2+";
const int BIAS = 1024;

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
, const float *rawde
)
{
  try
  {
    // prepare arrays with proper format
    uint32_t *n = new uint32_t[size_t(arrWidth) * arrHeight];
    float *nf = new float[size_t(arrWidth) * arrHeight];
    float *de = rawde ? new float[size_t(arrWidth) * arrHeight] : nullptr;
    // TODO parallelize
    for (int i = 0; i < arrWidth; ++i)
    {
      for (int j = 0; j < arrHeight; ++j)
      {
        // [x][y], don't flip vertically!
        size_t k = j + arrHeight * i;
        size_t e = j + arrHeight * i;
        if (PIXEL_UNEVALUATED == count[k] || GET_TRANS_GLITCH(trans[k]))
        {
          // not calculated, or glitched
          n[e] = 0;
          nf[e] = 0.0f;
          if (de) de[e] = 0.0f;
        }
        else if (count[k] == maxiter)
        {
          // unescaped
          n[e] = 0xFFffFFffU;
          nf[e] = 0.0f;
          if (de) de[e] = 0.0f;
        }
        else
        {
          n[e] = count[k] + BIAS;
          nf[e] = 1.0f - trans[k];
          if (de) de[e] = rawde[k];
        }
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
    header.insert(magic, StringAttribute(comment));
    header.insert("Iterations", IntAttribute(maxiter));
    header.insert("IterationsBias", IntAttribute(BIAS));
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

extern std::string ReadEXRComment(const std::string &filename)
{
  try
  {
    MultiPartInputFile in(filename.c_str());
    for (int p = 0; p < in.parts(); ++p)
    {
      const Header &h = in.header(p);
      for (Header::ConstIterator i = h.begin(); i != h.end(); ++i)
      {
        std::string name(i.name());
        if (name == magic)
        {
          const Attribute *a = &i.attribute();
          if (const StringAttribute *s = dynamic_cast<const StringAttribute *>(a))
          {
            return std::string(s->value());
          }
        }
      }
    }
  }
  catch (...)
  {
    return "";
  }
  return "";
}

extern bool ReadEXRMapFile(const std::string &filename)
{
  try
  {
    InputFile file(filename.c_str());
    int maxiter = INT_MAX - 1;
    int bias = BIAS;
    const Header &h = file.header();
    for (Header::ConstIterator i = h.begin(); i != h.end(); ++i)
    {
      std::string name(i.name());
      if (name == "Iterations")
      {
        const Attribute *a = &i.attribute();
        if (const IntAttribute *s = dynamic_cast<const IntAttribute *>(a))
        {
          maxiter = s->value();
        }
      }
      if (name == "IterationsBias")
      {
        const Attribute *a = &i.attribute();
        if (const IntAttribute *s = dynamic_cast<const IntAttribute *>(a))
        {
          bias = s->value();
        }
      }
    }
    Box2i dw = h.dataWindow();
    size_t width = dw.max.x - dw.min.x + 1;
    size_t height = dw.max.y - dw.min.y + 1;
    // prepare arrays with proper format
    uint32_t *N = new uint32_t[width * height];
    float *NF = new float[width * height];
    float *DE = new float[width * height];
    FrameBuffer fb;
    // [x][y]
    fb.insert("N" , Slice(IMF::UINT,  (char *) (&N [0] - dw.min.x - dw.min.y * width), sizeof(N [0]) * height, sizeof(N [0]) * 1, 1, 1, 0));
    fb.insert("NF", Slice(IMF::FLOAT, (char *) (&NF[0] - dw.min.x - dw.min.y * width), sizeof(NF[0]) * height, sizeof(NF[0]) * 1, 1, 1, 0.0f));
    fb.insert("DE", Slice(IMF::FLOAT, (char *) (&DE[0] - dw.min.x - dw.min.y * width), sizeof(DE[0]) * height, sizeof(DE[0]) * 1, 1, 1, 0.0f));
    file.setFrameBuffer(fb);
    file.readPixels(dw.min.y, dw.max.y);
    // copy to KF arrays
    g_SFT.SetIterations(maxiter);
    g_SFT.SetImageSize(width, height);
    int *count = g_SFT.GetArrayCount();
    float *trans = g_SFT.GetArrayTrans();
    float *de = g_SFT.GetArrayDE();
    // TODO parallelize
    for (size_t i = 0; i < width; ++i)
    {
      for (size_t j = 0; j < height; ++j)
      {
        // [x][y], don't flip vertically!
        size_t e = j + height * i;
        size_t k = j + height * i;
        if (N[e] == 0 && NF[e] == 0.0f)
        {
          count[k] = PIXEL_UNEVALUATED;
          trans[k] = 0.0f;
          if (de) de[k] = 0.0f;
        }
        else if (N[e] == 0xFFffFFffU)
        {
          count[k] = maxiter;
          trans[k] = 0.0f;
          if (de) de[k] = 0.0f;
        }
        else
        {
          count[k] = int(N[e]) - bias;
          trans[k] = 1.0f - NF[e];
          if (de) de[k] = DE[e];
        }
      }
    }
    g_SFT.ReinitializeBitmap();
    // FIXME leak on failure
    delete[] N;
    delete[] NF;
    delete[] DE;
    return true;
  }
  catch(...)
  {
    return false;
  }
}
