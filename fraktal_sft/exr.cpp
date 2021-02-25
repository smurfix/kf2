/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2020 Claude Heiland-Allen

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
#include <cstdlib>

#include "fraktal_sft.h"
#include "exr.h"

namespace IMF = OPENEXR_IMF_NAMESPACE;
using namespace IMF;
using namespace IMATH_NAMESPACE;

const char magic[] = "KallesFraktaler2+";
const int64_t BIAS = 1024;

extern int SaveEXR
( const std::string &filename
, const unsigned char *Data
, int nWidth
, int nHeight
, int nColors
, const std::string &comment
, int64_t maxiter
, int arrWidth
, int arrHeight
, const itercount_array &count
, const float *trans
, const float *phase
, const float *rawdex
, const float *rawdey
, const EXRChannels C
, int threads
)
{
  try
  {
    setGlobalThreadCount(threads);
    // prepare arrays with proper format
    uint32_t *n0 = C.N ? new uint32_t[size_t(arrWidth) * arrHeight] : nullptr;
    uint32_t *n1 = C.N && (maxiter + BIAS >= 0xFFffFFffLL) ? new uint32_t[size_t(arrWidth) * arrHeight] : nullptr;
    float *nf = C.NF ? new float[size_t(arrWidth) * arrHeight] : nullptr;
    const float *t = C.T ? phase : nullptr;
    const float *dex = C.DEX ? rawdex : nullptr;
    const float *dey = C.DEY ? rawdey : nullptr;
    // TODO parallelize
    for (int i = 0; i < arrWidth; ++i)
    {
      for (int j = 0; j < arrHeight; ++j)
      {
        // [x][y], don't flip vertically!
        size_t k = j + arrHeight * i;
        size_t e = j + arrHeight * i;
        if (PIXEL_UNEVALUATED == count[i][j] || GET_TRANS_GLITCH(trans[k]))
        {
          // not calculated, or glitched
          if (n1) n1[e] = 0;
          if (n0) n0[e] = 0;
          if (nf) nf[e] = 0.0f;
        }
        else if (count[i][j] == maxiter)
        {
          // unescaped
          if (n1) n1[e] = 0xFFffFFffU;
          if (n0) n0[e] = 0xFFffFFffU;
          if (nf) nf[e] = 0.0f;
        }
        else
        {
          int64_t n = count[i][j] + BIAS;
          if (n1) n1[e] = (n >> 32) & 0xFFffFFffLL;
          if (n0) n0[e] = n & 0xFFffFFffLL;
          if (nf) nf[e] = 1.0f - trans[k]; // FIXME this is in (0..1] instead of [0..1)
        }
      }
    }
    // prepare preview image
    Header header(arrWidth, arrHeight);
    if (C.Preview)
    {
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
    }
    // insert metadata
    header.insert(magic, StringAttribute(comment));
    header.insert("Iterations", IntAttribute(maxiter));
    header.insert("IterationsBias", IntAttribute(BIAS));
    // write image
    const half *rgb = g_SFT.GetArrayHalfColour();
    if (rgb)
    {
      if (C.R) header.channels().insert("R", Channel(IMF::HALF));
      if (C.G) header.channels().insert("G", Channel(IMF::HALF));
      if (C.B) header.channels().insert("B", Channel(IMF::HALF));
    }
    if (n1 && n0)
    {
      header.channels().insert("N0",  Channel(IMF::UINT));
      header.channels().insert("N1",  Channel(IMF::UINT));
    }
    else if (n0)
      header.channels().insert("N",  Channel(IMF::UINT));
    if (nf) header.channels().insert("NF", Channel(IMF::FLOAT));
    if (t) header.channels().insert("T", Channel(IMF::FLOAT));
    if (dex) header.channels().insert("DEX", Channel(IMF::FLOAT));
    if (dey) header.channels().insert("DEY", Channel(IMF::FLOAT));
    OutputFile of(filename.c_str(), header);
    FrameBuffer fb;
    if (rgb)
    {
      // [y][x]
      size_t row = g_SFT.GetArrayHalfColourStride();
      if (C.R) fb.insert("R", Slice(IMF::HALF, (char *) (rgb + 0), sizeof(*rgb) * 3, sizeof(*rgb) * row));
      if (C.G) fb.insert("G", Slice(IMF::HALF, (char *) (rgb + 1), sizeof(*rgb) * 3, sizeof(*rgb) * row));
      if (C.B) fb.insert("B", Slice(IMF::HALF, (char *) (rgb + 2), sizeof(*rgb) * 3, sizeof(*rgb) * row));
    }
    else
    {
      if (C.R || C.G || C.B)
        std::cerr << "no rgb" << std::endl;
    }
    // [x][y]
    if (n1 && n0)
    {
      fb.insert("N0", Slice(IMF::UINT,  (char *) n0, sizeof(*n0) * arrHeight, sizeof(*n0) * 1));
      fb.insert("N1", Slice(IMF::UINT,  (char *) n1, sizeof(*n1) * arrHeight, sizeof(*n1) * 1));
    }
    else if (n0)
      fb.insert("N",  Slice(IMF::UINT,  (char *) n0, sizeof(*n0) * arrHeight, sizeof(*n0) * 1));
    if (nf) fb.insert("NF", Slice(IMF::FLOAT, (char *) nf, sizeof(*nf) * arrHeight, sizeof(*nf) * 1));
    if (t) fb.insert("T", Slice(IMF::FLOAT, (char *) t, sizeof(*t) * arrHeight, sizeof(*t) * 1));
    if (dex) fb.insert("DEX", Slice(IMF::FLOAT, (char *) dex, sizeof(*dex) * arrHeight, sizeof(*dex) * 1));
    if (dey) fb.insert("DEY", Slice(IMF::FLOAT, (char *) dey, sizeof(*dey) * arrHeight, sizeof(*dey) * 1));
    of.setFrameBuffer(fb);
    of.writePixels(arrHeight);
    delete[] n0;
    delete[] n1;
    delete[] nf;
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

extern bool ReadEXRMapFile(const std::string &filename, int threads)
{
  bool retval = false;
  uint32_t *N0 = nullptr;
  uint32_t *N1 = nullptr;
  float *NF = nullptr;
  float *T = nullptr;
  float *DEX = nullptr;
  float *DEY = nullptr;
  try
  {
    setGlobalThreadCount(threads);
    InputFile file(filename.c_str());
    int64_t maxiter = INT_MAX - 1 - BIAS;
    int64_t bias = BIAS;
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
        if (const StringAttribute *s = dynamic_cast<const StringAttribute *>(a))
        {
          maxiter = atoll(s->value().c_str());
        }
      }
      if (name == "IterationsBias")
      {
        const Attribute *a = &i.attribute();
        if (const IntAttribute *s = dynamic_cast<const IntAttribute *>(a))
        {
          bias = s->value();
        }
        if (const StringAttribute *s = dynamic_cast<const StringAttribute *>(a))
        {
          bias = atoll(s->value().c_str());
        }
      }
    }
    Box2i dw = h.dataWindow();
    size_t width = dw.max.x - dw.min.x + 1;
    size_t height = dw.max.y - dw.min.y + 1;

    // prepare KF arrays
    g_SFT.SetIterations(maxiter);
    g_SFT.SetImageSize(width, height);
    // rely on internal memory layout of itercount_array for efficiency
    itercount_array count = g_SFT.GetArrayCount();
    N0 = count.get_lsb_array();
    N1 = count.get_msb_array(); // check what happens re: maxiter + bias >= 0xFFffFFffLL ?
    NF = g_SFT.GetArrayTrans();
    T = g_SFT.GetArrayPhase();
    DEX = g_SFT.GetArrayDEx();
    DEY = g_SFT.GetArrayDEy();

    FrameBuffer fb;
    // [x][y]
    if (N1)
    {
      fb.insert("N0", Slice(IMF::UINT,  (char *) (&N0[0] - dw.min.x - dw.min.y * width), sizeof(N0[0]) * height, sizeof(N0[0]) * 1, 1, 1, 0));
      fb.insert("N1", Slice(IMF::UINT,  (char *) (&N1[0] - dw.min.x - dw.min.y * width), sizeof(N1[0]) * height, sizeof(N1[0]) * 1, 1, 1, 0));
    }
    else
      fb.insert("N" , Slice(IMF::UINT,  (char *) (&N0[0] - dw.min.x - dw.min.y * width), sizeof(N0[0]) * height, sizeof(N0[0]) * 1, 1, 1, 0));
    if (NF) fb.insert("NF", Slice(IMF::FLOAT, (char *) (&NF[0] - dw.min.x - dw.min.y * width), sizeof(NF[0]) * height, sizeof(NF[0]) * 1, 1, 1, 0.0f));
    if (T) fb.insert("T", Slice(IMF::FLOAT, (char *) (&T[0] - dw.min.x - dw.min.y * width), sizeof(T[0]) * height, sizeof(T[0]) * 1, 1, 1, 0.0f));
    if (DEX) fb.insert("DEX", Slice(IMF::FLOAT, (char *) (&DEX[0] - dw.min.x - dw.min.y * width), sizeof(DEX[0]) * height, sizeof(DEX[0]) * 1, 1, 1, 0.0f));
    if (DEY) fb.insert("DEY", Slice(IMF::FLOAT, (char *) (&DEY[0] - dw.min.x - dw.min.y * width), sizeof(DEY[0]) * height, sizeof(DEY[0]) * 1, 1, 1, 0.0f));

    file.setFrameBuffer(fb);
    file.readPixels(dw.min.y, dw.max.y);

    // TODO parallelize
    for (size_t i = 0; i < width; ++i)
    {
      for (size_t j = 0; j < height; ++j)
      {
        // [x][y], don't flip vertically!
        size_t k = j + height * i;
        if ((N1 ? N1[k] == 0 : true) && N0[k] == 0 && (NF ? NF[k] == 0.0f : true))
        {
          count[i][j] = PIXEL_UNEVALUATED;
          if (NF) NF[k] = 0.0f;
          if (T) T[k] = 0.0f;
          if (DEX) DEX[k] = 0.0f;
          if (DEY) DEY[k] = 0.0f;
        }
        else if ((N1 ? N1[k] == 0xFFffFFffU : true) && N0[k] == 0xFFffFFffU)
        {
          count[i][j] = maxiter;
          if (NF) NF[k] = 0.0f;
          if (T) T[k] = 0.0f;
          if (DEX) DEX[k] = 0.0f;
          if (DEY) DEY[k] = 0.0f;
        }
        else
        {
          count[i][j] = ((N1 ? int64_t(N1[k]) << 32 : 0) | N0[k]) - bias;
          NF[k] = 1.0f - NF[k]; // FIXME this is in (0..1] instead of [0..1)
        }
      }
    }
    g_SFT.ReinitializeBitmap();
    retval = true;
  }
  catch(...)
  {
    retval = false;
  }
  return retval;
}
