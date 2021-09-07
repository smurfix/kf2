<?xml version="1.0" encoding="UTF-8"?>
<!--
Kalles Fraktaler 2
Copyright (C) 2021 Claude Heiland-Allen

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
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">
<xsl:for-each select="formula">

#include &lt;cmath&gt;
#include &lt;cstdio&gt;
#include &lt;cstdlib&gt;

#include "../fraktal_sft/CFixedFloat.h"
#include "../fraktal_sft/floatexp.h"
#include "../fraktal_sft/complex.h"

#include &lt;ImfNamespace.h&gt;
#include &lt;ImfMultiPartInputFile.h&gt;
#include &lt;ImfInputFile.h&gt;
#include &lt;ImfOutputFile.h&gt;
#include &lt;ImfHeader.h&gt;
#include &lt;ImfIntAttribute.h&gt;
#include &lt;ImfStringAttribute.h&gt;
#include &lt;ImfChannelList.h&gt;
#include &lt;ImfArray.h&gt;
#include &lt;ImfFrameBuffer.h&gt;
#include &lt;ImathBox.h&gt;
#include &lt;ImfPreviewImage.h&gt;

namespace IMF = OPENEXR_IMF_NAMESPACE;
using namespace IMF;
using namespace IMATH_NAMESPACE;

const char magic[] = "KallesFraktaler2+";
const int64_t BIAS = 1024;

#define PIXEL_UNEVALUATED INT_MIN
#define N 1
#define NF 2
#define T 4
#define DEX 8
#define DEY 16
#define R 32
#define G 64
#define B 128
#define Preview 256

extern int SaveEXR
( const std::string &amp;filename
, const unsigned char *Data
, int nWidth
, int nHeight
, int nColors
, const std::string &amp;comment
, int64_t maxiter
, int arrWidth
, int arrHeight
, const int32_t *count
, const float *trans
, const float *phase
, const float *rawdex
, const float *rawdey
, int threads
, int C
)
{
  try
  {
    setGlobalThreadCount(threads);
    // prepare arrays with proper format
    uint32_t *n0 = (C&amp;N) ? new uint32_t[size_t(arrWidth) * arrHeight] : nullptr;
    uint32_t *n1 = (C&amp;N) &amp;&amp; (maxiter + BIAS >= 0xFFffFFffLL) ? new uint32_t[size_t(arrWidth) * arrHeight] : nullptr;
    float *nf = (C&amp;NF) ? new float[size_t(arrWidth) * arrHeight] : nullptr;
    const float *t = (C&amp;T) ? phase : nullptr;
    const float *dex = (C&amp;DEX) ? rawdex : nullptr;
    const float *dey = (C&amp;DEY) ? rawdey : nullptr;
    // TODO parallelize
    for (int i = 0; i &lt; arrWidth; ++i)
    {
      for (int j = 0; j &lt; arrHeight; ++j)
      {
        // [x][y], don't flip vertically!
        size_t k = j * arrWidth + i;
        size_t e = k;
        if (PIXEL_UNEVALUATED == count[k] || trans[k] &lt; 0)
        {
          // not calculated, or glitched
          if (n1) n1[e] = 0;
          if (n0) n0[e] = 0;
          if (nf) nf[e] = 0.0f;
        }
        else if (count[k] == maxiter)
        {
          // unescaped
          if (n1) n1[e] = 0xFFffFFffU;
          if (n0) n0[e] = 0xFFffFFffU;
          if (nf) nf[e] = 0.0f;
        }
        else
        {
          int64_t n = count[k] + BIAS;
          if (n1) n1[e] = (n >> 32) &amp; 0xFFffFFffLL;
          if (n0) n0[e] = n &amp; 0xFFffFFffLL;
          if (nf) nf[e] = 1.0f - trans[k]; // FIXME this is in (0..1] instead of [0..1)
        }
      }
    }
    // prepare preview image
    Header header(arrWidth, arrHeight);
    if (C&amp;Preview)
    {
      Array2D&lt;PreviewRgba&gt; preview;
      preview.resizeErase(nHeight, nWidth);
      // TODO parallelize
      for (int j = 0; j &lt; nHeight; ++j)
      {
        for (int i = 0; i &lt; nWidth; ++i)
        {
          size_t k = (j * size_t(nWidth) + i) * 3;
          PreviewRgba &amp;o = preview[j][i];
          o.r = Data[k + 0];
          o.g = Data[k + 1];
          o.b = Data[k + 2];
          o.a = 255;
        }
      }
      header.setPreviewImage(PreviewImage(nWidth, nHeight, &amp;preview[0][0]));
    }
    // insert metadata
    header.insert(magic, StringAttribute(comment));
    header.insert("Iterations", IntAttribute(maxiter));
    header.insert("IterationsBias", IntAttribute(BIAS));
    // write image
    const half *rgb = nullptr;//g_SFT.GetArrayHalfColour();
    if (rgb)
    {
      if (C&amp;R) header.channels().insert("R", Channel(IMF::HALF));
      if (C&amp;G) header.channels().insert("G", Channel(IMF::HALF));
      if (C&amp;B) header.channels().insert("B", Channel(IMF::HALF));
    }
    if (n1 &amp;&amp; n0)
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
      size_t row = 3 * arrWidth;//g_SFT.GetArrayHalfColourStride();
      if (C&amp;R) fb.insert("R", Slice(IMF::HALF, (char *) (rgb + 0), sizeof(*rgb) * 3, sizeof(*rgb) * row));
      if (C&amp;G) fb.insert("G", Slice(IMF::HALF, (char *) (rgb + 1), sizeof(*rgb) * 3, sizeof(*rgb) * row));
      if (C&amp;B) fb.insert("B", Slice(IMF::HALF, (char *) (rgb + 2), sizeof(*rgb) * 3, sizeof(*rgb) * row));
    }
    // [x][y]
    if (n1 &amp;&amp; n0)
    {
      fb.insert("N0", Slice(IMF::UINT,  (char *) n0, sizeof(*n0) * 1, sizeof(*n0) * arrWidth));
      fb.insert("N1", Slice(IMF::UINT,  (char *) n1, sizeof(*n1) * 1, sizeof(*n1) * arrWidth));
    }
    else if (n0)
      fb.insert("N",  Slice(IMF::UINT,  (char *) n0, sizeof(*n0) * 1, sizeof(*n0) * arrWidth));
    if (nf) fb.insert("NF", Slice(IMF::FLOAT, (char *) nf, sizeof(*nf) * 1, sizeof(*nf) * arrWidth));
    if (t) fb.insert("T", Slice(IMF::FLOAT, (char *) t, sizeof(*t) * 1, sizeof(*t) * arrWidth));
    if (dex) fb.insert("DEX", Slice(IMF::FLOAT, (char *) dex, sizeof(*dex) * 1, sizeof(*dex) * arrWidth));
    if (dey) fb.insert("DEY", Slice(IMF::FLOAT, (char *) dey, sizeof(*dey) * 1, sizeof(*dey) * arrWidth));
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

int main(int argc, char **argv)
{
  using std::atoi;
  using std::atof;
  using std::log2;
  using std::log;
  using RR = CFixedFloat;
  using CC = complex&lt;RR&gt;;
  int width = atoi(argv[1]);
  int height = atoi(argv[2]);
  int maxiters = atoi(argv[3]);
  double zoom = atof(argv[6]);
  floatexp diverge = 256;
  diverge *= diverge;
  floatexp converge = 1e-14;
  converge *= converge;
  int prec = 10 + log2(height * zoom);
  Precision pp(prec);
  RR Cr0 = RR(argv[4]);
  RR Ci0 = RR(argv[5]);
  RR Ar = RR(argv[7]);
  RR Ai = RR(argv[8]);
  RR Sr = RR(argv[9]);
  RR Si = RR(argv[10]);
  CC C0(Cr0, Ci0);
  CC A(Ar, Ai);
  CC S(Sr, Si);
  unsigned char *ppm = new unsigned char[3 * width * height];
  int32_t *count = new int32_t[width * height];
  float *trans = new float[width * height];
  #pragma omp parallel for schedule(dynamic, 1)
  for (int j = 0; j &lt; height; ++j)
  {
    double y = (j + 0.5) / height - 0.5;
    RR Ci = Ci0 + y / zoom;
    for (int i = 0; i &lt; width; ++i)
    {
      double x = ((i + 0.5) / width - 0.5) * width / height;
      RR Cr = Cr0 + x / zoom;
      CC C(Cr, Ci);
      int k = j * width + i;
      ppm[3*k+0] = 0;
      ppm[3*k+1] = 0;
      ppm[3*k+2] = 0;
      count[k] = maxiters;
      trans[k] = 0;
      CC X = S;
      CC Xn;
      RR Xr, Xi, Xrn, Xin, Xr2, Xi2;
      floatexp d = 1.0/0.0, d1;
      for (int n = 0; n &lt; maxiters; ++n)
      {
<xsl:choose>

<xsl:when test="reference/@t='C'">
<xsl:value-of select="reference" />
</xsl:when>

<xsl:when test="reference/@t='R'">
        Xr = X.m_r;
        Xi = X.m_i;
        Xr2 = Xr * Xr;
        Xi2 = Xi * Xi;
        <xsl:value-of select="reference" />
        Xn = CC(Xrn, Xin);
</xsl:when>

<xsl:otherwise>
#error unknown reference t: <xsl:value-of select="reference/@t" />
</xsl:otherwise>

</xsl:choose>
<xsl:choose>

<xsl:when test="@convergent='1'">
        d1 = d;
        d = floatexp(norm(X - Xn));
        if (d1 &lt; converge &amp;&amp; d &lt; d1)
        {
          floatexp q = log(d) / log(d1);
          double f = double((log(-(log(converge))/2) - log(-(log(d))/2)) / log(q));
          double m = n + f;
#if 0
          #pragma omp critical
          {
            std::fprintf(stderr, "%d %d %g\n", j, i, m);
          }
#endif
          m /= maxiters;
          m *= 10;
          ppm[3*k+0] = ((int)(128 + 127 * cos(m * 2 * M_PI + 0))) &amp; 0xFF;
          ppm[3*k+1] = ((int)(128 + 127 * cos(m * 2 * M_PI + 1))) &amp; 0xFF;
          ppm[3*k+2] = ((int)(128 + 127 * cos(m * 2 * M_PI + 2))) &amp; 0xFF;
          count[k] = n;
          trans[k] = 1 - f;
          break;
        }
</xsl:when>

<xsl:otherwise>
        d1 = d;
        d = floatexp(norm(X));
        if (d &gt; diverge)
        {
          floatexp q = log(d) / log(d1);
          double f = double((log(log(diverge)/2) - log(log(d)/2)) / log(q));
          double m = n + f;
          ppm[3*k+0] = ((int)(m)      ) &amp; 0xFF;
          ppm[3*k+1] = ((int)(m) >>  8) &amp; 0xFF;
          ppm[3*k+2] = ((int)(m) >> 16) &amp; 0xFF;
          count[k] = n;
          trans[k] = 1 - f;
          break;
        }
</xsl:otherwise>

</xsl:choose>
        X = Xn;
      }
    }
  }
  SaveEXR("brute.exr", ppm, width, height, 3, "-", maxiters, width, height, count, trans, nullptr, nullptr, nullptr, 16, N+NF+Preview);
  std::printf("P6\n%d %d\n255\n", width, height);
  std::fwrite(ppm, 3 * width * height, 1, stdout);
  delete[] ppm;
  return 0;
}

</xsl:for-each>
</xsl:template>
</xsl:stylesheet>
