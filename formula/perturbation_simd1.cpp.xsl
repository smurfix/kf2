<?xml version="1.0" encoding="UTF-8"?>
<!--
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2021 Claude Heiland-Allen

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

#include "formula.h"
#include "../fraktal_sft/floatexp.h"
#include "../fraktal_sft/complex.h"
#include "../fraktal_sft/reference.h"
#include "simd1.h"
#include "simd2.h"
#include "simd3.h"
#include "simd4.h"

// hack for broadcasting in complex vector `op` complex scalar
#define C(double,doubleN) \
static inline complex&lt;doubleN&gt; operator+(const complex&lt;double&gt; &amp;a, const complex&lt;doubleN&gt; &amp;b) { return complex&lt;doubleN&gt;(a) + b; } \
static inline complex&lt;doubleN&gt; operator+(const complex&lt;doubleN&gt; &amp;a, const complex&lt;double&gt; &amp;b) { return a + complex&lt;doubleN&gt;(b); } \
static inline complex&lt;doubleN&gt; operator-(const complex&lt;double&gt; &amp;a, const complex&lt;doubleN&gt; &amp;b) { return complex&lt;doubleN&gt;(a) - b; } \
static inline complex&lt;doubleN&gt; operator-(const complex&lt;doubleN&gt; &amp;a, const complex&lt;double&gt; &amp;b) { return a - complex&lt;doubleN&gt;(b); } \
static inline complex&lt;doubleN&gt; operator*(const complex&lt;double&gt; &amp;a, const complex&lt;doubleN&gt; &amp;b) { return complex&lt;doubleN&gt;(a) * b; } \
static inline complex&lt;doubleN&gt; operator*(const complex&lt;doubleN&gt; &amp;a, const complex&lt;double&gt; &amp;b) { return a * complex&lt;doubleN&gt;(b); } \
static inline complex&lt;doubleN&gt; operator/(const complex&lt;double&gt; &amp;a, const complex&lt;doubleN&gt; &amp;b) { return complex&lt;doubleN&gt;(a) / b; } \
static inline complex&lt;doubleN&gt; operator/(const complex&lt;doubleN&gt; &amp;a, const complex&lt;double&gt; &amp;b) { return a / complex&lt;doubleN&gt;(b); }
C(double,double2)
C(double,double4)
C(double,double8)
C(double,double16)
C(double,long double)
C(long double,floatexp)
#undef C

<xsl:for-each select="formula">

template &lt;typename double1, typename intN, typename doubleN&gt;
bool perturbation_SIMD_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , const double1 &amp;m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double1 &amp;g_real, const double1 &amp;g_imag, const double1 &amp;p
  , const double1 &amp;g_FactorAR, const double1 &amp;g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , const int64_t chunksize
  )
{
  if (m_nFractalType == <xsl:value-of select="@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const double1 Ar = g_FactorAR;
    const double1 Ai = g_FactorAI;
    const complex&lt;double1&gt; A = { Ar, Ai };
    const complex&lt;doubleN&gt; c = { cr0, ci0 };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0 &amp;&amp; p == 2.0;
    doubleN test1 = test10;
    doubleN test2 = test20;
    doubleN phase = phase0;
    doubleN xr0 = xr00;
    doubleN xi0 = xi00;
    intN antal = antal0;
    intN bGlitch = bGlitch0;
    intN bBailed = test1 &gt; m_nBailout2;

    // vectorized loop
    const double1 *xptr = reference_ptr_x&lt;double1&gt;(m_Reference);
    const double1 *yptr = reference_ptr_y&lt;double1&gt;(m_Reference);
    const double1 *zptr = reference_ptr_z&lt;double1&gt;(m_Reference);
    if (all(antal == antal[0]))
    {
      for (; antal[0] + chunksize - 1 &lt; nMaxIter; antal = antal + chunksize)
      {
        const doubleN xr_saved = xr0;
        const doubleN xi_saved = xi0;
        const doubleN test1_saved = test1;
        for (int64_t q = 0; q &lt; chunksize; ++q)
        {
          int64_t antal_q = antal[0] + q;
          const double1 Xr = xptr[antal_q];
          const double1 Xi = yptr[antal_q];
          const double1 Xz = zptr[antal_q];

          const doubleN Xxr = Xr + xr0;
          const doubleN Xxi = Xi + xi0;
          const doubleN Xxr2 = Xxr * Xxr;
          const doubleN Xxi2 = Xxi * Xxi;
          test2 = test1;
          test1 = Xxr2 + Xxi2;
          bGlitch |= test1 &lt; Xz;
          if (! no_g)
          {
            test1 = pnorm(g_real, g_imag, p, Xxr, Xxi);
          }
          bBailed |= test1 &gt; m_nBailout2;
          doubleN xrn, xin;

      using T = double1;
      T dummyT;
      (void) dummyT;
      using V = doubleN;
      V dummyV;
      (void) dummyV;
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;double1&gt; X = {Xr, Xi};
      const complex&lt;doubleN&gt; x = {xr0, xi0}, Xx = {Xxr, Xxi};
      complex&lt;doubleN&gt; xn;
      (void) X; (void) x; (void) Xx;
@dc   {
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.m_r; xin = xn.m_i;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
      const doubleN cr = cr0, ci = ci0, xr = xr0, xi = xi0;
@d    {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>

          xr0 = xrn;
          xi0 = xin;
        }
        if (any(bGlitch) || any(bBailed))
        {
          // rollback last chunk
          xr0 = xr_saved;
          xi0 = xi_saved;
          test1 = test1_saved;
          bGlitch = bGlitch0;
          break;
        }
      }
    }

    // finish up unvectorized
    const int64_t M = sizeof(doubleN) / sizeof(double1);
    for (int64_t k = 0; k &lt; M; ++k)
    {
      double1 Xxr = 0, Xxi = 0;
      for (; antal[k] &lt; nMaxIter; antal[k] = antal[k] + 1)
      {
        int64_t antal_k = antal[k];
        const double1 Xr = xptr[antal_k];
        const double1 Xi = yptr[antal_k];
        const double1 Xz = zptr[antal_k];
        Xxr = Xr + xr0[k];
        Xxi = Xi + xi0[k];
        const double1 Xxr2 = Xxr * Xxr;
        const double1 Xxi2 = Xxi * Xxi;
        test2[k] = test1[k];
        test1[k] = Xxr2 + Xxi2;
        if (test1[k] &lt; Xz)
        {
          bGlitch[k] = true;
          if (! m_bNoGlitchDetection[k])
            break;
        }
        if (! no_g)
        {
          test1[k] = pnorm(g_real, g_imag, p, Xxr, Xxi);
        }
        if (test1[k] &gt; m_nBailout2)
        {
          phase[k] = atan2(double1(Xxi), double1(Xxr)) / M_PI / 2;
          phase[k] -= floor(phase[k]);
          break;
        }
        double1 xrn, xin;

      using T = double1;
      T dummyT;
      (void) dummyT;
      using V = double1;
      V dummyV;
      (void) dummyV;
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;double1&gt; X = {Xr, Xi}, x = {xr0[k], xi0[k]}, Xx = {Xxr, Xxi}, c = { cr0[k], ci0[k] };
      complex&lt;double1&gt; xn;
      (void) X; (void) x; (void) Xx;
@dc   {
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.m_r; xin = xn.m_i;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
      const double1 cr = cr0[k], ci = ci0[k], xr = xr0[k], xi = xi0[k];
@d    {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>

        xr0[k] = xrn;
        xi0[k] = xin;
      }
      xr00[k] = Xxr;
      xi00[k] = Xxi;
    }

    antal0 = antal;
    bGlitch0 = bGlitch;
    test10 = test1;
    test20 = test2;
    phase0 = phase;
    return true;
  }
  return false;
}

// explicit template instantiation

#if KF_SIMD >= 1
template bool perturbation_SIMD_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double, int2, double2&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int2 &amp;antal, double2 &amp;test1, double2 &amp;test2, double2 &amp;phase, int2 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int2 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double2 &amp;xr, double2 &amp;xi
  , const double2 &amp;cr, const double2 &amp;ci
  , const int64_t chunksize
  );
#endif

#if KF_SIMD >= 2
template bool perturbation_SIMD_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double, int4, double4&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int4 &amp;antal, double4 &amp;test1, double4 &amp;test2, double4 &amp;phase, int4 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int4 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double4 &amp;xr, double4 &amp;xi
  , const double4 &amp;cr, const double4 &amp;ci
  , const int64_t chunksize
  );
#endif

#if KF_SIMD >= 3
template bool perturbation_SIMD_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double, int8, double8&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int8 &amp;antal, double8 &amp;test1, double8 &amp;test2, double8 &amp;phase, int8 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int8 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double8 &amp;xr, double8 &amp;xi
  , const double8 &amp;cr, const double8 &amp;ci
  , const int64_t chunksize
  );
#endif

#if KF_SIMD >= 4
template bool perturbation_SIMD_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double, int16, double16&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int16 &amp;antal, double16 &amp;test1, double16 &amp;test2, double16 &amp;phase, int16 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int16 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double16 &amp;xr, double16 &amp;xi
  , const double16 &amp;cr, const double16 &amp;ci
  , const int64_t chunksize
  );
#endif

</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
