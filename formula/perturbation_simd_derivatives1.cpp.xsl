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

#include "../fraktal_sft/CFixedFloat.h"
#include "../fraktal_sft/floatexp.h"
#include "../fraktal_sft/complex.h"
#include "../fraktal_sft/reference.h"

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
bool perturbation_SIMD_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , const double1 &amp;m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double1 &amp;g_real, const double1 &amp;g_imag, const double1 &amp;p
  , const double1 &amp;g_FactorAR, const double1 &amp;g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , doubleN &amp;Jxa0, doubleN &amp;Jxb0, doubleN &amp;Jya0, doubleN &amp;Jyb0
  , const double1 &amp;e, const double1 &amp;h
  , const doubleN &amp;daa0, const doubleN &amp;dab0, const doubleN &amp;dba0, const doubleN &amp;dbb0
  , const int64_t chunksize
  , const bool noDerivativeGlitch
  )
{
  (void) Jxa0; // -Wunused-parameter
  (void) Jxb0; // -Wunused-parameter
  (void) Jya0; // -Wunused-parameter
  (void) Jyb0; // -Wunused-parameter
  (void) h; // -Wunused-parameter
  (void) e; // -Wunused-parameter
  (void) daa0; // -Wunused-parameter
  (void) dab0; // -Wunused-parameter
  (void) dba0; // -Wunused-parameter
  (void) dbb0; // -Wunused-parameter
  if (m_nFractalType == <xsl:value-of select="@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0 &amp;&amp; p == 2.0;
    const double1 Ar = g_FactorAR;
    const double1 Ai = g_FactorAI;
    const complex&lt;double1&gt; A = { Ar, Ai };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    (void) A; // -Wunused-variable
    doubleN test1 = test10;
    doubleN test2 = test20;
    doubleN phase = phase0;
    doubleN xr0 = xr00;
    doubleN xi0 = xi00;
    intN antal = antal0;
    intN bGlitch = bGlitch0;
    intN bBailed = test1 &gt; m_nBailout2;
<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
    doubleN dr_0 = Jxa0, di_0 = Jya0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    doubleN dxa0 = Jxa0, dxb0 = Jxb0, dya0 = Jya0, dyb0 = Jyb0;
</xsl:when>
</xsl:choose>

    // vectorized loop
    const double1 *xptr = reference_ptr_x&lt;double1&gt;(m_Reference);
    const double1 *yptr = reference_ptr_y&lt;double1&gt;(m_Reference);
    const double1 *zptr = reference_ptr_z&lt;double1&gt;(m_Reference);
    if (all(antal == antal[0]))
    {
      const doubleN dr0 = daa0, di0 = dba0;
      (void) dr0;
      (void) di0;
      for (; antal[0] + chunksize - 1 &lt; nMaxIter; antal = antal + chunksize)
      {
        const doubleN xr_saved = xr0;
        const doubleN xi_saved = xi0;
<xsl:choose>
<xsl:when test="derivative/@t='M'">
        const doubleN dxa_saved = dxa0, dxb_saved = dxb0, dya_saved = dya0, dyb_saved = dyb0;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
        const doubleN dr_saved = dr_0, di_saved = di_0;
</xsl:when>
</xsl:choose>
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
      const doubleN xr = xr0, xi = xi0, cr = cr0, ci = ci0;
      doubleN xrn, xin;
<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
      const doubleN dr = dr_0, di = di_0;
      doubleN drn, din;
</xsl:when>
<xsl:when test="derivative/@t='M'">
      const doubleN daa = daa0, dab = dab0, dba = dba0, dbb = dbb0;
      const doubleN dxa = dxa0, dxb = dxb0, dya = dya0, dyb = dyb0;
      doubleN dxan, dxbn, dyan, dybn;
</xsl:when>
</xsl:choose>

      using T = double;
      T dummyT;
      (void) dummyT;
      using V = doubleN;
      V dummyV;
      (void) dummyV;
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;double&gt; X = {Xr, Xi};
      const complex&lt;doubleN&gt; x = {xr, xi}, Xx = {Xxr, Xxi}, c = {cr, ci};
      complex&lt;doubleN&gt; xn;
      (void) X; (void) x; (void) Xx;
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@d    {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
<xsl:when test="derivative/@t='C'">
      const complex&lt;doubleN&gt; d = {dr, di}, d0 = {daa0, dba0}; <!-- FIXME matrix derivatives -->
      complex&lt;doubleN&gt; dn;
@dc   {
        <xsl:value-of select="derivative" />
      }
      drn = dn.m_r; din = dn.m_i;
</xsl:when>
</xsl:choose>
@dc   {
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.m_r; xin = xn.m_i;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
<xsl:choose>
<xsl:when test="derivative/@t='C'">
      const complex&lt;double&gt; X = {Xr, Xi};
      const complex&lt;doubleN&gt; x = {xr, xi}, Xx = {Xxr, Xxi}, c = {cr, ci}, d = {dr, di}, d0 = {daa, dba}; <!-- FIXME matrix derivatives -->
      complex&lt;doubleN&gt; dn;
      (void) X; (void) x; (void) Xx; (void) d;
@dc   {
        <xsl:value-of select="derivative" />
      }
      drn = dn.m_r; din = dn.m_i;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@d    {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
</xsl:choose>
@d    {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>

          xr0 = xrn;
          xi0 = xin;
<xsl:choose>
<xsl:when test="derivative/@t='M'">
          dxa0 = dxan; dxb0 = dxbn; dya0 = dyan; dyb0 = dybn;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
          dr_0 = drn; di_0 = din;
</xsl:when>
</xsl:choose>
        }

        if (any(bGlitch) || any(bBailed))
        {
          // rollback last chunk
          xr0 = xr_saved;
          xi0 = xi_saved;
<xsl:choose>
<xsl:when test="derivative/@t='M'">
          dxa0 = dxa_saved; dxb0 = dxb_saved; dya0 = dya_saved; dyb0 = dyb_saved;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
          dr_0 = dr_saved; di_0 = di_saved;
</xsl:when>
</xsl:choose>
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
      const double1 dr0 = daa0[k], di0 = dba0[k];
      (void) dr0;
      (void) di0;

      double1 Xxr = 0, Xxi = 0;
      for (; antal[k] &lt; nMaxIter; antal[k] = antal[k] + 1)
      {
        int64_t antal_k = antal[k];
        const double1 Xr = xptr[antal_k];
        const double1 Xi = yptr[antal_k];
        const double1 Xz = zptr[antal_k];
        const double1 cr = cr0[k], ci = ci0[k];
        const double1 xr = xr0[k], xi = xi0[k];
        Xxr = Xr + xr;
        Xxi = Xi + xi;
        const double1 Xxr2 = Xxr * Xxr;
        const double1 Xxi2 = Xxi * Xxi;
        test2[k] = test1[k];
        test1[k] = Xxr2 + Xxi2;
        if (test1[k] &lt; Xz)
        {
<xsl:choose>
<xsl:when test="@type='0' and @power='2'">
#ifdef KF_USE_TYPE_0_POWER_2_HAS_GLITCHED
        if (noDerivativeGlitch || type_0_power_2_pixel_has_glitched(cr, ci, xr, xi, Xr, Xi, dxa0[k] / h, dya0[k] / h, e, h)) // FIXME matrix derivatives
        {
#endif
</xsl:when>
</xsl:choose>
        bGlitch[k] = true;
        if (! m_bNoGlitchDetection[k])
          break;
<xsl:choose>
<xsl:when test="@type='0' and @power='2'">
#ifdef KF_USE_TYPE_0_POWER_2_HAS_GLITCHED
        }
#endif
</xsl:when>
</xsl:choose>
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
        double1 xrn = 0, xin = 0;

      using T = double1;
      T dummyT;
      (void) dummyT;
      using V = double1;
      V dummyV;
      (void) dummyV;
<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
      const double1 dr = dr_0[k], di = di_0[k];
      double1 drn = 0, din = 0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
      const double1 daa = daa0[k], dab = dab0[k], dba = dba0[k], dbb = dbb0[k];
      const double1 dxa = dxa0[k], dxb = dxb0[k], dya = dya0[k], dyb = dyb0[k];
      double1 dxan = 0, dxbn = 0, dyan = 0, dybn = 0;
</xsl:when>
</xsl:choose>
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;double1&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi}, c = {cr, ci};
      complex&lt;double1&gt; xn;
      (void) X; (void) x; (void) Xx;
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@d    {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
<xsl:when test="derivative/@t='C'">
      const complex&lt;double1&gt; d = {dr, di}, d0 = {daa0[k], dba0[k]}; <!-- FIXME matrix derivatives -->
      complex&lt;double1&gt; dn;
@dc   {
        <xsl:value-of select="derivative" />
      }
      drn = dn.m_r; din = dn.m_i;
</xsl:when>
</xsl:choose>
@dc   {
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.m_r; xin = xn.m_i;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
<xsl:choose>
<xsl:when test="derivative/@t='C'">
      const complex&lt;double1&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi}, c = {cr, ci}, d = {dr, di}, d0 = {daa[k], dba[k]}; <!-- FIXME matrix derivatives -->
      complex&lt;double1&gt; dn;
      (void) X; (void) x; (void) Xx; (void) d;
@dc   {
        <xsl:value-of select="derivative" />
      }
      drn = dn.m_r; din = dn.m_i;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@d    {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
</xsl:choose>
@d    {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>

<xsl:choose>
<xsl:when test="derivative/@t='M'">
      dxa0[k] = dxan; dxb0[k] = dxbn; dya0[k] = dyan; dyb0[k] = dybn;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
      dr_0[k] = drn; di_0[k] = din;
</xsl:when>
</xsl:choose>

        xr0[k] = xrn;
        xi0[k] = xin;

      }
      xr00[k] = Xxr;
      xi00[k] = Xxi;
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
    Jxa0[k] = dr_0[k];
    Jxb0[k] = -di_0[k];
    Jya0[k] = di_0[k];
    Jyb0[k] = dr_0[k];
</xsl:when>
<xsl:when test="derivative/@t='M'">
    Jxa0[k] = dxa0[k];
    Jxb0[k] = dxb0[k];
    Jya0[k] = dya0[k];
    Jyb0[k] = dyb0[k];
</xsl:when>
</xsl:choose>
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

#if KF_SIMD >= 1
template bool perturbation_SIMD_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double, int2, double2&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int2 &amp;antal0, double2 &amp;test10, double2 &amp;test20, double2 &amp;phase0, int2 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int2 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double2 &amp;xr0, double2 &amp;xi0
  , const double2 &amp;cr0, const double2 &amp;ci0
  , double2 &amp;Jxa0, double2 &amp;Jxb0, double2 &amp;Jya0, double2 &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double2 &amp;daa0, const double2 &amp;dab0, const double2 &amp;dba0, const double2 &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
  );
#endif

#if KF_SIMD >= 2
template bool perturbation_SIMD_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double, int4, double4&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int4 &amp;antal0, double4 &amp;test10, double4 &amp;test20, double4 &amp;phase0, int4 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int4 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double4 &amp;xr0, double4 &amp;xi0
  , const double4 &amp;cr0, const double4 &amp;ci0
  , double4 &amp;Jxa0, double4 &amp;Jxb0, double4 &amp;Jya0, double4 &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double4 &amp;daa0, const double4 &amp;dab0, const double4 &amp;dba0, const double4 &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
  );
#endif

#if KF_SIMD >= 3
template bool perturbation_SIMD_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double, int8, double8&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int8 &amp;antal0, double8 &amp;test10, double8 &amp;test20, double8 &amp;phase0, int8 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int8 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double8 &amp;xr0, double8 &amp;xi0
  , const double8 &amp;cr0, const double8 &amp;ci0
  , double8 &amp;Jxa0, double8 &amp;Jxb0, double8 &amp;Jya0, double8 &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double8 &amp;daa0, const double8 &amp;dab0, const double8 &amp;dba0, const double8 &amp;dbb0
  , const int64_t chunk_sizes
  , const bool noDerivativeGlitch
  );
#endif

#if KF_SIMD >= 4
template bool perturbation_SIMD_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double, int16, double16&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int16 &amp;antal0, double16 &amp;test10, double16 &amp;test20, double16 &amp;phase0, int16 &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const int16 &amp;m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double16 &amp;xr0, double16 &amp;xi0
  , const double16 &amp;cr0, const double16 &amp;ci0
  , double16 &amp;Jxa0, double16 &amp;Jxb0, double16 &amp;Jya0, double16 &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double16 &amp;daa0, const double16 &amp;dab0, const double16 &amp;dba0, const double16 &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
  );
#endif

</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
