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

<xsl:for-each select="formula[@convergent='1']">
template &lt;typename T&gt;
bool perturbation_convergent_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;smooth0, double &amp;phase0, bool &amp;bGlitch
  , const T &amp;m_nBailoutSmallP, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , T &amp;xr0, T &amp;xi0
  , const T &amp;cr, const T &amp;ci
  )
{
  using std::abs;
  using std::sqrt;
  using std::exp;
  using std::sin;
  using std::cos;
  using std::sinh;
  using std::log;
  using std::log1p;
  using std::atan2;
  using std::floor;
  if (m_nFractalType == <xsl:value-of select="@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const T Ar = g_FactorAR;
    const T Ai = g_FactorAI;
    const complex&lt;T&gt; A = { Ar, Ai };
    const complex&lt;T&gt; c = { cr, ci };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    int64_t antal = antal0;
    double test1 = test10;
    double smooth = smooth0;
    double phase = phase0;
    T xr = xr0;
    T xi = xi0;
    T Xxr = 1.0/0.0;
    T Xxi = 1.0/0.0;
    T Xxr_1 = 1.0/0.0;
    T Xxi_1 = 1.0/0.0;
    T Xxr_2 = 1.0/0.0;
    T Xxi_2 = 1.0/0.0;
    const T *xptr = reference_ptr_x&lt;T&gt;(m_Reference);
    const T *yptr = reference_ptr_y&lt;T&gt;(m_Reference);
    const T *zptr = reference_ptr_z&lt;T&gt;(m_Reference);
    for (; antal &lt; nMaxIter; antal++)
    {
      const T Xr = xptr[antal];
      const T Xi = yptr[antal];
      const T Xz = zptr[antal];
      Xxr_2 = Xxr_1;
      Xxi_2 = Xxi_1;
      Xxr_1 = Xxr;
      Xxi_1 = Xxi;
      Xxr = Xr + xr;
      Xxi = Xi + xi;

#if 0
      // divergent glitch bailout
      T ttest1 = Xxr * Xxr + Xxi * Xxi;
      if (ttest1 &lt; Xz)
      {
        bGlitch = true;
        test1 = double(ttest1);
        if (! m_bNoGlitchDetection)
        {
          break;
        }
      }

      // convergent glitch bailout
      ttest1 = xr * xr + xi * xi;
      if (ttest1 &gt; T(10.0))
      {
        bGlitch = true;
        test1 = double(1 / ttest1);
        if (! m_bNoGlitchDetection)
        {
          break;
        }
      }
#endif

      // convergent bailout
      const T deltar = Xxr - Xxr_1;
      const T deltai = Xxi - Xxi_1;
      const T delta = pnorm(g_real, g_imag, p, deltar, deltai);
      if (delta &lt; m_nBailoutSmallP)
      {
        const T delta1 = sqr(Xxr - Xxr_1) + sqr(Xxi - Xxi_1);
        const T delta2 = sqr(Xxr_1 - Xxr_2) + sqr(Xxi_1 - Xxi_2);
        const T q = log(delta1) / log(delta2);
        const T f = (log(-log(m_nBailoutSmallP)/p) - log(-log(delta)/p)) / log(q);
        smooth = double(f);
        phase = atan2(double(deltar/sqrt(delta1)), double(deltai/sqrt(delta1))) / M_PI / 2;
        phase -= floor(phase);
        break;
      }

      T xrn, xin;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;T&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi};
      complex&lt;T&gt; xn;
      //(void) X; (void) x; (void) Xx;
      using V = T;
      V dummy;
      (void) dummy;
@dc   {
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.m_r; xin = xn.m_i;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
      using V = T;
      V dummy;
      (void) dummy;
@d    {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>

      xr = xrn;
      xi = xin;
    }
    antal0 = antal;
    test10 = test1;
    smooth0 = smooth;
    phase0 = phase;
    xr0 = Xxr;
    xi0 = Xxi;
    return true;
  }
  return false;
}

template bool perturbation_convergent_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;float&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;smooth0, double &amp;phase0, bool &amp;bGlitch
  , const float &amp;m_nBailoutSmallP, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , float &amp;xr, float &amp;xi
  , const float &amp;cr, const float &amp;ci
  );
template bool perturbation_convergent_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;smooth0, double &amp;phase0, bool &amp;bGlitch
  , const double &amp;m_nBailoutSmallP, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double &amp;xr, double &amp;xi
  , const double &amp;cr, const double &amp;ci
  );
template bool perturbation_convergent_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;smooth0, double &amp;phase0, bool &amp;bGlitch
  , const long double &amp;m_nBailoutSmallP, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , long double &amp;xr, long double &amp;xi
  , const long double &amp;cr, const long double &amp;ci
  );
template bool perturbation_convergent_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;tfloatexp&lt;float,int32_t&gt;&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;smooth0, double &amp;phase0, bool &amp;bGlitch
  , const tfloatexp&lt;float,int32_t&gt; &amp;m_nBailoutSmallP, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;float,int32_t&gt; &amp;xr, tfloatexp&lt;float,int32_t&gt; &amp;xi
  , const tfloatexp&lt;float,int32_t&gt; &amp;cr, const tfloatexp&lt;float,int32_t&gt; &amp;ci
  );
template bool perturbation_convergent_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;tfloatexp&lt;double,int64_t&gt;&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;smooth0, double &amp;phase0, bool &amp;bGlitch
  , const tfloatexp&lt;double,int64_t&gt; &amp;m_nBailoutSmallP, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;double,int64_t&gt; &amp;xr, tfloatexp&lt;double,int64_t&gt; &amp;xi
  , const tfloatexp&lt;double,int64_t&gt; &amp;cr, const tfloatexp&lt;double,int64_t&gt; &amp;ci
  );
</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
