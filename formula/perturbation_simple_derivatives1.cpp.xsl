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

<xsl:for-each select="formula[not(@convergent='1')]">
template &lt;typename T&gt;
bool perturbation_simple_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , T &amp;xr0, T &amp;xi0
  , const T &amp;cr, const T &amp;ci
  , T &amp;Jxa0, T &amp;Jxb0, T &amp;Jya0, T &amp;Jyb0
  , const T &amp;e, const T &amp;h
  , const T &amp;daa, const T &amp;dab, const T &amp;dba, const T &amp;dbb
  , const bool noDerivativeGlitch
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
  (void) Jxa0; // -Wunused-parameter
  (void) Jxb0; // -Wunused-parameter
  (void) Jya0; // -Wunused-parameter
  (void) Jyb0; // -Wunused-parameter
  (void) h; // -Wunused-parameter
  (void) e; // -Wunused-parameter
  (void) daa; // -Wunused-parameter
  (void) dab; // -Wunused-parameter
  (void) dba; // -Wunused-parameter
  (void) dbb; // -Wunused-parameter
  if (m_nFractalType == <xsl:value-of select="@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
<xsl:for-each select="glitch">
    const double m_bGlitchLowTolerance = 0.0; // FIXME
    const double lfactorlo = log(<xsl:value-of select="@factorlo" />);
    const double lfactorhi = log(<xsl:value-of select="@factorhi" />);
    const double factor = exp(lfactorhi + m_bGlitchLowTolerance * (lfactorlo - lfactorhi));
</xsl:for-each>
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0 &amp;&amp; p == 2.0;
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
    double test2 = test20;
    double phase = phase0;
    T xr = xr0;
    T xi = xi0;
<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
    T dr = Jxa0, di = Jya0;
    const T dr0 = daa, di0 = dba;
    (void) dr0;
    (void) di0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    T dxa = Jxa0, dxb = Jxb0, dya = Jya0, dyb = Jyb0;
</xsl:when>
</xsl:choose>
    T Xxr = 0;
    T Xxi = 0;
    const T *xptr = reference_ptr_x&lt;T&gt;(m_Reference);
    const T *yptr = reference_ptr_y&lt;T&gt;(m_Reference);
    const T *zptr = reference_ptr_z&lt;T&gt;(m_Reference);
<xsl:for-each select="references[@t='R']">
    const T *zptr<xsl:value-of select="position()" /> = reference_ptr_z&lt;T&gt;(m_Reference, <xsl:value-of select="position()" />);
</xsl:for-each>
<xsl:for-each select="references[@t='C']">
    const T *zptr<xsl:value-of select="2 * (position() - 1) + 1" /> = reference_ptr_z&lt;T&gt;(m_Reference, <xsl:value-of select="2 * (position() - 1) + 1" />);
    const T *zptr<xsl:value-of select="2 * (position() - 1) + 2" /> = reference_ptr_z&lt;T&gt;(m_Reference, <xsl:value-of select="2 * (position() - 1) + 2" />);
</xsl:for-each>
    for (; antal &lt; nMaxIter; antal++)
    {
      const T Xr = xptr[antal];
      const T Xi = yptr[antal];
      const T Xz = zptr[antal];
<xsl:for-each select="references[@t='R']">
      const T <xsl:value-of select="@name" /> = zptr<xsl:value-of select="position()" />[antal];
</xsl:for-each>
<xsl:for-each select="references[@t='C']">
      const complex&lt;T&gt; <xsl:value-of select="@name" /> = complex&lt;T&gt;(zptr<xsl:value-of select="2 * (position() - 1) + 1" />[antal], zptr<xsl:value-of select="2 * (position() - 1) + 2" />[antal]);
</xsl:for-each>
      Xxr = Xr + xr;
      Xxi = Xi + xi;
      const T Xxr2 = Xxr * Xxr;
      const T Xxi2 = Xxi * Xxi;
      test2 = test1;
      const T ttest1 = Xxr2 + Xxi2;
      test1 = double(ttest1);
      if (ttest1 &lt; Xz)
      {
<xsl:choose>
<xsl:when test="@type='0' and @power='2'">
        if (noDerivativeGlitch || type_0_power_2_pixel_has_glitched(cr, ci, xr, xi, Xr, Xi, dr / h, di / h, e, h)) // FIXME matrix derivatives
        {
</xsl:when>
</xsl:choose>
        bGlitch = true;
        if (! m_bNoGlitchDetection)
          break;
<xsl:choose>
<xsl:when test="@type='0' and @power='2'">
        }
</xsl:when>
</xsl:choose>
      }
      if (! no_g)
      {
        test1 = double(pnorm(g_real, g_imag, p, Xxr, Xxi));
      }
      if (test1 &gt; m_nBailout2)
      {
        phase = atan2(double(Xxi), double(Xxr)) / M_PI / 2;
        phase -= floor(phase);
        break;
      }
      T xrn, xin;

<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
      T drn, din;
</xsl:when>
<xsl:when test="derivative/@t='M'">
      T dxan, dxbn, dyan, dybn;
</xsl:when>
</xsl:choose>

      using V = T;
      V dummy;
      (void) dummy;
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;T&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi};
      complex&lt;T&gt; xn;
      (void) X; (void) x; (void) Xx;
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@d    {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
<xsl:when test="derivative/@t='C'">
      const complex&lt;T&gt; d = {dr, di}, d0 = {daa, dba}; <!-- FIXME matrix derivatives -->
      complex&lt;T&gt; dn;
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
      const complex&lt;T&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi}, d = {dr, di}, d0 = {daa, dba}; <!-- FIXME matrix derivatives -->
      complex&lt;T&gt; dn;
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

      xr = xrn;
      xi = xin;
<xsl:choose>
<xsl:when test="derivative/@t='M'">
      dxa = dxan; dxb = dxbn; dya = dyan; dyb = dybn;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
      dr = drn; di = din;
</xsl:when>
</xsl:choose>
    }
    antal0 = antal;
    test10 = test1;
    test20 = test2;
    phase0 = phase;
    xr0 = Xxr;
    xi0 = Xxi;
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
    Jxa0 = dr; Jxb0 = -di; Jya0 = di; Jyb0 = dr;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    Jxa0 = dxa; Jxb0 = dxb; Jya0 = dya; Jyb0 = dyb;
</xsl:when>
</xsl:choose>
    return true;
  }
  return false;
}

template bool perturbation_simple_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;float&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , float &amp;xr0, float &amp;xi0
  , const float &amp;cr, const float &amp;ci
  , float &amp;Jxa0, float &amp;Jxb0, float &amp;Jya0, float &amp;Jyb0
  , const float &amp;e, const float &amp;h
  , const float &amp;daa, const float &amp;dab, const float &amp;dba, const float &amp;dbb
  , const bool noDerivativeGlitch
  );
template bool perturbation_simple_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double &amp;xr0, double &amp;xi0
  , const double &amp;cr, const double &amp;ci
  , double &amp;Jxa0, double &amp;Jxb0, double &amp;Jya0, double &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double &amp;daa, const double &amp;dab, const double &amp;dba, const double &amp;dbb
  , const bool noDerivativeGlitch
  );
template bool perturbation_simple_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;long double&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , long double &amp;xr0, long double &amp;xi0
  , const long double &amp;cr, const long double &amp;ci
  , long double &amp;Jxa0, long double &amp;Jxb0, long double &amp;Jya0, long double &amp;Jyb0
  , const long double &amp;e, const long double &amp;h
  , const long double &amp;daa, const long double &amp;dab, const long double &amp;dba, const long double &amp;dbb
  , const bool noDerivativeGlitch
  );
template bool perturbation_simple_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;tfloatexp&lt;float,int32_t&gt;&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;float,int32_t&gt; &amp;xr0, tfloatexp&lt;float,int32_t&gt; &amp;xi0
  , const tfloatexp&lt;float,int32_t&gt; &amp;cr, const tfloatexp&lt;float,int32_t&gt; &amp;ci
  , tfloatexp&lt;float,int32_t&gt; &amp;Jxa0, tfloatexp&lt;float,int32_t&gt; &amp;Jxb0, tfloatexp&lt;float,int32_t&gt; &amp;Jya0, tfloatexp&lt;float,int32_t&gt; &amp;Jyb0
  , const tfloatexp&lt;float,int32_t&gt; &amp;e, const tfloatexp&lt;float,int32_t&gt; &amp;h
  , const tfloatexp&lt;float,int32_t&gt; &amp;daa, const tfloatexp&lt;float,int32_t&gt; &amp;dab, const tfloatexp&lt;float,int32_t&gt; &amp;dba, const tfloatexp&lt;float,int32_t&gt; &amp;dbb
  , const bool noDerivativeGlitch
  );
template bool perturbation_simple_derivatives_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;tfloatexp&lt;double,int64_t&gt;&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;double,int64_t&gt; &amp;xr0, tfloatexp&lt;double,int64_t&gt; &amp;xi0
  , const tfloatexp&lt;double,int64_t&gt; &amp;cr, const tfloatexp&lt;double,int64_t&gt; &amp;ci
  , tfloatexp&lt;double,int64_t&gt; &amp;Jxa0, tfloatexp&lt;double,int64_t&gt; &amp;Jxb0, tfloatexp&lt;double,int64_t&gt; &amp;Jya0, tfloatexp&lt;double,int64_t&gt; &amp;Jyb0
  , const tfloatexp&lt;double,int64_t&gt; &amp;e, const tfloatexp&lt;double,int64_t&gt; &amp;h
  , const tfloatexp&lt;double,int64_t&gt; &amp;daa, const tfloatexp&lt;double,int64_t&gt; &amp;dab, const tfloatexp&lt;double,int64_t&gt; &amp;dba, const tfloatexp&lt;double,int64_t&gt; &amp;dbb
  , const bool noDerivativeGlitch
  );
</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
