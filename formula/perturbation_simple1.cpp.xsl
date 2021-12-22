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
bool perturbation_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , T &amp;xr0, T &amp;xi0
  , const T &amp;cr, const T &amp;ci
  , const bool singleref
  )
{
  using std::abs;
  using std::sqrt;
  using std::exp;
  using std::sin;
  using std::cos;
  using std::sinh;
  using std::cosh;
  using std::log;
  using std::log1p;
  using std::atan2;
  using std::floor;
  if (m_nFractalType == <xsl:value-of select="@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
<xsl:for-each select="glitch">
    const double glitchLowTolerance = 0.0; // FIXME
    const double lfactorlo = log(<xsl:value-of select="@factorlo" />);
    const double lfactorhi = log(<xsl:value-of select="@factorhi" />);
    const double factor = exp(lfactorhi + glitchLowTolerance * (lfactorlo - lfactorhi));
</xsl:for-each>
    const T Ar = g_FactorAR;
    const T Ai = g_FactorAI;
    const complex&lt;T&gt; A = { Ar, Ai };
    const complex&lt;T&gt; c = { cr, ci };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0 &amp;&amp; p == 2.0;
    int64_t antal = antal0;
    int64_t rantal = antal0;
    double test1 = test10;
    double test2 = test20;
    double phase = phase0;
    T xr = xr0;
    T xi = xi0;
    T Xxr = 0;
    T Xxi = 0;
    const int64_t N = reference_size_x(m_Reference);
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
      T Xr = xptr[rantal];
      T Xi = yptr[rantal];
      T Xz = zptr[rantal];
<xsl:for-each select="references[@t='R']">
      T <xsl:value-of select="@name" /> = zptr<xsl:value-of select="position()" />[rantal];
</xsl:for-each>
<xsl:for-each select="references[@t='C']">
      complex&lt;T&gt; <xsl:value-of select="@name" /> = complex&lt;T&gt;(zptr<xsl:value-of select="2 * (position() - 1) + 1" />[rantal], zptr<xsl:value-of select="2 * (position() - 1) + 2" />[rantal]);
</xsl:for-each>
      rantal++;
      Xxr = Xr + xr;
      Xxi = Xi + xi;
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      complex&lt;T&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi};
      (void) X; (void) x; (void) Xx;
</xsl:when>
</xsl:choose>
      T Xxr2 = Xxr * Xxr;
      T Xxi2 = Xxi * Xxi;
      test2 = test1;
      T ttest1 = Xxr2 + Xxi2;
      test1 = double(ttest1);
      if (singleref)
      {
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
        if (ttest1 &lt; xr * xr + xi * xi || rantal == N)
        {
          xr = Xxr;
          xi = Xxi;
          rantal = 0;
          Xr = 0;
          Xi = 0;
          Xz = 0;
<xsl:for-each select="references[@t='R']">
          <xsl:value-of select="@name" /> = T(<xsl:value-of select="@value" />);
</xsl:for-each>
<xsl:for-each select="references[@t='C']">
          <xsl:value-of select="@name" /> = complex&lt;T&gt;(T(<xsl:value-of select="@value" />), T(0));
</xsl:for-each>
          Xxr = Xr + xr;
          Xxi = Xi + xi;
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
          X = complex&lt;T&gt;(Xr, Xi);
          x = complex&lt;T&gt;(xr, xi);
          Xx = complex&lt;T&gt;(Xxr, Xxi);
</xsl:when>
</xsl:choose>
        }
      }
      else
      {
        if (ttest1 &lt; Xz)
        {
          bGlitch = true;
          if (! m_bNoGlitchDetection)
            break;
        }
        {
<xsl:for-each select="glitch/test">
          {
            const T lhs = <xsl:value-of select="lhs" />;
            const T rhs = <xsl:value-of select="rhs" />;
            if (<xsl:value-of select="cond" />)
            {
              bGlitch = true;
              test1 = double(<xsl:value-of select="size" />);
              if (! m_bNoGlitchDetection)
              {
                break;
              }
            }
          }
</xsl:for-each>
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
      }
      T xrn, xin;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      complex&lt;T&gt; xn;
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
    test20 = test2;
    phase0 = phase;
    xr0 = Xxr;
    xi0 = Xxi;
    return true;
  }
  return false;
}

template bool perturbation_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;float&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , float &amp;xr, float &amp;xi
  , const float &amp;cr, const float &amp;ci
  , const bool singleref
  );
template bool perturbation_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double &amp;xr, double &amp;xi
  , const double &amp;cr, const double &amp;ci
  , const bool singleref
  );
template bool perturbation_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , long double &amp;xr, long double &amp;xi
  , const long double &amp;cr, const long double &amp;ci
  , const bool singleref
  );
template bool perturbation_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;tfloatexp&lt;float,int32_t&gt;&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;float,int32_t&gt; &amp;xr, tfloatexp&lt;float,int32_t&gt; &amp;xi
  , const tfloatexp&lt;float,int32_t&gt; &amp;cr, const tfloatexp&lt;float,int32_t&gt; &amp;ci
  , const bool singleref
  );
template bool perturbation_simple_<xsl:value-of select="@type" />_<xsl:value-of select="@power" />&lt;tfloatexp&lt;double,int64_t&gt;&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;double,int64_t&gt; &amp;xr, tfloatexp&lt;double,int64_t&gt; &amp;xi
  , const tfloatexp&lt;double,int64_t&gt; &amp;cr, const tfloatexp&lt;double,int64_t&gt; &amp;ci
  , const bool singleref
  );
</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
