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
<xsl:template match="/"><xsl:for-each select="formula[@convergent='1']">
#include "formula.h"
#include "../fraktal_sft/floatexp.h"
#include "../fraktal_sft/complex.h"
#include "../fraktal_sft/reference.h"

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
  , const bool singleref
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
<xsl:for-each select="glitch">
    const T glitchLowTolerance = 0.0; // FIXME
    const T lfactorlo = log(<xsl:value-of select="@factorlo" />);
    const T lfactorhi = log(<xsl:value-of select="@factorhi" />);
    const T factor = exp(lfactorhi + glitchLowTolerance * (lfactorlo - lfactorhi));
</xsl:for-each>
    const T Ar = g_FactorAR;
    const T Ai = g_FactorAI;
    const complex&lt;T&gt; A = { Ar, Ai };
    const complex&lt;T&gt; c = { cr, ci };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    int64_t antal = antal0;
    int64_t rantal = antal0;
    double test1 = test10;
    double smooth = smooth0;
    double phase = phase0;
    T xr = xr0;
    T xi = xi0;
    T Xxr = 1.0/0.0;
    T Xxi = 1.0/0.0;
    T Xxr_1 = 1.0/0.0;
    T Xxi_1 = 1.0/0.0;
    T xr_1 = 1.0/0.0;
    T xi_1 = 1.0/0.0;
    const int64_t N = reference_size_x(m_Reference);
    const T *xptr = reference_ptr_x&lt;T&gt;(m_Reference);
    const T *yptr = reference_ptr_y&lt;T&gt;(m_Reference);
<xsl:for-each select="references[@t='R']">
    const T *zptr<xsl:value-of select="position() - 1" /> = reference_ptr_z&lt;T&gt;(m_Reference, <xsl:value-of select="position() - 1" />);
</xsl:for-each>
<xsl:for-each select="references[@t='C']">
    const T *zptr<xsl:value-of select="2 * (position() - 1) + 0" /> = reference_ptr_z&lt;T&gt;(m_Reference, <xsl:value-of select="2 * (position() - 1) + 0" />);
    const T *zptr<xsl:value-of select="2 * (position() - 1) + 1" /> = reference_ptr_z&lt;T&gt;(m_Reference, <xsl:value-of select="2 * (position() - 1) + 1" />);
</xsl:for-each>
    const T *uptr = reference_ptr_wx&lt;T&gt;(m_Reference);
    const T *vptr = reference_ptr_wy&lt;T&gt;(m_Reference);
    T delta = 1.0/0.0;
    T delta_1 = 1.0/0.0;
    for (; antal &lt; nMaxIter; antal++)
    {
      bool skip = true;
start:
      const int64_t ix = std::min(rantal, N - 1);
      const T Xr = ix >= 0 ? xptr[ix] : T(0);
      const T Xi = ix >= 0 ? yptr[ix] : T(0);
      const T Xu = ix >= 0 ? uptr[ix] : T(1.0/0.0); // FIXME check validity
      const T Xv = ix >= 0 ? vptr[ix] : T(1.0/0.0); // FIXME check validity
<xsl:for-each select="references[@t='R']">
      const T <xsl:value-of select="@name" /> = ix >= 0 ? zptr<xsl:value-of select="position() - 1" />[ix] : T(<xsl:value-of select="@value" />);
</xsl:for-each>
<xsl:for-each select="references[@t='C']">
      const complex&lt;T&gt; <xsl:value-of select="@name" /> = ix >= 0 ? complex&lt;T&gt;(zptr<xsl:value-of select="2 * (position() - 1) + 0" />[ix], zptr<xsl:value-of select="2 * (position() - 1) + 1" />[ix]) : complex&lt;T&gt;(T(<xsl:value-of select="@value" />), T(0));
</xsl:for-each>
      rantal++;
      Xxr = Xr + xr;
      Xxi = Xi + xi;
      const complex&lt;T&gt; X = { Xr, Xi }, x = { xr, xi }, Xx = { Xxr, Xxi };
      (void) X; (void) x; (void) Xx;

      if (singleref)
      {
        if (skip)
        {
          // convergent bailout
          const T delta_x = Xxr - Xxr_1;
          const T delta_y = Xxi - Xxi_1;
          delta_1 = delta;
          delta = pnorm(g_real, g_imag, p, delta_x, delta_y);
          if (delta_1 &lt; m_nBailoutSmallP &amp;&amp; delta &lt; delta_1)
          {
            const T q = log(delta) / log(delta_1);
            const T f = (log(-log(m_nBailoutSmallP)/p) - log(-log(delta)/p)) / log(q);
            smooth = double(f);
            const T delta_z = sqrt(sqr(delta_x) + sqr(delta_y));
            phase = atan2(double(delta_x/delta_z), double(delta_y/delta_z)) / M_PI / 2;
            phase -= floor(phase);
            break;
          }
          // singleref glitch avoidance
          if (Xxr * Xxr + Xxi * Xxi &lt; xr * xr + xi * xi)
          {
            xr = Xxr;
            xi = Xxi;
            rantal = -1;
            skip = false;
            goto start;
          }
        }
      }
      else
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
        // convergent bailout
        {
          const T delta_x = (xr - xr_1) + Xu;
          const T delta_y = (xi - xi_1) + Xv;
          delta_1 = delta;
          delta = pnorm(g_real, g_imag, p, delta_x, delta_y);
          if (delta_1 &lt; m_nBailoutSmallP &amp;&amp; delta &lt; delta_1)
          {
            const T q = log(delta) / log(delta_1);
            const T f = (log(-log(m_nBailoutSmallP)/p) - log(-log(delta)/p)) / log(q);
            smooth = double(f);
            const T delta_z = sqrt(sqr(delta_x) + sqr(delta_y));
            phase = atan2(double(delta_x/delta_z), double(delta_y/delta_z)) / M_PI / 2;
            phase -= floor(phase);
            break;
          }
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

      Xxr_1 = Xxr;
      Xxi_1 = Xxi;
      xr_1 = xr;
      xi_1 = xi;
      xr = xrn;
      xi = xin;
    }

    if (antal == nMaxIter &amp;&amp; N &lt; nMaxIter)
    {
      bGlitch = true;
    }
    if (std::isnan(smooth) || std::isinf(smooth))
    {
      bGlitch = true;
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
  , const bool singleref
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
  , const bool singleref
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
  , const bool singleref
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
  , const bool singleref
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
  , const bool singleref
  );
</xsl:for-each></xsl:template>
</xsl:stylesheet>
