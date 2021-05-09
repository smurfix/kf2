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

#define FORMULA2(function,type,power) function ## _ ## type ## _ ## power
#define FORMULA(a,b,c) FORMULA2(a,b,c)

#ifdef PASS1 // reference

<xsl:for-each select="formulas/group/formula">

bool FORMULA(reference,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( const int m_nFractalType, const int m_nPower
  , Reference *m_Reference
  , bool &amp;m_bStop, int64_t &amp;m_nRDone, int64_t &amp;m_nGlitchIter, int64_t &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const bool m_bGlitchLowTolerance
  )
{
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    m_nGlitchIter = m_nMaxIter + 1;
    int64_t nMaxIter = m_nMaxIter;
    int64_t i;
    double glitch = <xsl:value-of select="@glitch" />;
    if (m_bGlitchLowTolerance) {
      glitch = sqrt(glitch);
    }
    mp_bitcnt_t bits = mpfr_get_prec(Cr0.m_f.backend().data());
    mpfr_t Cr; mpfr_init2(Cr, bits); mpfr_set(Cr, Cr0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Ci; mpfr_init2(Ci, bits); mpfr_set(Ci, Ci0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Xr; mpfr_init2(Xr, bits); mpfr_set_d(Xr, g_SeedR, MPFR_RNDN);
    mpfr_t Xi; mpfr_init2(Xi, bits); mpfr_set_d(Xi, g_SeedI, MPFR_RNDN);
    floatexp Xrd = mpfr_get_fe(Xr);
    floatexp Xid = mpfr_get_fe(Xi);
    mpfr_t Xr2; mpfr_init2(Xr2, bits); mpfr_sqr(Xr2, Xr, MPFR_RNDN);
    mpfr_t Xi2; mpfr_init2(Xi2, bits); mpfr_sqr(Xi2, Xi, MPFR_RNDN);
    mpfr_t Xrn; mpfr_init2(Xrn, bits);
    mpfr_t Xin; mpfr_init2(Xin, bits);
    mpfr_t Ar; mpfr_init2(Ar, bits); mpfr_set_d(Ar, g_FactorAR, MPFR_RNDN);
    mpfr_t Ai; mpfr_init2(Ai, bits); mpfr_set_d(Ai, g_FactorAI, MPFR_RNDN);
<xsl:choose>
<xsl:when test="reference/@t='C'">
    complex&lt;CFixedFloat&gt; C, A, X, Xn;
    mpfr_set(C.m_r.m_f.backend().data(), Cr, MPFR_RNDN);
    mpfr_set(C.m_i.m_f.backend().data(), Ci, MPFR_RNDN);
    mpfr_set(A.m_r.m_f.backend().data(), Ar, MPFR_RNDN);
    mpfr_set(A.m_i.m_f.backend().data(), Ai, MPFR_RNDN);
</xsl:when>
</xsl:choose>

#define LOOP \
      mpfr_set(Xr, Xrn, MPFR_RNDN); \
      mpfr_set(Xi, Xin, MPFR_RNDN); \
      mpfr_sqr(Xr2, Xr, MPFR_RNDN); \
      mpfr_sqr(Xi2, Xi, MPFR_RNDN); \
      m_nRDone++; \
      Xrd = mpfr_get_fe(Xr); \
      Xid = mpfr_get_fe(Xi); \
      const floatexp abs_val = Xrd * Xrd + Xid * Xid; \
      const floatexp Xz = abs_val * glitch; \
      reference_append(m_Reference, Xrd, Xid, Xz); \
      if (abs_val &gt;= terminate){ \
        if (nMaxIter == m_nMaxIter){ \
          nMaxIter = i + 10; \
          if (nMaxIter &gt; m_nMaxIter) \
            nMaxIter = m_nMaxIter; \
          m_nGlitchIter = nMaxIter; \
        } \
      }

<xsl:choose>
<xsl:when test="reference/@t='C'">
for (i = 0; i &lt; nMaxIter &amp;&amp; !m_bStop; i++)
      {
        mpfr_set(X.m_r.m_f.backend().data(), Xr, MPFR_RNDN);
        mpfr_set(X.m_i.m_f.backend().data(), Xi, MPFR_RNDN);
        {
          <xsl:value-of select="reference" />
        }
        mpfr_set(Xrn, Xn.m_r.m_f.backend().data(), MPFR_RNDN);
        mpfr_set(Xin, Xn.m_i.m_f.backend().data(), MPFR_RNDN);
LOOP  }
</xsl:when>
<xsl:when test="reference/@t='R'">
#define DLOOP
@rr   {
        <xsl:value-of select="reference" />
      }
#undef DLOOP
</xsl:when>
</xsl:choose>

#undef LOOP
    mpfr_clear(Cr);
    mpfr_clear(Ci);
    mpfr_clear(Xr);
    mpfr_clear(Xi);
    mpfr_clear(Xr2);
    mpfr_clear(Xi2);
    mpfr_clear(Xrn);
    mpfr_clear(Xin);
    mpfr_clear(Ar);
    mpfr_clear(Ai);
    return true;
  }
  return false;
}

</xsl:for-each>

bool reference
  ( const int m_nFractalType, const int m_nPower
  , Reference *m_Reference
  , bool &amp;m_bStop, int64_t &amp;m_nRDone, int64_t &amp;m_nGlitchIter, int64_t &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const bool m_bGlitchLowTolerance
  )
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
        case <xsl:value-of select="@power" />:
          return FORMULA(reference,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
            ( m_nFractalType, m_nPower
            , m_Reference
            , m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter
            , Cr, Ci
            , g_SeedR, g_SeedI
            , g_FactorAR, g_FactorAI
            , terminate
            , m_bGlitchLowTolerance
            );
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

#endif

#ifdef PASS3 // perturbation

<xsl:for-each select="formulas/group/formula">

template &lt;typename T&gt;
bool FORMULA(perturbation_simple,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , T &amp;xr0, T &amp;xi0
  , const T &amp;cr, const T &amp;ci
  )
{
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
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
    double test1 = test10;
    double test2 = test20;
    double phase = phase0;
    T xr = xr0;
    T xi = xi0;
    T Xxr = 0;
    T Xxi = 0;
    const T *xptr = reference_ptr_x&lt;T&gt;(m_Reference);
    const T *yptr = reference_ptr_y&lt;T&gt;(m_Reference);
    const T *zptr = reference_ptr_z&lt;T&gt;(m_Reference);
    for (; antal &lt; nMaxIter; antal++)
    {
      const T Xr = xptr[antal];
      const T Xi = yptr[antal];
      const T Xz = zptr[antal];
      Xxr = Xr + xr;
      Xxi = Xi + xi;
      const T Xxr2 = Xxr * Xxr;
      const T Xxi2 = Xxi * Xxi;
      test2 = test1;
      const T ttest1 = Xxr2 + Xxi2;
      test1 = double(ttest1);
      if (ttest1 &lt; Xz)
      {
        bGlitch = true;
        if (! m_bNoGlitchDetection)
          break;
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
<xsl:when test="perturbation/@t='C'">
      const complex&lt;T&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi};
      complex&lt;T&gt; xn;
      (void) X; (void) x; (void) Xx;
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

</xsl:for-each>

template &lt;typename T&gt;
bool perturbation_simple
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , T &amp;xr, T &amp;xi
  , const T &amp;cr, const T &amp;ci
  )
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
        case <xsl:value-of select="@power" />:
          return FORMULA(perturbation_simple,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
            ( m_nFractalType, m_nPower
            , m_Reference
            , antal, test1, test2, phase, bGlitch
            , m_nBailout2, nMaxIter
            , m_bNoGlitchDetection, g_real, g_imag, p
            , g_FactorAR, g_FactorAI
            , xr, xi
            , cr, ci
            );
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

template bool perturbation_simple&lt;float&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , float &amp;xr, float &amp;xi
  , const float &amp;cr, const float &amp;ci
  );
template bool perturbation_simple&lt;double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , double &amp;xr, double &amp;xi
  , const double &amp;cr, const double &amp;ci
  );
template bool perturbation_simple&lt;long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , long double &amp;xr, long double &amp;xi
  , const long double &amp;cr, const long double &amp;ci
  );
template bool perturbation_simple&lt;tfloatexp&lt;float,int32_t&gt;&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;float,int32_t&gt; &amp;xr, tfloatexp&lt;float,int32_t&gt; &amp;xi
  , const tfloatexp&lt;float,int32_t&gt; &amp;cr, const tfloatexp&lt;float,int32_t&gt; &amp;ci
  );
template bool perturbation_simple&lt;tfloatexp&lt;double,int64_t&gt;&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;double,int64_t&gt; &amp;xr, tfloatexp&lt;double,int64_t&gt; &amp;xi
  , const tfloatexp&lt;double,int64_t&gt; &amp;cr, const tfloatexp&lt;double,int64_t&gt; &amp;ci
  );

#endif

#ifdef PASS4 // perturbation with derivatives

<xsl:for-each select="formulas/group/formula">

template &lt;typename T&gt;
bool FORMULA(perturbation_simple_derivatives,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
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
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
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
    for (; antal &lt; nMaxIter; antal++)
    {
      const T Xr = xptr[antal];
      const T Xi = yptr[antal];
      const T Xz = zptr[antal];
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
<xsl:when test="../@type='0' and @power='2'">
        if (noDerivativeGlitch || type_0_power_2_pixel_has_glitched(cr, ci, xr, xi, Xr, Xi, dr / h, di / h, e, h)) // FIXME matrix derivatives
        {
</xsl:when>
</xsl:choose>
        bGlitch = true;
        if (! m_bNoGlitchDetection)
          break;
<xsl:choose>
<xsl:when test="../@type='0' and @power='2'">
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

</xsl:for-each>

template &lt;typename T&gt;
bool perturbation_simple_derivatives
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , T &amp;xr, T &amp;xi
  , const T &amp;cr, const T &amp;ci
  , T &amp;Jxa, T &amp;Jxb, T &amp;Jya, T &amp;Jyb
  , const T &amp;e, const T &amp;h
  , const T &amp;daa, const T &amp;dab, const T &amp;dba, const T &amp;dbb
  , const bool noDerivativeGlitch
  )
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
        case <xsl:value-of select="@power" />:
          return FORMULA(perturbation_simple_derivatives,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
            ( m_nFractalType, m_nPower
            , m_Reference
            , antal, test1, test2, phase, bGlitch
            , m_nBailout2, nMaxIter
            , m_bNoGlitchDetection, g_real, g_imag, p
            , g_FactorAR, g_FactorAI
            , xr, xi
            , cr, ci
            , Jxa, Jxb, Jya, Jyb
            , e, h
            , daa, dab, dba, dbb
            , noDerivativeGlitch
            );
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

template bool perturbation_simple_derivatives&lt;float&gt;
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
template bool perturbation_simple_derivatives&lt;double&gt;
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
template bool perturbation_simple_derivatives&lt;long double&gt;
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
template bool perturbation_simple_derivatives&lt;tfloatexp&lt;float,int32_t&gt;&gt;
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
template bool perturbation_simple_derivatives&lt;tfloatexp&lt;double,int64_t&gt;&gt;
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

#endif

#ifdef PASS5 // perturbation with SIMD

<xsl:for-each select="formulas/group/formula">

template &lt;typename double1, typename intN, typename doubleN&gt;
bool FORMULA(perturbation_SIMD,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
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
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
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
        doubleN xr_saved = xr0;
        doubleN xi_saved = xi0;
        doubleN test1_saved = test1;
        for (int64_t q = 0; q &lt; chunksize; ++q)
        {
          int64_t antal_q = antal[0] + q;
          const double1 Xr = xptr[antal_q];
          const double1 Xi = yptr[antal_q];
          const double1 Xz = zptr[antal_q];

          doubleN Xxr = Xr + xr0;
          doubleN Xxi = Xi + xi0;
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
      doubleN cr = cr0, ci = ci0, xr = xr0, xi = xi0;
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
      double1 cr = cr0[k], ci = ci0[k], xr = xr0[k], xi = xi0[k];
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

</xsl:for-each>

template &lt;typename double1, typename intN, typename doubleN&gt;
bool perturbation_SIMD
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , const double1 &amp;m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double1 &amp;g_real, const double1 &amp;g_imag, const double1 &amp;p
  , const double1 &amp;g_FactorAR, const double1 &amp;g_FactorAI
  , doubleN &amp;xr, doubleN &amp;xi
  , const doubleN &amp;cr, const doubleN &amp;ci
  , const int64_t chunksize
  )
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
        case <xsl:value-of select="@power" />:
          return FORMULA(perturbation_SIMD,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
            ( m_nFractalType, m_nPower
            , m_Reference
            , antal0, test10, test20, phase0, bGlitch0
            , m_nBailout2, nMaxIter
            , m_bNoGlitchDetection, g_real, g_imag, p
            , g_FactorAR, g_FactorAI
            , xr, xi
            , cr, ci
            , chunksize
            );
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

// explicit template instantiation

#if KF_SIMD >= 1
template bool perturbation_SIMD&lt;double, int2, double2&gt;
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
template bool perturbation_SIMD&lt;double, int4, double4&gt;
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
template bool perturbation_SIMD&lt;double, int8, double8&gt;
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
template bool perturbation_SIMD&lt;double, int16, double16&gt;
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

#endif

#ifdef PASSA // perturbation with SIMD and derivatives

<xsl:for-each select="formulas/group/formula">

template &lt;typename double1, typename intN, typename doubleN&gt;
bool FORMULA(perturbation_SIMD_derivatives,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
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
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
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
        doubleN xr_saved = xr0;
        doubleN xi_saved = xi0;
<xsl:choose>
<xsl:when test="derivative/@t='M'">
        doubleN dxa_saved = dxa0, dxb_saved = dxb0, dya_saved = dya0, dyb_saved = dyb0;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
        doubleN dr_saved = dr_0, di_saved = di_0;
</xsl:when>
</xsl:choose>
        doubleN test1_saved = test1;
        for (int64_t q = 0; q &lt; chunksize; ++q)
        {
          int64_t antal_q = antal[0] + q;
          const double1 Xr = xptr[antal_q];
          const double1 Xi = yptr[antal_q];
          const double1 Xz = zptr[antal_q];
          doubleN Xxr = Xr + xr0;
          doubleN Xxi = Xi + xi0;
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
      doubleN xr = xr0, xi = xi0, cr = cr0, ci = ci0;
      doubleN xrn, xin;
<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
      doubleN dr = dr_0, di = di_0;
      doubleN drn, din;
</xsl:when>
<xsl:when test="derivative/@t='M'">
      doubleN daa = daa0, dab = dab0, dba = dba0, dbb = dbb0;
      doubleN dxa = dxa0, dxb = dxb0, dya = dya0, dyb = dyb0;
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
        double1 xr = xr0[k], xi = xi0[k];
        Xxr = Xr + xr;
        Xxi = Xi + xi;
        const double1 Xxr2 = Xxr * Xxr;
        const double1 Xxi2 = Xxi * Xxi;
        test2[k] = test1[k];
        test1[k] = Xxr2 + Xxi2;
        if (test1[k] &lt; Xz)
        {
<xsl:choose>
<xsl:when test="../@type='0' and @power='2'">
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
<xsl:when test="../@type='0' and @power='2'">
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
      double1 dr = dr_0[k], di = di_0[k];
      double1 drn = 0, din = 0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
      double1 daa = daa0[k], dab = dab0[k], dba = dba0[k], dbb = dbb0[k];
      double1 dxa = dxa0[k], dxb = dxb0[k], dya = dya0[k], dyb = dyb0[k];
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

</xsl:for-each>

template &lt;typename double1, typename intN, typename doubleN&gt;
bool perturbation_SIMD_derivatives
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , const double1 &amp;m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double1 &amp;g_real, const double1 &amp;g_imag, const double1 &amp;p
  , const double1 &amp;g_FactorAR, const double1 &amp;g_FactorAI
  , doubleN &amp;xr0, doubleN &amp;xi0
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , doubleN &amp;Jxa0, doubleN &amp;Jxb0, doubleN &amp;Jya0, doubleN &amp;Jyb0
  , const double1 &amp;e, const double1 &amp;h
  , const doubleN &amp;daa0, const doubleN &amp;dab0, const doubleN &amp;dba0, const doubleN &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
  )
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
        case <xsl:value-of select="@power" />:
          return FORMULA(perturbation_SIMD_derivatives,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
            ( m_nFractalType, m_nPower
            , m_Reference
            , antal0, test10, test20, phase0, bGlitch0
            , m_nBailout2, nMaxIter
            , m_bNoGlitchDetection, g_real, g_imag, p
            , g_FactorAR, g_FactorAI
            , xr0, xi0
            , cr0, ci0
            , Jxa0, Jxb0, Jya0, Jyb0
            , e, h
            , daa0, dab0, dba0, dbb0
            , chunk_size
            , noDerivativeGlitch
            );
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

#if KF_SIMD >= 1
template bool perturbation_SIMD_derivatives&lt;double, int2, double2&gt;
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
template bool perturbation_SIMD_derivatives&lt;double, int4, double4&gt;
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
template bool perturbation_SIMD_derivatives&lt;double, int8, double8&gt;
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
template bool perturbation_SIMD_derivatives&lt;double, int16, double16&gt;
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

#endif

#ifdef PASS6 // perturbation with scaling

<xsl:for-each select="//scaled/..">

template &lt;typename mantissa, typename exponent&gt;
bool FORMULA(perturbation_scaled,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;mantissa, exponent&gt; &amp;xr00, tfloatexp&lt;mantissa, exponent&gt; &amp;xi00
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;cr, const tfloatexp&lt;mantissa, exponent&gt; &amp;ci
  )
{
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const mantissa LARGE_MANTISSA = sizeof(mantissa) == 4 ? 1e30 : 1e300;
    const mantissa SMALL_MANTISSA = sizeof(mantissa) == 4 ? 1e-18 : 1e-154;
    (void) SMALL_MANTISSA;
    const mantissa w2threshold = exp(log(LARGE_MANTISSA) / <xsl:value-of select="@power" />);
    const mantissa Ar = g_FactorAR;
    const mantissa Ai = g_FactorAI;
    const complex&lt;mantissa&gt; A = { Ar, Ai };
    const complex&lt;tfloatexp&lt;mantissa, exponent&gt;&gt; c = { cr, ci };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    const bool no_g = g_real == 1 &amp;&amp; g_imag == 1 &amp;&amp; p == 2;
    double test1 = test10;
    double test2 = test20;
    double phase = phase0;
    int64_t antal = antal0;
    bool bGlitch = bGlitch0;
    tfloatexp&lt;mantissa, exponent&gt; Xxr = 0;
    tfloatexp&lt;mantissa, exponent&gt; Xxi = 0;

    const int64_t size_N = reference_size_N(m_Reference);
    const int64_t *Nptr = reference_ptr_N(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Xptr = reference_ptr_X&lt;mantissa, exponent&gt;(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Yptr = reference_ptr_Y&lt;mantissa, exponent&gt;(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Zptr = reference_ptr_Z&lt;mantissa, exponent&gt;(m_Reference);

    int64_t k = 0, n = 0;
    tfloatexp&lt;mantissa, exponent&gt; X = 0, Y = 0, Z = 0;
    do
    {
      if (k &lt; size_N)
      {
        n = Nptr[k];
        X = Xptr[k];
        Y = Yptr[k];
        Z = Zptr[k];
        ++k;
      }
      else
      {
        n = nMaxIter;
      }
    }
    while (n &lt; antal);

    // rescale
    tfloatexp&lt;mantissa, exponent&gt; S = sqrt(xr00 * xr00 + xi00 * xi00);
    mantissa s = mantissa(S);
    mantissa wr = mantissa(xr00 / S);
    mantissa wi = mantissa(xi00 / S);
    mantissa ur = mantissa(cr / S);
    mantissa ui = mantissa(ci / S);
    mantissa u = mantissa(sqrt(cr * cr + ci * ci) / S);

    const mantissa *xptr = reference_ptr_x&lt;mantissa&gt;(m_Reference);
    const mantissa *yptr = reference_ptr_y&lt;mantissa&gt;(m_Reference);
    const mantissa *zptr = reference_ptr_z&lt;mantissa&gt;(m_Reference);
    for (; antal &lt; nMaxIter; antal++)
    {
      bool full_iteration = antal == n;
      if (full_iteration)
      {
        using T = tfloatexp&lt;mantissa, exponent&gt;;
        T dummyT;
        (void) dummyT;
        using V = tfloatexp&lt;mantissa, exponent&gt;;
        V dummyV;
        (void) dummyV;
        const tfloatexp&lt;mantissa, exponent&gt; Xr = X;
        const tfloatexp&lt;mantissa, exponent&gt; Xi = Y;
        const tfloatexp&lt;mantissa, exponent&gt; Xz = Z;
        if (k &lt; size_N)
        {
          n = Nptr[k];
          X = Xptr[k];
          Y = Yptr[k];
          Z = Zptr[k];
          ++k;
        }
        else
        {
          n = nMaxIter;
        }
        const tfloatexp&lt;mantissa, exponent&gt; xr = S * wr;
        const tfloatexp&lt;mantissa, exponent&gt; xi = S * wi;
        Xxr = Xr + xr;
        Xxi = Xi + xi;
        const tfloatexp&lt;mantissa, exponent&gt; Xxr2 = Xxr * Xxr;
        const tfloatexp&lt;mantissa, exponent&gt; Xxi2 = Xxi * Xxi;
        const tfloatexp&lt;mantissa, exponent&gt; xr2 = xr * xr;
        const tfloatexp&lt;mantissa, exponent&gt; xi2 = xi * xi;
        const tfloatexp&lt;mantissa, exponent&gt; Xr2 = Xr * Xr;
        const tfloatexp&lt;mantissa, exponent&gt; Xi2 = Xi * Xi;
        test2 = test1;
        tfloatexp&lt;mantissa, exponent&gt; ftest1 = Xxr2 + Xxi2;
        test1 = double(ftest1);
        if (ftest1 &lt; Xz)
        {
          bGlitch = true;
          if (! m_bNoGlitchDetection)
            break;
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
        tfloatexp&lt;mantissa, exponent&gt; xrn, xin;
        {
@d      {
          <xsl:value-of select="perturbation" />
        }
        }
        // rescale
        S = sqrt(xrn * xrn + xin * xin);
        s = mantissa(S);
        wr = mantissa(xrn / S);
        wi = mantissa(xin / S);
        ur = mantissa(cr / S);
        ui = mantissa(ci / S);
        u = mantissa(sqrt(cr * cr + ci * ci) / S);
      }
      else
      {
        using T = mantissa;
        T dummyT;
        (void) dummyT;
        using V = mantissa;
        V dummyV;
        (void) dummyV;
        const mantissa Xr = xptr[antal];
        const mantissa Xi = yptr[antal];
        const mantissa Xz = zptr[antal];
        const mantissa wr2 = wr * wr;
        (void) wr2;
        const mantissa wi2 = wi * wi;
        (void) wi2;
        const mantissa Xxrd = Xr + wr * s;
        const mantissa Xxid = Xi + wi * s;
        const mantissa Xxr2 = Xxrd * Xxrd;
        const mantissa Xxi2 = Xxid * Xxid;
        test2 = test1;
        test1 = Xxr2 + Xxi2;
        if (test1 &lt; Xz)
        {
          bGlitch = true;
          if (! m_bNoGlitchDetection)
          {
            Xxr = Xxrd;
            Xxi = Xxid;
            break;
          }
        }
        if (! no_g)
        {
          test1 = double(pnorm(g_real, g_imag, p, Xxrd, Xxid));
        }
        if (test1 &gt; m_nBailout2)
        {
          Xxr = Xxrd;
          Xxi = Xxid;
          phase = atan2(double(Xxid), double(Xxrd)) / M_PI / 2;
          phase -= floor(phase);
          break;
        }
        mantissa wrn, win;
        if (false) { }
<xsl:for-each select="scaled/threshold">
        else if (s &lt;= <xsl:value-of select="@s" /> &amp;&amp; u &lt;= <xsl:value-of select="@u" />)
        {
@d      {
          <xsl:value-of select="." />
        }
        }
</xsl:for-each>
        else
        {
          // assert(! "scaled/threshold");
          wrn = 0;
          win = 0;
        }
        const mantissa w2 = wrn * wrn + win * win;
        if (w2 &lt; w2threshold)
        {
          wr = wrn;
          wi = win;
        }
        else
        {
          // rescale
          tfloatexp&lt;mantissa, exponent&gt; xrn = S * wrn;
          tfloatexp&lt;mantissa, exponent&gt; xin = S * win;
          S = sqrt(xrn * xrn + xin * xin);
          s = mantissa(S);
          wr = mantissa(xrn / S);
          wi = mantissa(xin / S);
          ur = mantissa(cr / S);
          ui = mantissa(ci / S);
          u = mantissa(sqrt(cr * cr + ci * ci) / S);
        }
      }

    }

    antal0 = antal;
    bGlitch0 = bGlitch;
    test10 = test1;
    test20 = test2;
    phase0 = phase;
    xr00 = Xxr;
    xi00 = Xxi;
    return true;
  }
  return false;
}

</xsl:for-each>

template &lt;typename mantissa, typename exponent&gt;
bool perturbation_scaled
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;mantissa, exponent&gt; &amp;xr00, tfloatexp&lt;mantissa, exponent&gt; &amp;xi00
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;cr0, const tfloatexp&lt;mantissa, exponent&gt; &amp;ci0
  )
{
<xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return FORMULA(perturbation_scaled,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
      ( m_nFractalType, m_nPower
      , m_Reference
      , antal0, test10, test20, phase0, bGlitch0
      , m_nBailout2, nMaxIter
      , m_bNoGlitchDetection, g_real, g_imag, p
      , g_FactorAR, g_FactorAI
      , xr00, xi00
      , cr0, ci0
      );
</xsl:for-each>
  return false;
}

template bool perturbation_scaled&lt;float, int32_t&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;float, int32_t&gt; &amp;xr00, tfloatexp&lt;float, int32_t&gt; &amp;xi00
  , const tfloatexp&lt;float, int32_t&gt; &amp;cr0, const tfloatexp&lt;float, int32_t&gt; &amp;ci0
  );
template bool perturbation_scaled&lt;double, int64_t&gt;
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;double, int64_t&gt; &amp;xr00, tfloatexp&lt;double, int64_t&gt; &amp;xi00
  , const tfloatexp&lt;double, int64_t&gt; &amp;cr0, const tfloatexp&lt;double, int64_t&gt; &amp;ci0
  );

#endif

#if 0
#ifdef PASS7 // perturbation with SIMD and scaling

<xsl:for-each select="//scaled/..">

template &lt;typename intN, typename doubleN&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , const int64_t chunksize
  , const double s, const double S
  )
{
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const double Ar = g_FactorAR;
    const double Ai = g_FactorAI;
    const complex&lt;double&gt; A = { Ar, Ai };
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

    int64_t K = 0, N = 0;
    const int64_t K_size = reference_size_N(m_Reference);
    const int64_t *Nptr = reference_ptr_N(m_Reference);
    const floatexp *Xptr = reference_ptr_X&lt;double, int64_t&gt;(m_Reference);
    const floatexp *Yptr = reference_ptr_Y&lt;double, int64_t&gt;(m_Reference);
    const floatexp *Zptr = reference_ptr_Z&lt;double, int64_t&gt;(m_Reference);
    floatexp X = 0, Y = 0, Z = 0;
    do
    {
      if (K &lt; K_size)
      {
        N = Nptr[K];
        X = Xptr[K];
        Y = Yptr[K];
        Z = Zptr[K];
        K++;
      }
      else
      {
        N = nMaxIter;
      }
    }
    while (N &lt; antal[0]);
    int64_t K_saved = K;
    int64_t N_saved = N;
    floatexp X_saved = X;
    floatexp Y_saved = Y;
    floatexp Z_saved = Z;

    // vectorized loop
    const double *xptr = reference_ptr_x&lt;double&gt;(m_Reference);
    const double *yptr = reference_ptr_y&lt;double&gt;(m_Reference);
    const double *zptr = reference_ptr_z&lt;double&gt;(m_Reference);
    if (all(antal == antal[0]))
    {
      for (; antal[0] + chunksize - 1 &lt; nMaxIter; antal = antal + chunksize)
      {
        K_saved = K;
        N_saved = N;
        X_saved = X;
        Y_saved = Y;
        Z_saved = Z;
        doubleN xr_saved = xr0;
        doubleN xi_saved = xi0;
        doubleN test1_saved = test1;
        for (int64_t q = 0; q &lt; chunksize; ++q)
        {
          double Xr, Xi, Xz;
          int64_t antal_q = antal[0] + q;
          if (antal_q &lt; N)
          {
            Xr = xptr[antal_q];
            Xi = yptr[antal_q];
            Xz = zptr[antal_q];
          }
          else
          {
            Xr = double(X);
            Xi = double(Y);
            Xz = double(Z);
            if (K &lt; K_size)
            {
              N = Nptr[K];
              X = Xptr[K];
              Y = Yptr[K];
              Z = Zptr[K];
              ++K;
            }
            else
            {
              N = nMaxIter;
            }
          }

          doubleN Xxr = Xr + xr0 * s;
          doubleN Xxi = Xi + xi0 * s;
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

      using T = double;
      T dummyT;
      (void) dummyT;
      using V = doubleN;
      V dummyV;
      (void) dummyV;
<xsl:choose>
<xsl:when test="scaled/@t='R'">
      doubleN cr = cr0, ci = ci0, xr = xr0, xi = xi0;
@d    {
        <xsl:value-of select="scaled" />
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
    int64_t M = sizeof(doubleN) / sizeof(double);
    for (int64_t k = 0; k &lt; M; ++k)
    {
      double Xxr = 0, Xxi = 0;
      K = K_saved;
      N = N_saved;
      X = X_saved;
      Y = Y_saved;
      Z = Z_saved;
      for (; antal[k] &lt; nMaxIter; antal[k] = antal[k] + 1)
      {
        double Xr, Xi, Xz;
        int64_t antal_k = antal[k];
        if (antal_k &lt; N)
        {
          Xr = xptr[antal_k];
          Xi = yptr[antal_k];
          Xz = zptr[antal_k];
        }
        else
        {
          Xr = double(X);
          Xi = double(Y);
          Xz = double(Z);
          if (K &lt; K_size)
          {
            N = Nptr[K];
            X = Xptr[K];
            Y = Yptr[K];
            Z0 = Zptr[K];
            ++K;
          }
          else
          {
            N = nMaxIter;
          }
        }
        Xxr = Xr + xr0[k] * s;
        Xxi = Xi + xi0[k] * s;
        const double Xxr2 = Xxr * Xxr;
        const double Xxi2 = Xxi * Xxi;
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
          phase[k] = atan2(double(Xxi), double(Xxr)) / M_PI / 2;
          phase[k] -= floor(phase[k]);
          break;
        }
        using T = double;
        T dummy;
        (void) dummy;
        using V = double;
        V dummyV;
        (void) dummyV;
        double xrn, xin;

<xsl:choose>
<xsl:when test="scaled/@t='R'">
      double cr = cr0[k], ci = ci0[k], xr = xr0[k], xi = xi0[k];
@d    {
        <xsl:value-of select="scaled" />
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

</xsl:for-each>

template &lt;typename intN, typename doubleN&gt;
bool perturbation
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , const int64_t chunksize
  , const double s, const double S
  )
{
<xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
      ( m_nFractalType, m_nPower
      , m_Reference
      , antal0, test10, test20, phase0, bGlitch0
      , m_nBailout2, nMaxIter
      , m_bNoGlitchDetection, g_real, g_imag, p
      , g_FactorAR, g_FactorAI
      , xr00, xi00
      , cr0, ci0
      , chunksize
      , s, S
      );
</xsl:for-each>
  return false;
}

#if KF_SIMD >= 1
template bool perturbation&lt;int2, double2&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int2 &amp;antal0, double2 &amp;test10, double2 &amp;test20, double2 &amp;phase0, int2 &amp;bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const int2 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double2 &amp;xr00, double2 &amp;xi00
  , const double2 &amp;cr0, const double2 &amp;ci0
  , const int64_t chunksize
  , const double s, const double S
  );
#endif

#if KF_SIMD >= 2
template bool perturbation&lt;int4, double4&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int4 &amp;antal0, double4 &amp;test10, double4 &amp;test20, double4 &amp;phase0, int4 &amp;bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const int4 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double4 &amp;xr00, double4 &amp;xi00
  , const double4 &amp;cr0, const double4 &amp;ci0
  , const int64_t chunksize
  , const double s, const double S
  );
#endif

#if KF_SIMD >= 3
template bool perturbation&lt;int8, double8&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int8 &amp;antal0, double8 &amp;test10, double8 &amp;test20, double8 &amp;phase0, int8 &amp;bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const int8 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double8 &amp;xr00, double8 &amp;xi00
  , const double8 &amp;cr0, const double8 &amp;ci0
  , const int64_t chunksize
  , const double s, const double S
  );
#endif

#if KF_SIMD >= 4
template bool perturbation&lt;int16, double16&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int16 &amp;antal0, double16 &amp;test10, double16 &amp;test20, double16 &amp;phase0, int16 &amp;bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const int16 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double16 &amp;xr00, double16 &amp;xi00
  , const double16 &amp;cr0, const double16 &amp;ci0
  , const int64_t chunksize
  , const double s, const double S
  );
#endif

#endif
#endif

#ifdef PASS8 // perturbation with derivatives and scaling

<xsl:for-each select="//scaled/..">

template &lt;typename mantissa, typename exponent&gt;
bool FORMULA(perturbation_scaled_derivatives,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;mantissa, exponent&gt; &amp;xr0, tfloatexp&lt;mantissa, exponent&gt; &amp;xi0
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;cr, const tfloatexp&lt;mantissa, exponent&gt; &amp;ci
  , tfloatexp&lt;mantissa, exponent&gt; &amp;Jxa0F, tfloatexp&lt;mantissa, exponent&gt; &amp;Jxb0F, tfloatexp&lt;mantissa, exponent&gt; &amp;Jya0F, tfloatexp&lt;mantissa, exponent&gt; &amp;Jyb0F
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;daaF, const tfloatexp&lt;mantissa, exponent&gt; &amp;dabF, const tfloatexp&lt;mantissa, exponent&gt; &amp;dbaF, const tfloatexp&lt;mantissa, exponent&gt; &amp;dbbF
  )
{
  (void) Jxa0F; // -Wunused-parameter
  (void) Jxb0F; // -Wunused-parameter
  (void) Jya0F; // -Wunused-parameter
  (void) Jyb0F; // -Wunused-parameter
  (void) daaF; // -Wunused-parameter
  (void) dabF; // -Wunused-parameter
  (void) dbaF; // -Wunused-parameter
  (void) dbbF; // -Wunused-parameter
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const mantissa LARGE_MANTISSA = sizeof(mantissa) == 4 ? 1e30 : 1e300;
    const mantissa SMALL_MANTISSA = sizeof(mantissa) == 4 ? 1e-18 : 1e-154;
    (void) SMALL_MANTISSA;
    const mantissa w2threshold = exp(log(LARGE_MANTISSA) / <xsl:value-of select="@power" />);
    const mantissa d2threshold = exp(log(LARGE_MANTISSA) / (<xsl:value-of select="@power" /> - 1));
    const mantissa Ar = g_FactorAR;
    const mantissa Ai = g_FactorAI;
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    const bool no_g = g_real == 1 &amp;&amp; g_imag == 1 &amp;&amp; p == 2;
    mantissa test1 = test10;
    mantissa test2 = test20;
    mantissa phase = phase0;
    int64_t antal = antal0;
    bool bGlitch = bGlitch0;
    tfloatexp&lt;mantissa, exponent&gt; XxrF = 0;
    tfloatexp&lt;mantissa, exponent&gt; XxiF = 0;
<xsl:choose>
<xsl:when test="derivative/@t='C'">
    assert(! "perturbation scaled derivative C implemented");
</xsl:when>
<xsl:when test="derivative/@t='R'">
    tfloatexp&lt;mantissa, exponent&gt; drF = Jxa0F, diF = Jya0F;
    const tfloatexp&lt;mantissa, exponent&gt; dr0F = daaF, di0F = dbaF;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    tfloatexp&lt;mantissa, exponent&gt; dxaF = Jxa0F, dxbF = Jxb0F, dyaF = Jya0F, dybF = Jyb0F;
</xsl:when>
</xsl:choose>

    int64_t K = 0, N = 0;
    const int64_t K_size = reference_size_N(m_Reference);
    const int64_t *Nptr = reference_ptr_N(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Xptr = reference_ptr_X&lt;mantissa, exponent&gt;(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Yptr = reference_ptr_Y&lt;mantissa, exponent&gt;(m_Reference);
    const tfloatexp&lt;mantissa, exponent&gt; *Zptr = reference_ptr_Z&lt;mantissa, exponent&gt;(m_Reference);
    tfloatexp&lt;mantissa, exponent&gt; X = 0, Y = 0, Z0 = 0;
    do
    {
      if (K &lt; K_size)
      {
        N = Nptr[K];
        X = Xptr[K];
        Y = Yptr[K];
        Z0 = Zptr[K];
        K++;
      }
      else
      {
        N = nMaxIter;
      }
    }
    while (N &lt; antal);

    // rescale
    tfloatexp&lt;mantissa, exponent&gt; S = sqrt(xr0 * xr0 + xi0 * xi0);
    if (S == 0)
    {
      S = sqrt(cr * cr + ci * ci);
    }
    if (S == 0)
    {
      S = 1;
    }
    mantissa s = mantissa(S);
    mantissa wr = mantissa(xr0 / S);
    mantissa wi = mantissa(xi0 / S);
    mantissa ur = mantissa(cr / S);
    mantissa ui = mantissa(ci / S);
    mantissa u = mantissa(sqrt(cr * cr + ci * ci) / S);
<xsl:choose>
<xsl:when test="derivative/@t='R'">
    tfloatexp&lt;mantissa, exponent&gt; J0 = sqrt(drF * drF + diF * diF);
    if (J0 &lt; 1)
    {
      J0 = 1;
    }
    tfloatexp&lt;mantissa, exponent&gt; J = J0;
    mantissa drD = mantissa(drF / J);
    mantissa diD = mantissa(diF / J);
    mantissa dr0D = mantissa(dr0F / J);
    mantissa di0D = mantissa(di0F / J);
</xsl:when>
<xsl:when test="derivative/@t='M'">
    tfloatexp&lt;mantissa, exponent&gt; J0 = sqrt(dxaF * dxaF + dxbF * dxbF + dyaF * dyaF + dybF * dybF);
    if (J0 &lt; 1)
    {
      J0 = 1;
    }
    tfloatexp&lt;mantissa, exponent&gt; J = J0;
    mantissa dxaD = mantissa(dxaF / J);
    mantissa dyaD = mantissa(dyaF / J);
    mantissa dxbD = mantissa(dxbF / J);
    mantissa dybD = mantissa(dybF / J);
    mantissa daaD = mantissa(daaF / J);
    mantissa dbaD = mantissa(dbaF / J);
    mantissa dabD = mantissa(dabF / J);
    mantissa dbbD = mantissa(dbbF / J);
</xsl:when>
</xsl:choose>

    const mantissa *xptr = reference_ptr_x&lt;mantissa&gt;(m_Reference);
    const mantissa *yptr = reference_ptr_y&lt;mantissa&gt;(m_Reference);
    const mantissa *zptr = reference_ptr_z&lt;mantissa&gt;(m_Reference);
    for (; antal &lt; nMaxIter; antal++)
    {
      bool full_iteration = antal == N;
      if (full_iteration)
      {
        using T = tfloatexp&lt;mantissa, exponent&gt;;
        T dummyT;
        (void) dummyT;
        using V = tfloatexp&lt;mantissa, exponent&gt;;
        V dummyV;
        (void) dummyV;
        const tfloatexp&lt;mantissa, exponent&gt; Xr = X;
        const tfloatexp&lt;mantissa, exponent&gt; Xi = Y;
        const tfloatexp&lt;mantissa, exponent&gt; Xz = Z0;
        if (K &lt; K_size)
        {
          N = Nptr[K];
          X = Xptr[K];
          Y = Yptr[K];
          Z0 = Zptr[K];
          ++K;
        }
        else
        {
          N = nMaxIter;
        }
        const tfloatexp&lt;mantissa, exponent&gt; xr = S * wr;
        const tfloatexp&lt;mantissa, exponent&gt; xi = S * wi;
        XxrF = Xr + xr;
        XxiF = Xi + xi;
        const tfloatexp&lt;mantissa, exponent&gt; Xxr2 = XxrF * XxrF;
        const tfloatexp&lt;mantissa, exponent&gt; Xxi2 = XxiF * XxiF;
        const tfloatexp&lt;mantissa, exponent&gt; xr2 = xr * xr;
        const tfloatexp&lt;mantissa, exponent&gt; xi2 = xi * xi;
        const tfloatexp&lt;mantissa, exponent&gt; Xr2 = Xr * Xr;
        const tfloatexp&lt;mantissa, exponent&gt; Xi2 = Xi * Xi;
        test2 = test1;
        tfloatexp&lt;mantissa, exponent&gt; ftest1 = Xxr2 + Xxi2;
        test1 = mantissa(ftest1);
        if (ftest1 &lt; Xz)
        {
          bGlitch = true;
          if (! m_bNoGlitchDetection)
            break;
        }
        if (! no_g)
        {
          test1 = mantissa(pnorm(g_real, g_imag, p, XxrF, XxiF));
        }
        if (test1 &gt; m_nBailout2)
        {
          phase = atan2(mantissa(XxiF), mantissa(XxrF)) / M_PI / 2;
          phase -= floor(phase);
          break;
        }
        const tfloatexp&lt;mantissa, exponent&gt; Xxr = XxrF;
        const tfloatexp&lt;mantissa, exponent&gt; Xxi = XxiF;
        tfloatexp&lt;mantissa, exponent&gt; xrn, xin, drn, din, dxan, dyan, dxbn, dybn;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
        {
          (void) dxan;
          (void) dyan;
          (void) dxbn;
          (void) dybn;
          const tfloatexp&lt;mantissa, exponent&gt; dr = drD * J;
          const tfloatexp&lt;mantissa, exponent&gt; di = diD * J;
          const tfloatexp&lt;mantissa, exponent&gt; dr0 = dr0F;
          const tfloatexp&lt;mantissa, exponent&gt; di0 = di0F;
@d        {
            <xsl:value-of select="derivative" />
          }
        }
</xsl:when>
<xsl:when test="derivative/@t='M'">
        {
          (void) drn;
          (void) din;
          const tfloatexp&lt;mantissa, exponent&gt; dxa = dxaD * J;
          const tfloatexp&lt;mantissa, exponent&gt; dxb = dxbD * J;
          const tfloatexp&lt;mantissa, exponent&gt; dya = dyaD * J;
          const tfloatexp&lt;mantissa, exponent&gt; dyb = dybD * J;
          const tfloatexp&lt;mantissa, exponent&gt; daa = daaF;
          const tfloatexp&lt;mantissa, exponent&gt; dab = dabF;
          const tfloatexp&lt;mantissa, exponent&gt; dba = dbaF;
          const tfloatexp&lt;mantissa, exponent&gt; dbb = dbbF;
@d        {
            <xsl:value-of select="derivative" />
          }
        }
</xsl:when>
<xsl:when test="derivative/@t='C'">
        {
          assert(! "derivative type C implemented")
        }
</xsl:when>
</xsl:choose>
        {
@d      {
          <xsl:value-of select="perturbation" />
        }
        }
        // rescale
        S = sqrt(xrn * xrn + xin * xin);
        s = mantissa(S);
        wr = mantissa(xrn / S);
        wi = mantissa(xin / S);
        ur = mantissa(cr / S);
        ui = mantissa(ci / S);
        u = mantissa(sqrt(cr * cr + ci * ci) / S);
<xsl:choose>
<xsl:when test="derivative/@t='R'">
        J = sqrt(drn * drn + din * din);
        drD = mantissa(drn / J);
        diD = mantissa(din / J);
        dr0D = mantissa(dr0F / J);
        di0D = mantissa(di0F / J);
</xsl:when>
<xsl:when test="derivative/@t='M'">
        J = sqrt(dxan * dxan + dyan * dyan + dxbn * dxbn + dybn * dybn);
        dxaD = mantissa(dxan / J);
        dyaD = mantissa(dyan / J);
        dxbD = mantissa(dxbn / J);
        dybD = mantissa(dybn / J);
        daaD = mantissa(daaF / J);
        dbaD = mantissa(dbaF / J);
        dabD = mantissa(dabF / J);
        dbbD = mantissa(dbbF / J);
</xsl:when>
</xsl:choose>
      }
      else
      {
        using T = mantissa;
        T dummyT;
        (void) dummyT;
        using V = mantissa;
        V dummyV;
        (void) dummyV;
        const mantissa Xr = xptr[antal];
        const mantissa Xi = yptr[antal];
        const mantissa Xz = zptr[antal];
        const mantissa wr2 = wr * wr;
        (void) wr2;
        const mantissa wi2 = wi * wi;
        (void) wi2;
        const mantissa Xxrd = Xr + wr * s;
        const mantissa Xxid = Xi + wi * s;
        const mantissa Xxr2 = Xxrd * Xxrd;
        const mantissa Xxi2 = Xxid * Xxid;
        test2 = test1;
        test1 = Xxr2 + Xxi2;
        if (test1 &lt; Xz)
        {
          bGlitch = true;
          if (! m_bNoGlitchDetection)
          {
            XxrF = Xxrd;
            XxiF = Xxid;
            break;
          }
        }
        if (! no_g)
        {
          test1 = double(pnorm(g_real, g_imag, p, Xxrd, Xxid));
        }
        if (test1 &gt; m_nBailout2)
        {
          XxrF = Xxrd;
          XxiF = Xxid;
          phase = atan2(double(Xxid), double(Xxrd)) / M_PI / 2;
          phase -= floor(phase);
          break;
        }
        const mantissa Xxr = Xxrd;
        const mantissa Xxi = Xxid;
        mantissa drn, din, dxan, dyan, dxbn, dybn;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
        {
          (void) dxan;
          (void) dyan;
          (void) dxbn;
          (void) dybn;
          const mantissa dr = drD;
          const mantissa di = diD;
          const mantissa dr0 = dr0D;
          const mantissa di0 = di0D;
@d        {
            <xsl:value-of select="derivative" />
          }
        }
</xsl:when>
<xsl:when test="derivative/@t='M'">
        {
          (void) drn;
          (void) din;
          const mantissa dxa = dxaD;
          const mantissa dxb = dxbD;
          const mantissa dya = dyaD;
          const mantissa dyb = dybD;
          const mantissa daa = daaD;
          const mantissa dab = dabD;
          const mantissa dba = dbaD;
          const mantissa dbb = dbbD;
@d        {
            <xsl:value-of select="derivative" />
          }
        }
</xsl:when>
</xsl:choose>
        mantissa wrn, win;
        if (false) { }
<xsl:for-each select="scaled/threshold">
        else if (s &lt;= <xsl:value-of select="@s" /> &amp;&amp; u &lt;= <xsl:value-of select="@u" />)
        {
@d      {
          <xsl:value-of select="." />
        }
        }
</xsl:for-each>
        else
        {
          // assert(! "scaled/threshold");
          wrn = 0;
          win = 0;
        }
        const mantissa w2 = wrn * wrn + win * win;
        if (w2 &lt; w2threshold) // FIXME threshold depends on power
        {
          wr = wrn;
          wi = win;
        }
        else
        {
          // rescale
          tfloatexp&lt;mantissa, exponent&gt; xrn = S * wrn;
          tfloatexp&lt;mantissa, exponent&gt; xin = S * win;
          S = sqrt(xrn * xrn + xin * xin);
          s = mantissa(S);
          wr = mantissa(xrn / S);
          wi = mantissa(xin / S);
          ur = mantissa(cr / S);
          ui = mantissa(ci / S);
          u = mantissa(sqrt(cr * cr + ci * ci) / S);
        }
<xsl:choose>
<xsl:when test="derivative/@t='R'">
        const mantissa d2 = drn * drn + din * din;
        if (d2 &lt; d2threshold) // FIXME threshold depends on power
        {
          drD = drn;
          diD = din;
        }
        else
        {
          tfloatexp&lt;mantissa, exponent&gt; drF = J * drn;
          tfloatexp&lt;mantissa, exponent&gt; diF = J * din;
          J = sqrt(drF * drF + diF * diF);
          drD = mantissa(drF / J);
          diD = mantissa(diF / J);
          dr0D = mantissa(dr0F / J);
          di0D = mantissa(di0F / J);
        }
</xsl:when>
<xsl:when test="derivative/@t='M'">
        const mantissa d2 = dxan * dxan + dxbn * dxbn + dyan * dyan + dybn * dybn;
        if (d2 &lt; d2threshold) // FIXME threshold depends on power
        {
          dxaD = dxan;
          dyaD = dyan;
          dxbD = dxbn;
          dybD = dybn;
        }
        else
        {
          tfloatexp&lt;mantissa, exponent&gt; dxaF = J * dxan;
          tfloatexp&lt;mantissa, exponent&gt; dyaF = J * dyan;
          tfloatexp&lt;mantissa, exponent&gt; dxbF = J * dxbn;
          tfloatexp&lt;mantissa, exponent&gt; dybF = J * dybn;
          J = sqrt(dxaF * dxaF + dyaF * dyaF + dxbF * dxbF + dybF * dybF);
          dxaD = mantissa(dxaF / J);
          dxbD = mantissa(dxbF / J);
          dyaD = mantissa(dyaF / J);
          dybD = mantissa(dybF / J);
          daaD = mantissa(daaF / J);
          dbaD = mantissa(dbaF / J);
          dabD = mantissa(dabF / J);
          dbbD = mantissa(dbbF / J);
        }
</xsl:when>
</xsl:choose>
      }
    }


    antal0 = antal;
    bGlitch0 = bGlitch;
    test10 = test1;
    test20 = test2;
    phase0 = phase;
    xr0 = XxrF;
    xi0 = XxiF;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
    Jxa0F =  drD * J;
    Jxb0F = -diD * J;
    Jya0F =  diD * J;
    Jyb0F =  drD * J;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    Jxa0F = dxaD * J;
    Jxb0F = dxbD * J;
    Jya0F = dyaD * J;
    Jyb0F = dybD * J;
</xsl:when>
</xsl:choose>
    return true;
  }
  return false;
}

</xsl:for-each>

template &lt;typename mantissa, typename exponent&gt;
bool perturbation_scaled_derivatives
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;mantissa, exponent&gt; &amp;xr0, tfloatexp&lt;mantissa, exponent&gt; &amp;xi0
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;cr, const tfloatexp&lt;mantissa, exponent&gt; &amp;ci
  , tfloatexp&lt;mantissa, exponent&gt; &amp;Jxa0F, tfloatexp&lt;mantissa, exponent&gt; &amp;Jxb0F, tfloatexp&lt;mantissa, exponent&gt; &amp;Jya0F, tfloatexp&lt;mantissa, exponent&gt; &amp;Jyb0F
  , const tfloatexp&lt;mantissa, exponent&gt; &amp;daaF, const tfloatexp&lt;mantissa, exponent&gt; &amp;dabF, const tfloatexp&lt;mantissa, exponent&gt; &amp;dbaF, const tfloatexp&lt;mantissa, exponent&gt; &amp;dbbF
  )
{
<xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return FORMULA(perturbation_scaled_derivatives,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
      ( m_nFractalType, m_nPower
      , m_Reference
      , antal0, test10, test20, phase0, bGlitch0
      , m_nBailout2, nMaxIter
      , m_bNoGlitchDetection, g_real, g_imag, p
      , g_FactorAR, g_FactorAI
      , xr0, xi0
      , cr, ci
      , Jxa0F, Jxb0F, Jya0F, Jyb0F
      , daaF, dabF, dbaF, dbbF
      );
</xsl:for-each>
  return false;
}

template bool perturbation_scaled_derivatives&lt;float, int32_t&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;float, int32_t&gt; &amp;xr0, tfloatexp&lt;float, int32_t&gt; &amp;xi0
  , const tfloatexp&lt;float, int32_t&gt; &amp;cr, const tfloatexp&lt;float, int32_t&gt; &amp;ci
  , tfloatexp&lt;float, int32_t&gt; &amp;Jxa0F, tfloatexp&lt;float, int32_t&gt; &amp;Jxb0F, tfloatexp&lt;float, int32_t&gt; &amp;Jya0F, tfloatexp&lt;float, int32_t&gt; &amp;Jyb0F
  , const tfloatexp&lt;float, int32_t&gt; &amp;daaF, const tfloatexp&lt;float, int32_t&gt; &amp;dabF, const tfloatexp&lt;float, int32_t&gt; &amp;dbaF, const tfloatexp&lt;float, int32_t&gt; &amp;dbbF
  );
template bool perturbation_scaled_derivatives&lt;double, int64_t&gt;
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double &amp;m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &amp;g_real, const double &amp;g_imag, const double &amp;p
  , const double &amp;g_FactorAR, const double &amp;g_FactorAI
  , tfloatexp&lt;double, int64_t&gt; &amp;xr0, tfloatexp&lt;double, int64_t&gt; &amp;xi0
  , const tfloatexp&lt;double, int64_t&gt; &amp;cr, const tfloatexp&lt;double, int64_t&gt; &amp;ci
  , tfloatexp&lt;double, int64_t&gt; &amp;Jxa0F, tfloatexp&lt;double, int64_t&gt; &amp;Jxb0F, tfloatexp&lt;double, int64_t&gt; &amp;Jya0F, tfloatexp&lt;double, int64_t&gt; &amp;Jyb0F
  , const tfloatexp&lt;double, int64_t&gt; &amp;daaF, const tfloatexp&lt;double, int64_t&gt; &amp;dabF, const tfloatexp&lt;double, int64_t&gt; &amp;dbaF, const tfloatexp&lt;double, int64_t&gt; &amp;dbbF
  );
#endif

#ifdef PASS9 // miscellaneous

bool scaling_supported(const int m_nFractalType, const int m_nPower, const bool derivatives)
{
  <xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return true;
  </xsl:for-each>
  return false;
}

void combo5_addstrings(HWND hWnd, const int IDC_COMBO5)
{
  SendDlgItemMessage(hWnd,IDC_COMBO5,CB_ADDSTRING,0,(LPARAM)"(Hybrid)");
  <xsl:for-each select="formulas/group">
    SendDlgItemMessage(hWnd,IDC_COMBO5,CB_ADDSTRING,0,(LPARAM)"<xsl:value-of select="@name" />"); // <xsl:value-of select="@type" />
  </xsl:for-each>
}

int validate_power_for_fractal_type(const int m_nFractalType, const int m_nPower)
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
        default:
      <xsl:for-each select="formula">
        case <xsl:value-of select="@power" />: return <xsl:value-of select="@power" />;
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
    default:
      return 2;
  }
}

void update_power_dropdown_for_fractal_type(HWND hWnd, const int IDC_COMBO3, const int m_nFractalType, const int m_nPower)
{
  SendDlgItemMessage(hWnd,IDC_COMBO3,CB_RESETCONTENT,0,0);
  int selected = 0;
  int ix = 0;
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
    {
      <xsl:for-each select="formula">
        if (<xsl:value-of select="@power" /> == m_nPower) selected = ix;
        SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"<xsl:value-of select="@power" />");
        ix++;
      </xsl:for-each>
      break;
    }
  </xsl:for-each>
  }
  SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,selected,0);
  EnableWindow(GetDlgItem(hWnd,IDC_COMBO3), ix > 1);
}

#endif

// EOF
</xsl:template>
</xsl:stylesheet>
