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

template &lt;typename T&gt;
bool FORMULA(reference,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( const int m_nFractalType, const int m_nPower
  , T *m_db_dxr, T *m_db_dxi, double *m_db_z
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
    T Xrd = mpfr_get(Xr, T(0.0), MPFR_RNDN);
    T Xid = mpfr_get(Xi, T(0.0), MPFR_RNDN);
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
      Xrd = mpfr_get(Xr, T(0.0), MPFR_RNDN); \
      Xid = mpfr_get(Xi, T(0.0), MPFR_RNDN); \
      const double abs_val = double(Xrd * Xrd + Xid * Xid); \
      const double Xz = abs_val * glitch; \
      m_db_dxr[i] = Xrd; \
      m_db_dxi[i] = Xid; \
      m_db_z[i] = Xz; \
      if (abs_val &gt;= terminate){ \
        if (nMaxIter == m_nMaxIter){ \
          nMaxIter = i + 3; \
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

template &lt;typename T&gt;
bool reference
  ( const int m_nFractalType, const int m_nPower
  , T *m_dxr, T *m_dxi, double *m_db_z
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
            , m_dxr, m_dxi, m_db_z
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

template bool reference&lt;double&gt;
  ( const int m_nFractalType, const int m_nPower
  , double *m_db_dxr, double *m_db_dxi, double *m_db_z
  , bool &amp;m_bStop, int64_t &amp;m_nRDone, int64_t &amp;m_nGlitchIter, int64_t &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const bool m_bGlitchLowTolerance
  );
template bool reference&lt;long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , long double *m_db_dxr, long double *m_db_dxi, double *m_db_z
  , bool &amp;m_bStop, int64_t &amp;m_nRDone, int64_t &amp;m_nGlitchIter, int64_t &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const bool m_bGlitchLowTolerance
  );
template bool reference&lt;floatexp&gt;
  ( const int m_nFractalType, const int m_nPower
  , floatexp *m_db_dxr, floatexp *m_db_dxi, double *m_db_z
  , bool &amp;m_bStop, int64_t &amp;m_nRDone, int64_t &amp;m_nGlitchIter, int64_t &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const bool m_bGlitchLowTolerance
  );

#endif

#ifdef PASS3 // perturbation

<xsl:for-each select="formulas/group/formula">

template &lt;typename T&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
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
    for (; antal &lt; nMaxIter; antal++)
    {
      const T Xr = m_db_dxr[antal];
      const T Xi = m_db_dxi[antal];
      const double Xz = m_db_z[antal];
      Xxr = Xr + xr;
      Xxi = Xi + xi;
      const T Xxr2 = Xxr * Xxr;
      const T Xxi2 = Xxi * Xxi;
      test2 = test1;
      test1 = double(Xxr2 + Xxi2);
      if (test1 &lt; Xz)
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
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const T *m_dxr, const T *m_dxi, const double *m_db_z
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
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
          return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
            ( m_nFractalType, m_nPower
            , m_dxr, m_dxi, m_db_z
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

template bool perturbation&lt;double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const double *m_dxr, const double *m_dxi, const double *m_db_z
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double &amp;xr, double &amp;xi
  , const double &amp;cr, const double &amp;ci
  );
template bool perturbation&lt;long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const long double *m_dxr, const long double *m_dxi, const double *m_db_z
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , long double &amp;xr, long double &amp;xi
  , const long double &amp;cr, const long double &amp;ci
  );
template bool perturbation&lt;floatexp&gt;
  ( const int m_nFractalType, const int m_nPower
  , const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , floatexp &amp;xr, floatexp &amp;xi
  , const floatexp &amp;cr, const floatexp &amp;ci
  );

#endif

#ifdef PASS4 // perturbation with derivatives

<xsl:for-each select="formulas/group/formula">

template &lt;typename T&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
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
    for (; antal &lt; nMaxIter; antal++)
    {
      const T Xr = m_db_dxr[antal];
      const T Xi = m_db_dxi[antal];
      const double Xz = m_db_z[antal];
      Xxr = Xr + xr;
      Xxi = Xi + xi;
      const T Xxr2 = Xxr * Xxr;
      const T Xxi2 = Xxi * Xxi;
      test2 = test1;
      test1 = double(Xxr2 + Xxi2);
      if (test1 &lt; Xz)
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
bool perturbation
  ( int m_nFractalType, int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
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
          return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
            ( m_nFractalType, m_nPower
            , m_db_dxr, m_db_dxi, m_db_z
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

template bool perturbation&lt;double&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double &amp;xr0, double &amp;xi0
  , const double &amp;cr, const double &amp;ci
  , double &amp;Jxa0, double &amp;Jxb0, double &amp;Jya0, double &amp;Jyb0
  , const double &amp;e, const double &amp;h
  , const double &amp;daa, const double &amp;dab, const double &amp;dba, const double &amp;dbb
  , const bool noDerivativeGlitch
  );
template bool perturbation&lt;long double&gt;
  ( int m_nFractalType, int m_nPower
  , const long double *m_db_dxr, const long double *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , long double &amp;xr0, long double &amp;xi0
  , const long double &amp;cr, const long double &amp;ci
  , long double &amp;Jxa0, long double &amp;Jxb0, long double &amp;Jya0, long double &amp;Jyb0
  , const long double &amp;e, const long double &amp;h
  , const long double &amp;daa, const long double &amp;dab, const long double &amp;dba, const long double &amp;dbb
  , const bool noDerivativeGlitch
  );
template bool perturbation&lt;floatexp&gt;
  ( int m_nFractalType, int m_nPower
  , const floatexp *m_db_dxr, const floatexp *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase, bool &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , floatexp &amp;xr0, floatexp &amp;xi0
  , const floatexp &amp;cr, const floatexp &amp;ci
  , floatexp &amp;Jxa0, floatexp &amp;Jxb0, floatexp &amp;Jya0, floatexp &amp;Jyb0
  , const floatexp &amp;e, const floatexp &amp;h
  , const floatexp &amp;daa, const floatexp &amp;dab, const floatexp &amp;dba, const floatexp &amp;dbb
  , const bool noDerivativeGlitch
  );

#endif

#ifdef PASS5 // perturbation with SIMD

<xsl:for-each select="formulas/group/formula">

template &lt;typename intN, typename doubleN&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , const int64_t chunksize
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
    // vectorized loop
    if (all(antal == antal[0]))
    {
      for (; antal[0] + chunksize - 1 &lt; nMaxIter; antal = antal + chunksize)
      {
        doubleN xr_saved = xr0;
        doubleN xi_saved = xi0;
        doubleN test1_saved = test1;
        for (int64_t q = 0; q &lt; chunksize; ++q)
        {
          const double Xr = m_db_dxr[antal[0] + q];
          const double Xi = m_db_dxi[antal[0] + q];
          const double Xz = m_db_z[antal[0] + q];
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

      using T = double;
      T dummyT;
      (void) dummyT;
      using V = doubleN;
      V dummyV;
      (void) dummyV;
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;double&gt; X = {Xr, Xi};
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
    const int64_t N = sizeof(doubleN) / sizeof(double);
    for (int64_t k = 0; k &lt; N; ++k)
    {
      double Xxr = 0, Xxi = 0;
      for (; antal[k] &lt; nMaxIter; antal[k] = antal[k] + 1)
      {
        const double Xr = m_db_dxr[antal[k]];
        const double Xi = m_db_dxi[antal[k]];
        const double Xz = m_db_z[antal[k]];
        Xxr = Xr + xr0[k];
        Xxi = Xi + xi0[k];
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
        double xrn, xin;

      using T = double;
      T dummyT;
      (void) dummyT;
      using V = double;
      V dummyV;
      (void) dummyV;
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;double&gt; X = {Xr, Xi}, x = {xr0[k], xi0[k]}, Xx = {Xxr, Xxi}, c = { cr0[k], ci0[k] };
      complex&lt;double&gt; xn;
      (void) X; (void) x; (void) Xx;
@dc   {
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.m_r; xin = xn.m_i;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
      double cr = cr0[k], ci = ci0[k], xr = xr0[k], xi = xi0[k];
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

template &lt;typename intN, typename doubleN&gt;
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , intN &amp;antal, doubleN &amp;test1, doubleN &amp;test2, doubleN &amp;phase, intN &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
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
          return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
            ( m_nFractalType, m_nPower
            , m_db_dxr, m_db_dxi, m_db_z
            , antal, test1, test2, phase, bGlitch
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
template bool perturbation&lt;int2, double2&gt;
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int2 &amp;antal, double2 &amp;test1, double2 &amp;test2, double2 &amp;phase, int2 &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const int2 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double2 &amp;xr, double2 &amp;xi
  , const double2 &amp;cr, const double2 &amp;ci
  , const int64_t chunksize
  );
#endif

#if KF_SIMD >= 2
template bool perturbation&lt;int4, double4&gt;
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int4 &amp;antal, double4 &amp;test1, double4 &amp;test2, double4 &amp;phase, int4 &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const int4 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double4 &amp;xr, double4 &amp;xi
  , const double4 &amp;cr, const double4 &amp;ci
  , const int64_t chunksize
  );
#endif

#if KF_SIMD >= 3
template bool perturbation&lt;int8, double8&gt;
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int8 &amp;antal, double8 &amp;test1, double8 &amp;test2, double8 &amp;phase, int8 &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const int8 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double8 &amp;xr, double8 &amp;xi
  , const double8 &amp;cr, const double8 &amp;ci
  , const int64_t chunksize
  );
#endif

#if KF_SIMD >= 4
template bool perturbation&lt;int16, double16&gt;
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int16 &amp;antal, double16 &amp;test1, double16 &amp;test2, double16 &amp;phase, int16 &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const int16 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double16 &amp;xr, double16 &amp;xi
  , const double16 &amp;cr, const double16 &amp;ci
  , const int64_t chunksize
  );
#endif

#endif

#ifdef PASSA // perturbation with SIMD and derivatives

<xsl:for-each select="formulas/group/formula">

template &lt;typename intN, typename doubleN&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , doubleN &amp;Jxa0, doubleN &amp;Jxb0, doubleN &amp;Jya0, doubleN &amp;Jyb0
  , const double e, const double h
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
    const double Ar = g_FactorAR;
    const double Ai = g_FactorAI;
    const complex&lt;double&gt; A = { Ar, Ai };
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
          const double Xr = m_db_dxr[antal[0] + q];
          const double Xi = m_db_dxi[antal[0] + q];
          const double Xz = m_db_z[antal[0] + q];
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
    const int64_t N = sizeof(doubleN) / sizeof(double);
    for (int64_t k = 0; k &lt; N; ++k)
    {
      const double dr0 = daa0[k], di0 = dba0[k];
      (void) dr0;
      (void) di0;
      double Xxr = 0, Xxi = 0;
      for (; antal[k] &lt; nMaxIter; antal[k] = antal[k] + 1)
      {
        const double Xr = m_db_dxr[antal[k]];
        const double Xi = m_db_dxi[antal[k]];
        const double Xz = m_db_z[antal[k]];
        const double cr = cr0[k], ci = ci0[k];
        double xr = xr0[k], xi = xi0[k];
        Xxr = Xr + xr;
        Xxi = Xi + xi;
        const double Xxr2 = Xxr * Xxr;
        const double Xxi2 = Xxi * Xxi;
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
          phase[k] = atan2(double(Xxi), double(Xxr)) / M_PI / 2;
          phase[k] -= floor(phase[k]);
          break;
        }
        double xrn = 0, xin = 0;

      using T = double;
      T dummyT;
      (void) dummyT;
      using V = double;
      V dummyV;
      (void) dummyV;
<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
      double dr = dr_0[k], di = di_0[k];
      double drn = 0, din = 0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
      double daa = daa0[k], dab = dab0[k], dba = dba0[k], dbb = dbb0[k];
      double dxa = dxa0[k], dxb = dxb0[k], dya = dya0[k], dyb = dyb0[k];
      double dxan = 0, dxbn = 0, dyan = 0, dybn = 0;
</xsl:when>
</xsl:choose>
<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;double&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi}, c = {cr, ci};
      complex&lt;double&gt; xn;
      (void) X; (void) x; (void) Xx;
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
@d    {
        <xsl:value-of select="derivative" />
      }
</xsl:when>
<xsl:when test="derivative/@t='C'">
      const complex&lt;double&gt; d = {dr, di}, d0 = {daa0[k], dba0[k]}; <!-- FIXME matrix derivatives -->
      complex&lt;double&gt; dn;
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
      const complex&lt;double&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi}, c = {cr, ci}, d = {dr, di}, d0 = {daa[k], dba[k]}; <!-- FIXME matrix derivatives -->
      complex&lt;double&gt; dn;
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

template &lt;typename intN, typename doubleN&gt;
bool perturbation
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, doubleN &amp;phase0, intN &amp;bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const intN &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &amp;xr0, doubleN &amp;xi0
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , doubleN &amp;Jxa0, doubleN &amp;Jxb0, doubleN &amp;Jya0, doubleN &amp;Jyb0
  , const double e, const double h
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
          return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
            ( m_nFractalType, m_nPower
            , m_db_dxr, m_db_dxi, m_db_z
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
template bool perturbation&lt;int2, double2&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int2 &amp;antal0, double2 &amp;test10, double2 &amp;test20, double2 &amp;phase0, int2 &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const int2 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double2 &amp;xr0, double2 &amp;xi0
  , const double2 &amp;cr0, const double2 &amp;ci0
  , double2 &amp;Jxa0, double2 &amp;Jxb0, double2 &amp;Jya0, double2 &amp;Jyb0
  , const double e, const double h
  , const double2 &amp;daa0, const double2 &amp;dab0, const double2 &amp;dba0, const double2 &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
  );
#endif

#if KF_SIMD >= 2
template bool perturbation&lt;int4, double4&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int4 &amp;antal0, double4 &amp;test10, double4 &amp;test20, double4 &amp;phase0, int4 &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const int4 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double4 &amp;xr0, double4 &amp;xi0
  , const double4 &amp;cr0, const double4 &amp;ci0
  , double4 &amp;Jxa0, double4 &amp;Jxb0, double4 &amp;Jya0, double4 &amp;Jyb0
  , const double e, const double h
  , const double4 &amp;daa0, const double4 &amp;dab0, const double4 &amp;dba0, const double4 &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
  );
#endif

#if KF_SIMD >= 3
template bool perturbation&lt;int8, double8&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int8 &amp;antal0, double8 &amp;test10, double8 &amp;test20, double8 &amp;phase0, int8 &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const int8 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double8 &amp;xr0, double8 &amp;xi0
  , const double8 &amp;cr0, const double8 &amp;ci0
  , double8 &amp;Jxa0, double8 &amp;Jxb0, double8 &amp;Jya0, double8 &amp;Jyb0
  , const double e, const double h
  , const double8 &amp;daa0, const double8 &amp;dab0, const double8 &amp;dba0, const double8 &amp;dbb0
  , const int64_t chunk_sizes
  , const bool noDerivativeGlitch
  );
#endif

#if KF_SIMD >= 4
template bool perturbation&lt;int16, double16&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int16 &amp;antal0, double16 &amp;test10, double16 &amp;test20, double16 &amp;phase0, int16 &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const int16 &amp;m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double16 &amp;xr0, double16 &amp;xi0
  , const double16 &amp;cr0, const double16 &amp;ci0
  , double16 &amp;Jxa0, double16 &amp;Jxb0, double16 &amp;Jya0, double16 &amp;Jyb0
  , const double e, const double h
  , const double16 &amp;daa0, const double16 &amp;dab0, const double16 &amp;dba0, const double16 &amp;dbb0
  , const int64_t chunk_size
  , const bool noDerivativeGlitch
  );
#endif

#endif

#ifdef PASS6 // perturbation with scaling

<xsl:for-each select="//scaled/..">

template &lt;typename T&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( const int m_nFractalType, const int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch0
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , T &amp;xr00, T &amp;xi00
  , const T &amp;cr0, const T &amp;ci0
  , const T &amp;s, const T &amp;S
  )
{
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const double Ar = g_FactorAR;
    const double Ai = g_FactorAI;
    const complex&lt;double&gt; A = { Ar, Ai };
    const complex&lt;T&gt; c = { cr0, ci0 };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0 &amp;&amp; p == 2.0;
    double test1 = test10;
    double test2 = test20;
    double phase = phase0;
    T xr0 = xr00;
    T xi0 = xi00;
    int64_t antal = antal0;
    bool bGlitch = bGlitch0;
    T Xxr = 0;
    T Xxi = 0;
      for (; antal &lt; nMaxIter; antal = antal + 1)
      {
        const T Xr = m_db_dxr[antal];
        const T Xi = m_db_dxi[antal];
        const double Xz = m_db_z[antal];
        Xxr = Xr + xr0 * s;
        Xxi = Xi + xi0 * s;
        const T Xxr2 = Xxr * Xxr;
        const T Xxi2 = Xxi * Xxi;
        test2 = test1;
        test1 = double(Xxr2 + Xxi2);
        if (test1 &lt; Xz)
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

      using V = T;
      V dummyV;
      (void) dummyV;
<xsl:choose>
<xsl:when test="scaled/@t='R'">
      T cr = cr0, ci = ci0, xr = xr0, xi = xi0;
@d    {
        <xsl:value-of select="scaled" />
      }
</xsl:when>
</xsl:choose>

        xr0 = xrn;
        xi0 = xin;
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

template &lt;typename T&gt;
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , T &amp;xr00, T &amp;xi00
  , const T &amp;cr0, const T &amp;ci0
  , const T &amp;s, const T &amp;S
  )
{
<xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
      ( m_nFractalType, m_nPower
      , m_db_dxr, m_db_dxi, m_db_z
      , antal, test1, test2, phase, bGlitch
      , m_nBailout2, nMaxIter
      , m_bNoGlitchDetection, g_real, g_imag, p
      , g_FactorAR, g_FactorAI
      , xr00, xi00
      , cr0, ci0
      , s, S
      );
</xsl:for-each>
  return false;
}

template bool perturbation&lt;double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double &amp;xr00, double &amp;xi00
  , const double &amp;cr0, const double &amp;ci0
  , const double &amp;s, const double &amp;S
  );
template bool perturbation&lt;long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const long double *m_db_dxr, const long double *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , long double &amp;xr00, long double &amp;xi00
  , const long double &amp;cr0, const long double &amp;ci0
  , const long double &amp;s, const long double &amp;S
  );

#endif

#ifdef PASS7 // perturbation with SIMD and scaling

<xsl:for-each select="//scaled/..">

template &lt;typename intN, typename doubleN&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
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
    // vectorized loop
    if (all(antal == antal[0]))
    {
      for (; antal[0] + chunksize - 1 &lt; nMaxIter; antal = antal + chunksize)
      {
        doubleN xr_saved = xr0;
        doubleN xi_saved = xi0;
        doubleN test1_saved = test1;
        for (int64_t q = 0; q &lt; chunksize; ++q)
        {
          const double Xr = m_db_dxr[antal[0] + q];
          const double Xi = m_db_dxi[antal[0] + q];
          const double Xz = m_db_z[antal[0] + q];
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
    int64_t N = sizeof(doubleN) / sizeof(double);
    for (int64_t k = 0; k &lt; N; ++k)
    {
      double Xxr = 0, Xxi = 0;
      for (; antal[k] &lt; nMaxIter; antal[k] = antal[k] + 1)
      {
        const double Xr = m_db_dxr[antal[k]];
        const double Xi = m_db_dxi[antal[k]];
        const double Xz = m_db_z[antal[k]];
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
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
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
      , m_db_dxr, m_db_dxi, m_db_z
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
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
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
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
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
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
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
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
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

#ifdef PASS8 // perturbation with derivatives and scaling

<xsl:for-each select="//scaled/..">

template &lt;typename D, typename Z&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const Z *m_db_dxr, const Z *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , Z &amp;xr0, Z &amp;xi0
  , const Z &amp;cr, const Z &amp;ci
  , D &amp;Jxa0, D &amp;Jxb0, D &amp;Jya0, D &amp;Jyb0
  , const D &amp;e, const D &amp;h
  , const D &amp;daa, const D &amp;dab, const D &amp;dba, const D &amp;dbb
  , const Z &amp;s, const Z &amp;S
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
    const Z Ar = g_FactorAR;
    const Z Ai = g_FactorAI;
    const complex&lt;Z&gt; A = { Ar, Ai };
    const complex&lt;Z&gt; c = { cr, ci };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    int64_t antal = antal0;
    double test1 = test10;
    double test2 = test20;
    double phase = phase0;
    Z xr = xr0;
    Z xi = xi0;
<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
    D dr = Jxa0, di = Jya0;
    const D dr0 = daa, di0 = dba;
    (void) dr0;
    (void) di0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    D dxa = Jxa0, dxb = Jxb0, dya = Jya0, dyb = Jyb0;
</xsl:when>
</xsl:choose>
    Z Xxr = 0;
    Z Xxi = 0;
    for (; antal &lt; nMaxIter; antal++)
    {
      const Z Xr = m_db_dxr[antal];
      const Z Xi = m_db_dxi[antal];
      const double Xz = m_db_z[antal];
      Xxr = Xr + xr * s;
      Xxi = Xi + xi * s;
      const Z Xxr2 = Xxr * Xxr;
      const Z Xxi2 = Xxi * Xxi;
      test2 = test1;
      test1 = double(Xxr2 + Xxi2);
      if (test1 &lt; Xz)
      {
<xsl:choose>
<xsl:when test="../@type='0' and @power='2'">
#ifdef KF_USE_TYPE_0_POWER_2_HAS_GLITCHED
        if (noDerivativeGlitch || type_0_power_2_pixel_has_glitched(cr, ci, xr, xi, Xr, Xi, dxa / h, dya / h, e, h)) // FIXME matrix derivatives
        {
#endif
</xsl:when>
</xsl:choose>
        bGlitch = true;
        if (! m_bNoGlitchDetection)
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
        test1 = double(pnorm(g_real, g_imag, p, Xxr, Xxi));
      }
      if (test1 &gt; m_nBailout2)
      {
        phase = atan2(double(Xxi), double(Xxr)) / M_PI / 2;
        phase -= floor(phase);
        break;
      }
      Z xrn, xin;

<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
      D drn = 0, din = 0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
      D dxan = 0, dxbn = 0, dyan = 0, dybn = 0;
</xsl:when>
</xsl:choose>
<xsl:choose>
<xsl:when test="scaled/@t='R'">
{
  using T = D;
  T dummyT;
  (void) dummyT;
  using V = D;
  V dummyV;
  (void) dummyV;
<xsl:choose>
<xsl:when test="derivative/@t='C'">
      const complex&lt;Z&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi};
      const complex&lt;D&gt; d = {dr, di}, d0 = {daa, dba}; <!-- FIXME matrix derivatives -->
      complex&lt;D&gt; dn;
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
}
  using T = Z;
  T dummyT;
  (void) dummyT;
  using V = Z;
  V dummyV;
  (void) dummyV;

@d    {
        <xsl:value-of select="scaled" />
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

template &lt;typename D, typename Z&gt;
bool perturbation
  ( int m_nFractalType, int m_nPower
  , const Z *m_db_dxr, const Z *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal, double &amp;test1, double &amp;test2, double &amp;phase, bool &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , Z &amp;xr, Z &amp;xi
  , const Z &amp;cr, const Z &amp;ci
  , D &amp;Jxa0, D &amp;Jxb0, D &amp;Jya0, D &amp;Jyb0
  , const D &amp;e, const D &amp;h
  , const D &amp;daa, const D &amp;dab, const D &amp;dba, const D &amp;dbb
  , const Z &amp;s, const Z &amp;S
  , const bool noDerivativeGlitch
  )
{
<xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
      ( m_nFractalType, m_nPower
      , m_db_dxr, m_db_dxi, m_db_z
      , antal, test1, test2, phase, bGlitch
      , m_nBailout2, nMaxIter
      , m_bNoGlitchDetection, g_real, g_imag, p
      , g_FactorAR, g_FactorAI
      , xr, xi
      , cr, ci
      , Jxa0, Jxb0, Jya0, Jyb0
      , e, h
      , daa, dab, dba, dbb
      , s, S
      , noDerivativeGlitch
      );
</xsl:for-each>
  return false;
}

template bool perturbation&lt;long double, double&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , double &amp;xr0, double &amp;xi0
  , const double &amp;cr, const double &amp;ci
  , long double &amp;Jxa0, long double &amp;Jxb0, long double &amp;Jya0, long double &amp;Jyb0
  , const long double &amp;e, const long double &amp;h
  , const long double &amp;daa, const long double &amp;dab, const long double &amp;dba, const long double &amp;dbb
  , const double &amp;s, const double &amp;S
  , const bool noDerivativeGlitch
  );
template bool perturbation&lt;floatexp, long double&gt;
  ( int m_nFractalType, int m_nPower
  , const long double *m_db_dxr, const long double *m_db_dxi, const double *m_db_z
  , int64_t &amp;antal0, double &amp;test10, double &amp;test20, double &amp;phase0, bool &amp;bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , long double &amp;xr0, long double &amp;xi0
  , const long double &amp;cr, const long double &amp;ci
  , floatexp &amp;Jxa0, floatexp &amp;Jxb0, floatexp &amp;Jya0, floatexp &amp;Jyb0
  , const floatexp &amp;e, const floatexp &amp;h
  , const floatexp &amp;daa, const floatexp &amp;dab, const floatexp &amp;dba, const floatexp &amp;dbb
  , const long double &amp;s, const long double &amp;S
  , const bool noDerivativeGlitch
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
