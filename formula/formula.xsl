<?xml version="1.0" encoding="UTF-8"?>
<!--
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2018 Claude Heiland-Allen

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

#define FORMULA2(function,type,power) function ## _ ## type ## _ ## power
#define FORMULA(a,b,c) FORMULA2(a,b,c)

using std::abs;

<xsl:for-each select="formulas/group/formula">

template &lt;typename T&gt;
bool FORMULA(reference,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( const int m_nFractalType, const int m_nPower
  , T *m_db_dxr, T *m_db_dxi, double *m_db_z
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
  , T &amp;dr0, T &amp;di0
  , const T &amp;daa, const T &amp;dab, const T &amp;dba, const T &amp;dbb
  )
{
  (void) dr0; // -Wunused-parameter
  (void) di0; // -Wunused-parameter
  (void) daa; // -Wunused-parameter
  (void) dab; // -Wunused-parameter
  (void) dba; // -Wunused-parameter
  (void) dbb; // -Wunused-parameter
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    bool stored = false;
    double old_absval = 0;
    double abs_val = 0;
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
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
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
    T dr = dr0, di = di0;
    T drn = 0, din = 0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    T dxa = daa, dxb = dab, dya = dba, dyb = dbb;
    T dxan = 0, dxbn = 0, dyan = 0, dybn = 0;
</xsl:when>
</xsl:choose>
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
<xsl:choose><xsl:when test="derivative/@t='R' or derivative/@t='C'"> \
    dr = drn; di = din; \
</xsl:when><xsl:when test="derivative/@t='M'"> \
    dxa = dxan; dxb = dxbn; dya = dyan, dyb = dybn; \
</xsl:when></xsl:choose> \
      mpfr_set(Xr, Xrn, MPFR_RNDN); \
      mpfr_set(Xi, Xin, MPFR_RNDN); \
      mpfr_sqr(Xr2, Xr, MPFR_RNDN); \
      mpfr_sqr(Xi2, Xi, MPFR_RNDN); \
      m_nRDone++; \
      Xrd = mpfr_get(Xr, T(0.0), MPFR_RNDN); \
      Xid = mpfr_get(Xi, T(0.0), MPFR_RNDN); \
      old_absval = abs_val; \
      abs_val = double(g_real * Xrd * Xrd + g_imag * Xid * Xid); \
      const double Xz = abs_val * glitch; \
      m_db_dxr[i] = Xrd; \
      m_db_dxi[i] = Xid; \
      m_db_z[i] = Xz; \
      if (abs_val &gt;= 4) \
      { \
        if (terminate == 4 &amp;&amp; !stored) \
        { \
          stored = true; \
          antal = i; \
          test1 = abs_val; \
          test2 = old_absval; \
        } \
      } \
      if (abs_val &gt;= terminate){ \
        if (terminate &gt; 4 &amp;&amp; !stored) \
        { \
          stored = true; \
          antal = i; \
          test1 = abs_val; \
          test2 = old_absval; \
        } \
        if (nMaxIter == m_nMaxIter){ \
          nMaxIter = i + 3; \
          if (nMaxIter &gt; m_nMaxIter) \
            nMaxIter = m_nMaxIter; \
          m_nGlitchIter = nMaxIter; \
        } \
      }

<xsl:choose>
<xsl:when test="reference/@t='C'">
@rd   {
        const T Xxr = Xrd; (void) Xxr;
        const T Xxi = Xid; (void) Xxi;
        const complex&lt;T&gt; Xx(Xxr, Xxi), A(g_FactorAR, g_FactorAI);
<xsl:choose>
<xsl:when test="derivative/@t='C'">
        const complex&lt;T&gt; d(dr, di);
        complex&lt;T&gt; dn(0.0, 0.0);
        <xsl:value-of select="derivative" />
        drn = dn.m_r; din = dn.m_i;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
        <xsl:value-of select="derivative" />
</xsl:when>
</xsl:choose>
      }
for (i = 0; i &lt; nMaxIter &amp;&amp; !m_bStop; i++)
      {
        mpfr_set(X.m_r.m_f.backend().data(), Xr, MPFR_RNDN);
        mpfr_set(X.m_i.m_f.backend().data(), Xi, MPFR_RNDN);
        {
          <xsl:value-of select="reference" />
          DLOOP
        }
        mpfr_set(Xrn, Xn.m_r.m_f.backend().data(), MPFR_RNDN);
        mpfr_set(Xin, Xn.m_i.m_f.backend().data(), MPFR_RNDN);
LOOP  }
#undef DLOOP
</xsl:when>
<xsl:when test="reference/@t='R'">
@rd   {
        const T Xxr = Xrd; (void) Xxr;
        const T Xxi = Xid; (void) Xxi;
<xsl:choose>
<xsl:when test="derivative/@t='C'">
        const complex&lt;T&gt; Xx(Xxr, Xxi), d(dr, di), A(g_FactorAR, g_FactorAI);
        complex&lt;T&gt; dn(0.0, 0.0);
        <xsl:value-of select="derivative" />
        drn = dn.m_r; din = dn.m_i;
</xsl:when>
<xsl:when test="derivative/@t='R' or derivative/@t='M'">
        <xsl:value-of select="derivative" />
</xsl:when>
</xsl:choose>
      }
@rr   {
        <xsl:value-of select="reference" />
      }
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
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
    dr0 = dr; di0 = di;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    dr0 = (Xrd * dxa + Xid * dya) / sqrt(test1);
    di0 = (Xrd * dxb + Xid * dyb) / sqrt(test1);
</xsl:when>
</xsl:choose>
    return true;
  }
  return false;
}

template &lt;typename T&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , T &amp;xr0, T &amp;xi0
  , const T &amp;cr, const T &amp;ci
  , T &amp;dr0, T &amp;di0
  , const T &amp;e, const T &amp;h
  , const T &amp;daa, const T &amp;dab, const T &amp;dba, const T &amp;dbb
  )
{
  (void) dr0; // -Wunused-parameter
  (void) di0; // -Wunused-parameter
  (void) h; // -Wunused-parameter
  (void) e; // -Wunused-parameter
  (void) daa; // -Wunused-parameter
  (void) dab; // -Wunused-parameter
  (void) dba; // -Wunused-parameter
  (void) dbb; // -Wunused-parameter
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const complex&lt;T&gt; A = { g_FactorAR, g_FactorAI };
    const complex&lt;T&gt; c = { cr, ci };
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    int antal = antal0;
    double test1 = test10;
    double test2 = test20;
    T xr = xr0;
    T xi = xi0;
    (void) dr0; // -Wunused-parameter
    (void) di0; // -Wunused-parameter
<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
    T dr = dr0, di = di0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    T dxa = daa, dxb = dab, dya = dba, dyb = dbb;
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
      test2 = test1;
      test1 = double(g_real * Xxr * Xxr + g_imag * Xxi * Xxi);
      if (test1 &lt; Xz)
      {
<xsl:choose>
<xsl:when test="../@type='0' and @power='2'">
        if (type_0_power_2_pixel_has_glitched(cr, ci, xr, xi, Xr, Xi, daa, dbb, e, h)) // FIXME matrix derivatives
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
      if (test1 &gt; m_nBailout2)
      {
        break;
      }
      T xrn, xin;

<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
      T drn = 0, din = 0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
      T dxan = 0, dxbn = 0, dyan = 0, dybn = 0;
</xsl:when>
</xsl:choose>

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
      const complex&lt;T&gt; d = {dr, di};
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
      const complex&lt;T&gt; X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi}, d = {dr, di};
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
    xr0 = xr;
    xi0 = xi;
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
    dr0 = dr; di0 = di;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    dr0 = (Xxr * dxa + Xxi * dya) / sqrt(test1);
    di0 = (Xxr * dxb + Xxi * dyb) / sqrt(test1);
</xsl:when>
</xsl:choose>
    return true;
  }
  return false;
}

</xsl:for-each>

bool reference_double(const int m_nFractalType, const int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &amp;antal, double &amp;test1, double &amp;test2, double &amp;dr, double &amp;di, const double &amp;daa, const double &amp;dab, const double &amp;dba, const double &amp;dbb)
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
          return FORMULA(reference,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)&lt;double&gt;(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, m_bGlitchLowTolerance, antal, test1, test2, dr, di, daa, dab, dba, dbb);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

bool reference_long_double(const int m_nFractalType, const int m_nPower, long double *m_ldxr, long double *m_ldxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &amp;antal, double &amp;test1, double &amp;test2, long double &amp;dr, long double &amp;di, const long double &amp;daa, const long double &amp;dab, const long double &amp;dba, const long double &amp;dbb)
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
          return FORMULA(reference,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)&lt;long double&gt;(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, m_bGlitchLowTolerance, antal, test1, test2, dr, di, daa, dab, dba, dbb);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

bool reference_floatexp(const int m_nFractalType, const int m_nPower, floatexp *m_dxr, floatexp *m_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &amp;antal, double &amp;test1, double &amp;test2, floatexp &amp;dr, floatexp &amp;di, const floatexp &amp;daa, const floatexp &amp;dab, const floatexp &amp;dba, const floatexp &amp;dbb)
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
          return FORMULA(reference,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)&lt;floatexp&gt;(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, m_bGlitchLowTolerance, antal, test1, test2, dr, di, daa, dab, dba, dbb);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

bool perturbation_double(const int m_nFractalType, const int m_nPower, const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, double &amp;xr, double &amp;xi, const double cr, const double ci, double &amp;dr, double &amp;di, const double &amp;e, const double &amp;h, const double &amp;daa, const double &amp;dab, const double &amp;dba, const double &amp;dbb)
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
          return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)&lt;double&gt;(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci, dr, di, e, h, daa, dab, dba, dbb);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

bool perturbation_long_double(const int m_nFractalType, const int m_nPower, const long double *dxr, const long double *dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, long double &amp;xr, long double &amp;xi, const long double cr, const long double ci, long double &amp;dr, long double &amp;di, const long double &amp;e, const long double &amp;h, const long double &amp;daa, const long double &amp;dab, const long double &amp;dba, const long double &amp;dbb)
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
          return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)&lt;long double&gt;(m_nFractalType, m_nPower, dxr, dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci, dr, di, e, h, daa, dab, dba, dbb);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

bool perturbation_floatexp(const int m_nFractalType, const int m_nPower, const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, floatexp &amp;xr, floatexp &amp;xi, const floatexp cr, const floatexp ci, floatexp &amp;dr, floatexp &amp;di, const floatexp &amp;e, const floatexp &amp;h, const floatexp &amp;daa, const floatexp &amp;dab, const floatexp &amp;dba, const floatexp &amp;dbb)
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
          return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)&lt;floatexp&gt;(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci, dr, di, e, h, daa, dab, dba, dbb);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

void combo5_addstrings(HWND hWnd, const int IDC_COMBO5)
{
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

// EOF
</xsl:template>
</xsl:stylesheet>
