<?xml version="1.0" encoding="UTF-8"?>
<!--
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2019 Claude Heiland-Allen

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

#ifdef PASS1 // reference

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
  )
{
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
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
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
          return FORMULA(reference,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, m_bGlitchLowTolerance, antal, test1, test2);
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
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
  );
template bool reference&lt;long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , long double *m_db_dxr, long double *m_db_dxi, double *m_db_z
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
  );
template bool reference&lt;floatexp&gt;
  ( const int m_nFractalType, const int m_nPower
  , floatexp *m_db_dxr, floatexp *m_db_dxi, double *m_db_z
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
  );

#endif

#ifdef PASS2 // reference with derivatives

<xsl:for-each select="formulas/group/formula">

template &lt;typename S, typename T&gt;
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
  , S &amp;dr0, S &amp;di0
  , const S &amp;daa, const S &amp;dab, const S &amp;dba, const S &amp;dbb
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
    S dr = dr0, di = di0;
    S drn = 0, din = 0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    S dxa = daa, dxb = dab, dya = dba, dyb = dbb;
    S dxan = 0, dxbn = 0, dyan = 0, dybn = 0;
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
        const complex&lt;S&gt; d(dr, di), d0(daa, dba); <!-- FIXME matrix derivatives -->
        complex&lt;S&gt; dn(0.0, 0.0);
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
        const complex&lt;S&gt; Xx(Xxr, Xxi), A(g_FactorAR, g_FactorAI);
        const complex&lt;T&gt; d(dr, di);
        const complex&lt;S&gt; d0(daa, dba); <!-- FIXME matrix derivatives -->
        complex&lt;S&gt; dn(0.0, 0.0);
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

</xsl:for-each>

template &lt;typename S, typename T&gt;
bool reference
  ( const int m_nFractalType, const int m_nPower
  , T *m_db_dxr, T *m_db_dxi, double *m_db_z
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
  , S &amp;dr, S &amp;di
  , const S &amp;daa, const S &amp;dab, const S &amp;dba, const S &amp;dbb
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
          return FORMULA(reference,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, m_bGlitchLowTolerance, antal, test1, test2, dr, di, daa, dab, dba, dbb);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

template bool reference&lt;double, double&gt;
  ( const int m_nFractalType, const int m_nPower
  , double *m_db_dxr, double *m_db_dxi, double *m_db_z
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
  , double &amp;dr0, double &amp;di0
  , const double &amp;daa, const double &amp;dab, const double &amp;dba, const double &amp;dbb
  );
template bool reference&lt;long double, double&gt;
  ( const int m_nFractalType, const int m_nPower
  , double *m_db_dxr, double *m_db_dxi, double *m_db_z
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
  , long double &amp;dr0, long double &amp;di0
  , const long double &amp;daa, const long double &amp;dab, const long double &amp;dba, const long double &amp;dbb
  );
template bool reference&lt;long double, long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , long double *m_db_dxr, long double *m_db_dxi, double *m_db_z
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
  , long double &amp;dr0, long double &amp;di0
  , const long double &amp;daa, const long double &amp;dab, const long double &amp;dba, const long double &amp;dbb
  );
template bool reference&lt;floatexp, long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , long double *m_db_dxr, long double *m_db_dxi, double *m_db_z
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
  , floatexp &amp;dr0, floatexp &amp;di0
  , const floatexp &amp;daa, const floatexp &amp;dab, const floatexp &amp;dba, const floatexp &amp;dbb
  );
template bool reference&lt;floatexp, floatexp&gt;
  ( const int m_nFractalType, const int m_nPower
  , floatexp *m_db_dxr, floatexp *m_db_dxi, double *m_db_z
  , int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter
  , const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &amp;antal, double &amp;test1, double &amp;test2
  , floatexp &amp;dr0, floatexp &amp;di0
  , const floatexp &amp;daa, const floatexp &amp;dab, const floatexp &amp;dba, const floatexp &amp;dbb
  );

#endif

#ifdef PASS3 // perturbation

<xsl:for-each select="formulas/group/formula">

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
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0;
    int antal = antal0;
    double test1 = test10;
    double test2 = test20;
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
      test2 = test1;
      if (no_g)
      {
        test1 = double(Xxr * Xxr + Xxi * Xxi);
      }
      else
      {
        test1 = double(g_real * Xxr * Xxr + g_imag * Xxi * Xxi);
      }
      if (test1 &lt; Xz)
      {
        bGlitch = true;
        if (! m_bNoGlitchDetection)
          break;
      }
      if (test1 &gt; m_nBailout2)
      {
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
    xr0 = xr;
    xi0 = xi;
    return true;
  }
  return false;
}

</xsl:for-each>

template &lt;typename T&gt;
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const T *m_dxr, const T *m_dxi, const double *m_db_z
  , int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
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
          return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci);
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
  , int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double &amp;xr, double &amp;xi
  , const double &amp;cr, const double &amp;ci
  );
template bool perturbation&lt;long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const long double *m_dxr, const long double *m_dxi, const double *m_db_z
  , int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , long double &amp;xr, long double &amp;xi
  , const long double &amp;cr, const long double &amp;ci
  );
template bool perturbation&lt;floatexp&gt;
  ( const int m_nFractalType, const int m_nPower
  , const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z
  , int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
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
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0;
    const T Ar = g_FactorAR;
    const T Ai = g_FactorAI;
    const complex&lt;T&gt; A = { Ar, Ai };
    const complex&lt;T&gt; c = { cr, ci };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
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
    T dxa = dr0, dxb = -di0, dya = di0, dyb = dr0;
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
      if (no_g)
        test1 = double(Xxr * Xxr + Xxi * Xxi);
      else
        test1 = double(g_real * Xxr * Xxr + g_imag * Xxi * Xxi);
      if (test1 &lt; Xz)
      {
<xsl:choose>
<xsl:when test="../@type='0' and @power='2'">
        if (type_0_power_2_pixel_has_glitched(cr, ci, xr, xi, Xr, Xi, dxa / h, dya / h, e, h)) // FIXME matrix derivatives
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

template &lt;typename T&gt;
bool perturbation
  ( int m_nFractalType, int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , T &amp;xr, T &amp;xi
  , const T &amp;cr, const T &amp;ci
  , T &amp;dr, T &amp;di
  , const T &amp;e, const T &amp;h
  , const T &amp;daa, const T &amp;dab, const T &amp;dba, const T &amp;dbb
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
          return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci, dr, di, e, h, daa, dab, dba, dbb);
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
  , int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double &amp;xr0, double &amp;xi0
  , const double &amp;cr, const double &amp;ci
  , double &amp;dr0, double &amp;di0
  , const double &amp;e, const double &amp;h
  , const double &amp;daa, const double &amp;dab, const double &amp;dba, const double &amp;dbb
  );
template bool perturbation&lt;long double&gt;
  ( int m_nFractalType, int m_nPower
  , const long double *m_db_dxr, const long double *m_db_dxi, const double *m_db_z
  , int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , long double &amp;xr0, long double &amp;xi0
  , const long double &amp;cr, const long double &amp;ci
  , long double &amp;dr0, long double &amp;di0
  , const long double &amp;e, const long double &amp;h
  , const long double &amp;daa, const long double &amp;dab, const long double &amp;dba, const long double &amp;dbb
  );
template bool perturbation&lt;floatexp&gt;
  ( int m_nFractalType, int m_nPower
  , const floatexp *m_db_dxr, const floatexp *m_db_dxi, const double *m_db_z
  , int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , floatexp &amp;xr0, floatexp &amp;xi0
  , const floatexp &amp;cr, const floatexp &amp;ci
  , floatexp &amp;dr0, floatexp &amp;di0
  , const floatexp &amp;e, const floatexp &amp;h
  , const floatexp &amp;daa, const floatexp &amp;dab, const floatexp &amp;dba, const floatexp &amp;dbb
  );

#endif

#ifdef PASS5 // perturbation with SIMD

<xsl:for-each select="formulas/group/formula">

template &lt;typename intN, typename doubleN&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, intN &amp;bGlitch0
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , const int &amp;chunksize
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
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0;
    doubleN test1 = test10;
    doubleN test2 = test20;
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
        for (int q = 0; q &lt; chunksize; ++q)
        {
          const double Xr = m_db_dxr[antal[0] + q];
          const double Xi = m_db_dxi[antal[0] + q];
          const double Xz = m_db_z[antal[0] + q];
          doubleN Xxr = Xr + xr0;
          doubleN Xxi = Xi + xi0;
          test2 = test1;
          if (no_g)
          {
            test1 = Xxr * Xxr + Xxi * Xxi;
          }
          else
          {
            test1 = g_real * Xxr * Xxr + g_imag * Xxi * Xxi;
          }
          bGlitch |= test1 &lt; Xz;
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
    const int N = sizeof(doubleN) / sizeof(double);
    for (int k = 0; k &lt; N; ++k)
    {
      for (; antal[k] &lt; nMaxIter; antal[k] = antal[k] + 1)
      {
        const double Xr = m_db_dxr[antal[k]];
        const double Xi = m_db_dxi[antal[k]];
        const double Xz = m_db_z[antal[k]];
        double Xxr = Xr + xr0[k];
        double Xxi = Xi + xi0[k];
        test2[k] = test1[k];
        if (no_g)
        {
          test1[k] = Xxr * Xxr + Xxi * Xxi;
        }
        else
        {
          test1[k] = g_real * Xxr * Xxr + g_imag * Xxi * Xxi;
        }
        if (test1[k] &lt; Xz)
        {
          bGlitch[k] = true;
          if (! m_bNoGlitchDetection)
            break;
        }
        if (test1[k] &gt; m_nBailout2)
        {
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
    }

    antal0 = antal;
    bGlitch0 = bGlitch;
    test10 = test1;
    test20 = test2;
    xr00 = xr0;
    xi00 = xi0;
    return true;
  }
  return false;
}

</xsl:for-each>

template &lt;typename intN, typename doubleN&gt;
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , intN &amp;antal, doubleN &amp;test1, doubleN &amp;test2, intN &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &amp;xr, doubleN &amp;xi
  , const doubleN &amp;cr, const doubleN &amp;ci
  , const int &amp;chunksize
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
          return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci, chunksize);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

// explicit template instantiation

template bool perturbation&lt;int2, double2&gt;
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int2 &amp;antal, double2 &amp;test1, double2 &amp;test2, int2 &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_b2oGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double2 &amp;xr, double2 &amp;xi
  , const double2 &amp;cr, const double2 &amp;ci
  , const int &amp;chunksize
  );
template bool perturbation&lt;int4, double4&gt;
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int4 &amp;antal, double4 &amp;test1, double4 &amp;test2, int4 &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_b4oGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double4 &amp;xr, double4 &amp;xi
  , const double4 &amp;cr, const double4 &amp;ci
  , const int &amp;chunksize
  );
template bool perturbation&lt;int8, double8&gt;
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int8 &amp;antal, double8 &amp;test1, double8 &amp;test2, int8 &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_b8oGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double8 &amp;xr, double8 &amp;xi
  , const double8 &amp;cr, const double8 &amp;ci
  , const int &amp;chunksize
  );
template bool perturbation&lt;int16, double16&gt;
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int16 &amp;antal, double16 &amp;test1, double16 &amp;test2, int16 &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_b16oGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double16 &amp;xr, double16 &amp;xi
  , const double16 &amp;cr, const double16 &amp;ci
  , const int &amp;chunksize
  );

#endif

#ifdef PASS6 // perturbation with scaling

<xsl:for-each select="//scaled/..">

template &lt;typename T&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( const int m_nFractalType, const int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch0
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
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
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0;
    double test1 = test10;
    double test2 = test20;
    T xr0 = xr00;
    T xi0 = xi00;
    int antal = antal0;
    int bGlitch = bGlitch0;
      for (; antal &lt; nMaxIter; antal = antal + 1)
      {
        const T Xr = m_db_dxr[antal];
        const T Xi = m_db_dxi[antal];
        const double Xz = m_db_z[antal];
        T Xxr = Xr + xr0 * s;
        T Xxi = Xi + xi0 * s;
        test2 = test1;
        if (no_g)
        {
          test1 = Xxr * Xxr + Xxi * Xxi;
        }
        else
        {
          test1 = g_real * Xxr * Xxr + g_imag * Xxi * Xxi;
        }
        if (test1 &lt; Xz)
        {
          bGlitch = true;
          if (! m_bNoGlitchDetection)
            break;
        }
        if (test1 &gt; m_nBailout2)
        {
          break;
        }
        double xrn, xin;

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
    xr00 = xr0;
    xi00 = xi0;
    return true;
  }
  return false;
}

</xsl:for-each>

template &lt;typename T&gt;
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
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
      , antal, test1, test2, bGlitch
      , m_nBailout2, nMaxIter
      , m_bNoGlitchDetection, g_real, g_imag
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
  , int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double &amp;xr00, double &amp;xi00
  , const double &amp;cr0, const double &amp;ci0
  , const double &amp;s, const double &amp;S
  );
template bool perturbation&lt;long double&gt;
  ( const int m_nFractalType, const int m_nPower
  , const long double *m_db_dxr, const long double *m_db_dxi, const double *m_db_z
  , int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
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
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, intN &amp;bGlitch0
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , const int &amp;chunksize
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
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0;
    doubleN test1 = test10;
    doubleN test2 = test20;
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
        for (int q = 0; q &lt; chunksize; ++q)
        {
          const double Xr = m_db_dxr[antal[0] + q];
          const double Xi = m_db_dxi[antal[0] + q];
          const double Xz = m_db_z[antal[0] + q];
          doubleN Xxr = Xr + xr0 * s;
          doubleN Xxi = Xi + xi0 * s;
          test2 = test1;
          if (no_g)
          {
            test1 = Xxr * Xxr + Xxi * Xxi;
          }
          else
          {
            test1 = g_real * Xxr * Xxr + g_imag * Xxi * Xxi;
          }
          bGlitch |= test1 &lt; Xz;
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
    int N = sizeof(doubleN) / sizeof(double);
    for (int k = 0; k &lt; N; ++k)
    {
      for (; antal[k] &lt; nMaxIter; antal[k] = antal[k] + 1)
      {
        const double Xr = m_db_dxr[antal[k]];
        const double Xi = m_db_dxi[antal[k]];
        const double Xz = m_db_z[antal[k]];
        double Xxr = Xr + xr0[k] * s;
        double Xxi = Xi + xi0[k] * s;
        test2[k] = test1[k];
        if (no_g)
        {
          test1[k] = Xxr * Xxr + Xxi * Xxi;
        }
        else
        {
          test1[k] = g_real * Xxr * Xxr + g_imag * Xxi * Xxi;
        }
        if (test1[k] &lt; Xz)
        {
          bGlitch[k] = true;
          if (! m_bNoGlitchDetection)
            break;
        }
        if (test1[k] &gt; m_nBailout2)
        {
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
    }

    antal0 = antal;
    bGlitch0 = bGlitch;
    test10 = test1;
    test20 = test2;
    xr00 = xr0;
    xi00 = xi0;
    return true;
  }
  return false;
}

</xsl:for-each>

template &lt;typename intN, typename doubleN&gt;
bool perturbation
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , intN &amp;antal0, doubleN &amp;test10, doubleN &amp;test20, intN &amp;bGlitch0
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &amp;xr00, doubleN &amp;xi00
  , const doubleN &amp;cr0, const doubleN &amp;ci0
  , const int &amp;chunksize
  , const double s, const double S
  )
{
<xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
      ( m_nFractalType, m_nPower
      , m_db_dxr, m_db_dxi, m_db_z
      , antal0, test10, test20, bGlitch0
      , m_nBailout2, nMaxIter
      , m_bNoGlitchDetection, g_real, g_imag
      , g_FactorAR, g_FactorAI
      , xr00, xi00
      , cr0, ci0
      , chunksize
      , s, S
      );
</xsl:for-each>
  return false;
}

template bool perturbation&lt;int2, double2&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int2 &amp;antal0, double2 &amp;test10, double2 &amp;test20, int2 &amp;bGlitch0
  , double m_nBailout2, const int nMaxIter
  , const int m_b2oGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double2 &amp;xr00, double2 &amp;xi00
  , const double2 &amp;cr0, const double2 &amp;ci0
  , const int &amp;chunksize
  , const double s, const double S
  );

template bool perturbation&lt;int4, double4&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int4 &amp;antal0, double4 &amp;test10, double4 &amp;test20, int4 &amp;bGlitch0
  , double m_nBailout2, const int nMaxIter
  , const int m_b4oGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double4 &amp;xr00, double4 &amp;xi00
  , const double4 &amp;cr0, const double4 &amp;ci0
  , const int &amp;chunksize
  , const double s, const double S
  );

template bool perturbation&lt;int8, double8&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int8 &amp;antal0, double8 &amp;test10, double8 &amp;test20, int8 &amp;bGlitch0
  , double m_nBailout2, const int nMaxIter
  , const int m_b8oGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double8 &amp;xr00, double8 &amp;xi00
  , const double8 &amp;cr0, const double8 &amp;ci0
  , const int &amp;chunksize
  , const double s, const double S
  );

template bool perturbation&lt;int16, double16&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int16 &amp;antal0, double16 &amp;test10, double16 &amp;test20, int16 &amp;bGlitch0
  , double m_nBailout2, const int nMaxIter
  , const int m_b16oGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double16 &amp;xr00, double16 &amp;xi00
  , const double16 &amp;cr0, const double16 &amp;ci0
  , const int &amp;chunksize
  , const double s, const double S
  );

#endif

#ifdef PASS8 // perturbation with derivatives and scaling

<xsl:for-each select="//scaled/..">

template &lt;typename D, typename Z&gt;
bool FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
  ( int m_nFractalType, int m_nPower
  , const Z *m_db_dxr, const Z *m_db_dxi, const double *m_db_z
  , int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , Z &amp;xr0, Z &amp;xi0
  , const Z &amp;cr, const Z &amp;ci
  , D &amp;dr0, D &amp;di0
  , const D &amp;e, const D &amp;h
  , const D &amp;daa, const D &amp;dab, const D &amp;dba, const D &amp;dbb
  , const Z &amp;s, const Z &amp;S
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
    bool no_g = g_real == 1.0 &amp;&amp; g_imag == 1.0;
    const Z Ar = g_FactorAR;
    const Z Ai = g_FactorAI;
    const complex&lt;Z&gt; A = { Ar, Ai };
    const complex&lt;Z&gt; c = { cr, ci };
    (void) Ar; // -Wunused-variable
    (void) Ai; // -Wunused-variable
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    int antal = antal0;
    double test1 = test10;
    double test2 = test20;
    Z xr = xr0;
    Z xi = xi0;
    (void) dr0; // -Wunused-parameter
    (void) di0; // -Wunused-parameter
<xsl:choose>
<xsl:when test="derivative/@t='C' or derivative/@t='R'">
    D dr = dr0, di = di0;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    D dxa = dr0, dxb = -di0, dya = di0, dyb = dr0;
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
      test2 = test1;
      if (no_g)
        test1 = double(Xxr * Xxr + Xxi * Xxi);
      else
        test1 = double(g_real * Xxr * Xxr + g_imag * Xxi * Xxi);
      if (test1 &lt; Xz)
      {
<xsl:choose>
<xsl:when test="../@type='0' and @power='2'">
#if 0
        if (type_0_power_2_pixel_has_glitched(cr, ci, xr, xi, Xr, Xi, dxa / h, dya / h, e, h)) // FIXME matrix derivatives
        {
#endif
</xsl:when>
</xsl:choose>
        bGlitch = true;
        if (! m_bNoGlitchDetection)
          break;
<xsl:choose>
<xsl:when test="../@type='0' and @power='2'">
#if 0
        }
#endif
</xsl:when>
</xsl:choose>
      }
      if (test1 &gt; m_nBailout2)
      {
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
    xr0 = xr;
    xi0 = xi;
<xsl:choose>
<xsl:when test="derivative/@t='R' or derivative/@t='C'">
    dr0 = dr; di0 = di;
</xsl:when>
<xsl:when test="derivative/@t='M'">
    Z t = 1 / sqrt(test1);
    dr0 = Xxr * t * dxa + Xxi * t * dya;
    di0 = Xxr * t * dxb + Xxi * t * dyb;
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
  , int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , Z &amp;xr, Z &amp;xi
  , const Z &amp;cr, const Z &amp;ci
  , D &amp;dr, D &amp;di
  , const D &amp;e, const D &amp;h
  , const D &amp;daa, const D &amp;dab, const D &amp;dba, const D &amp;dbb
  , const Z &amp;s, const Z &amp;S
  )
{
<xsl:for-each select="//scaled/..">
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
    return FORMULA(perturbation,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)
      ( m_nFractalType, m_nPower
      , m_db_dxr, m_db_dxi, m_db_z
      , antal, test1, test2, bGlitch
      , m_nBailout2, nMaxIter
      , m_bNoGlitchDetection, g_real, g_imag
      , g_FactorAR, g_FactorAI
      , xr, xi
      , cr, ci
      , dr, di
      , e, h
      , daa, dab, dba, dbb
      , s, S
      );
</xsl:for-each>
  return false;
}

template bool perturbation&lt;long double, double&gt;
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , double &amp;xr0, double &amp;xi0
  , const double &amp;cr, const double &amp;ci
  , long double &amp;dr0, long double &amp;di0
  , const long double &amp;e, const long double &amp;h
  , const long double &amp;daa, const long double &amp;dab, const long double &amp;dba, const long double &amp;dbb
  , const double &amp;s, const double &amp;S
  );
template bool perturbation&lt;floatexp, long double&gt;
  ( int m_nFractalType, int m_nPower
  , const long double *m_db_dxr, const long double *m_db_dxi, const double *m_db_z
  , int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , long double &amp;xr0, long double &amp;xi0
  , const long double &amp;cr, const long double &amp;ci
  , floatexp &amp;dr0, floatexp &amp;di0
  , const floatexp &amp;e, const floatexp &amp;h
  , const floatexp &amp;daa, const floatexp &amp;dab, const floatexp &amp;dba, const floatexp &amp;dbb
  , const long double &amp;s, const long double &amp;S
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
