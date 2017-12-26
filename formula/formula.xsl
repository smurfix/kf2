<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">

#include "../fraktal_sft/CFixedFloat.h"
#include "../fraktal_sft/floatexp.h"
#include "../fraktal_sft/complex.h"

#define FORMULA2(function,type,power) function ## _ ## type ## _ ## power
#define FORMULA(a,b,c) FORMULA2(a,b,c)

using std::abs;

static inline long double ConvertFromFixedFloat(const CFixedFloat &amp;f)
{
  using std::ldexp;
  signed long int e = 0;
  long double l = mpfr_get_ld_2exp(&amp;e, f.m_f.backend().data(), MPFR_RNDN);
  l = ldexp(l, e);
  return l;
}


<xsl:for-each select="formulas/group/formula">


bool FORMULA(reference_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(const int m_nFractalType, const int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &amp;antal, double &amp;test1, double &amp;test2, double &amp;dr0, double &amp;di0)
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
    double Xrd = mpfr_get_d(Xr, MPFR_RNDN);
    double Xid = mpfr_get_d(Xi, MPFR_RNDN);
    mpfr_t Xr2; mpfr_init2(Xr2, bits); mpfr_sqr(Xr2, Xr, MPFR_RNDN);
    mpfr_t Xi2; mpfr_init2(Xi2, bits); mpfr_sqr(Xi2, Xi, MPFR_RNDN);
    mpfr_t Xrn; mpfr_init2(Xrn, bits);
    mpfr_t Xin; mpfr_init2(Xin, bits);
    mpfr_t Ar; mpfr_init2(Ar, bits); mpfr_set_d(Ar, g_FactorAR, MPFR_RNDN);
    mpfr_t Ai; mpfr_init2(Ai, bits); mpfr_set_d(Ai, g_FactorAI, MPFR_RNDN);
    double dr = dr0, di = di0;
    double drn = 0, din = 0;
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
      dr = drn; di = din; \
      mpfr_set(Xr, Xrn, MPFR_RNDN); \
      mpfr_set(Xi, Xin, MPFR_RNDN); \
      mpfr_sqr(Xr2, Xr, MPFR_RNDN); \
      mpfr_sqr(Xi2, Xi, MPFR_RNDN); \
      m_nRDone++; \
      Xrd = mpfr_get_d(Xr, MPFR_RNDN); \
      Xid = mpfr_get_d(Xi, MPFR_RNDN); \
      old_absval = abs_val; \
      abs_val = g_real * Xrd * Xrd + g_imag * Xid * Xid; \
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
        const double Xxr = Xrd; (void) Xxr;
        const double Xxi = Xid; (void) Xxi;
        const complex&lt;double&gt; Xx(Xxr, Xxi), d(dr, di), A(g_FactorAR, g_FactorAI);
        complex&lt;double&gt; dn(0.0, 0.0);
        <xsl:value-of select="derivative" />
        drn = dn.m_r; din = dn.m_i;
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
        const double Xxr = Xrd; (void) Xxr;
        const double Xxi = Xid; (void) Xxi;
        <xsl:value-of select="derivative" />
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
    dr0 = dr; di0 = di;
    return true;
  }
  return false;
}

bool FORMULA(reference_long_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(const int m_nFractalType, const int m_nPower, long double *m_ldxr, long double *m_ldxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &amp;antal, double &amp;test1, double &amp;test2, long double &amp;dr0, long double &amp;di0)
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
    const bool no_g = g_real == 1 &amp;&amp; g_imag == 1;
    mp_bitcnt_t bits = mpfr_get_prec(Cr0.m_f.backend().data());
    mpfr_t Cr; mpfr_init2(Cr, bits); mpfr_set(Cr, Cr0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Ci; mpfr_init2(Ci, bits); mpfr_set(Ci, Ci0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Xr; mpfr_init2(Xr, bits); mpfr_set_d(Xr, g_SeedR, MPFR_RNDN);
    mpfr_t Xi; mpfr_init2(Xi, bits); mpfr_set_d(Xi, g_SeedI, MPFR_RNDN);
    long double Xrl = mpfr_get_ld(Xr, MPFR_RNDN); \
    long double Xil = mpfr_get_ld(Xi, MPFR_RNDN); \
    mpfr_t Xr2; mpfr_init2(Xr2, bits); mpfr_sqr(Xr2, Xr, MPFR_RNDN);
    mpfr_t Xi2; mpfr_init2(Xi2, bits); mpfr_sqr(Xi2, Xi, MPFR_RNDN);
    mpfr_t Xrn; mpfr_init2(Xrn, bits);
    mpfr_t Xin; mpfr_init2(Xin, bits);
    mpfr_t Ar; mpfr_init2(Ar, bits); mpfr_set_d(Ar, g_FactorAR, MPFR_RNDN);
    mpfr_t Ai; mpfr_init2(Ai, bits); mpfr_set_d(Ai, g_FactorAI, MPFR_RNDN);
    long double dr = dr0, di = di0;
    long double drn = 0, din = 0;
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
      dr = drn, di = din; \
      mpfr_set(Xr, Xrn, MPFR_RNDN); \
      mpfr_set(Xi, Xin, MPFR_RNDN); \
      mpfr_sqr(Xr2, Xr, MPFR_RNDN); \
      mpfr_sqr(Xi2, Xi, MPFR_RNDN); \
      m_nRDone++; \
      Xrl = mpfr_get_ld(Xr, MPFR_RNDN); \
      Xil = mpfr_get_ld(Xi, MPFR_RNDN); \
      old_absval = abs_val; \
      if (no_g) \
        abs_val = Xrl * Xrl + Xil * Xil; \
      else \
        abs_val = g_real * Xrl * Xrl + g_imag * Xil * Xil; \
      const double Xz = abs_val * glitch; \
      m_ldxr[i] = Xrl; \
      m_ldxi[i] = Xil; \
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
        const long double Xxr = Xrl; (void) Xxr;
        const long double Xxi = Xil; (void) Xxi;
        const complex&lt;long double&gt; Xx(Xxr, Xxi), d(dr, di), A(g_FactorAR, g_FactorAI);
        complex&lt;long double&gt; dn(0.0, 0.0);
        <xsl:value-of select="derivative" />
        drn = dn.m_r; din = dn.m_i;
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
        const long double Xxr = Xrl; (void) Xxr;
        const long double Xxi = Xil; (void) Xxi;
        <xsl:value-of select="derivative" />
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
    dr0 = dr; di0 = di;
    return true;
  }
  return false;
}

bool FORMULA(reference_floatexp,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(const int m_nFractalType, const int m_nPower, floatexp *m_dxr, floatexp *m_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &amp;antal, double &amp;test1, double &amp;test2, floatexp &amp;dr0, floatexp &amp;di0)
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
    const bool no_g = g_real == 1 &amp;&amp; g_imag == 1;
    mp_bitcnt_t bits = mpfr_get_prec(Cr0.m_f.backend().data());
    mpfr_t Cr; mpfr_init2(Cr, bits); mpfr_set(Cr, Cr0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Ci; mpfr_init2(Ci, bits); mpfr_set(Ci, Ci0.m_f.backend().data(), MPFR_RNDN);
    mpfr_t Xr; mpfr_init2(Xr, bits); mpfr_set_d(Xr, g_SeedR, MPFR_RNDN);
    mpfr_t Xi; mpfr_init2(Xi, bits); mpfr_set_d(Xi, g_SeedI, MPFR_RNDN);
    floatexp Xrf = mpfr_get_fe(Xr); \
    floatexp Xif = mpfr_get_fe(Xi); \
    mpfr_t Xr2; mpfr_init2(Xr2, bits); mpfr_sqr(Xr2, Xr, MPFR_RNDN);
    mpfr_t Xi2; mpfr_init2(Xi2, bits); mpfr_sqr(Xi2, Xi, MPFR_RNDN);
    mpfr_t Xrn; mpfr_init2(Xrn, bits);
    mpfr_t Xin; mpfr_init2(Xin, bits);
    mpfr_t Ar; mpfr_init2(Ar, bits); mpfr_set_d(Ar, g_FactorAR, MPFR_RNDN);
    mpfr_t Ai; mpfr_init2(Ai, bits); mpfr_set_d(Ai, g_FactorAI, MPFR_RNDN);
    floatexp dr = dr0, di = di0;
    floatexp drn = 0, din = 0;
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
      dr = drn, di = din; \
      mpfr_set(Xr, Xrn, MPFR_RNDN); \
      mpfr_set(Xi, Xin, MPFR_RNDN); \
      mpfr_sqr(Xr2, Xr, MPFR_RNDN); \
      mpfr_sqr(Xi2, Xi, MPFR_RNDN); \
      m_nRDone++; \
      Xrf = mpfr_get_fe(Xr); \
      Xif = mpfr_get_fe(Xi); \
      old_absval = abs_val; \
      if (no_g) \
        abs_val = (Xrf * Xrf + Xif * Xif).todouble(); \
      else \
        abs_val = (g_real * Xrf * Xrf + g_imag * Xif * Xif).todouble(); \
      const double Xz = abs_val * glitch; \
      m_dxr[i] = Xrf; \
      m_dxi[i] = Xif; \
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
        const floatexp Xxr = Xrf; (void) Xxr;
        const floatexp Xxi = Xif; (void) Xxi;
        const complex&lt;floatexp&gt; Xx(Xxr, Xxi), d(dr, di), A(g_FactorAR, g_FactorAI);
        complex&lt;floatexp&gt; dn(0.0, 0.0);
        <xsl:value-of select="derivative" />
        drn = dn.m_r; din = dn.m_i;
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
        const floatexp Xxr = Xrf; (void) Xxr;
        const floatexp Xxi = Xif; (void) Xxi;
        <xsl:value-of select="derivative" />
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
    dr0 = dr; di0 = di;
    return true;
  }
  return false;
}


bool FORMULA(perturbation_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(     int m_nFractalType, int m_nPower, const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z, int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch, double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI,      double &amp;xr0,      double &amp;xi0, const      double cr, const      double ci,      double &amp;dr0,      double &amp;di0)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const dcomplex A = { g_FactorAR, g_FactorAI };
    const dcomplex c = { cr, ci };
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    int antal = antal0;
    double test1 = test10;
    double test2 = test20;
    double xr = xr0;
    double xi = xi0;
    double dr = dr0, di = di0;
    for (; antal &lt; nMaxIter; antal++)
    {
      const double Xr = m_db_dxr[antal];
      const double Xi = m_db_dxi[antal];
      const double Xz = m_db_z[antal];
      const double Xxr = Xr + xr;
      const double Xxi = Xi + xi;
      test2 = test1;
      test1 = g_real * Xxr * Xxr + g_imag * Xxi * Xxi;
      if (test1 &lt; Xz)
      {
        if (! m_bNoGlitchDetection)
          test1 = m_nBailout2 * 2;
        bGlitch = true;
        break;
      }
      if (test1 &gt; m_nBailout2)
      {
        break;
      }
      double xrn, xin, drn = 0, din = 0;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const dcomplex X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi}, d = {dr, di};
      dcomplex xn, dn;
@dc   {
        <xsl:value-of select="derivative" />
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.re; xin = xn.im; drn = dn.re; din = dn.im;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@d    {
        <xsl:value-of select="derivative" />
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>

      xr = xrn;
      xi = xin;
      dr = drn; di = din;
    }
    antal0 = antal;
    test10 = test1;
    test20 = test2;
    xr0 = xr;
    xi0 = xi;
    dr0 = dr; di0 = di;
    return true;
  }
  return false;
}

bool FORMULA(perturbation_long_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, const long double *dxr, const long double *dxi, const double *m_db_z, int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch, double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, long double &amp;xr0, long double &amp;xi0, const long double cr, const long double ci, long double &amp;dr0, long double &amp;di0)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const ldcomplex A = { g_FactorAR, g_FactorAI };
    const ldcomplex c = { cr, ci };
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    const bool no_g = g_real == 1 &amp;&amp; g_imag == 1;
    int antal = antal0;
    double test1 = test10;
    double test2 = test20;
    long double xr = xr0;
    long double xi = xi0;
    long double dr = dr0, di = di0;
    for (; antal &lt; nMaxIter; antal++)
    {
      const long double Xr = dxr[antal];
      const long double Xi = dxi[antal];
      const double      Xz = m_db_z[antal];
      const long double Xxr = Xr + xr;
      const long double Xxi = Xi + xi;
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
        if (! m_bNoGlitchDetection)
          test1 = m_nBailout2 * 2;
        bGlitch = true;
        break;
      }
      if (test1 &gt; m_nBailout2)
      {
        break;
      }
      long double xrn, xin, drn = 0, din = 0;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const ldcomplex X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi}, d = {dr, di};
      ldcomplex xn, dn;
@ldc  {
        <xsl:value-of select="derivative" />
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.re; xin = xn.im; drn = dn.re; din = dn.im;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@ld   {
        <xsl:value-of select="derivative" />
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>

      xr = xrn;
      xi = xin;
      dr = drn; di = din;
    }
    antal0 = antal;
    test10 = test1;
    test20 = test2;
    xr0 = xr;
    xi0 = xi;
    dr0 = dr; di0 = di;
    return true;
  }
  return false;
}

bool FORMULA(perturbation_floatexp,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z, int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch, double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, floatexp &amp;xr0, floatexp &amp;xi0, const floatexp cr, const floatexp ci, floatexp &amp;dr0, floatexp &amp;di0)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /> &amp;&amp; m_nPower == <xsl:value-of select="@power" />)
  {
    const fecomplex A = { g_FactorAR, g_FactorAI };
    const fecomplex c = { cr, ci };
    (void) A; // -Wunused-variable
    (void) c; // -Wunused-variable
    int antal = antal0;
    double test1 = test10;
    double test2 = test20;
    floatexp xr = xr0;
    floatexp xi = xi0;
    floatexp dr = dr0, di = di0;
    for (; antal &lt; nMaxIter; antal++)
    {
      const floatexp Xr = m_dxr[antal];
      const floatexp Xi = m_dxi[antal];
      const double   Xz = m_db_z[antal];
      const floatexp Xxr = Xr + xr;
      const floatexp Xxi = Xi + xi;
      test2 = test1;
      test1 = (g_real*Xxr*Xxr + g_imag*Xxi*Xxi).todouble();
      if (test1 &lt; Xz){
        if (!m_bNoGlitchDetection)
          test1 = m_nBailout2 * 2;
        bGlitch = true;
        break;
      }
      if (test1 &gt; m_nBailout2)
      {
        break;
      }
      floatexp xrn, xin, drn = 0, din = 0;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const fecomplex X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi}, d = {dr, di};
      fecomplex xn, dn;
@fec  {
        <xsl:value-of select="derivative" />
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.re; xin = xn.im; drn = dn.re; din = dn.im;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@fe   {
        <xsl:value-of select="derivative" />
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>

      xr = xrn;
      xi = xin;
      dr = drn; di = din;
    }

    antal0 = antal;
    test10 = test1;
    test20 = test2;
    xr0 = xr;
    xi0 = xi;
    dr0 = dr; di0 = di;
    return true;
  }
  return false;
}


</xsl:for-each>


bool reference_double(const int m_nFractalType, const int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &amp;antal, double &amp;test1, double &amp;test2, double &amp;dr, double &amp;di)
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
          return FORMULA(reference_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, m_bGlitchLowTolerance, antal, test1, test2, dr, di);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}


bool reference_long_double(const int m_nFractalType, const int m_nPower, long double *m_ldxr, long double *m_ldxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &amp;antal, double &amp;test1, double &amp;test2, long double &amp;dr, long double &amp;di)
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
          return FORMULA(reference_long_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, m_bGlitchLowTolerance, antal, test1, test2, dr, di);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}

bool reference_floatexp(const int m_nFractalType, const int m_nPower, floatexp *m_dxr, floatexp *m_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &amp;antal, double &amp;test1, double &amp;test2, floatexp &amp;dr, floatexp &amp;di)
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
          return FORMULA(reference_floatexp,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, m_bGlitchLowTolerance, antal, test1, test2, dr, di);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}



bool perturbation_double(const int m_nFractalType, const int m_nPower, const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, double &amp;xr, double &amp;xi, const double cr, const double ci, double &amp;dr, double &amp;di)
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
          return FORMULA(perturbation_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci, dr, di);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}



bool perturbation_long_double(const int m_nFractalType, const int m_nPower, const long double *dxr, const long double *dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, long double &amp;xr, long double &amp;xi, const long double cr, const long double ci, long double &amp;dr, long double &amp;di)
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
          return FORMULA(perturbation_long_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, dxr, dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci, dr, di);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}



bool perturbation_floatexp(const int m_nFractalType, const int m_nPower, const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, floatexp &amp;xr, floatexp &amp;xi, const floatexp cr, const floatexp ci, floatexp &amp;dr, floatexp &amp;di)
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
          return FORMULA(perturbation_floatexp,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci, dr, di);
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
