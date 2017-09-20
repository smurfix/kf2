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
  long double l = mpf_get_d_2exp(&amp;e, f.m_f.backend().data());
  l = ldexp(l, e);
  if ((mpf_sgn(f.m_f.backend().data()) >= 0) != (l >= 0)) l = -l; // workaround GMP bug
  return l;
}


<xsl:for-each select="formulas/group/formula">


bool FORMULA(reference_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
    mp_bitcnt_t bits = mpf_get_prec(Cr0.m_f.backend().data());
    mpf_t Cr; mpf_init2(Cr, bits); mpf_set(Cr, Cr0.m_f.backend().data());
    mpf_t Ci; mpf_init2(Ci, bits); mpf_set(Ci, Ci0.m_f.backend().data());
    mpf_t Xr; mpf_init2(Xr, bits); mpf_set_d(Xr, g_SeedR);
    mpf_t Xi; mpf_init2(Xi, bits); mpf_set_d(Xi, g_SeedI);
    mpf_t Xr2; mpf_init2(Xr2, bits); mpf_mul(Xr2, Xr, Xr);
    mpf_t Xi2; mpf_init2(Xi2, bits); mpf_mul(Xi2, Xi, Xi);
    mpf_t Xrn; mpf_init2(Xrn, bits);
    mpf_t Xin; mpf_init2(Xin, bits);
    mpf_t Ar; mpf_init2(Ar, bits); mpf_set_d(Ar, g_FactorAR);
    mpf_t Ai; mpf_init2(Ai, bits); mpf_set_d(Ai, g_FactorAI);

#define LOOP \
      mpf_set(Xr, Xrn); \
      mpf_set(Xi, Xin); \
      mpf_mul(Xr2, Xr, Xr); \
      mpf_mul(Xi2, Xi, Xi); \
      m_nRDone++; \
      const double Xrd = mpf_get_d(Xr); \
      const double Xid = mpf_get_d(Xi); \
      const double abs_val = g_real * Xrd * Xrd + g_imag * Xid * Xid; \
      const double Xz = abs_val * <xsl:value-of select="@glitch" />; \
      m_db_dxr[i] = Xrd; \
      m_db_dxi[i] = Xid; \
      m_db_z[i] = Xz; \
      if (abs_val &gt;= terminate) { \
        if (nMaxIter == m_nMaxIter) { \
          nMaxIter = i + 3; \
          if (nMaxIter &gt; m_nMaxIter) \
            nMaxIter = m_nMaxIter; \
          m_nGlitchIter = nMaxIter; \
        } \
      }

<xsl:choose>
<xsl:when test="reference/@t='C'">
@rc   {
        <xsl:value-of select="reference" />
      }
</xsl:when>
<xsl:when test="reference/@t='R'">
@rr   {
        <xsl:value-of select="reference" />
      }
</xsl:when>
</xsl:choose>

#undef LOOP

    mpf_clear(Cr);
    mpf_clear(Ci);
    mpf_clear(Xr);
    mpf_clear(Xi);
    mpf_clear(Xr2);
    mpf_clear(Xi2);
    mpf_clear(Xrn);
    mpf_clear(Xin);
    mpf_clear(Ar);
    mpf_clear(Ai);

    return true;
  }
  return false;
}

bool FORMULA(reference_long_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, long double *m_ldxr, long double *m_ldxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
    const bool no_g = g_real == 1 &amp;&amp; g_imag == 1;
    mp_bitcnt_t bits = mpf_get_prec(Cr0.m_f.backend().data());
    mpf_t Cr; mpf_init2(Cr, bits); mpf_set(Cr, Cr0.m_f.backend().data());
    mpf_t Ci; mpf_init2(Ci, bits); mpf_set(Ci, Ci0.m_f.backend().data());
    mpf_t Xr; mpf_init2(Xr, bits); mpf_set_d(Xr, g_SeedR);
    mpf_t Xi; mpf_init2(Xi, bits); mpf_set_d(Xi, g_SeedI);
    mpf_t Xr2; mpf_init2(Xr2, bits); mpf_mul(Xr2, Xr, Xr);
    mpf_t Xi2; mpf_init2(Xi2, bits); mpf_mul(Xi2, Xi, Xi);
    mpf_t Xrn; mpf_init2(Xrn, bits);
    mpf_t Xin; mpf_init2(Xin, bits);
    mpf_t Ar; mpf_init2(Ar, bits); mpf_set_d(Ar, g_FactorAR);
    mpf_t Ai; mpf_init2(Ai, bits); mpf_set_d(Ai, g_FactorAI);

#define LOOP \
      mpf_set(Xr, Xrn); \
      mpf_set(Xi, Xin); \
      mpf_mul(Xr2, Xr, Xr); \
      mpf_mul(Xi2, Xi, Xi); \
      m_nRDone++; \
      const long double Xrl = mpf_get_ld(Xr); \
      const long double Xil = mpf_get_ld(Xi); \
      double abs_val; \
      if (no_g) \
        abs_val = Xrl * Xrl + Xil * Xil; \
      else \
        abs_val = g_real * Xrl * Xrl + g_imag * Xil * Xil; \
      const double Xz = abs_val * <xsl:value-of select="@glitch" />; \
      m_ldxr[i] = Xrl; \
      m_ldxi[i] = Xil; \
      m_db_z[i] = Xz; \
      if (abs_val &gt;= terminate) { \
        if (nMaxIter == m_nMaxIter) { \
          nMaxIter = i + 3; \
          if (nMaxIter &gt; m_nMaxIter) \
            nMaxIter = m_nMaxIter; \
          m_nGlitchIter = nMaxIter; \
        } \
      }

<xsl:choose>
<xsl:when test="reference/@t='C'">
@rc   {
        <xsl:value-of select="reference" />
      }
</xsl:when>
<xsl:when test="reference/@t='R'">
@rr   {
        <xsl:value-of select="reference" />
      }
</xsl:when>
</xsl:choose>

#undef LOOP

    mpf_clear(Cr);
    mpf_clear(Ci);
    mpf_clear(Xr);
    mpf_clear(Xi);
    mpf_clear(Xr2);
    mpf_clear(Xi2);
    mpf_clear(Xrn);
    mpf_clear(Xin);
    mpf_clear(Ar);
    mpf_clear(Ai);

    return true;
  }
  return false;
}

bool FORMULA(reference_floatexp,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, floatexp *m_dxr, floatexp *m_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr0, const CFixedFloat &amp;Ci0, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const floatexp real, const floatexp imag)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
    const bool no_g = real == 1 &amp;&amp; imag == 1;
    mp_bitcnt_t bits = mpf_get_prec(Cr0.m_f.backend().data());
    mpf_t Cr; mpf_init2(Cr, bits); mpf_set(Cr, Cr0.m_f.backend().data());
    mpf_t Ci; mpf_init2(Ci, bits); mpf_set(Ci, Ci0.m_f.backend().data());
    mpf_t Xr; mpf_init2(Xr, bits); mpf_set_d(Xr, g_SeedR);
    mpf_t Xi; mpf_init2(Xi, bits); mpf_set_d(Xi, g_SeedI);
    mpf_t Xr2; mpf_init2(Xr2, bits); mpf_mul(Xr2, Xr, Xr);
    mpf_t Xi2; mpf_init2(Xi2, bits); mpf_mul(Xi2, Xi, Xi);
    mpf_t Xrn; mpf_init2(Xrn, bits);
    mpf_t Xin; mpf_init2(Xin, bits);
    mpf_t Ar; mpf_init2(Ar, bits); mpf_set_d(Ar, g_FactorAR);
    mpf_t Ai; mpf_init2(Ai, bits); mpf_set_d(Ai, g_FactorAI);

#define LOOP \
      mpf_set(Xr, Xrn); \
      mpf_set(Xi, Xin); \
      mpf_mul(Xr2, Xr, Xr); \
      mpf_mul(Xi2, Xi, Xi); \
      m_nRDone++; \
      const floatexp Xrf = mpf_get_fe(Xr); \
      const floatexp Xif = mpf_get_fe(Xi); \
      double abs_val; \
      if (no_g) \
        abs_val = (Xrf * Xrf + Xif * Xif).todouble(); \
      else \
        abs_val = (real * Xrf * Xrf + imag * Xif * Xif).todouble(); \
      const double Xz = abs_val * <xsl:value-of select="@glitch" />; \
      m_dxr[i] = Xrf; \
      m_dxi[i] = Xif; \
      m_db_z[i] = Xz; \
      if (abs_val &gt;= terminate) { \
        if (nMaxIter == m_nMaxIter) { \
          nMaxIter = i + 3; \
          if (nMaxIter &gt; m_nMaxIter) \
            nMaxIter = m_nMaxIter; \
          m_nGlitchIter = nMaxIter; \
        } \
      }

<xsl:choose>
<xsl:when test="reference/@t='C'">
@rc   {
        <xsl:value-of select="reference" />
      }
</xsl:when>
<xsl:when test="reference/@t='R'">
@rr   {
        <xsl:value-of select="reference" />
      }
</xsl:when>
</xsl:choose>

#undef LOOP

    mpf_clear(Cr);
    mpf_clear(Ci);
    mpf_clear(Xr);
    mpf_clear(Xi);
    mpf_clear(Xr2);
    mpf_clear(Xi2);
    mpf_clear(Xrn);
    mpf_clear(Xin);
    mpf_clear(Ar);
    mpf_clear(Ai);

    return true;
  }
  return false;
}


bool FORMULA(perturbation_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z, int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch, double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, const double xr0, const double xi0, const double cr, const double ci)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    const dcomplex A = { g_FactorAR, g_FactorAI };
    const dcomplex c = { cr, ci };
    int antal = antal0;
    double test1 = test10;
    double test2 = test20;
    double xr = xr0;
    double xi = xi0;
    for (; antal &lt; nMaxIter &amp;&amp; test1 &lt;= m_nBailout2; antal++)
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
      }
      double xrn, xin;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const dcomplex X = {Xr, Xi}, x = {xr, xi};
      dcomplex xn;
@dc   {
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.re; xin = xn.im;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
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
    return true;
  }
  return false;
}

bool FORMULA(perturbation_long_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, const long double *dxr, const long double *dxi, const double *m_db_z, int &amp;antal0, double &amp;test10, double &amp;test20, int &amp;bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, double g_FactorAR, double g_FactorAI, long double xr, long double xi, const long double cr, const long double ci)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    const ldcomplex A = { g_FactorAR, g_FactorAI };
    const ldcomplex c = { cr, ci };
    const bool no_g = g_real == 1 &amp;&amp; g_imag == 1;
    int antal = antal0;
    double test1 = test10;
    double test2 = test20;
    for (; antal &lt; nMaxIter &amp;&amp; test1 &lt;= m_nBailout2; antal++)
    {
      const long double Xr = dxr[antal];
      const long double Xi = dxi[antal];
      const double Xz = m_db_z[antal];
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
      }
      long double xrn, xin;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const ldcomplex X = {Xr, Xi}, x = {xr, xi};
      ldcomplex xn;
@ldc  {
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.re; xin = xn.im;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@ld   {
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
    return true;
  }
  return false;
}

bool FORMULA(perturbation_floatexp,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, floatexp real, floatexp imag, double g_FactorAR, double g_FactorAI, floatexp xr, floatexp xi, const floatexp cr, const floatexp ci)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    const fecomplex A = { g_FactorAR, g_FactorAI };
    const fecomplex c = { cr, ci };
		for (; antal &lt; nMaxIter &amp;&amp; test1 &lt;= m_nBailout2; antal++)
    {
      const floatexp Xr = m_dxr[antal];
      const floatexp Xi = m_dxi[antal];
      const floatexp Xxr = Xr + xr;
      const floatexp Xxi = Xi + xi;
      test2 = test1;
      test1 = (real*Xxr*Xxr + imag*Xxi*Xxi).todouble();
      if (test1 &lt; m_db_z[antal]){
        if (!m_bNoGlitchDetection)
          test1 = m_nBailout2 * 2;
        bGlitch = true;
      }
      floatexp xrn, xin;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const fecomplex X = {Xr, Xi}, x = {xr, xi};
      fecomplex xn;
@fec  {
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.re; xin = xn.im;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
@fe   {
        <xsl:value-of select="perturbation" />
      }
</xsl:when>
</xsl:choose>

      xr = xrn;
      xi = xin;
    }
    return true;
  }
  return false;
}


</xsl:for-each>


bool reference_double(int m_nFractalType, int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, double g_SeedR, double g_SeedI, double g_FactorAR, double g_FactorAI, double terminate, double g_real, double g_imag)
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
      <xsl:choose><xsl:when test="not(@power) or @power=''">
        default:
      </xsl:when><xsl:when test="@power!=''">
        case <xsl:value-of select="@power" />:
      </xsl:when></xsl:choose>
          return FORMULA(reference_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}


bool reference_long_double(int m_nFractalType, int m_nPower, long double *m_ldxr, long double *m_ldxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, double g_SeedR, double g_SeedI, double g_FactorAR, double g_FactorAI, double terminate, double g_real, double g_imag)
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
      <xsl:choose><xsl:when test="not(@power) or @power=''">
        default:
      </xsl:when><xsl:when test="@power!=''">
        case <xsl:value-of select="@power" />:
      </xsl:when></xsl:choose>
          return FORMULA(reference_long_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}


bool reference_floatexp(int m_nFractalType, int m_nPower, floatexp *m_dxr, floatexp *m_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, double g_SeedR, double g_SeedI, double g_FactorAR, double g_FactorAI, double terminate, floatexp real, floatexp imag)
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
      <xsl:choose><xsl:when test="not(@power) or @power=''">
        default:
      </xsl:when><xsl:when test="@power!=''">
        case <xsl:value-of select="@power" />:
      </xsl:when></xsl:choose>
          return FORMULA(reference_floatexp,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, Cr, Ci, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, real, imag);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}



bool perturbation_double(int m_nFractalType, int m_nPower, const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, double g_FactorAR, double g_FactorAI, double xr, double xi, const double cr, const double ci)
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
      <xsl:choose><xsl:when test="not(@power) or @power=''">
        default:
      </xsl:when><xsl:when test="@power!=''">
        case <xsl:value-of select="@power" />:
      </xsl:when></xsl:choose>
          return FORMULA(perturbation_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}



bool perturbation_long_double(int m_nFractalType, int m_nPower, const long double *dxr, const long double *dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, double g_FactorAR, double g_FactorAI, long double xr, long double xi, const long double cr, const long double ci)
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
      <xsl:choose><xsl:when test="not(@power) or @power=''">
        default:
      </xsl:when><xsl:when test="@power!=''">
        case <xsl:value-of select="@power" />:
      </xsl:when></xsl:choose>
          return FORMULA(perturbation_long_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, dxr, dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}



bool perturbation_floatexp(int m_nFractalType, int m_nPower, const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, floatexp real, floatexp imag, double g_FactorAR, double g_FactorAI, floatexp xr, floatexp xi, const floatexp cr, const floatexp ci)
{
  switch (m_nFractalType)
  {
  <xsl:for-each select="formulas/group">
    // <xsl:value-of select="@name" />
    case <xsl:value-of select="@type" />:
      switch (m_nPower)
      {
      <xsl:for-each select="formula">
      <xsl:choose><xsl:when test="not(@power) or @power=''">
        default:
      </xsl:when><xsl:when test="@power!=''">
        case <xsl:value-of select="@power" />:
      </xsl:when></xsl:choose>
          return FORMULA(perturbation_floatexp,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, real, imag, g_FactorAR, g_FactorAI, xr, xi, cr, ci);
      </xsl:for-each>
      }
      break;
  </xsl:for-each>
  }
  return false;
}


void combo5_addstrings(HWND hWnd, int IDC_COMBO5)
{
  <xsl:for-each select="formulas/group">
    SendDlgItemMessage(hWnd,IDC_COMBO5,CB_ADDSTRING,0,(LPARAM)"<xsl:value-of select="@name" />"); // <xsl:value-of select="@type" />
  </xsl:for-each>
}


// EOF
</xsl:template>
</xsl:stylesheet>
