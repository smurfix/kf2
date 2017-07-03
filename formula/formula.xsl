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

static long double ConvertFromFixedFloat(const CFixedFloat &amp;f)
{
#ifdef KF_FLOAT_BACKEND_MPFR
  return mpfr_get_ld(f.m_f.backend().data(), MPFR_RNDN);
#else
  using std::ldexp;
  signed long int e = 0;
  long double l = mpf_get_d_2exp(&amp;e, f.m_f.backend().data());
  l = ldexp(l, e);
  if ((mpf_sgn(f.m_f.backend().data()) >= 0) != (l >= 0)) l = -l; // workaround GMP bug
  return l;
#endif
}

template &lt;typename T&gt;
inline T diffabs(const T &amp;c, const T &amp;d)
{
  if (c &gt; 0) {
    if (c + d &gt; 0)    return d;
    else if (d == -c) return d;
    else if (d &lt; -c)  return -d - 2 * c;
  } else if (c &lt; 0) {
    if (c + d &gt; 0)    return d + 2 * c;
    else if (d == -c) return -d;
    else if (d &lt; -c)  return -d;
  }
  return abs(d);
}


<xsl:for-each select="formulas/group/formula">


bool FORMULA(reference_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, double g_SeedR, double g_SeedI, double g_FactorAR, double g_FactorAI, double terminate, double g_real, double g_imag)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    CFixedFloat Xr = g_SeedR, Xi = g_SeedI, Xr2 = Xr.Square(), Xi2 = Xi.Square(), Xin, Xrn;
    complex&lt;CFixedFloat&gt; A(g_FactorAR,g_FactorAI);
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
    for (i = 0; i &lt; nMaxIter &amp;&amp; !m_bStop; i++)
    {

<xsl:choose>
<xsl:when test="reference/@t='C'">
      complex&lt;CFixedFloat&gt; X(Xr, Xi), C(Cr, Ci), Xn;
      {
        typedef double T;
        <xsl:value-of select="reference" />
      }
      Xrn = Xn.m_r; Xin = Xn.m_i;
</xsl:when>
<xsl:when test="reference/@t='R'">
      {
        typedef double T;
        <xsl:value-of select="reference" />
      }
</xsl:when>
</xsl:choose>

      Xr = Xrn;
      Xi = Xin;
      Xr2 = Xr.Square();
      Xi2 = Xi.Square();
      m_nRDone++;
      m_db_dxr[i] = Xr.ToDouble();
      m_db_dxi[i] = Xi.ToDouble();
      double abs_val = (g_real * m_db_dxr[i] * m_db_dxr[i] + g_imag * m_db_dxi[i] * m_db_dxi[i]);
      m_db_z[i] = abs_val * <xsl:value-of select="@glitch" />;
      if (abs_val &gt;= terminate) {
        if (nMaxIter == m_nMaxIter) {
          nMaxIter = i + 3;
          if (nMaxIter &gt; m_nMaxIter)
            nMaxIter = m_nMaxIter;
          m_nGlitchIter = nMaxIter;
        }
      }
    }
    return true;
  }
  return false;
}

bool FORMULA(reference_long_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, long double *m_ldxr, long double *m_ldxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, double g_SeedR, double g_SeedI, double g_FactorAR, double g_FactorAI, double terminate, double g_real, double g_imag)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    CFixedFloat Xr = g_SeedR, Xi = g_SeedI, Xr2 = Xr.Square(), Xi2 = Xi.Square(), Xin, Xrn;
    complex&lt;CFixedFloat&gt; A(g_FactorAR,g_FactorAI);
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
		for (i = 0; i &lt; nMaxIter &amp;&amp; !m_bStop; i++)
    {

<xsl:choose>
<xsl:when test="reference/@t='C'">
      complex&lt;CFixedFloat&gt; X(Xr, Xi), C(Cr, Ci), Xn;
      {
        typedef long double T;
        <xsl:value-of select="reference" />
      }
      Xrn = Xn.m_r; Xin = Xn.m_i;
</xsl:when>
<xsl:when test="reference/@t='R'">
      {
        typedef long double T;
        <xsl:value-of select="reference" />
      }
</xsl:when>
</xsl:choose>

      Xr = Xrn;
      Xi = Xin;
      Xr2 = Xr.Square();
      Xi2 = Xi.Square();
      m_nRDone++;
      m_ldxr[i] = ConvertFromFixedFloat(Xr);
      m_ldxi[i] = ConvertFromFixedFloat(Xi);
      long double abs_val = (g_real * m_ldxr[i] * m_ldxr[i] + g_imag * m_ldxi[i] * m_ldxi[i]);
      m_db_z[i] = abs_val * <xsl:value-of select="@glitch" />;
      if (abs_val &gt;= terminate) {
        if (nMaxIter == m_nMaxIter) {
          nMaxIter = i + 3;
          if (nMaxIter &gt; m_nMaxIter)
            nMaxIter = m_nMaxIter;
          m_nGlitchIter = nMaxIter;
        }
      }
		}
    return true;
  }
  return false;
}

bool FORMULA(reference_floatexp,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, floatexp *m_dxr, floatexp *m_dxi, double *m_db_z, int &amp;m_bStop, int &amp;m_nRDone, int &amp;m_nGlitchIter, int &amp;m_nMaxIter, const CFixedFloat &amp;Cr, const CFixedFloat &amp;Ci, double g_SeedR, double g_SeedI, double g_FactorAR, double g_FactorAI, double terminate, floatexp real, floatexp imag)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    CFixedFloat Xr = g_SeedR, Xi = g_SeedI, Xr2 = Xr.Square(), Xi2 = Xi.Square(), Xin, Xrn;
    complex&lt;CFixedFloat&gt; A(g_FactorAR,g_FactorAI);
    m_nGlitchIter = m_nMaxIter + 1;
    int nMaxIter = m_nMaxIter;
    int i;
		for (i = 0; i &lt; nMaxIter &amp;&amp; !m_bStop; i++)
    {

<xsl:choose>
<xsl:when test="reference/@t='C'">
      complex&lt;CFixedFloat&gt; X(Xr, Xi), C(Cr, Ci), Xn;
      {
        typedef floatexp T;
        <xsl:value-of select="reference" />
      }
      Xrn = Xn.m_r; Xin = Xn.m_i;
</xsl:when>
<xsl:when test="reference/@t='R'">
      {
        typedef floatexp T;
        <xsl:value-of select="reference" />
      }
</xsl:when>
</xsl:choose>


      Xr = Xrn;
      Xi = Xin;
      Xr2 = Xr.Square();
      Xi2 = Xi.Square();
      m_nRDone++;
			m_dxr[i] = Xr;
			m_dxi[i] = Xi;
			double abs_val = (real * m_dxr[i] * m_dxr[i] + imag * m_dxi[i] * m_dxi[i]).todouble();
			m_db_z[i] = abs_val * <xsl:value-of select="@glitch" />;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter > m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
		}
    return true;
  }
  return false;
}

bool FORMULA(perturbation_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, double g_FactorAR, double g_FactorAI, double xr, double xi, const double cr, const double ci)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    const complex&lt;double&gt; A(g_FactorAR, g_FactorAI);
    for (; antal &lt; nMaxIter &amp;&amp; test1 &lt;= m_nBailout2; antal++)
    {
      const double Xr = m_db_dxr[antal];
      const double Xi = m_db_dxi[antal];
      const double Xxr = Xr + xr;
      const double Xxi = Xi + xi;
      test2 = test1;
      test1 = g_real * Xxr * Xxr + g_imag * Xxi * Xxi;
      if (test1 &lt; m_db_z[antal])
      {
        if (! m_bNoGlitchDetection)
          test1 = m_nBailout2 * 2;
        bGlitch = true;
      }
      const double Xr2 = Xr * Xr;
      const double Xi2 = Xi * Xi;
      const double xr2 = xr * xr;
      const double xi2 = xi * xi;
      double xrn, xin;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;double&gt; X(Xr, Xi), x(xr, xi), c(cr, ci), X2(X * X), x2(x * x);
      complex&lt;double&gt; xn;
      {
        typedef double T;
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.m_r; xin = xn.m_i;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
      {
        typedef double T;
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

bool FORMULA(perturbation_long_double,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, const long double *dxr, const long double *dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, double g_FactorAR, double g_FactorAI, long double xr, long double xi, const long double cr, const long double ci)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    const complex&lt;long double&gt; A(g_FactorAR, g_FactorAI);
    for (; antal &lt; nMaxIter &amp;&amp; test1 &lt;= m_nBailout2; antal++)
    {
      const long double Xr = dxr[antal];
      const long double Xi = dxi[antal];
      const long double Xxr = Xr + xr;
      const long double Xxi = Xi + xi;
      test2 = test1;
      test1 = g_real * Xxr * Xxr + g_imag * Xxi * Xxi;
      if (test1 &lt; m_db_z[antal])
      {
        if (! m_bNoGlitchDetection)
          test1 = m_nBailout2 * 2;
        bGlitch = true;
      }
      const long double Xr2 = Xr * Xr;
      const long double Xi2 = Xi * Xi;
      const long double xr2 = xr * xr;
      const long double xi2 = xi * xi;
      long double xrn, xin;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;long double&gt; X(Xr, Xi), x(xr, xi), c(cr, ci), X2(X * X), x2(x * x);
      complex&lt;long double&gt; xn;
      {
        typedef long double T;
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.m_r; xin = xn.m_i;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
      {
        typedef long double T;
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

bool FORMULA(perturbation_floatexp,<xsl:value-of select="../@type" />,<xsl:value-of select="@power" />)(int m_nFractalType, int m_nPower, const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z, int &amp;antal, double &amp;test1, double &amp;test2, int &amp;bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, floatexp real, floatexp imag, double g_FactorAR, double g_FactorAI, floatexp xr, floatexp xi, const floatexp cr, const floatexp ci)
{
  if (m_nFractalType == <xsl:value-of select="../@type" /><xsl:choose><xsl:when test="@power!=''"> &amp;&amp; m_nPower == <xsl:value-of select="@power" /></xsl:when></xsl:choose>)
  {
    const complex&lt;floatexp&gt; A(g_FactorAR, g_FactorAI);
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
      const floatexp Xr2 = Xr * Xr;
      const floatexp Xi2 = Xi * Xi;
      const floatexp xr2 = xr * xr;
      const floatexp xi2 = xi * xi;
      floatexp xrn, xin;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const complex&lt;floatexp&gt; X(Xr, Xi), x(xr, xi), c(cr, ci), X2(X * X), x2(x * x);
      complex&lt;floatexp&gt; xn;
      {
        typedef floatexp T;
        <xsl:value-of select="perturbation" />
      }
      xrn = xn.m_r; xin = xn.m_i;
</xsl:when>
<xsl:when test="perturbation/@t='R'">
      {
        typedef floatexp T;
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
