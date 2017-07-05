<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">


typedef struct
{
  int antal;
  int bGlitch;
  double test1;
  double test2;
} p_status;


typedef struct
{
  int count;
  int width;
  int height;
  int antal;
  int nMaxiter
  int m_bNoGlitchDetection
  int m_nSmoothMethod;
  int m_nPower;
  double m_nBailout2;
  double g_real;
  double g_imag;
  double g_FactorAR;
  double g_FactorAI;
  double m_nBailout;
} p_config;


<xsl:for-each select="formulas/group/formula">


p_status perturbation_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_point
( const dcomplex *ref;
, const double *refz;
, const p_status status0
, const p_config config
, const double cr
, const double ci
, double xr
, double xi
)
{
  const dcomplex A = { config.g_FactorAR, config.g_FactorAI };
  int antal = status0.antal;
  int bGlitch = status0.bGlitch;
  double test1 = status0.test1;
  double test2 = status0.test2;
  for (; antal &lt; config.nMaxIter &amp;&amp; test1 &lt;= config.m_nBailout2; antal++)
  {
      const double Xr = ref[antal - config.antal].re;
      const double Xi = ref[antal - config.antal].im;
      const double Xxr = Xr + xr;
      const double Xxi = Xi + xi;
      test2 = test1;
      test1 = config.g_real * Xxr * Xxr + config.g_imag * Xxi * Xxi;
      if (test1 &lt; refz[antal - config.antal])
      {
        if (! config.m_bNoGlitchDetection)
          test1 = config.m_nBailout2 * 2;
        bGlitch = true;
      }
      const double Xr2 = Xr * Xr;
      const double Xi2 = Xi * Xi;
      const double xr2 = xr * xr;
      const double xi2 = xi * xi;
      double xrn, xin;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const dcomplex X = { Xr, Xi }, x = { xr, xi }, c = { cr, ci }, X2 = dc_mul(X, X), x2 = dc_mul(x, x);
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
  p_status status1 = { antal, bGlitch, test1, test2 };
  return status1;
}


p_status perturbation_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_point
( const fecomplex *ref
, const double *refz
, const p_status status0
, const p_config config
, const floatexp cr
, const floatexp ci
, const floatexp xr0
, const floatexp xi0
)
{
  const fecomplex A = { fe_floatexp(config.g_FactorAR, 0), fe_floatexp(config.g_FactorAI, 0) };
  int antal = status0.antal;
  int bGlitch = status0.bGlitch;
  double test1 = status0.test1;
  double test2 = status0.test2;
  floatexp xr = xr0;
  floatexp xi = xi0;
  for (; antal &lt; config.nMaxIter &amp;&amp; test1 &lt;= config.m_nBailout2; antal++)
  {
      const floatexp Xr = ref[antal - config.antal].re;
      const floatexp Xi = ref[antal - config.antal].im;
      const double Xxr = fe_double(fe_add(Xr, xr));
      const double Xxi = fe_double(fe_add(Xi, xi));
      test2 = test1;
      test1 = config.g_real * Xxr * Xxr + config.g_imag * Xxi * Xxi;
      if (test1 &lt; refz[antal - config.antal])
      {
        if (! config.m_bNoGlitchDetection)
          test1 = config.m_nBailout2 * 2;
        bGlitch = true;
      }
      const floatexp Xr2 = fe_mul(Xr, Xr);
      const floatexp Xi2 = fe_mul(Xi, Xi);
      const floatexp xr2 = fe_mul(xr, xr);
      const floatexp xi2 = fe_mul(xi, xi);
      floatexp xrn, xin;

<xsl:choose>
<xsl:when test="perturbation/@t='C'">
      const fecomplex X = { Xr, Xi }, x = { xr, xi }, c = { cr, ci }, X2 = fec_mul(X, X), x2 = fec_mul(x, x);
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
  p_status status1 = { antal, bGlitch, test1, test2 };
  return status1;
}


__kernel void perturbation_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
( __global const dcomplex *ref
, __global const double *refz
, __global const p_config *config0
, __global const double *cx
, __global int *m_nPixels;
, __global float *m_nTrans;
)
{
  int ix = get_global_id(0);
  p_config config = *config0;
  if (0 &lt;= ix &amp;&amp; ix &lt; config.count)
  {
    p_status status = { config.antal, false, 0.0, 0.0 };
 
    status = perturbation_double_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_point
      (ref, refz, status, config, cx[4*ix+0], cx[4*ix+1], cx[4*ix+2], cx[4*ix+3]);

    if (status.antal == config.m_nGlitchIter)
      status.bGlitch = true;
    if (status.antal == config.m_nMaxIter)
    {
      m_nPixels[ix] = status.antal;
      m_nTrans[ix] = 0;
    }
    else
    {
      int p = status.antal;
      if (!status.bGlitch &amp;&amp; config.m_nSmoothMethod == 1){
        double div = sqrt(status.test1) - sqrt(status.test2);
        if (div != 0)
          m_nTrans[ix] = (sqrt(status.test1) - config.m_nBailout) / div;
        else
          m_nTrans[ix] = 0;
      }
      else if (!status.bGlitch &amp;&amp; config.m_nSmoothMethod == 0){
          double t = log(log(sqrt(status.test1))) / log((double)config.m_nPower);
          if (! (-1.0/0.0 &lt; t &amp;&amp; t &lt; 1.0/0.0))
            t = 0;
          while (t&lt;0){
            int offs = 1 + (int)t;
            p += offs;
            t += offs;
          }
          while (t&gt;1){
            int offs = (int)t;
            p -= offs;
            t -= offs;
          }
          m_nTrans[ix] = t;
      }
      if (status.bGlitch &amp;&amp; !config.m_bNoGlitchDetection){
        m_nTrans[ix] = 2;
        p = config.m_nMaxIter - 1;
      }
      m_nPixels[ix] = p;
    }
  }
}


__kernel void perturbation_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />
( __global const fecomplex *ref
, __global const double *refz
, __global const p_config *config0
, __global const floatexp *cx
, __global int *m_nPixels;
, __global float *m_nTrans;
)
{
  int ix = get_global_id(0);
  p_config config = *config0;
  if (0 &lt;= ix &amp;&amp; ix &lt; config.count)
  {
    p_status status = { config.antal, false, 0.0, 0.0 };

    status = perturbation_floatexp_<xsl:value-of select="../@type" />_<xsl:value-of select="@power" />_point
      (ref, refz, status, config, cx[4*ix+0], cx[4*ix+1], cx[4*ix+2], cx[4*ix+3]);
  
    if (status.antal == config.m_nGlitchIter)
      status.bGlitch = true;
    if (status.antal == config.m_nMaxIter)
    {
      m_nPixels[ix] = status.antal;
      m_nTrans[ix] = 0;
    }
    else
    {
      int p = status.antal;
      if (!status.bGlitch &amp;&amp; config.m_nSmoothMethod == 1){
        double div = sqrt(status.test1) - sqrt(status.test2);
        if (div != 0)
          m_nTrans[ix] = (sqrt(status.test1) - config.m_nBailout) / div;
        else
          m_nTrans[ix] = 0;
      }
      else if (!status.bGlitch &amp;&amp; config.m_nSmoothMethod == 0){
          double t = log(log(sqrt(status.test1))) / log((double)config.m_nPower);
          if (! (-1.0/0.0 &lt; t &amp;&amp; t &lt; 1.0/0.0))
            t = 0;
          while (t&lt;0){
            int offs = 1 + (int)t;
            p += offs;
            t += offs;
          }
          while (t&gt;1){
            int offs = (int)t;
            p -= offs;
            t -= offs;
          }
          m_nTrans[ix] = t;
      }
      if (status.bGlitch &amp;&amp; !config.m_bNoGlitchDetection){
        m_nTrans[ix] = 2;
        p = config.m_nMaxIter - 1;
      }
      m_nPixels[ix] = p;
    }
  }
}

</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
