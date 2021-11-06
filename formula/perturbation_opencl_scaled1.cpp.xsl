<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">

#define STR(s) #s

<xsl:for-each select="//scaled/..">

const char *perturbation_opencl_scaled_loop_<xsl:value-of select="@type" />_<xsl:value-of select="@power" /> = STR(

void perturbation_double_loop
( __global const p_config    *g
, __global const mantissa    *m_refx
, __global const mantissa    *m_refy
, __global const mantissa    *m_refz
,                p_status_d  *l
)
{
}

void perturbation_floatexp_loop
( __global const p_config    *g
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
}

void perturbation_scaled_loop
( __global const p_config    *g
, __global const mantissa    *m_refx
, __global const mantissa    *m_refy
, __global const mantissa    *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
  const mantissa w2threshold = exp(log(LARGE_MANTISSA) / <xsl:value-of select="@power" />);
  const floatexp zero = fe_floatexp(0.0, 0);
  const floatexp one = fe_floatexp(1.0, 0);
  const mantissa Ar = g->g_FactorAR;
  const mantissa Ai = g->g_FactorAI;
  const dcomplex A = { Ar COMMA Ai };
  const bool no_g = g->g_real == 1.0 &amp;&amp; g->g_imag == 1.0 &amp;&amp; g->norm_p == 2.0;
  const floatexp cr = l->cr;
  const floatexp ci = l->ci;
  floatexp xr = l->xr;
  floatexp xi = l->xi;
  int count = 0;
  int stanza = 0;
  mantissa test1 = l->test1;
  mantissa test2 = l->test2;
  long antal = l->antal;
  long rantal = antal;
  floatexp Xxr = zero;
  floatexp Xxi = zero;
) "\n#ifdef TRIANGLE_INEQUALITY_AVERAGE\n" STR(
  const mantissa Cr = m_refx[0];
  const mantissa Ci = m_refy[0];
  const mantissa Ccr = Cr + fe_double(cr);
  const mantissa Cci = Ci + fe_double(ci);
  const mantissa abs_c = sqrt(Ccr * Ccr + Cci * Cci);
  mantissa tia_sum_old = 0;
  mantissa tia_sum = 0;
  long tia_count = 0;
) "\n#endif\n" STR(
  long k = 0; long n = 0;
  floatexp Xrf; floatexp Xif; floatexp Xzf;
  do
  {
    if (k &lt; g->m_nRSize)
    {
      n = m_refN[k];
      Xrf = m_refX[k];
      Xif = m_refY[k];
      Xzf = m_refZ[k];
      k++;
    }
    else
    {
      n = g->nMaxIter;
    }
  }
  while (n &lt; rantal);

  floatexp S = fe_sqrt(fe_add(fe_sqr(xr), fe_sqr(xi)));
  mantissa s = fe_double(S);
  mantissa wr = fe_double(fe_div(xr, S));
  mantissa wi = fe_double(fe_div(xi, S));
  mantissa ur = fe_double(fe_div(cr, S));
  mantissa ui = fe_double(fe_div(ci, S));
  mantissa u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));

  for (; antal &lt; g->nMaxIter &amp;&amp; rantal &lt; g->reference_size_x; antal++)
  {
    if (rantal &lt; n)
    {
      mantissa Xr = m_refx[rantal];
      mantissa Xi = m_refy[rantal];
      mantissa Xz = m_refz[rantal];
      rantal++;
      mantissa wr2 = wr * wr;
      (void) wr2;
      mantissa wi2 = wi * wi;
      (void) wi2;
      mantissa Xxrd = Xr + wr * s;
      mantissa Xxid = Xi + wi * s;
      mantissa Xxr2 = Xxrd * Xxrd;
      mantissa Xxi2 = Xxid * Xxid;
      test2 = test1;
      test1 = Xxr2 + Xxi2;
      if (g->singleref)
      {
        mantissa test0 = test1;
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
        }
        if (test1 &gt; g->m_nBailout2)
        {
          Xxr = fe_floatexp(Xxrd, 0);
          Xxi = fe_floatexp(Xxid, 0);
          break;
        }
        if (test0 &lt; s * s * (wr2 + wi2) || rantal == g->reference_size_x)
        {
          const floatexp xr = fe_floatexp(Xxrd, 0);
          const floatexp xi = fe_floatexp(Xxid, 0);
          rantal = 0;
          Xr = 0;
          Xi = 0;
          Xz = 0;
          Xxrd = fe_double(/* Xr + */ xr);
          Xxid = fe_double(/* Xi + */ xi);
          Xxr2 = Xxrd * Xxrd;
          Xxi2 = Xxid * Xxid;
          test1 = Xxr2 + Xxi2;
          if (! no_g)
          {
            test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
          }
          // rescale
          S = fe_sqrt(fe_add(fe_sqr(xr), fe_sqr(xi)));
          s = fe_double(S);
          wr = fe_double(fe_div(xr, S));
          wi = fe_double(fe_div(xi, S));
          ur = fe_double(fe_div(cr, S));
          ui = fe_double(fe_div(ci, S));
          u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
          wr2 = wr * wr;
          wi2 = wi * wi;
        }
      }
      else
      {
        if (test1 &lt; Xz)
        {
          l->bGlitch = true;
          if (! l->bNoGlitchDetection)
          {
            Xxr = fe_floatexp(Xxrd, 0);
            Xxi = fe_floatexp(Xxid, 0);
            break;
          }
        }
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
        }
        if (test1 &gt; g->m_nBailout2)
        {
          Xxr = fe_floatexp(Xxrd, 0);
          Xxi = fe_floatexp(Xxid, 0);
          break;
        }
      }
      mantissa wrn; mantissa win;
      if (false) { }
<xsl:for-each select="scaled/threshold">
      else if (s &lt;= <xsl:value-of select="@s" /> &amp;&amp; u &lt;= <xsl:value-of select="@u" />)
      {
@cld  {
        <xsl:value-of select="." />
      }
      }
</xsl:for-each>
      else
      {
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
        floatexp xrn = fe_muld(S, wrn);
        floatexp xin = fe_muld(S, win);
        S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
        s = fe_double(S);
        wr = fe_double(fe_div(xrn, S));
        wi = fe_double(fe_div(xin, S));
        ur = fe_double(fe_div(cr, S));
        ui = fe_double(fe_div(ci, S));
        u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
      }
    }

    else
    {
      rantal++;
      floatexp Xr = Xrf;
      floatexp Xi = Xif;
      floatexp Xz = Xzf;
      if (k &lt; g->m_nRSize)
      {
        n = m_refN[k];
        Xrf = m_refX[k];
        Xif = m_refY[k];
        Xzf = m_refZ[k];
        k++;
      }
      else
      {
        if (g->singleref)
        {
          k = 0;
          n = m_refN[k];
          Xrf = m_refX[k];
          Xif = m_refY[k];
          Xzf = m_refZ[k];
          k++;
        }
        else
        {
          n = g->nMaxIter;
        }
      }
      floatexp xr = fe_muld(S, wr);
      floatexp xi = fe_muld(S, wi);
      Xxr = fe_add(Xr, xr);
      Xxi = fe_add(Xi, xi);
      floatexp Xxr2 = fe_sqr(Xxr);
      floatexp Xxi2 = fe_sqr(Xxi);
      floatexp xr2 = fe_sqr(xr);
      floatexp xi2 = fe_sqr(xi);
      floatexp Xr2 = fe_sqr(Xr);
      floatexp Xi2 = fe_sqr(Xi);
      test2 = test1;
      floatexp ftest1 = fe_add(Xxr2, Xxi2);
      test1 = fe_double(ftest1);
      if (g->singleref)
      {
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr), fe_double(Xxi));
        }
        if (test1 &gt; g->m_nBailout2)
        {
          break;
        }
        if (fe_lt(ftest1, fe_add(fe_sqr(xr), fe_sqr(xi))) || rantal == g->reference_size_x)
        {
          xr = Xxr;
          xi = Xxi;
          rantal = 0;
          Xr = zero;
          Xi = zero;
          Xz = zero;
          Xxr = fe_add(Xr, xr);
          Xxi = fe_add(Xi, xi);
          Xxr2 = fe_sqr(Xxr);
          Xxi2 = fe_sqr(Xxi);
          xr2 = fe_sqr(xr);
          xi2 = fe_sqr(xi);
          Xr2 = fe_sqr(Xr);
          Xi2 = fe_sqr(Xi);
          ftest1 = fe_add(Xxr2, Xxi2);
          test1 = fe_double(ftest1);
          if (! no_g)
          {
            test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr), fe_double(Xxi));
          }
          // rescale
          S = fe_sqrt(fe_add(fe_sqr(xr), fe_sqr(xi)));
          s = fe_double(S);
          wr = fe_double(fe_div(xr, S));
          wi = fe_double(fe_div(xi, S));
          ur = fe_double(fe_div(cr, S));
          ui = fe_double(fe_div(ci, S));
          u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
        }
      }
      else
      {
        if (fe_lt(ftest1, Xz))
        {
          l->bGlitch = true;
          if (! l->bNoGlitchDetection)
            break;
        }
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr), fe_double(Xxi));
        }
        if (test1 &gt; g->m_nBailout2)
        {
          break;
        }
      }
      floatexp xrn; floatexp xin;
      {
@clfe {
        <xsl:value-of select="perturbation" />
      }
      }
      S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
      s = fe_double(S);
      wr = fe_double(fe_div(xrn, S));
      wi = fe_double(fe_div(xin, S));
      ur = fe_double(fe_div(cr, S));
      ui = fe_double(fe_div(ci, S));
      u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
    }
) "\n#ifdef TRIANGLE_INEQUALITY_AVERAGE\n" STR(
    {
      const mantissa Xxrd = fe_double(Xxr);
      const mantissa Xxid = fe_double(Xxi);
      const mantissa abs_z = sqrt(Xxrd * Xxrd + Xxid * Xxid);
      const mantissa zsubcr = Xxrd - Ccr;
      const mantissa zsubci = Xxid - Cci;
      const mantissa abs_z_sub_c = sqrt(zsubcr * zsubcr + zsubci * zsubci);
      const mantissa abs_z_sub_c_sub_abs_c = fabs(abs_z_sub_c - abs_c);
      const mantissa abs_z_sub_c_add_abs_c = abs_z_sub_c + abs_c;
      const mantissa tia_inc = (abs_z - abs_z_sub_c_sub_abs_c) / (abs_z_sub_c_add_abs_c - abs_z_sub_c_sub_abs_c);
      if (antal != 0 &amp;&amp; antal != g->nMaxIter - 1 &amp;&amp; ! isnan(tia_inc) &amp;&amp; ! isinf(tia_inc))
      {
        tia_sum_old = tia_sum;
        tia_sum += tia_inc;
        tia_count += 1;
      }
    }
) "\n#endif\n" STR(
  }
  l->antal = antal;
  l->test1 = test1;
  l->test2 = test2;
  l->xr = Xxr;
  l->xi = Xxi;
) "\n#ifdef TRIANGLE_INEQUALITY_AVERAGE\n" STR(
  {
    const mantissa Xxrd = fe_double(Xxr);
    const mantissa Xxid = fe_double(Xxi);
    const mantissa avg = tia_sum / tia_count;
    const mantissa avg_old = tia_sum_old / (tia_count - 1);
    const mantissa abs_z = sqrt(Xxrd * Xxrd + Xxid * Xxid);
    const mantissa abs_R = sqrt(g->m_nBailout2);
    const mantissa p = exp(l->log_m_nPower);
    mantissa f = (log(log(abs_z)) - log(log(abs_R))) / log(p);
    if (abs_z &lt; abs_R)
    {
      f = 0;
    }
    const mantissa tia = avg + (avg_old - avg) * f;
    l->tia = tia;
  }
) "\n#endif\n" STR(
}

);

const char *perturbation_opencl_scaled_derivatives_loop_<xsl:value-of select="@type" />_<xsl:value-of select="@power" /> = STR(

void perturbation_double_loop
( __global const p_config    *g
, __global const mantissa    *m_refx
, __global const mantissa    *m_refy
, __global const mantissa    *m_refz
,                p_status_d  *l
)
{
}

void perturbation_floatexp_loop
( __global const p_config    *g
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
}

void perturbation_scaled_loop
( __global const p_config    *g
, __global const mantissa    *m_refx
, __global const mantissa    *m_refy
, __global const mantissa    *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
  const mantissa w2threshold = exp(log(LARGE_MANTISSA) / <xsl:value-of select="@power" />);
  const mantissa d2threshold = exp(log(LARGE_MANTISSA) / (<xsl:value-of select="@power" /> - 1));
  const floatexp zero = fe_floatexp(0.0, 0);
  const floatexp one = fe_floatexp(1.0, 0);
  const mantissa Ar = g->g_FactorAR;
  const mantissa Ai = g->g_FactorAI;
  const dcomplex A = { Ar COMMA Ai };
  const bool no_g = g->g_real == 1.0 &amp;&amp; g->g_imag == 1.0 &amp;&amp; g->norm_p == 2.0;
  const floatexp cr = l->cr;
  const floatexp ci = l->ci;
  floatexp xr = l->xr;
  floatexp xi = l->xi;

  mantissa test1 = l->test1;
  mantissa test2 = l->test2;
  long antal = l->antal;
  long rantal = antal;
  bool bGlitch = l->bGlitch;
  floatexp XxrF = zero;
  floatexp XxiF = zero;
<xsl:choose>
<xsl:when test="derivative/@t='C'">
  assert(! "perturbation scaled derivative C implemented");
</xsl:when>
<xsl:when test="derivative/@t='R'">
  floatexp drF = l->dxa; floatexp diF = l->dya;
  const floatexp dr0F = l->daa; const floatexp di0F = l->dba;
</xsl:when>
<xsl:when test="derivative/@t='M'">
  floatexp dxaF = l->dxa; floatexp dxbF = l->dxb; floatexp dyaF = l->dya; floatexp dybF = l->dyb;
</xsl:when>
</xsl:choose>

  long K = 0; long N = 0;
  floatexp X = zero; floatexp Y = zero; floatexp Z0 = zero;
  do
  {
    if (K &lt; g->m_nRSize)
    {
      N = m_refN[K];
      X = m_refX[K];
      Y = m_refY[K];
      Z0 = m_refZ[K];
      K++;
    }
    else
    {
      N = g->nMaxIter;
    }
  }
  while (N &lt; rantal);

  floatexp S = fe_sqrt(fe_add(fe_sqr(xr), fe_sqr(xi)));
  if (fe_cmp(S, zero) == 0)
  {
    S = fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci)));
  }
  if (fe_cmp(S, zero) == 0)
  {
    S = one;
  }
  mantissa s = fe_double(S);
  mantissa wr = fe_double(fe_div(xr, S));
  mantissa wi = fe_double(fe_div(xi, S));
  mantissa ur = fe_double(fe_div(cr, S));
  mantissa ui = fe_double(fe_div(ci, S));
  mantissa u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
<xsl:choose>
<xsl:when test="derivative/@t='R'">
  floatexp J = S;
  mantissa drD = fe_double(fe_div(drF, J));
  mantissa diD = fe_double(fe_div(diF, J));
  mantissa dr0D = fe_double(fe_div(dr0F, J));
  mantissa di0D = fe_double(fe_div(di0F, J));
</xsl:when>
<xsl:when test="derivative/@t='M'">
  floatexp J = S;
  mantissa dxaD = fe_double(fe_div(dxaF, J));
  mantissa dyaD = fe_double(fe_div(dyaF, J));
  mantissa dxbD = fe_double(fe_div(dxbF, J));
  mantissa dybD = fe_double(fe_div(dybF, J));
  mantissa daaD = fe_double(fe_div(l->daa, J));
  mantissa dbaD = fe_double(fe_div(l->dba, J));
  mantissa dabD = fe_double(fe_div(l->dab, J));
  mantissa dbbD = fe_double(fe_div(l->dbb, J));
</xsl:when>
</xsl:choose>
) "\n#ifdef TRIANGLE_INEQUALITY_AVERAGE\n" STR(
  const mantissa Cr = m_refx[0];
  const mantissa Ci = m_refy[0];
  const mantissa Ccr = Cr + fe_double(cr);
  const mantissa Cci = Ci + fe_double(ci);
  const mantissa abs_c = sqrt(Ccr * Ccr + Cci * Cci);
  mantissa tia_sum_old = 0;
  mantissa tia_sum = 0;
  long tia_count = 0;
) "\n#endif\n" STR(

  for (; antal &lt; g->nMaxIter &amp;&amp; rantal &lt; g->reference_size_x; antal++)
  {
    bool full_iteration = rantal == N;
    if (full_iteration)
    {
      rantal++;
      floatexp Xr = X;
      floatexp Xi = Y;
      floatexp Xz = Z0;
      if (K &lt; g->m_nRSize)
      {
        N = m_refN[K];
        X = m_refX[K];
        Y = m_refY[K];
        Z0 = m_refZ[K];
        K++;
      }
      else
      {
        if (g->singleref)
        {
          K = 0;
          N = m_refN[K];
          X = m_refX[K];
          Y = m_refY[K];
          Z0 = m_refZ[K];
          K++;
        }
        else
        {
          N = g->nMaxIter;
        }
      }
      floatexp xr = fe_muld(S, wr);
      floatexp xi = fe_muld(S, wi);
      XxrF = fe_add(Xr, xr);
      XxiF = fe_add(Xi, xi);
      floatexp Xxr2 = fe_sqr(XxrF);
      floatexp Xxi2 = fe_sqr(XxiF);
      floatexp xr2 = fe_sqr(xr);
      floatexp xi2 = fe_sqr(xi);
      floatexp Xr2 = fe_sqr(Xr);
      floatexp Xi2 = fe_sqr(Xi);
      test2 = test1;
      floatexp ftest1 = fe_add(Xxr2, Xxi2);
      test1 = fe_double(ftest1);
      if (g->singleref)
      {
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(XxrF), fe_double(XxiF));
        }
        if (test1 &gt; g->m_nBailout2)
        {
          break;
        }
        if (fe_lt(ftest1, fe_add(xr2, xi2)) || rantal == g->reference_size_x)
        {
          xr = XxrF;
          xi = XxiF;
          rantal = 0;
          Xr = zero;
          Xi = zero;
          Xz = zero;
          XxrF = fe_add(Xr, xr);
          XxiF = fe_add(Xi, xi);
          Xxr2 = fe_sqr(XxrF);
          Xxi2 = fe_sqr(XxiF);
          xr2 = fe_sqr(xr);
          xi2 = fe_sqr(xi);
          Xr2 = fe_sqr(Xr);
          Xi2 = fe_sqr(Xi);
          ftest1 = fe_add(Xxr2, Xxi2);
          test1 = fe_double(ftest1);
          if (! no_g)
          {
            test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(XxrF), fe_double(XxiF));
          }
          // rescale
          S = fe_sqrt(fe_add(fe_sqr(xr), fe_sqr(xi)));
          s = fe_double(S);
          wr = fe_double(fe_div(xr, S));
          wi = fe_double(fe_div(xi, S));
          ur = fe_double(fe_div(cr, S));
          ui = fe_double(fe_div(ci, S));
          u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
        }
      }
      else
      {
        if (fe_lt(ftest1, Xz))
        {
          bGlitch = true;
          if (! g->m_bNoGlitchDetection)
            break;
        }
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(XxrF), fe_double(XxiF));
        }
        if (test1 &gt; g->m_nBailout2)
        {
          break;
        }
      }
      const floatexp Xxr = XxrF;
      const floatexp Xxi = XxiF;
      floatexp xrn; floatexp xin; floatexp drn; floatexp din; floatexp dxan; floatexp dyan; floatexp dxbn; floatexp dybn;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
      {
        (void) dxan;
        (void) dyan;
        (void) dxbn;
        (void) dybn;
        const floatexp dr = fe_muld(J, drD);
        const floatexp di = fe_muld(J, diD);
        const floatexp dr0 = dr0F;
        const floatexp di0 = di0F;
@clfe     {
          <xsl:value-of select="derivative" />
        }
      }
</xsl:when>
<xsl:when test="derivative/@t='M'">
      {
        (void) drn;
        (void) din;
        const floatexp dxa = fe_muld(J, dxaD);
        const floatexp dxb = fe_muld(J, dxbD);
        const floatexp dya = fe_muld(J, dyaD);
        const floatexp dyb = fe_muld(J, dybD);
        const floatexp daa = l->daa;
        const floatexp dab = l->dab;
        const floatexp dba = l->dba;
        const floatexp dbb = l->dbb;
@clfe     {
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
@clfe   {
        <xsl:value-of select="perturbation" />
      }
      }
      S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
      s = fe_double(S);
      wr = fe_double(fe_div(xrn, S));
      wi = fe_double(fe_div(xin, S));
      ur = fe_double(fe_div(cr, S));
      ui = fe_double(fe_div(ci, S));
      u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
<xsl:choose>
<xsl:when test="derivative/@t='R'">
      J = S;
      drD = fe_double(fe_div(drn, J));
      diD = fe_double(fe_div(din, J));
      dr0D = fe_double(fe_div(dr0F, J));
      di0D = fe_double(fe_div(di0F, J));
</xsl:when>
<xsl:when test="derivative/@t='M'">
      J = S;
      dxaD = fe_double(fe_div(dxan, J));
      dyaD = fe_double(fe_div(dyan, J));
      dxbD = fe_double(fe_div(dxbn, J));
      dybD = fe_double(fe_div(dybn, J));
      daaD = fe_double(fe_div(l->daa, J));
      dbaD = fe_double(fe_div(l->dba, J));
      dabD = fe_double(fe_div(l->dab, J));
      dbbD = fe_double(fe_div(l->dbb, J));
</xsl:when>
</xsl:choose>
    }
    else
    {
      mantissa Xr = m_refx[rantal];
      mantissa Xi = m_refy[rantal];
      mantissa Xz = m_refz[rantal];
      rantal++;
      mantissa wr2 = wr * wr;
      (void) wr2;
      mantissa wi2 = wi * wi;
      (void) wi2;
      mantissa Xxrd = Xr + wr * s;
      mantissa Xxid = Xi + wi * s;
      mantissa Xxr2 = Xxrd * Xxrd;
      mantissa Xxi2 = Xxid * Xxid;
      test2 = test1;
      test1 = Xxr2 + Xxi2;
      if (g->singleref)
      {
        mantissa test0 = test1;
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
        }
        if (test1 &gt; g->m_nBailout2)
        {
          XxrF = fe_floatexp(Xxrd, 0);
          XxiF = fe_floatexp(Xxid, 0);
          break;
        }
        if (test0 &lt; s * s * (wr2 + wi2) || rantal == g->reference_size_x)
        {
          const floatexp xr = fe_floatexp(Xxrd, 0);
          const floatexp xi = fe_floatexp(Xxid, 0);
          rantal = 0;
          Xr = 0;
          Xi = 0;
          Xz = 0;
          Xxrd = fe_double(/* Xr + */ xr);
          Xxid = fe_double(/* Xi + */ xi);
          Xxr2 = Xxrd * Xxrd;
          Xxi2 = Xxid * Xxid;
          test1 = Xxr2 + Xxi2;
          if (! no_g)
          {
            test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
          }
          // rescale
          S = fe_sqrt(fe_add(fe_sqr(xr), fe_sqr(xi)));
          s = fe_double(S);
          wr = fe_double(fe_div(xr, S));
          wi = fe_double(fe_div(xi, S));
          ur = fe_double(fe_div(cr, S));
          ui = fe_double(fe_div(ci, S));
          u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
          wr2 = wr * wr;
          wi2 = wi * wi;
        }
      }
      else
      {
        if (test1 &lt; Xz)
        {
          bGlitch = true;
          if (! g->m_bNoGlitchDetection)
          {
            XxrF = fe_floatexp(Xxrd, 0);
            XxiF = fe_floatexp(Xxid, 0);
            break;
          }
        }
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxrd, Xxid);
        }
        if (test1 &gt; g->m_nBailout2)
        {
          XxrF = fe_floatexp(Xxrd, 0);
          XxiF = fe_floatexp(Xxid, 0);
          break;
        }
      }
      const mantissa Xxr = Xxrd;
      const mantissa Xxi = Xxid;
      mantissa drn; mantissa din; mantissa dxan; mantissa dyan; mantissa dxbn; mantissa dybn;
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
@cld      {
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
@cld      {
          <xsl:value-of select="derivative" />
        }
      }
</xsl:when>
</xsl:choose>
      mantissa wrn; mantissa win;
      if (false) { }
<xsl:for-each select="scaled/threshold">
      else if (s &lt;= <xsl:value-of select="@s" /> &amp;&amp; u &lt;= <xsl:value-of select="@u" />)
      {
@cld    {
        <xsl:value-of select="." />
      }
      }
</xsl:for-each>
      else
      {
        wrn = 0;
        win = 0;
      }
      const mantissa w2 = wrn * wrn + win * win;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
      const mantissa d2 = drn * drn + din * din;
      if (w2 &lt; w2threshold &amp;&amp; d2 &lt; d2threshold)
      {
        wr = wrn;
        wi = win;
        drD = drn;
        diD = din;
      }
      else
      {
        floatexp xrn = fe_muld(S, wrn);
        floatexp xin = fe_muld(S, win);
        floatexp drF = fe_muld(J, drn);
        floatexp diF = fe_muld(J, din);
        S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
        s = fe_double(S);
        wr = fe_double(fe_div(xrn, S));
        wi = fe_double(fe_div(xin, S));
        ur = fe_double(fe_div(cr, S));
        ui = fe_double(fe_div(ci, S));
        u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
        J = S;
        drD = fe_double(fe_div(drF, J));
        diD = fe_double(fe_div(diF, J));
        dr0D = fe_double(fe_div(dr0F, J));
        di0D = fe_double(fe_div(di0F, J));
      }
</xsl:when>
<xsl:when test="derivative/@t='M'">
      const mantissa d2 = dxan * dxan + dxbn * dxbn + dyan * dyan + dybn * dybn;
      if (w2 &lt; w2threshold &amp;&amp; d2 &lt; d2threshold)
      {
        wr = wrn;
        wi = win;
        dxaD = dxan;
        dyaD = dyan;
        dxbD = dxbn;
        dybD = dybn;
      }
      else
      {
        floatexp xrn = fe_muld(S, wrn);
        floatexp xin = fe_muld(S, win);
        floatexp dxaF = fe_muld(J, dxan);
        floatexp dyaF = fe_muld(J, dyan);
        floatexp dxbF = fe_muld(J, dxbn);
        floatexp dybF = fe_muld(J, dybn);
        S = fe_sqrt(fe_add(fe_sqr(xrn), fe_sqr(xin)));
        s = fe_double(S);
        wr = fe_double(fe_div(xrn, S));
        wi = fe_double(fe_div(xin, S));
        ur = fe_double(fe_div(cr, S));
        ui = fe_double(fe_div(ci, S));
        u = fe_double(fe_div(fe_sqrt(fe_add(fe_sqr(cr), fe_sqr(ci))), S));
        J = S;
        dxaD = fe_double(fe_div(dxaF, J));
        dxbD = fe_double(fe_div(dxbF, J));
        dyaD = fe_double(fe_div(dyaF, J));
        dybD = fe_double(fe_div(dybF, J));
        daaD = fe_double(fe_div(l->daa, J));
        dbaD = fe_double(fe_div(l->dba, J));
        dabD = fe_double(fe_div(l->dab, J));
        dbbD = fe_double(fe_div(l->dbb, J));
      }
</xsl:when>
</xsl:choose>
    }
) "\n#ifdef TRIANGLE_INEQUALITY_AVERAGE\n" STR(
    {
      const mantissa Xxrd = fe_double(XxrF);
      const mantissa Xxid = fe_double(XxiF);
      const mantissa abs_z = sqrt(Xxrd * Xxrd + Xxid * Xxid);
      const mantissa zsubcr = Xxrd - Ccr;
      const mantissa zsubci = Xxid - Cci;
      const mantissa abs_z_sub_c = sqrt(zsubcr * zsubcr + zsubci * zsubci);
      const mantissa abs_z_sub_c_sub_abs_c = fabs(abs_z_sub_c - abs_c);
      const mantissa abs_z_sub_c_add_abs_c = abs_z_sub_c + abs_c;
      const mantissa tia_inc = (abs_z - abs_z_sub_c_sub_abs_c) / (abs_z_sub_c_add_abs_c - abs_z_sub_c_sub_abs_c);
      if (antal != 0 &amp;&amp; antal != g->nMaxIter - 1 &amp;&amp; ! isnan(tia_inc) &amp;&amp; ! isinf(tia_inc))
      {
        tia_sum_old = tia_sum;
        tia_sum += tia_inc;
        tia_count += 1;
      }
    }
) "\n#endif\n" STR(
  }

  l->antal = antal;
  l->bGlitch = bGlitch;
  l->test1 = test1;
  l->test2 = test2;
  l->xr = XxrF;
  l->xi = XxiF;
<xsl:choose>
<xsl:when test="derivative/@t='R'">
  l->dxa = fe_muld(J,  drD);
  l->dxb = fe_muld(J, -diD);
  l->dya = fe_muld(J,  diD);
  l->dyb = fe_muld(J,  drD);
</xsl:when>
<xsl:when test="derivative/@t='M'">
  l->dxa = fe_muld(J, dxaD);
  l->dxb = fe_muld(J, dxbD);
  l->dya = fe_muld(J, dyaD);
  l->dyb = fe_muld(J, dybD);
</xsl:when>
</xsl:choose>
) "\n#ifdef TRIANGLE_INEQUALITY_AVERAGE\n" STR(
  {
    const mantissa Xxrd = fe_double(XxrF);
    const mantissa Xxid = fe_double(XxiF);
    const mantissa avg = tia_sum / tia_count;
    const mantissa avg_old = tia_sum_old / (tia_count - 1);
    const mantissa abs_z = sqrt(Xxrd * Xxrd + Xxid * Xxid);
    const mantissa abs_R = sqrt(g->m_nBailout2);
    const mantissa p = exp(l->log_m_nPower);
    mantissa f = (log(log(abs_z)) - log(log(abs_R))) / log(p);
    if (abs_z &lt; abs_R)
    {
      f = 0;
    }
    const mantissa tia = avg + (avg_old - avg) * f;
    l->tia = tia;
  }
) "\n#endif\n" STR(
}

);

</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
