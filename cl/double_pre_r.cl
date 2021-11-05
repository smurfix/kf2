void perturbation_double_loop
( __global const p_config   *g
, __global const mantissa   *m_refx
, __global const mantissa   *m_refy
, __global const mantissa   *m_refz
,                p_status_d *l
)
{
  const mantissa Ar = g->g_FactorAR;
  const mantissa Ai = g->g_FactorAI;
  const dcomplex A = { Ar, Ai };
  bool no_g = g->g_real == 1.0 && g->g_imag == 1.0 && g->norm_p == 2.0;
  // type R formulas
  const mantissa cr = l->cr;
  const mantissa ci = l->ci;
  mantissa xr = l->xr;
  mantissa xi = l->xi;
  mantissa dr = l->dxa;
  mantissa di = l->dya;
  const mantissa dr0 = l->daa;
  const mantissa di0 = l->dba;
  // conditions
  mantissa test1 = l->test1;
  mantissa test2 = l->test2;
  long antal = l->antal;
  long rantal = antal;
  mantissa Xxr = 0;
  mantissa Xxi = 0;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
  const mantissa Cr = m_refx[0]; // FIXME
  const mantissa Ci = m_refy[0]; // FIXME
  const mantissa Ccr = Cr + cr;
  const mantissa Cci = Ci + ci;
  const mantissa abs_c2 = Ccr * Ccr + Cci * Cci;
  const mantissa abs_c = sqrt(abs_c2);
  mantissa tia_sum_old = 0;
  mantissa tia_sum = 0;
  long tia_count = 0;
#endif
  for (; antal < g->nMaxIter && rantal < g->reference_size_x; antal++)
  {
    mantissa Xr = m_refx[rantal];
    mantissa Xi = m_refy[rantal];
    mantissa Xz = m_refz[rantal];
    rantal++;
    Xxr = Xr + xr;
    Xxi = Xi + xi;
    mantissa Xxr2 = Xxr * Xxr;
    mantissa Xxi2 = Xxi * Xxi;
    test2 = test1;
    test1 = Xxr2 + Xxi2;
    if (g->singleref)
    {
      mantissa test0 = test1;
      if (! no_g)
      {
        test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxr, Xxi);
      }
      if (test1 > g->m_nBailout2)
      {
        break;
      }
      if (test0 < xr * xr + xi * xi || rantal == g->reference_size_x)
      {
        xr = Xxr;
        xi = Xxi;
        rantal = 0;
        Xr = 0;
        Xi = 0;
        Xz = 0;
        Xxr = Xr + xr;
        Xxi = Xi + xi;
        Xxr2 = Xxr * Xxr;
        Xxi2 = Xxi * Xxi;
        test1 = Xxr2 + Xxi2;
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxr, Xxi);
        }
      }
    }
    else
    {
      if (test1 < Xz)
      {
        l->bGlitch = true;
        if (! l->bNoGlitchDetection)
          break;
      }
      if (! no_g)
      {
        test1 = pnorm(g->g_real, g->g_imag, g->norm_p, Xxr, Xxi);
      }
      if (test1 > g->m_nBailout2)
      {
        break;
      }
    }
    mantissa xrn, xin, drn, din;
// continued...
