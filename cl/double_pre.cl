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
  const mantissa cr = l->cr;
  const mantissa ci = l->ci;
  mantissa xr = l->xr;
  mantissa xi = l->xi;
  // hybrids
  int count = 0;
  int stanza = 0;
  // conditions
  mantissa test1 = l->test1;
  mantissa test2 = l->test2;
  long antal = l->antal;
  mantissa Xxr = 0;
  mantissa Xxi = 0;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
  const mantissa Cr = m_refx[0]; // FIXME
  const mantissa Ci = m_refy[0]; // FIXME
  const mantissa Ccr = Cr + cr;
  const mantissa Cci = Ci + ci;
  const mantissa abs_c = sqrt(Ccr * Ccr + Cci * Cci);
  mantissa tia_sum_old = 0;
  mantissa tia_sum = 0;
  long tia_count = 0;
#endif
  for (; antal < g->nMaxIter; antal++)
  {
    const mantissa Xr = m_refx[antal - g->nMinIter];
    const mantissa Xi = m_refy[antal - g->nMinIter];
    const mantissa Xz = m_refz[antal - g->nMinIter];

    Xxr = Xr + xr;
    Xxi = Xi + xi;
    const mantissa Xxr2 = Xxr * Xxr;
    const mantissa Xxi2 = Xxi * Xxi;
    test2 = test1;
    test1 = Xxr2 + Xxi2;
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
    mantissa xrn, xin;
// continued...
