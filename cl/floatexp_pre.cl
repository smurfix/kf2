void perturbation_floatexp_loop
( __global const p_config    *g
, __global const floatexp    *m_refx
, __global const floatexp    *m_refy
, __global const floatexp    *m_refz
,                p_status_fe *l
)
{
  const floatexp zero = fe_floatexp(0.0, 0);
  const floatexp one = fe_floatexp(1.0, 0);
  const mantissa Ar = g->g_FactorAR;
  const mantissa Ai = g->g_FactorAI;
  const fecomplex A = { fe_floatexp(Ar, 0), fe_floatexp(Ai, 0) };
  bool no_g = g->g_real == 1.0 && g->g_imag == 1.0 && g->norm_p == 2.0;
  const floatexp cr = l->cr;
  const floatexp ci = l->ci;
  floatexp xr = l->xr;
  floatexp xi = l->xi;
  // hybrids
  int count = 0;
  int stanza = 0;
  // conditions
  mantissa test1 = l->test1;
  mantissa test2 = l->test2;
  long antal = l->antal;
  floatexp Xxr = zero;
  floatexp Xxi = zero;

  for (; antal < g->nMaxIter; antal++)
  {
    const floatexp Xr = m_refx[antal - g->nMinIter];
    const floatexp Xi = m_refy[antal - g->nMinIter];
    const floatexp Xz = m_refz[antal - g->nMinIter];
    Xxr = fe_add(Xr, xr);
    Xxi = fe_add(Xi, xi);
    const floatexp Xxr2 = fe_sqr(Xxr);
    const floatexp Xxi2 = fe_sqr(Xxi);
    test2 = test1;
    const floatexp ftest1 = fe_add(Xxr2, Xxi2);
    test1 = fe_double(ftest1);
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
    if (test1 > g->m_nBailout2)
    {
      break;
    }
    floatexp xrn, xin;
// continued...
