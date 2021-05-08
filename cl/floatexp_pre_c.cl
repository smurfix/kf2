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
  const double Ar = g->g_FactorAR;
  const double Ai = g->g_FactorAI;
  bool no_g = g->g_real == 1.0 && g->g_imag == 1.0 && g->norm_p == 2.0;
  const fecomplex c = {l->cr, l->ci};
  fecomplex x = {l->xr, l->xi};
  // type C derivatives
  fecomplex d = {l->dxa, l->dya};
  const fecomplex d0 = {l->daa, l->dba};
  // conditions
  double test1 = l->test1;
  double test2 = l->test2;
  long antal = l->antal;
  floatexp Xxr = zero;
  floatexp Xxi = zero;
  for (; antal < g->nMaxIter; antal++)
  {
    const floatexp Xr = fe_floatexp(m_refx[antal - g->nMinIter], 0);
    const floatexp Xi = fe_floatexp(m_refy[antal - g->nMinIter], 0);
    const floatexp Xz = fe_floatexp(m_refz[antal - g->nMinIter], 0);
    Xxr = fe_add(Xr, x.re);
    Xxi = fe_add(Xi, x.im);
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
    const fecomplex X = {Xr, Xi}, Xx = {Xxr, Xxi};
    fecomplex xn, dn;
// continued...
