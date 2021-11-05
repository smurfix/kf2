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
  const floatexp Ar = fe_floatexp(g->g_FactorAR, 0);
  const floatexp Ai = fe_floatexp(g->g_FactorAI, 0);
  const fecomplex A = { Ar, Ai };
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
  long rantal = antal;
  floatexp Xxr = zero;
  floatexp Xxi = zero;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
  const mantissa Cr = fe_double(m_refx[0]); // FIXME
  const mantissa Ci = fe_double(m_refy[0]); // FIXME
  const mantissa Ccr = Cr + fe_double(cr);
  const mantissa Cci = Ci + fe_double(ci);
  const mantissa abs_c2 = Ccr * Ccr + Cci * Cci;
  const mantissa abs_c = sqrt(abs_c2);
  floatexp tia_sum_old = fe_floatexp(0, 0);
  floatexp tia_sum = fe_floatexp(0, 0);
  long tia_count = 0;
#endif
  for (; antal < g->nMaxIter && rantal < g->reference_size_x; antal++)
  {
    floatexp Xr = m_refx[rantal];
    floatexp Xi = m_refy[rantal];
    floatexp Xz = m_refz[rantal];
    rantal++;
    Xxr = fe_add(Xr, x.re);
    Xxi = fe_add(Xi, x.im);
    floatexp Xxr2 = fe_sqr(Xxr);
    floatexp Xxi2 = fe_sqr(Xxi);
    test2 = test1;
    floatexp ttest1 = fe_add(Xxr2, Xxi2);
    test1 = fe_double(ttest1);
    if (g->singleref)
    {
      if (! no_g)
      {
        test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr), fe_double(Xxi));
      }
      if (test1 > g->m_nBailout2)
      {
        break;
      }
      if (fe_lt(ttest1, fe_add(fe_sqr(xr), fe_sqr(xi))) || rantal == g->reference_size_x)
      {
        x.re = Xxr;
        x.im = Xxi;
        rantal = 0;
        Xr = zero;
        Xi = zero;
        Xz = zero;
        Xxr = fe_add(Xr, x.re);
        Xxi = fe_add(Xi, x.im);
        Xxr2 = fe_sqr(Xxr);
        Xxi2 = fe_sqr(Xxi);
        ttest1 = fe_add(Xxr2, Xxi2);
        test1 = fe_double(ttest1);
        if (! no_g)
        {
          test1 = pnorm(g->g_real, g->g_imag, g->norm_p, fe_double(Xxr), fe_double(Xxi));
        }
      }
    }
    else
    {
      if (fe_lt(ttest1, Xz))
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
    }
    const fecomplex X = {Xr, Xi}, Xx = {Xxr, Xxi};
    fecomplex xn, dn;
// continued...
