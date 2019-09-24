void perturbation_floatexp_loop
( __global const p_config    *g
, __global const floatexp    *m_db_dxr
, __global const floatexp    *m_db_dxi
, __global const double      *m_db_z
,                p_status_fe *l
)
{
  const floatexp zero = fe_floatexp(0.0, 0);
  const double Ar = g->g_FactorAR;
  const double Ai = g->g_FactorAI;
  bool no_g = g->g_real == 1.0 && g->g_imag == 1.0;
  const floatexp cr = l->cr;
  const floatexp ci = l->ci;
  floatexp xr = l->xr;
  floatexp xi = l->xi;
  // type C derivatives
  floatexp dr = l->dr;
  floatexp di = l->di;
  // type M derivatives
  floatexp daa = l->daa;
  floatexp dab = l->dab;
  floatexp dba = l->dba;
  floatexp dbb = l->dbb;
  floatexp dxa = dr;
  floatexp dxb = fe_neg(di);
  floatexp dya = di;
  floatexp dyb = dr;
  // conditions
  double test1 = l->test1;
  double test2 = l->test2;
  long antal = l->antal;
  floatexp Xxr = zero;
  floatexp Xxi = zero;
  for (; antal < g->nMaxIter; antal++)
  {
    const floatexp Xr = m_db_dxr[antal - g->nMinIter];
    const floatexp Xi = m_db_dxi[antal - g->nMinIter];
    const double   Xz = m_db_z  [antal - g->nMinIter];
    Xxr = fe_add(Xr, xr);
    Xxi = fe_add(Xi, xi);
    test2 = test1;
    if (no_g)
    {
      test1 = fe_double(fe_add(fe_sqr(Xxr), fe_sqr(Xxi)));
    }
    else
    {
      test1 = fe_double(fe_add(fe_dmul(g->g_real, fe_sqr(Xxr)), fe_dmul(g->g_imag, fe_sqr(Xxi))));
    }
    if (test1 < Xz)
    {
      l->bGlitch = true;
      if (! g->m_bNoGlitchDetection)
        break;
    }
    if (test1 > g->m_nBailout2)
    {
      break;
    }
    const fecomplex X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi};
    floatexp xrn = zero;
    floatexp xin = zero;
    fecomplex xn = {zero, zero};
    const fecomplex d = {dr, di}, d0 = {daa, dba}; // FIXME matrix derivatives
    floatexp drn = zero, din = zero;
    fecomplex dn = {zero, zero};
    floatexp dxan = zero, dxbn = zero, dyan = zero, dybn = zero;
// continued...
