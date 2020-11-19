void perturbation_double_loop
( __global const p_config   *g
, __global const double     *m_db_dxr
, __global const double     *m_db_dxi
, __global const double     *m_db_z
,                p_status_d *l
)
{
  const double Ar = g->g_FactorAR;
  const double Ai = g->g_FactorAI;
  bool no_g = g->g_real == 1.0 && g->g_imag == 1.0 && g->norm_p == 2.0;
  // type C formulas
  const dcomplex c = {l->cr, l->ci};
  dcomplex x = {l->xr, l->xi};
  dcomplex d = {l->dxa, l->dya};
  const dcomplex d0 = {l->daa, l->dba};
  // conditions
  double test1 = l->test1;
  double test2 = l->test2;
  long antal = l->antal;
  double Xxr = 0;
  double Xxi = 0;
  for (; antal < g->nMaxIter; antal++)
  {
    const double Xr = m_db_dxr[antal - g->nMinIter];
    const double Xi = m_db_dxi[antal - g->nMinIter];
    const double Xz = m_db_z  [antal - g->nMinIter];
    Xxr = Xr + x.re;
    Xxi = Xi + x.im;
    const double Xxr2 = Xxr * Xxr;
    const double Xxi2 = Xxi * Xxi;
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
    const dcomplex X = {Xr, Xi}, Xx = {Xxr, Xxi};
    dcomplex xn, dn;
// continued...
