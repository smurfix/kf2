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
  // type R formulas
  const double cr = l->cr;
  const double ci = l->ci;
  double xr = l->xr;
  double xi = l->xi;
  double dr = l->dxa;
  double di = l->dya;
  const double dr0 = l->daa;
  const double di0 = l->dba;
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
    Xxr = Xr + xr;
    Xxi = Xi + xi;
    const double Xxr2 = Xxr * Xxr;
    const double Xxi2 = Xxi * Xxi;
    test2 = test1;
    test1 = Xxr2 + Xxi2;
    if (test1 < Xz)
    {
      l->bGlitch = true;
      if (! g->m_bNoGlitchDetection)
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
    double xrn, xin, drn, din;
// continued...
