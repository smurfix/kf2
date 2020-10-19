void perturbation_softfloat_loop
( __global const p_config    *g
, __global const softfloat   *m_db_dxr
, __global const softfloat   *m_db_dxi
, __global const softfloat   *m_db_z
,                p_status_sf *l
)
{
  const softfloat zero = sf_zero();
  const softfloat Ar = sf_from_double(g->g_FactorAR);
  const softfloat Ai = sf_from_double(g->g_FactorAI);
  bool no_g = g->g_real == 1.0 && g->g_imag == 1.0;
  const softfloat cr = l->cr;
  const softfloat ci = l->ci;
  softfloat xr = l->xr;
  softfloat xi = l->xi;
  // type C derivatives
  softfloat dr = l->dxa;
  softfloat di = l->dya;
  // type M derivatives
  softfloat daa = l->daa;
  softfloat dab = l->dab;
  softfloat dba = l->dba;
  softfloat dbb = l->dbb;
  softfloat dxa = l->dxa;
  softfloat dxb = l->dxb;
  softfloat dya = l->dya;
  softfloat dyb = l->dyb;
  softfloat g_real = sf_from_double(g->g_real);
  softfloat g_imag = sf_from_double(g->g_imag);
  softfloat m_nBailout2 = sf_from_double(g->m_nBailout2);
  // conditions
  softfloat test1 = l->test1;
  softfloat test2 = l->test2;
  long antal = l->antal;
  softfloat Xxr = zero;
  softfloat Xxi = zero;
  for (; antal < g->nMaxIter; antal++)
  {
    const softfloat Xr = m_db_dxr[antal - g->nMinIter];
    const softfloat Xi = m_db_dxi[antal - g->nMinIter];
    const softfloat Xz = m_db_z  [antal - g->nMinIter];
    Xxr = sf_add(Xr, xr);
    Xxi = sf_add(Xi, xi);
    test2 = test1;
    if (no_g)
    {
      test1 = sf_add(sf_sqr(Xxr), sf_sqr(Xxi));
    }
    else
    {
      test1 = sf_add(sf_mul(g_real, sf_sqr(Xxr)), sf_mul(g_imag, sf_sqr(Xxi)));
    }
    if (sf_lt(test1, Xz))
    {
      l->bGlitch = true;
      if (! g->m_bNoGlitchDetection)
        break;
    }
    if (sf_gt(test1, m_nBailout2))
    {
      break;
    }
    const sfcomplex X = {Xr, Xi}, x = {xr, xi}, Xx = {Xxr, Xxi};
    softfloat xrn = zero;
    softfloat xin = zero;
    sfcomplex xn = {zero, zero};
    const sfcomplex d = {dr, di}, d0 = {daa, dba}; // FIXME matrix derivatives
    softfloat drn = zero, din = zero;
    sfcomplex dn = {zero, zero};
    softfloat dxan = zero, dxbn = zero, dyan = zero, dybn = zero;
// continued...
