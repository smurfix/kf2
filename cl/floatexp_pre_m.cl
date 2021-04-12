void perturbation_floatexp_loop
( __global const p_config    *g
, __global const double      *m_refx
, __global const double      *m_refy
, __global const double      *m_refz
, __global const long        *m_refN
, __global const floatexp    *m_refX
, __global const floatexp    *m_refY
, __global const floatexp    *m_refZ
,                p_status_fe *l
)
{
  const floatexp zero = fe_floatexp(0.0, 0);
  const floatexp one = fe_floatexp(1.0, 0);
  const double Ar = g->g_FactorAR;
  const double Ai = g->g_FactorAI;
  bool no_g = g->g_real == 1.0 && g->g_imag == 1.0 && g->norm_p == 2.0;
  const floatexp cr = l->cr;
  const floatexp ci = l->ci;
  floatexp xr = l->xr;
  floatexp xi = l->xi;
  // type M derivatives
  floatexp daa = l->daa;
  floatexp dab = l->dab;
  floatexp dba = l->dba;
  floatexp dbb = l->dbb;
  floatexp dxa = l->dxa;
  floatexp dxb = l->dxb;
  floatexp dya = l->dya;
  floatexp dyb = l->dyb;
  // hybrids
  int count = 0;
  int stanza = 0;
  // conditions
  double test1 = l->test1;
  double test2 = l->test2;
  long antal = l->antal;
  floatexp Xxr = zero;
  floatexp Xxi = zero;

  long k = 0, n = 0;
  floatexp Xrf, Xif, Xzf;
  do
  {
    if (k < g->m_nRSize)
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
  while (n < antal);
  for (; antal < g->nMaxIter; antal++)
  {
    floatexp Xr, Xi, Xz;
    if (antal < n)
    {
      Xr = fe_floatexp(m_refx[antal - g->nMinIter], 0);
      Xi = fe_floatexp(m_refy[antal - g->nMinIter], 0);
      Xz = fe_floatexp(m_refz[antal - g->nMinIter], 0);
    }
    else
    {
      Xr = Xrf;
      Xi = Xif;
      Xz = Xzf;
      if (k < g->m_nRSize)
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
    floatexp xrn, xin, dxan, dxbn, dyan, dybn;
// continued...
