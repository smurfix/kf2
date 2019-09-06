/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2018 Claude Heiland-Allen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include "fraktal_sft.h"
#include "floatexp.h"
#include "complex.h"
#include "../formula/formula.h"

static int Perturbation_Var(int antal,const long double *dxr,const long double *dxi, long double Dr, long double Di, long double D0r, long double D0i,double &test1, double &test2, int m_nBailout2, int m_nMaxIter,const double *m_db_z,BOOL &bGlitch,int m_nPower,const int *m_pnExpConsts, const bool m_bNoGlitchDetection)
{
  long double yr, yi;
  bGlitch=FALSE;
  if(antal<m_nMaxIter && test1 <= m_nBailout2){
    for(;antal<m_nMaxIter;antal++){
      yr=dxr[antal]+Dr;
      yi=dxi[antal]+Di;
      test2=test1;
      test1 = g_real*yr*yr + g_imag*yi*yi;
      if(test1<m_db_z[antal]){
        bGlitch=TRUE;
        if (! m_bNoGlitchDetection)
          break;
      }
      complex<long double> y(yr, yi);
      complex<long double> X(dxr[antal],dxi[antal]);
      complex<long double> D(Dr,Di);
      complex<long double> D0(D0r,D0i);
      complex<long double> c(m_pnExpConsts[0],0);
      int nXExp=m_nPower-2, nDExp=2, ci=1;
      complex<long double> Dn = c*(X^(m_nPower-1))*D;
      while(nXExp){
        c.m_r = m_pnExpConsts[ci++];
        Dn += c*(X^nXExp)*(D^nDExp);
        nXExp--;
        nDExp++;
      }
      Di = Dn.m_i;
      Dr = Dn.m_r;
    }
  }
  return antal;
}

static int Perturbation_Var(int antal,const long double *dxr,const long double *dxi, long double Dr, long double Di, long double D0r, long double D0i,double &test1, double &test2, int m_nBailout2, int m_nMaxIter,const double *m_db_z,BOOL &bGlitch,int m_nPower,const int *m_pnExpConsts, long double &dr, long double &di, const bool m_bNoGlitchDetection)
{
  complex<long double> d(dr, di);
  long double yr, yi;
  bGlitch=FALSE;
  if(antal<m_nMaxIter && test1 <= m_nBailout2){
    for(;antal<m_nMaxIter;antal++){
      yr=dxr[antal]+Dr;
      yi=dxi[antal]+Di;
      test2=test1;
      test1 = g_real*yr*yr + g_imag*yi*yi;
      if(test1<m_db_z[antal]){
        bGlitch=TRUE;
        if (! m_bNoGlitchDetection)
          break;
      }
      complex<long double> y(yr, yi);
      d = m_nPower * d * (y ^ (m_nPower - 1)) + 1;
      complex<long double> X(dxr[antal],dxi[antal]);
      complex<long double> D(Dr,Di);
      complex<long double> D0(D0r,D0i);
      complex<long double> c(m_pnExpConsts[0],0);
      int nXExp=m_nPower-2, nDExp=2, ci=1;
      complex<long double> Dn = c*(X^(m_nPower-1))*D;
      while(nXExp){
        c.m_r = m_pnExpConsts[ci++];
        Dn += c*(X^nXExp)*(D^nDExp);
        nXExp--;
        nDExp++;
      }
      Di = Dn.m_i;
      Dr = Dn.m_r;
    }
  }
  dr = d.m_r;
  di = d.m_i;
  return antal;
}

void CFraktalSFT::MandelCalcLDBL()
{
  m_bIterChanged = TRUE;
  long double dr = 0, di = 0;
  floatexp ldr = 0, ldi = 0;
  int antal, x, y, w, h;

  int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);
  while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
    int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
    if (m_nPixels[x][y] != PIXEL_UNEVALUATED){
      SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y, w, h);
      continue;
    }
    if (GuessPixel(x, y, w, h))
      continue;

    // Series approximation
    floatexp D0r = 0;
    floatexp D0i = 0;
    floatexp daa0 = 1;
    floatexp dab0 = 0;
    floatexp dba0 = 0;
    floatexp dbb0 = 1;
    GetPixelCoordinates(x, y, D0r, D0i, daa0, dab0, dba0, dbb0);

    floatexp TDnr;
    floatexp TDni;
    floatexp dxa1, dxb1, dya1, dyb1;
    DoApproximation(antal, D0r, D0i, TDnr, TDni, dxa1, dxb1, dya1, dyb1);
    floatexp TDDnr = dxa1;
    floatexp TDDni = dya1;

    double test1 = 0, test2 = 0;
    BOOL bGlitch = FALSE;
    {

      long double dbD0r = D0r.toLongDouble();
      long double dbD0i = D0i.toLongDouble();
      long double Dr = TDnr.toLongDouble();
      long double Di = TDni.toLongDouble();
      dr = TDDnr.toLongDouble();
      di = TDDni.toLongDouble();

      if (m_nFractalType == 0 && m_nPower > 10) // FIXME matrix derivatives
      { // FIXME check this is still ok around long double vs scaled double zoom threshold e600
        antal = GetDerivatives()
          ? Perturbation_Var(antal, m_ldxr, m_ldxi, Dr, Di, dbD0r, dbD0i, test1, test2, m_nBailout2, nMaxIter, m_db_z, bGlitch, m_nPower, m_pnExpConsts, dr, di, m_bNoGlitchDetection)
          : Perturbation_Var(antal, m_ldxr, m_ldxi, Dr, Di, dbD0r, dbD0i, test1, test2, m_nBailout2, nMaxIter, m_db_z, bGlitch, m_nPower, m_pnExpConsts, m_bNoGlitchDetection)
          ;
        long double pixel_spacing = m_lPixelSpacing;
        dr *= pixel_spacing;
        di *= pixel_spacing;
      }
      else if (m_nScalingOffsetL)
      {
	Dr = TDnr.toLongDouble(m_nScalingOffsetL);
	Di = TDni.toLongDouble(m_nScalingOffsetL);
	dbD0r = D0r.toLongDouble(m_nScalingOffsetL);
	dbD0i = D0i.toLongDouble(m_nScalingOffsetL);
	ldr = TDDnr;
	ldi = TDDni;
	ldr *= m_fPixelSpacing;
	ldi *= m_fPixelSpacing;
	bool ok = GetDerivatives()
	  ? perturbation(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, ldr, ldi, (floatexp)(m_epsilon), m_fPixelSpacing, daa0, dab0, dba0, dbb0, m_nScalingL, 1 / m_nScalingL)
	  : perturbation(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, m_nScalingL, 1 / m_nScalingL)
	  ;
	assert(ok && "perturbation_long_double_scaled");
      }
      else
      {
	long double daa = daa0.toLongDouble();
	long double dab = dab0.toLongDouble();
	long double dba = dba0.toLongDouble();
	long double dbb = dbb0.toLongDouble();
	dr *= m_lPixelSpacing;
	di *= m_lPixelSpacing;
	bool ok = GetDerivatives()
	  ? perturbation(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, dr, di, (long double)(m_epsilon), m_lPixelSpacing, daa, dab, dba, dbb)
	  : perturbation(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i)
	  ;
	assert(ok && "perturbation_long_double");
      }
    }
    long double pixel_spacing = m_lPixelSpacing;
    if (m_nScalingOffsetL)
    {
      // nop
    }
    else if (m_nPower > 10)
    {
      ldr = dr * pixel_spacing;
      ldi = di * pixel_spacing;
    }
    else
    {
      ldr = dr;
      ldi = di;
    }
    double de = GetDerivatives()
      ? sqrt(test1) * log(test1) / double(sqrt(ldr * ldr + ldi * ldi))
      : 0
      ;
    OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, de);
    InterlockedIncrement((LPLONG)&m_nDone);
    OutputPixelData(x, y, w, h, bGlitch);
  }
}
