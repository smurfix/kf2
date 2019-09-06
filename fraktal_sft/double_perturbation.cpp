/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2019 Claude Heiland-Allen

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
#include "complex.h"
#include "../formula/formula.h"

void CFraktalSFT::MandelCalc()
{
  m_bIterChanged = TRUE;
  double yr, yi, dr, di;
  long double ldr = 0, ldi = 0;
  double epsilon(m_epsilon);
  int antal, x, y, w, h;

  // vectorization
  double16 Dr16, Di16, dbD0r16, dbD0i16, test116, test216;
  int16 antal16, bGlitch16, x16, y16, w16, h16;
  int k = 0;
  const int chunksize = GetSIMDChunkSize();
  const int vectorsize = GetSIMDVectorSize();
  const bool vectorized = ! GetDerivatives() && (m_nFractalType == 0 ? ! (m_nPower > 10) : true) && vectorsize > 1;

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

    double dbD0r = D0r.todouble();
    double dbD0i = D0i.todouble();
    double Dr = TDnr.todouble();
    double Di = TDni.todouble();
    dr = TDDnr.todouble();
    di = TDDni.todouble();

    if (m_nFractalType == 0 && m_nPower > 10)
    {
      if (GetDerivatives())
      {
	complex<double> d(dr, di);
	bool no_g = g_real == 1.0 && g_imag == 1.0;
	if (antal<nMaxIter && test1 <= m_nBailout2){
	  for (; antal<nMaxIter; antal++){
	    yr = m_db_dxr[antal] + Dr;
	    yi = m_db_dxi[antal] + Di;
	    test2 = test1;
	    if (no_g)
	      test1 = yr*yr + yi*yi;
	    else
	      test1 = g_real*yr*yr + g_imag*yi*yi;
	    if (test1<m_db_z[antal]){
	      bGlitch = TRUE;
	      if (! m_bNoGlitchDetection)
		break;
	    }
	    if (test1 > m_nBailout2)
	    {
	      break;
	    }
	    complex<double> yri(yr, yi);
	    d = m_nPower * d * (yri ^ (m_nPower - 1)) + 1;
	    complex<double> X(m_db_dxr[antal], m_db_dxi[antal]);
	    complex<double> D(Dr, Di);
	    complex<double> D0(dbD0r, dbD0i);
	    complex<double> c(m_pnExpConsts[0], 0);
	    int nXExp = m_nPower - 2, nDExp = 2, ci = 1;
	    complex<double> Dn = c*(X^(m_nPower - 1))*D;
	    while (nXExp){
	      c.m_r = m_pnExpConsts[ci++];
	      Dn += c*(X^nXExp)*(D^nDExp);
	      nXExp--;
	      nDExp++;
	    }
	    Dn += (D^m_nPower) + D0;
	    Di = Dn.m_i;
	    Dr = Dn.m_r;
	  }
	}
	dr = d.m_r;
	di = d.m_i;
      }
      else
      {
	if (antal<nMaxIter && test1 <= m_nBailout2){
	  bool no_g = g_real == 1.0 && g_imag == 1.0;
	  for (; antal<nMaxIter; antal++){
	    yr = m_db_dxr[antal] + Dr;
	    yi = m_db_dxi[antal] + Di;
	    test2 = test1;
	    if (no_g)
	      test1 = yr*yr + yi*yi;
	    else
	      test1 = g_real*yr*yr + g_imag*yi*yi;
	    if (test1<m_db_z[antal]){
	      bGlitch = TRUE;
	      if (! m_bNoGlitchDetection)
		break;
	    }
	    if (test1 > m_nBailout2)
	    {
	      break;
	    }
	    complex<double> yri(yr, yi);
	    complex<double> X(m_db_dxr[antal], m_db_dxi[antal]);
	    complex<double> D(Dr, Di);
	    complex<double> D0(dbD0r, dbD0i);
	    complex<double> c(m_pnExpConsts[0], 0);
	    int nXExp = m_nPower - 2, nDExp = 2, ci = 1;
	    complex<double> Dn = c*(X^(m_nPower - 1))*D;
	    while (nXExp){
	      c.m_r = m_pnExpConsts[ci++];
	      Dn += c*(X^nXExp)*(D^nDExp);
	      nXExp--;
	      nDExp++;
	    }
	    Dn += (D^m_nPower) + D0;
	    Di = Dn.m_i;
	    Dr = Dn.m_r;
	  }
	}
      }
      ldr = dr * m_lPixelSpacing;
      ldi = di * m_lPixelSpacing;

    }
    else
    {
      if (vectorized)
      {
	x16[k] = x;
	y16[k] = y;
	w16[k] = w;
	h16[k] = h;
	Dr16[k] = m_nScalingOffset ? TDnr.todouble(m_nScalingOffset) : Dr;
	Di16[k] = m_nScalingOffset ? TDni.todouble(m_nScalingOffset) : Di;
	dbD0r16[k] = m_nScalingOffset ? D0r.todouble(m_nScalingOffset) : dbD0r;
	dbD0i16[k] = m_nScalingOffset ? D0i.todouble(m_nScalingOffset) : dbD0i;
	antal16[k] = antal;
	test116[k] = test1;
	test216[k] = test2;
	bGlitch16[k] = bGlitch;
	k = k + 1;
	if (k == vectorsize)
	{
#define GO \
	    for (int q = 0; q < vectorsize; ++q) \
	    { \
	      antalv[q] = antal16[q]; \
	      bGlitchv[q] = bGlitch16[q]; \
	      test1v[q] = test116[q]; \
	      test2v[q] = test216[q]; \
	      Drv[q] = Dr16[q]; \
	      Div[q] = Di16[q]; \
	      dbD0rv[q] = dbD0r16[q]; \
	      dbD0iv[q] = dbD0i16[q]; \
	    } \
	    bool ok = m_nScalingOffset \
	      ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize, m_nScaling, 1 / m_nScaling) \
	      : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize) \
	      ; \
	    for (int q = 0; q < vectorsize; ++q) \
	    { \
	      antal16[q] = antalv[q]; \
	      bGlitch16[q] = bGlitchv[q]; \
	      test116[q] = test1v[q]; \
	      test216[q] = test2v[q]; \
	      Dr16[q] = Drv[q]; \
	      Di16[q] = Div[q]; \
	      dbD0r16[q] = dbD0rv[q]; \
	      dbD0i16[q] = dbD0iv[q]; \
	    }
	  if (vectorsize == 2)
	  {
	    int2 antalv, bGlitchv;
	    double2 test1v, test2v, Drv, Div, dbD0rv, dbD0iv;
	    GO
	    assert(ok && "perturbation_double2");
	  }
	  else if (vectorsize == 4)
	  {
	    int4 antalv, bGlitchv;
	    double4 test1v, test2v, Drv, Div, dbD0rv, dbD0iv;
	    GO
	    assert(ok && "perturbation_double4");
	  }
	  else if (vectorsize == 8)
	  {
	    int8 antalv, bGlitchv;
	    double8 test1v, test2v, Drv, Div, dbD0rv, dbD0iv;
	    GO
	    assert(ok && "perturbation_double8");
	  }
	  else if (vectorsize == 16)
	  {
	    int16 antalv, bGlitchv;
	    double16 test1v, test2v, Drv, Div, dbD0rv, dbD0iv;
	    GO
	    assert(ok && "perturbation_double16");
	  }
	  else
	  {
	    assert(! "valid vectorsize");
	  }
#undef GO
	}
      }
      else
      {
	if (m_nScalingOffset)
	{
	  Dr = TDnr.todouble(m_nScalingOffset);
	  Di = TDni.todouble(m_nScalingOffset);
	  dbD0r = D0r.todouble(m_nScalingOffset);
	  dbD0i = D0i.todouble(m_nScalingOffset);
	  ldr = TDDnr.toLongDouble();
	  ldi = TDDni.toLongDouble();
	  long double daa = daa0.toLongDouble();
	  long double dab = dab0.toLongDouble();
	  long double dba = dba0.toLongDouble();
	  long double dbb = dbb0.toLongDouble();
	  ldr *= m_lPixelSpacing;
	  ldi *= m_lPixelSpacing;
	  bool ok = GetDerivatives()
	    ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, ldr, ldi, (long double)(m_epsilon), m_lPixelSpacing, daa, dab, dba, dbb, m_nScaling, 1 / m_nScaling)
	    : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, m_nScaling, 1 / m_nScaling)
	    ;
	  assert(ok && "perturbation_double_scaled");
	}
	else
	{
	  double daa = daa0.todouble();
	  double dab = dab0.todouble();
	  double dba = dba0.todouble();
	  double dbb = dbb0.todouble();
	  dr *= m_dPixelSpacing;
	  di *= m_dPixelSpacing;
	  bool ok = GetDerivatives()
	    ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, dr, di, epsilon, m_dPixelSpacing, daa, dab, dba, dbb)
	    : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i)
	    ;
	  assert(ok && "perturbation_double");
	  ldr = dr;
	  ldi = di;
	}
      }
    }

    if (vectorized)
    {
      if (k == vectorsize)
      {
        for (k = 0; k < vectorsize; ++k)
        {
          OutputIterationData(x16[k], y16[k], w16[k], h16[k], bGlitch16[k], antal16[k], test116[k], test216[k], 0);
          InterlockedIncrement((LPLONG)&m_nDone);
          OutputPixelData(x16[k], y16[k], w16[k], h16[k], bGlitch16[k]);
        }
        k = 0;
      }
    }
    else
    {
      double de = GetDerivatives()
	? sqrt(test1) * log(test1) / sqrt(ldr * ldr + ldi * ldi)
	: 0
	;
      OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, de);
      InterlockedIncrement((LPLONG)&m_nDone);
      OutputPixelData(x, y, w, h, bGlitch);
    }
  }
  if (vectorized)
  {
    int leftover = k;
    for (k = 0; k < leftover; ++k)
    {
      x = x16[k];
      y = y16[k];
      w = w16[k];
      h = h16[k];
      antal = antal16[k];
      double test1 = test116[k];
      double test2 = test216[k];
      int bGlitch = bGlitch16[k];
      double Dr = Dr16[k];
      double Di = Di16[k];
      double dbD0r = dbD0r16[k];
      double dbD0i = dbD0i16[k];
      bool ok = m_nScalingOffset
        ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, m_nScaling, 1 / m_nScaling)
        : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i)
        ;
      assert(ok && "perturbation_double");
      OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, 0);
      InterlockedIncrement((LPLONG)&m_nDone);
      OutputPixelData(x, y, w, h, bGlitch);
    }
  }
}
