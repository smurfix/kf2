/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2020 Claude Heiland-Allen

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
  double yr = 0, yi = 0, dr, di;
  long double ldr = 0, ldi = 0;
  double epsilon(m_epsilon);
  int x, y, w, h;
  int64_t antal;
  const double nBailout = GetBailoutRadius();
  const double p = GetBailoutNorm();
  const double nBailout2 = p < 1.0/0.0 ? pow(nBailout, p) : nBailout;

  // vectorization
  double16 Dr16, Di16, dbD0r16, dbD0i16, test116, test216, phase16, dr16, di16, daa16, dab16, dba16, dbb16;
  int16 antal16, bGlitch16, bNoGlitchDetection16, x16, y16, w16, h16;
  int k = 0;
  const int64_t chunksize = GetSIMDChunkSize();
  const int vectorsize = GetSIMDVectorSize();
  const bool derivatives = GetDerivatives();
  const bool vectorized = (derivatives ? ! m_nScalingOffset : true) && (m_nFractalType == 0 ? ! (m_nPower > 10) : true) && vectorsize > 1;

  int64_t nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);
  while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
    int64_t nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
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

    double test1 = 0, test2 = 0, phase = 0;
    bool bNoGlitchDetection = m_bNoGlitchDetection || (x == g_nAddRefX && y == g_nAddRefY);
    bool bGlitch = false;

    double dbD0r = D0r.todouble();
    double dbD0i = D0i.todouble();
    double Dr = TDnr.todouble();
    double Di = TDni.todouble();
    dr = TDDnr.todouble();
    di = TDDni.todouble();

    if (m_nFractalType == 0 && m_nPower > 10)
    {
      if (derivatives)
      {
        complex<double> d(dr, di);
        bool no_g = g_real == 1.0 && g_imag == 1.0 && p == 2.0;
        if (antal<nMaxIter && test1 <= nBailout2){
          for (; antal<nMaxIter; antal++){
            yr = m_db_dxr[antal] + Dr;
            yi = m_db_dxi[antal] + Di;
            test2 = test1;
            test1 = yr*yr + yi*yi;
            if (test1<m_db_z[antal]){
              bGlitch = TRUE;
              if (! bNoGlitchDetection)
                break;
            }
            if (! no_g)
            {
              test1 = pnorm(g_real, g_imag, p, yr, yi);
            }
            if (test1 > nBailout2)
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
        if (! (test1 <= nBailout2))
        {
          phase = atan2(yi, yr) / M_PI / 2;
          phase -= floor(phase);
        }
        dr = d.m_r;
        di = d.m_i;
      }
      else
      {
        if (antal<nMaxIter && test1 <= nBailout2){
          bool no_g = g_real == 1.0 && g_imag == 1.0 && p == 2.0;
          for (; antal<nMaxIter; antal++){
            yr = m_db_dxr[antal] + Dr;
            yi = m_db_dxi[antal] + Di;
            test2 = test1;
            test1 = yr*yr + yi*yi;
            if (test1<m_db_z[antal]){
              bGlitch = TRUE;
              if (! bNoGlitchDetection)
                break;
            }
            if (! no_g)
            {
              test1 = pnorm(g_real, g_imag, p, yr, yi);
            }
            if (test1 > nBailout2)
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
        if (! (test1 <= nBailout2))
        {
          phase = atan2(yi, yr) / M_PI / 2;
          phase -= floor(phase);
        }
      }
      dr *= m_lPixelSpacing;
      di *= m_lPixelSpacing;

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
        phase16[k] = phase;
        bGlitch16[k] = bGlitch;
        bNoGlitchDetection16[k] = bNoGlitchDetection;
        daa16[k] = daa0.todouble();
        dab16[k] = dab0.todouble();
        dba16[k] = dba0.todouble();
        dbb16[k] = dbb0.todouble();
        dr16[k] = dr * m_dPixelSpacing;
        di16[k] = di * m_dPixelSpacing;
        k = k + 1;
        if (k == vectorsize)
        {
          bool ok = false;
#define GO(intN,doubleN) \
            intN antalv, bGlitchv, bNoGlitchDetectionv; \
            doubleN test1v, test2v, phasev, Drv, Div, dbD0rv, dbD0iv, drv, div, daav, dabv, dbav, dbbv; \
            for (int q = 0; q < vectorsize; ++q) \
            { \
              antalv[q] = antal16[q]; \
              bGlitchv[q] = bGlitch16[q]; \
              test1v[q] = test116[q]; \
              test2v[q] = test216[q]; \
              phasev[q] = phase16[q]; \
              Drv[q] = Dr16[q]; \
              Div[q] = Di16[q]; \
              dbD0rv[q] = dbD0r16[q]; \
              dbD0iv[q] = dbD0i16[q]; \
              if (derivatives) \
              { \
                drv[q] = dr16[q]; \
                div[q] = di16[q]; \
                daav[q] = daa16[q]; \
                dabv[q] = dab16[q]; \
                dbav[q] = dba16[q]; \
                dbbv[q] = dbb16[q]; \
              } \
            } \
            ok = m_nScalingOffset \
              ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, phasev, bGlitchv, nBailout2, nMaxIter, bNoGlitchDetectionv, g_real, g_imag, p, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize, m_nScaling, 1 / m_nScaling) \
              : derivatives \
              ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, phasev, bGlitchv, nBailout2, nMaxIter, bNoGlitchDetectionv, g_real, g_imag, p, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, drv, div, epsilon, m_dPixelSpacing, daav, dabv, dbav, dbbv, chunksize) \
              : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, phasev, bGlitchv, nBailout2, nMaxIter, bNoGlitchDetectionv, g_real, g_imag, p, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize) \
              ; \
            for (int q = 0; q < vectorsize; ++q) \
            { \
              antal16[q] = antalv[q]; \
              bGlitch16[q] = bGlitchv[q]; \
              bNoGlitchDetection16[q] = bNoGlitchDetectionv[q]; \
              test116[q] = test1v[q]; \
              test216[q] = test2v[q]; \
              phase16[q] = phasev[q]; \
              Dr16[q] = Drv[q]; \
              Di16[q] = Div[q]; \
              dbD0r16[q] = dbD0rv[q]; \
              dbD0i16[q] = dbD0iv[q]; \
              if (derivatives) \
              { \
                dr16[q] = drv[q]; \
                di16[q] = div[q]; \
                daa16[q] = daav[q]; \
                dab16[q] = dabv[q]; \
                dba16[q] = dbav[q]; \
                dbb16[q] = dbbv[q]; \
              } \
            }
#if KF_SIMD >= 1
          if (vectorsize == 2)
          {
            GO(int2, double2)
            assert(ok && "perturbation_double2");
          }
#endif
#if KF_SIMD >= 2
          if (vectorsize == 4)
          {
            GO(int4, double4)
            assert(ok && "perturbation_double4");
          }
#endif
#if KF_SIMD >= 3
          if (vectorsize == 8)
          {
            GO(int8, double8)
            assert(ok && "perturbation_double8");
          }
#endif
#if KF_SIMD >= 4
          if (vectorsize == 16)
          {
            GO(int16, double16)
            assert(ok && "perturbation_double16");
          }
#endif
#undef GO
          assert(ok && "valid vectorsize");
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
          bool ok = derivatives
            ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, ldr, ldi, (long double)(m_epsilon), m_lPixelSpacing, daa, dab, dba, dbb, m_nScaling, 1 / m_nScaling)
            : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, m_nScaling, 1 / m_nScaling)
            ;
          assert(ok && "perturbation_double_scaled");
	  dr = ldr;
	  di = ldi;
        }
        else
        {
          double daa = daa0.todouble();
          double dab = dab0.todouble();
          double dba = dba0.todouble();
          double dbb = dbb0.todouble();
          dr *= m_dPixelSpacing;
          di *= m_dPixelSpacing;
          bool ok = derivatives
            ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, dr, di, epsilon, m_dPixelSpacing, daa, dab, dba, dbb)
            : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i)
            ;
          assert(ok && "perturbation_double");
        }
      }
    }

    if (vectorized)
    {
      if (k == vectorsize)
      {
        for (k = 0; k < vectorsize; ++k)
        {
	  complex<double> z(Dr16[k], Di16[k]);
	  complex<double> dc(dr16[k], di16[k]);
	  complex<double> de = derivatives ? abs(z) * log(abs(z)) / dc : 0;
          OutputIterationData(x16[k], y16[k], w16[k], h16[k], bGlitch16[k], antal16[k], test116[k], test216[k], phase16[k], nBailout, de);
          InterlockedIncrement((LPLONG)&m_nDone);
          OutputPixelData(x16[k], y16[k], w16[k], h16[k], bGlitch16[k]);
        }
        k = 0;
      }
    }
    else
    {
      complex<double> z(Dr, Di);
      complex<double> dc(dr, di);
      complex<double> de = derivatives ? abs(z) * log(abs(z)) / dc : 0;
      OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, phase, nBailout, de);
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
      double phase = phase16[k];
      bool bGlitch = bGlitch16[k];
      bool bNoGlitchDetection = bNoGlitchDetection16[k];
      double Dr = Dr16[k];
      double Di = Di16[k];
      double dbD0r = dbD0r16[k];
      double dbD0i = dbD0i16[k];
      double dr = dr16[k];
      double di = di16[k];
      double daa = daa16[k];
      double dab = dab16[k];
      double dba = dba16[k];
      double dbb = dbb16[k];
      bool ok = m_nScalingOffset
        ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, m_nScaling, 1 / m_nScaling)
        : derivatives
        ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, dr, di, epsilon, m_dPixelSpacing, daa, dab, dba, dbb)
        : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i)
        ;
      assert(ok && "perturbation_double");
      complex<double> z(Dr16[k], Di16[k]);
      complex<double> dc(dr16[k], di16[k]);
      complex<double> de = derivatives ? abs(z) * log(abs(z)) / dc : 0;
      OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, phase, nBailout, de);
      InterlockedIncrement((LPLONG)&m_nDone);
      OutputPixelData(x, y, w, h, bGlitch);
    }
  }
}
