/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2021 Claude Heiland-Allen

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
#include "hybrid.h"

void CFraktalSFT::MandelCalc()
{
  m_bIterChanged = TRUE;
  double yr = 0, yi = 0;
  double epsilon(m_epsilon);
  int x, y, w, h;
  int64_t antal;
  const double nBailout = GetBailoutRadius();
  const double p = GetBailoutNorm();
  const double nBailout2 = p < 1.0/0.0 ? pow(nBailout, p) : nBailout;
  const double Cx = m_rref.ToDouble();
  const double Cy = m_iref.ToDouble();
  const double s = m_dPixelSpacing;
  const long double ls = m_lPixelSpacing;
  const mat2 TK = GetTransformMatrix();
  const bool noDerivativeGlitch = ! GetDerivativeGlitch();

  // vectorization
  double16 Dr16, Di16, dbD0r16, dbD0i16, test116, test216, phase16, Jxa16, Jxb16, Jya16, Jyb16, daa16, dab16, dba16, dbb16;
  int16 antal16, bGlitch16, bNoGlitchDetection16, x16, y16, w16, h16;
  int k = 0;
  const int64_t chunksize = GetSIMDChunkSize();
  const int vectorsize = std::min(int(GetSIMDVectorSize()), int(1 << KF_SIMD));
  const bool derivatives = GetDerivatives();
  const bool vectorized = (derivatives ? ! m_nScalingOffset : true) && (m_nFractalType == 0 ? ! (m_nPower > 10) : true) && (! GetUseHybridFormula()) && vectorsize > 1 && KF_SIMD > 0;

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
    daa0 = 1; dab0 = 0; dba0 = 0; dbb0 = 1;

    floatexp TDnr;
    floatexp TDni;
    floatexp dxa1, dxb1, dya1, dyb1;
    DoApproximation(antal, D0r, D0i, TDnr, TDni, dxa1, dxb1, dya1, dyb1);

    double test1 = 0, test2 = 0, phase = 0;
    bool bNoGlitchDetection = m_bNoGlitchDetection || (x == g_nAddRefX && y == g_nAddRefY);
    bool bGlitch = false;

    double dbD0r = D0r.todouble();
    double dbD0i = D0i.todouble();
    double Dr = TDnr.todouble();
    double Di = TDni.todouble();
    double Jxa = dxa1.todouble();
    double Jxb = dxb1.todouble();
    double Jya = dya1.todouble();
    double Jyb = dyb1.todouble();
    complex<double> de = 0;

    if (GetUseHybridFormula())
    {

      int power = 1;
      if (derivatives)
      {
        dual<2, double> dDr = Dr; dDr.dx[0] = 1; dDr.dx[1] = 0;
        dual<2, double> dDi = Di; dDi.dx[0] = 0; dDi.dx[1] = 1;
        dual<2, double> ddbD0r = dbD0r; ddbD0r.dx[0] = 1; ddbD0r.dx[1] = 0;
        dual<2, double> ddbD0i = dbD0i; ddbD0i.dx[0] = 0; ddbD0i.dx[1] = 1;
        bool ok = perturbation(GetHybridFormula(), Cx, Cy, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, dDr, dDi, ddbD0r, ddbD0i, power);
        assert(ok && "perturbation_double_dual_hybrid");
        de = compute_de(dDr.x, dDi.x, dDr.dx[0], dDr.dx[1], dDi.dx[0], dDi.dx[1], s, TK);
      }
      else
      {
        bool ok = perturbation(GetHybridFormula(), Cx, Cy, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, Dr, Di, dbD0r, dbD0i, power);
        assert(ok && "perturbation_double_hybrid");
      }
      OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, phase, nBailout, de, power);
      InterlockedIncrement((LPLONG)&m_nDone);
      OutputPixelData(x, y, w, h, bGlitch);

    }
    else if (m_nFractalType == 0 && m_nPower > 10)
    {

      if (derivatives)
      {
        complex<double> d(Jxa, Jya);
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
        Jxa = d.m_r;
        Jxb = -d.m_i;
        Jya = d.m_i;
        Jyb = d.m_r;
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
        Jxa16[k] = Jxa;
        Jxb16[k] = Jxb;
        Jya16[k] = Jya;
        Jyb16[k] = Jyb;
        k = k + 1;
        if (k == vectorsize)
        {
          bool ok = false;
#define GO(intN,doubleN) \
            intN antalv, bGlitchv, bNoGlitchDetectionv; \
            doubleN test1v, test2v, phasev, Drv, Div, dbD0rv, dbD0iv, Jxav, Jxbv, Jyav, Jybv, daav, dabv, dbav, dbbv; \
            for (int q = 0; q < vectorsize; ++q) \
            { \
              antalv[q] = antal16[q]; \
              bGlitchv[q] = bGlitch16[q]; \
              bNoGlitchDetectionv[q] = bNoGlitchDetection16[q]; \
              test1v[q] = test116[q]; \
              test2v[q] = test216[q]; \
              phasev[q] = phase16[q]; \
              Drv[q] = Dr16[q]; \
              Div[q] = Di16[q]; \
              dbD0rv[q] = dbD0r16[q]; \
              dbD0iv[q] = dbD0i16[q]; \
              if (derivatives) \
              { \
                Jxav[q] = Jxa16[q]; \
                Jxbv[q] = Jxb16[q]; \
                Jyav[q] = Jya16[q]; \
                Jybv[q] = Jyb16[q]; \
                daav[q] = daa16[q]; \
                dabv[q] = dab16[q]; \
                dbav[q] = dba16[q]; \
                dbbv[q] = dbb16[q]; \
              } \
            } \
            ok = m_nScalingOffset \
              ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, phasev, bGlitchv, nBailout2, nMaxIter, bNoGlitchDetectionv, g_real, g_imag, p, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize, m_nScaling, 1 / m_nScaling) \
              : derivatives \
              ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, phasev, bGlitchv, nBailout2, nMaxIter, bNoGlitchDetectionv, g_real, g_imag, p, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, Jxav, Jxbv, Jyav, Jybv, epsilon, m_dPixelSpacing, daav, dabv, dbav, dbbv, chunksize, noDerivativeGlitch) \
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
                Jxa16[q] = Jxav[q]; \
                Jxb16[q] = Jxbv[q]; \
                Jya16[q] = Jyav[q]; \
                Jyb16[q] = Jybv[q]; \
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
          long double lJdxa = dxa1.toLongDouble();
          long double lJdxb = dxb1.toLongDouble();
          long double lJdya = dya1.toLongDouble();
          long double lJdyb = dyb1.toLongDouble();
          long double daa = daa0.toLongDouble();
          long double dab = dab0.toLongDouble();
          long double dba = dba0.toLongDouble();
          long double dbb = dbb0.toLongDouble();
          bool ok = derivatives
            ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, lJdxa, lJdxb, lJdya, lJdyb, (long double)(m_epsilon), m_lPixelSpacing, daa, dab, dba, dbb, m_nScaling, 1 / m_nScaling, noDerivativeGlitch)
            : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, m_nScaling, 1 / m_nScaling)
            ;
          assert(ok && "perturbation_double_scaled");
          de = compute_de((long double)(Dr), (long double)(Di), lJdxa, lJdxb, lJdya, lJdyb, ls, TK);
        }
        else
        {
          double daa = daa0.todouble();
          double dab = dab0.todouble();
          double dba = dba0.todouble();
          double dbb = dbb0.todouble();
          bool ok = derivatives
            ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, Jxa, Jxb, Jya, Jyb, epsilon, m_dPixelSpacing, daa, dab, dba, dbb, noDerivativeGlitch)
            : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i)
            ;
          assert(ok && "perturbation_double");
          de = compute_de(Dr, Di, Jxa, Jxb, Jya, Jyb, s, TK);
        }
      }
    }

    if (vectorized)
    {
      if (k == vectorsize)
      {
        for (k = 0; k < vectorsize; ++k)
        {
          de = compute_de(Dr16[k], Di16[k], Jxa16[k], Jxb16[k], Jya16[k], Jyb16[k], s, TK);
          OutputIterationData(x16[k], y16[k], w16[k], h16[k], bGlitch16[k], antal16[k], test116[k], test216[k], phase16[k], nBailout, de, m_nPower);
          InterlockedIncrement((LPLONG)&m_nDone);
          OutputPixelData(x16[k], y16[k], w16[k], h16[k], bGlitch16[k]);
        }
        k = 0;
      }
    }
    else
    {
      if (! GetUseHybridFormula())
      {
        OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, phase, nBailout, de, m_nPower);
        InterlockedIncrement((LPLONG)&m_nDone);
        OutputPixelData(x, y, w, h, bGlitch);
      }
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
      double Jxa = Jxa16[k];
      double Jxb = Jxb16[k];
      double Jya = Jya16[k];
      double Jyb = Jyb16[k];
      double daa = daa16[k];
      double dab = dab16[k];
      double dba = dba16[k];
      double dbb = dbb16[k];
      bool ok = m_nScalingOffset
        ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, m_nScaling, 1 / m_nScaling)
        : derivatives
        ? perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, Jxa, Jxb, Jya, Jyb, epsilon, m_dPixelSpacing, daa, dab, dba, dbb, noDerivativeGlitch)
        : perturbation(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i)
        ;
      assert(ok && "perturbation_double");
      complex<double> de = compute_de(Dr, Di, Jxa, Jxb, Jya, Jyb, s, TK);
      OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, phase, nBailout, de, m_nPower);
      InterlockedIncrement((LPLONG)&m_nDone);
      OutputPixelData(x, y, w, h, bGlitch);
    }
  }
}
