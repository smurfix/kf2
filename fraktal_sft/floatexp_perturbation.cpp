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
#include <float.h>
#include "complex.h"

#include "../formula/formula.h"

BOOL ISFLOATOK(double a);
extern double g_real;
extern double g_imag;
extern double g_FactorAR;
extern double g_FactorAI;
#define _abs(a) ((_abs_val=(a))>0?_abs_val:-_abs_val)
floatexp lb_abs_exp(const floatexp &c, const floatexp &d)
{
	floatexp abs_val, _abs_val, _2=2;
	if (c>0){
		if (c + d>0)
			abs_val = d;
		else if (d == -c)
			abs_val = d;
		else if (d<-c)
			abs_val = -d - _2 * c;
	}
	else if (c == 0)
		abs_val = _abs(d);
	else if (c < 0){
		if (c + d>0)
			abs_val = d + _2 * c;
		else if (d == -c)
			abs_val = -d;
		else if (d < -c)
			abs_val = -d;
	}
	return abs_val;
}

void CFraktalSFT::MandelCalcEXP()
{
	m_bIterChanged = TRUE;
	floatexp yr, yi;
	int64_t antal;
	int x, y, w, h;
	floatexp real(g_real), imag(g_imag);
	floatexp epsilon(m_epsilon);
	const bool derivatives = GetDerivatives();
  const double nBailout = GetBailoutRadius();
  const double p = GetBailoutNorm();
  const double nBailout2 = p < 1.0/0.0 ? pow(nBailout, p) : nBailout;
	const bool no_g = g_real == 1.0 && g_imag == 1.0 && p == 2.0;
	const floatexp s = m_fPixelSpacing;
	const mat2 TK = GetTransformMatrix();
	const floatexp Cx = floatexp(m_rref);
	const floatexp Cy = floatexp(m_iref);


	while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
		if (m_nPixels[x][y] != PIXEL_UNEVALUATED){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y, w, h);
			if (m_bMirrored)
				Mirror(x, y);
			continue;
		}
    if (GuessPixel(x, y, w, h))
      continue;

		// Series approximation
		floatexp D0r = 0;
		floatexp D0i = 0;
		floatexp daa = 1;
		floatexp dab = 0;
		floatexp dba = 0;
		floatexp dbb = 1;
		GetPixelCoordinates(x, y, D0r, D0i, daa, dab, dba, dbb);
		daa = 1; dab = 0; dba = 0; dbb = 1;

		floatexp Dr;
		floatexp Di;
		floatexp dxa1, dxb1, dya1, dyb1;
		DoApproximation(antal, D0r, D0i, Dr, Di, dxa1, dxb1, dya1, dyb1);

		complex<double> de = 0;
		double test1 = 0, test2 = 0, phase = 0;
    bool bNoGlitchDetection = m_bNoGlitchDetection || (x == g_nAddRefX && y == g_nAddRefY);
		bool bGlitch = FALSE;
		int64_t nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);

		if (GetUseHybridFormula())
		{

			int power = 1;
			if (derivatives)
			{
				dual<2, floatexp> dDr = Dr; dDr.dx[0] = 1; dDr.dx[1] = 0;
				dual<2, floatexp> dDi = Di; dDi.dx[0] = 0; dDi.dx[1] = 1;
				dual<2, floatexp> ddbD0r = D0r; ddbD0r.dx[0] = 1; ddbD0r.dx[1] = 0;
				dual<2, floatexp> ddbD0i = D0i; ddbD0i.dx[0] = 0; ddbD0i.dx[1] = 1;
				bool ok = perturbation(GetHybridFormula(), Cx, Cy, m_dxr, m_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, dDr, dDi, ddbD0r, ddbD0i, power);
				assert(ok && "perturbation_floatexp_dual_hybrid");
				de = compute_de(dDr.x, dDi.x, dDr.dx[0], dDr.dx[1], dDi.dx[0], dDi.dx[1], s, TK);
			}
			else
			{
				bool ok = perturbation(GetHybridFormula(), Cx, Cy, m_dxr, m_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, Dr, Di, D0r, D0i, power);
				assert(ok && "perturbation_floatexp_hybrid");
			}
			OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, phase, nBailout, de, power);
			InterlockedIncrement((LPLONG)&m_nDone);
			OutputPixelData(x, y, w, h, bGlitch);

		}
		else if (m_nFractalType == 0 && m_nPower > 10) // FIXME matrix derivatives
		{
			if (derivatives)
			{
			complex<floatexp> d(dxa1, dya1);
			if (antal<nMaxIter && test1 <= nBailout2){
				for (; antal<nMaxIter; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (yr*yr + yi*yi).todouble();
					if (test1<m_db_z[antal]){
						bGlitch = TRUE;
						if (! bNoGlitchDetection)
							break;
					}
					if (! no_g)
					{
						test1 = double(pnorm(g_real, g_imag, p, yr, yi));
					}
					if (test1 > nBailout2)
					{
						break;
					}
					complex<floatexp> y(yr, yi);
					d = m_nPower * d * (y ^ (m_nPower - 1)) + 1;
					complex<floatexp> X(m_dxr[antal], m_dxi[antal]);
					complex<floatexp> D(Dr, Di);
					complex<floatexp> D0(D0r, D0i);
					complex<floatexp> c(m_pnExpConsts[0], 0);
					int nXExp = m_nPower - 2, nDExp = 2, ci = 1;
					complex<floatexp> Dn = c*(X^(m_nPower - 1))*D;
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
				Dr = yr;
				Di = yi;
			}
		  if (! (test1 <= nBailout2))
		  {
		    phase = atan2(double(yi), double(yr)) / M_PI / 2;
		    phase -= floor(phase);
		  }
				dxa1 = d.m_r;
				dxb1 = -d.m_i;
				dya1 = d.m_i;
				dyb1 = d.m_r;
				de = compute_de(yr, yi, dxa1, dxb1, dya1, dyb1, s, TK);
			} else {
			if (antal<nMaxIter && test1 <= nBailout2){
				for (; antal<nMaxIter; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (yr*yr + yi*yi).todouble();
					if (test1<m_db_z[antal]){
						bGlitch = TRUE;
						if (! bNoGlitchDetection)
							break;
					}
					if (! no_g)
					{
						test1 = double(pnorm(g_real, g_imag, p, yr, yi));
					}
					if (test1 > nBailout2)
					{
						break;
					}
					complex<floatexp> y(yr, yi);
					complex<floatexp> X(m_dxr[antal], m_dxi[antal]);
					complex<floatexp> D(Dr, Di);
					complex<floatexp> D0(D0r, D0i);
					complex<floatexp> c(m_pnExpConsts[0], 0);
					int nXExp = m_nPower - 2, nDExp = 2, ci = 1;
					complex<floatexp> Dn = c*(X^(m_nPower - 1))*D;
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
				Dr = yr;
				Di = yi;
			}
		  if (! (test1 <= nBailout2))
		  {
		    phase = atan2(double(yi), double(yr)) / M_PI / 2;
		    phase -= floor(phase);
		  }
			}

		}
		else
		{

			bool ok = derivatives
			  ? perturbation(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, D0r, D0i, dxa1, dxb1, dya1, dyb1, epsilon, m_fPixelSpacing, daa, dab, dba, dbb)
			  : perturbation(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, antal, test1, test2, phase, bGlitch, nBailout2, nMaxIter, bNoGlitchDetection, g_real, g_imag, p, g_FactorAR, g_FactorAI, Dr, Di, D0r, D0i)
			  ;
			assert(ok && "perturbation_floatexp()");
			de = derivatives
			  ? compute_de(Dr, Di, dxa1, dxb1, dya1, dyb1, s, TK)
			  : 0
			  ;

		}

		OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, phase, nBailout, de, m_nPower);
		InterlockedIncrement((LPLONG)&m_nDone);
		OutputPixelData(x, y, w, h, bGlitch);
	}
}
