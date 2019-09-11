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
	floatexp Dnr, Dni, yr, yi;
	int64_t antal;
	int x, y, w, h;
	floatexp real(g_real), imag(g_imag), _abs_val;
	floatexp epsilon(m_epsilon);


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

		floatexp Dr;
		floatexp Di;
		floatexp dxa1, dxb1, dya1, dyb1;
		DoApproximation(antal, D0r, D0i, Dr, Di, dxa1, dxb1, dya1, dyb1);
		floatexp dr = dxa1;
		floatexp di = dya1;

		double test1 = 0, test2 = 0;
		bool bGlitch = FALSE;
		int64_t nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);

    if (m_nFractalType == 0 && m_nPower > 10) // FIXME matrix derivatives
		{
			if (GetDerivatives())
			{
			complex<floatexp> d(dr, di);
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						bGlitch = TRUE;
						if (! m_bNoGlitchDetection)
							break;
					}
					if (test1 > m_nBailout2)
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
			}
			dr = d.m_r * m_fPixelSpacing;
			di = d.m_i * m_fPixelSpacing;
			} else {
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						bGlitch = TRUE;
						if (! m_bNoGlitchDetection)
							break;
					}
					if (test1 > m_nBailout2)
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
			}
			}
		}
    else
    {

			dr *= m_fPixelSpacing;
			di *= m_fPixelSpacing;
			bool ok = GetDerivatives()
			  ? perturbation(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, D0r, D0i, dr, di, epsilon, m_fPixelSpacing, daa, dab, dba, dbb)
			  : perturbation(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, D0r, D0i)
			  ;
			assert(ok && "perturbation_floatexp()");

		}

		double de = GetDerivatives()
		  ? sqrt(test1) * log(test1) / double(sqrt(dr * dr + di * di))
		  : 0
		  ;

		OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, de);

		InterlockedIncrement((LPLONG)&m_nDone);
    OutputPixelData(x, y, w, h, bGlitch);
	}
}
