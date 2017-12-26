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
	int antal, x, y, w, h;
	floatexp real(g_real), imag(g_imag), _abs_val;


	while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
		if (m_nPixels[x][y] != -1){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			if (m_bMirrored)
				Mirror(x, y);
			continue;
		}
    if (GuessPixel(x, y, w, h))
      continue;

		// Series approximation
		floatexp c = m_C;
		floatexp s = m_S;
		floatexp dbD0r = m_DX[m_nX / 2] + c*(m_DX[x] - m_DX[m_nX / 2]) + s*(m_DY[y] - m_DY[m_nY / 2]);
		floatexp dbD0i = m_DY[m_nY / 2] - s*(m_DX[x] - m_DX[m_nX / 2]) + c*(m_DY[y] - m_DY[m_nY / 2]);
		floatexp D0r = dbD0r;//(cr-rref);
		floatexp D0i = dbD0i;
		floatexp Dr;
		floatexp Di;
		floatexp dr;
		floatexp di;
		DoApproximation(antal, D0r, D0i, Dr, Di, dr, di);

		double test1 = 0, test2 = 0;
		BOOL bGlitch = FALSE;
		int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);

    if (m_nFractalType == 0 && m_nPower > 10)
		{
			complex<floatexp> d(dr, di);
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
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
			dr = d.m_r;
			di = d.m_i;

		}
    else
    {

			bool ok = perturbation_floatexp(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, D0r, D0i, dr, di);
			assert(ok && "perturbation_floatexp()");

		}

		floatexp pixel_spacing = m_fPixelSpacing;
		dr = dr * pixel_spacing;
		di = di * pixel_spacing;
		double de = double(sqrt(test1) * log(test1) / sqrt(dr * dr + di * di).todouble());

		OutputIterationData(x, y, bGlitch, antal, test1, test2, de);

		InterlockedIncrement((LPLONG)&m_nDone);
    OutputPixelData(x, y, w, h, bGlitch);
	}
}
