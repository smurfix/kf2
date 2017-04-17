#include "fraktal_sft.h"
#include <float.h>
#include "complex.h"

#include "../formula/formulas.h"

#define GUESS

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

void CFraktalSFT::MandelCalcEXP(int nXStart, int nXStop)
{
	m_bIterChanged = TRUE;
	floatexp Dnr, Dni, yr, yi;
	floatexp _2 = 2, _3 = 3, _5=5, _4 = 4, _6 = 6, _8=8, _9=9, _10=10, _12=12, _20=20, _24=24, _30=30, _40=40, _60=60;
	int antal, x, y;
	int nPStep, nStepSize;
	floatexp real(g_real), imag(g_imag), _abs_val;


	while (!m_bStop && m_P.GetPixel(x, y)){
		nStepSize = nPStep = m_P.GetStep();
		if (nPStep>1)
			nPStep = 0;
		else
			nPStep = 1;
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
		if (m_nPixels[x][y] != -1){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			if (m_bMirrored)
				Mirror(x, y);
			continue;
		}
#ifdef GUESS
		if (nPStep && nStepSize==1){
			if (x && x<m_nX - 1 && m_nPixels[x - 1][y] != -1 && m_nPixels[x - 1][y] == m_nPixels[x + 1][y]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y] + m_nTrans[x + 1][y])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y))*m_row;
				int nIndex2 = (x + 1) * 3 + (m_bmi->biHeight - 1 - (y))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 1 && m_nPixels[x][y - 1] != -1 && m_nPixels[x][y - 1] == m_nPixels[x][y + 1]){
				m_nTrans[x][y] = (m_nTrans[x][y - 1] + m_nTrans[x][y + 1])*.5;
				int nIndex1 = (x)* 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				int nIndex2 = (x)* 3 + (m_bmi->biHeight - 1 - (y + 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x][y - 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 1 && x && x<m_nX - 1 && m_nPixels[x - 1][y - 1] != -1 && m_nPixels[x - 1][y - 1] == m_nPixels[x + 1][y + 1]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y - 1] + m_nTrans[x + 1][y + 1])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				int nIndex2 = (x + 1) * 3 + (m_bmi->biHeight - 1 - (y + 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y - 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 1 && x && x<m_nX - 1 && m_nPixels[x - 1][y + 1] != -1 && m_nPixels[x - 1][y + 1] == m_nPixels[x + 1][y - 1]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y + 1] + m_nTrans[x + 1][y - 1])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y + 1))*m_row;
				int nIndex2 = (x + 1) * 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y + 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
#ifdef HARD_GUESS_EXP
			if (x && x<m_nX - 2 && m_nPixels[x - 1][y] != -1 && m_nPixels[x - 1][y] == m_nPixels[x + 2][y]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y] + m_nTrans[x + 2][y])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - y)*m_row;
				int nIndex2 = (x + 2) * 3 + (m_bmi->biHeight - 1 - y)*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 2 && m_nPixels[x][y - 1] != -1 && m_nPixels[x][y - 1] == m_nPixels[x][y + 2]){
				m_nTrans[x][y] = (m_nTrans[x][y - 1] + m_nTrans[x][y + 2])*.5;
				int nIndex1 = (x)* 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				int nIndex2 = (x)* 3 + (m_bmi->biHeight - 1 - (y + 2))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x][y - 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 2 && x && x<m_nX - 2 && m_nPixels[x - 1][y - 1] != -1 && m_nPixels[x - 1][y - 1] == m_nPixels[x + 2][y + 2]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y - 1] + m_nTrans[x + 2][y + 2])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				int nIndex2 = (x + 2) * 3 + (m_bmi->biHeight - 1 - (y + 2))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y - 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 2 && x && x<m_nX - 2 && m_nPixels[x - 1][y + 2] != -1 && m_nPixels[x - 1][y + 2] == m_nPixels[x + 2][y - 1]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y + 2] + m_nTrans[x + 2][y - 1])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y + 2))*m_row;
				int nIndex2 = (x + 2) * 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y + 2];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y>1 && y<m_nY - 2 && x && x<m_nX - 2 && m_nPixels[x - 1][y + 2] != -1 && m_nPixels[x - 1][y + 2] == m_nPixels[x + 2][y - 2]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y + 2] + m_nTrans[x + 2][y - 2])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y + 2))*m_row;
				int nIndex2 = (x + 2) * 3 + (m_bmi->biHeight - 1 - (y - 2))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y + 2];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
#endif
		}
#endif
		if (m_nPixels[x][y] != -1){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			if (m_bMirrored)
				Mirror(x, y);
			continue;
		}
		// Series approximation
		floatexp c = m_C;
		floatexp s = m_S;
		floatexp dbD0r = m_DX[m_nX / 2] + c*(m_DX[x] - m_DX[m_nX / 2]) + s*(m_DY[y] - m_DY[m_nY / 2]);
		floatexp dbD0i = m_DY[m_nY / 2] - s*(m_DX[x] - m_DX[m_nX / 2]) + c*(m_DY[y] - m_DY[m_nY / 2]);

		floatexp D0r = dbD0r;//(cr-rref);
		floatexp D0i = dbD0i;
		floatexp Dr = D0r;
		floatexp Di = D0i;
		if (m_nMaxApproximation){
			antal = m_nMaxApproximation - 1;
			Dnr = m_APr[0] * D0r - m_APi[0] * D0i;
			Dni = m_APr[0] * D0i + m_APi[0] * D0r;
			floatexp D_r = D0r*D0r - D0i*D0i;
			floatexp D_i = (D0r*D0i).mul2();
			Dnr += m_APr[1] * D_r - m_APi[1] * D_i;
			Dni += m_APr[1] * D_i + m_APi[1] * D_r;
			int k;
			for (k = 2; k<m_nTerms; k++){
				floatexp  t = D_r*D0r - D_i*D0i;
				D_i = D_r*D0i + D_i*D0r;
				D_r = t;
				Dnr += m_APr[k] * D_r - m_APi[k] * D_i;
				Dni += m_APr[k] * D_i + m_APi[k] * D_r;
			}
			Dr = Dnr;
			Di = Dni;
		}
		else{
			antal = 0;
			Dr = D0r;
			Di = D0i;
		}


		double test1 = 0, test2 = 0;
		BOOL bGlitch = FALSE;
		int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);
		if (m_nFractalType == 2 && m_nPower == 2){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}
					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;

					floatexp c = r*r - i*i;
					floatexp d = _2*r*a + a2 - _2*i*b - b2;
					if (c>0){
						if (c + d>0)
							Dnr = d;
						else if (d == -c)
							Dnr = d;
						else if (d<-c)
							Dnr = -d - _2*c;
					}
					else if (c == 0)
						Dnr = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dnr = d + _2*c;
						else if (d == -c)
							Dnr = -d;
						else if (d < -c)
							Dnr = -d;
					}
					Dnr = Dnr + a0;

					c = r*i;
					d = r*b + a*i + a*b;
					if (c>0){
						if (c + d>0)
							Dni = d;
						else if (d == -c)
							Dni = d;
						else if (d<-c)
							Dni = -d - _2*c;
					}
					else if (c == 0)
						Dni = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dni = d + _2*c;
						else if (d == -c)
							Dni = -d;
						else if (d < -c)
							Dni = -d;
					}
					Dni = b0 - _2*Dni;

					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		if (m_nFractalType == 2 && m_nPower == 3){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}
					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;
					floatexp r2 = r*r;
					floatexp i2 = i*i;
					floatexp a2 = a*a;
					floatexp b2 = b*b;

					floatexp c = r*(r2 - _3*i2);
					floatexp d = a*(_3*r2 + a2) + _3*r*(a2 - _2*i*b - b2) - _3*a*(i2 + _2*i*b + b2);
					if (c>0){
						if (c + d>0)
							Dnr = d;
						else if (d == -c)
							Dnr = d;
						else if (d<-c)
							Dnr = -d - _2*c;
					}
					else if (c == 0)
						Dnr = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dnr = d + _2*c;
						else if (d == -c)
							Dnr = -d;
						else if (d < -c)
							Dnr = -d;
					}
					Dnr = Dnr + a0;

					c = i*(_3*r2 - i2);
					d = _3*i*(_2*r*a + a2 - b2) + _3*b*(r2 + _2*r*a + a2) - b*(_3*i2 + b2);
					if (c>0){
						if (c + d>0)
							Dni = d;
						else if (d == -c)
							Dni = d;
						else if (d<-c)
							Dni = -d - _2*c;
					}
					else if (c == 0)
						Dni = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dni = d + _2*c;
						else if (d == -c)
							Dni = -d;
						else if (d < -c)
							Dni = -d;
					}
					Dni = Dni + b0;

					Di = Dni;
					Dr = Dnr;
				}
			}
		}
			// 4th Power Buffalo
			else if (m_nFractalType == 2 && m_nPower == 4){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						floatexp c = x2*x2+y2*y2-_6*x2*y2;
						floatexp d = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr+=a0;

						c = _4*x2*x*y-_4*x*y2*y;
						d = -_4*a*y2*y-_12*b*x*y2-_12*a*b*y2+_12*a*x2*y-_12*b2*x*y+_12*a2*x*y-_12*a*b2*y+_4*a2*a*y+_4*b*x2*x+_12*a*b*x2-_4*b2*b*x+_12*a2*b*x-_4*a*b2*b+_4*a2*a*b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni+=b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 5th Power Buffalo
			else if (m_nFractalType == 2 && m_nPower == 5){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						floatexp c = _5*x*y2*y2-_10*x2*x*y2+x2*x2*x;
						floatexp d = _20*x*b*y2*y-_30*x2*a*y2+_30*x*b2*y2-_30*x*a2*y2-_20*x2*x*b*y-_60*x2*a*b*y+_20*x*b2*b*y-_60*x*a2*b*y+_5*x2*x2*a-_10*x2*x*b2+_10*x2*x*a2-_30*x2*a*b2+_10*x2*a2*a+_5*x*b2*b2-_30*x*a2*b2+_5*x*a2*a2+_5*a*y2*y2+_20*a*b*y2*y+_30*a*b2*y2-_10*a2*a*y2+_20*a*b2*b*y-_20*a2*a*b*y+_5*a*b2*b2-_10*a2*a*b2+a2*a2*a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr+=a0;

						c = y2*y2*y-_10*y2*y*x2+_5*y*x2*x2;
						d = _5*y2*y2*b-_20*y2*y*a*x+_10*y2*y*b2-_10*y2*y*a2-_30*y2*b*x2-_60*y2*a*b*x+_10*y2*b2*b-_30*y2*a2*b+_20*y*a*x2*x-_30*y*b2*x2+_30*y*a2*x2-_60*y*a*b2*x+_20*y*a2*a*x+_5*y*b2*b2-_30*y*a2*b2+_5*y*a2*a2+_5*b*x2*x2+_20*b*a*x2*x-_10*b2*b*x2+_30*b*a2*x2-_20*b2*b*a*x+_20*b*a2*a*x+b2*b2*b-_10*b2*b*a2+_5*b*a2*a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni+=b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
		if (m_nFractalType == 3 && m_nPower == 2){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;

					floatexp c = r*r - i*i;
					floatexp d = _2*r*a + a2 - _2*i*b - b2;
					if (c>0){
						if (c + d>0)
							Dnr = d;
						else if (d == -c)
							Dnr = d;
						else if (d<-c)
							Dnr = -d - _2*c;
					}
					else if (c == 0)
						Dnr = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dnr = d + _2*c;
						else if (d == -c)
							Dnr = -d;
						else if (d < -c)
							Dnr = -d;
					}
					Dnr = Dnr + a0;

					Dni = _2*r*b + _2*a*i + _2*a*b + b0;


					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		if (m_nFractalType == 3 && m_nPower == 3){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;
					floatexp r2 = r*r;
					floatexp i2 = i*i;
					floatexp a2 = a*a;
					floatexp b2 = b*b;

					floatexp c = r*(r2 - _3*i2);
					floatexp d = a*(_3*r2 + a2) + _3*r*(a2 - _2*i*b - b2) - _3*a*(i2 + _2*i*b + b2);
					if (c>0){
						if (c + d>0)
							Dnr = d;
						else if (d == -c)
							Dnr = d;
						else if (d<-c)
							Dnr = -d - _2*c;
					}
					else if (c == 0)
						Dnr = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dnr = d + _2*c;
						else if (d == -c)
							Dnr = -d;
						else if (d < -c)
							Dnr = -d;
					}
					Dnr = Dnr + a0;

					Dni = _3*i*(_2*r*a + a2 - b2) + _3*b*(r2 + _2*r*a + a2) - b*(b2 + _3*i2) + b0;

					Di = Dni;
					Dr = Dnr;
				}
			}
		}
			// 4th Celtic Buffalo
			else if (m_nFractalType == 3 && m_nPower == 4){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						floatexp c = x2*x2+y2*y2-_6*x2*y2;
						floatexp d = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 + _4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2 -_12*a*x*y2-_6*a2*y2-_12*b*x2*y-_24*a*b*x*y-_12*a2*b*y-_6*b2*x2-_12*a*b2*x-_6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr+=a0;
						

						Dni = _12*x2*y*a+_12*x*y*a2-_12*x*y2*b-_12*x*y*b2+_4*x2*x*b+_12*x2*b*a+_12*x*b*a2-_4*x*b2*b+_4*a2*a*y-_4*a*y2*y-_12*a*y2*b-_12*a*y*b2+_4*a2*a*b-_4*a*b2*b + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 5th Celtic Buffalo
			else if (m_nFractalType == 3 && m_nPower == 5){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						floatexp c = x2*x2*x - _10*x2*x*y2 + _5*x*y2*y2;
						floatexp d = _20*x*b*y2*y-_30*x2*a*y2+_30*x*b2*y2-_30*x*a2*y2-_20*x2*x*b*y-_60*x2*a*b*y+_20*x*b2*b*y-_60*x*a2*b*y+_5*x2*x2*a-_10*x2*x*b2+_10*x2*x*a2-_30*x2*a*b2+_10*x2*a2*a+_5*x*b2*b2-_30*x*a2*b2+_5*x*a2*a2+_5*a*y2*y2+_20*a*b*y2*y+_30*a*b2*y2-_10*a2*a*y2+_20*a*b2*b*y-_20*a2*a*b*y+_5*a*b2*b2-_10*a2*a*b2+a2*a2*a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr+=a0;
						
						Dni = _20*y*x2*x*a+_30*y*x2*a2+_20*y*x*a2*a+_5*y*a2*a2-_30*y2*x2*b-_30*y*x2*b2-_20*y2*y*x*a-_60*y2*x*a*b-_60*y*x*a*b2-_10*y2*y*a2-_30*y2*a2*b-_30*y*a2*b2+_5*y2*y2*b+_10*y2*y*b2+_10*y2*b2*b+_5*y*b2*b2+_5*b*x2*x2+_20*b*x2*x*a+_30*b*x2*a2+_20*b*x*a2*a+_5*b*a2*a2-_10*b2*b*x2-_20*b2*b*x*a-_10*b2*b*a2+b2*b2*b +b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
		// Mandelbar
		else if (m_nFractalType == 4 && m_nPower == 2){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;

					Dnr = _2*r*a + a2 - b2 - _2*b*i + a0;
					Dni = b0 - (r*b + a*i + a*b)*_2;


					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		// Cubic Mandelbar
		else if (m_nFractalType == 4 && m_nPower == 3){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;

					floatexp r2 = r*r;
					floatexp i2 = i*i;
					floatexp a2 = a*a;
					floatexp b2 = b*b;

					Dnr = a0 - a*(_3*r2 + a2) + _3*r*(b2 + _2*i*b - a2) + _3*a*(i2 + _2*i*b + b2);
					Dni = _6*r*i*a + _3*i*a2 - _3*i2*b - _3*i*b2 + _3*r2*b + _6*r*a*b + _3*a2*b - b2*b + b0;

					Di = Dni;
					Dr = Dnr;
				}
			}
		}
			// 4th Power Mandelbar
			else if (m_nFractalType == 4 && m_nPower == 4){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2 + a0;
						Dni = -_12*x2*y*a-_12*x*y*a2+_12*x*y2*b+_12*x*y*b2-_4*x2*x*b-_12*x2*b*a-_12*x*b*a2+_4*x*b2*b-_4*a2*a*y+_4*a*y2*y+_12*a*y2*b+_12*a*y*b2-_4*a2*a*b+_4*a*b2*b + b0;


						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 5th Power Mandelbar
			else if (m_nFractalType == 4 && m_nPower == 5){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr = _5*x2*x2*a+_10*x2*x*a2+_10*x2*a2*a+_5*x*a2*a2-_20*x2*x*y*b-_10*x2*x*b2-_30*x2*a*y2-_60*x2*a*y*b-_30*x2*a*b2-_30*x*a2*y2-_60*x*a2*y*b-_30*x*a2*b2+_20*x*y2*y*b+_30*x*y2*b2+_20*x*y*b2*b+_5*x*b2*b2+a2*a2*a-_10*a2*a*y2-_20*a2*a*y*b-_10*a2*a*b2+_5*a*y2*y2+_20*a*y2*y*b+_30*a*y2*b2+_20*a*y*b2*b+_5*a*b2*b2 + a0;
						Dni = -_20*y*x2*x*a-_30*y*x2*a2-_20*y*x*a2*a-_5*y*a2*a2+_30*y2*x2*b+_30*y*x2*b2+_20*y2*y*x*a+_60*y2*x*a*b+_60*y*x*a*b2+_10*y2*y*a2+_30*y2*a2*b+_30*y*a2*b2-_5*y2*y2*b-_10*y2*y*b2-_10*y2*b2*b-_5*y*b2*b2-_5*b*x2*x2-_20*b*x2*x*a-_30*b*x2*a2-_20*b*x*a2*a-_5*b*a2*a2+_10*b2*b*x2+_20*b2*b*x*a+_10*b2*b*a2-b2*b2*b + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
		//Mandelbar Celtic
		else if (m_nFractalType == 5){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;

					floatexp c = r*r - i*i;
					floatexp d = _2*r*a + a2 - _2*i*b - b2;
					if (c>0){
						if (c + d>0)
							Dnr = d;
						else if (d == -c)
							Dnr = d;
						else if (d<-c)
							Dnr = -d - _2*c;
					}
					else if (c == 0)
						Dnr = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dnr = d + _2*c;
						else if (d == -c)
							Dnr = -d;
						else if (d < -c)
							Dnr = -d;
					}
					Dnr = Dnr + a0;

					Dni = b0 - _2*(r*b + a*i + a*b);


					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		// Perpendicular Mandelbrot
		else if (m_nFractalType == 6){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;

					Dnr = _2*r*a + a2 - b2 - _2*b*i + a0;

					floatexp c = r;
					floatexp d = a;
					if (c>0){
						if (c + d>0)
							Dni = d;
						else if (d == -c)
							Dni = d;
						else if (d<-c)
							Dni = -d - _2*c;
					}
					else if (c == 0)
						Dni = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dni = d + _2*c;
						else if (d == -c)
							Dni = -d;
						else if (d < -c)
							Dni = -d;
					}
					Dni = b0 - Dni*i*_2 - (r + a).abs()*b*_2;


					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		//Perpendicular Burning Ship
		else if (m_nFractalType == 7){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;

					Dnr = _2*r*a + a2 - b2 - _2*b*i + a0;

					floatexp c = i;
					floatexp d = b;
					if (c>0){
						if (c + d>0)
							Dni = d;
						else if (d == -c)
							Dni = d;
						else if (d<-c)
							Dni = -d - _2*c;
					}
					else if (c == 0)
						Dni = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dni = d + _2*c;
						else if (d == -c)
							Dni = -d;
						else if (d < -c)
							Dni = -d;
					}
					Dni = b0 - Dni*r*_2 - a*(i + b).abs()*_2;


					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		//Perpendicular Celtic
		else if (m_nFractalType == 8){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;

					floatexp c = r*r - i*i;
					floatexp d = _2*r*a + a2 - _2*i*b - b2;
					if (c>0){
						if (c + d>0)
							Dnr = d;
						else if (d == -c)
							Dnr = d;
						else if (d<-c)
							Dnr = -d - _2*c;
					}
					else if (c == 0)
						Dnr = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dnr = d + _2*c;
						else if (d == -c)
							Dnr = -d;
						else if (d < -c)
							Dnr = -d;
					}
					Dnr = Dnr + a0;

					c = r;
					d = a;
					if (c>0){
						if (c + d>0)
							Dni = d;
						else if (d == -c)
							Dni = d;
						else if (d<-c)
							Dni = -d - _2*c;
					}
					else if (c == 0)
						Dni = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dni = d + _2*c;
						else if (d == -c)
							Dni = -d;
						else if (d < -c)
							Dni = -d;
					}
					Dni = b0 - (r + a).abs()*b*_2 - Dni*i*_2;

					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		//Perpendicular Buffalo
		else if (m_nFractalType == 9){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;

					floatexp c = r*r - i*i;
					floatexp d = _2*r*a + a2 - _2*i*b - b2;
					if (c>0){
						if (c + d>0)
							Dnr = d;
						else if (d == -c)
							Dnr = d;
						else if (d<-c)
							Dnr = -d - _2*c;
					}
					else if (c == 0)
						Dnr = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dnr = d + _2*c;
						else if (d == -c)
							Dnr = -d;
						else if (d < -c)
							Dnr = -d;
					}
					Dnr = Dnr + a0;

					c = i;
					d = b;
					if (c>0){
						if (c + d>0)
							Dni = d;
						else if (d == -c)
							Dni = d;
						else if (d<-c)
							Dni = -d - _2*c;
					}
					else if (c == 0)
						Dni = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dni = d + _2*c;
						else if (d == -c)
							Dni = -d;
						else if (d < -c)
							Dni = -d;
					}
					Dni = b0 - Dni*r*_2 - a*(i + b).abs()*_2;

					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		else if (m_nFractalType == 10){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;
					floatexp r2 = r*r;
					floatexp i2 = i*i;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp ar = a*r;
					floatexp ib = i*b;
					floatexp ab;

					if (r>0){
						if (r + a>0)
							Dnr = a;
						else if (a == -r)
							Dnr = a;
						else if (a<-r)
							Dnr = -a - _2*r;
					}
					else if (r == 0)
						Dnr = a.abs();
					else if (r < 0){
						if (r + a>0)
							Dnr = a + _2*r;
						else if (a == -r)
							Dnr = -a;
						else if (a < -r)
							Dnr = -a;
					}
					ab = r + a;
					Dnr = (r2 - _3*i2) * Dnr + (_2*ar + a2 - _6*ib - _3*b2)*ab.abs() + a0;

					floatexp c = i*(_3*r2 - i2);
					floatexp d = _3*i*(_2*r*a + a2 - b2) + _3*b*(r2 + _2*r*a + a2) - b*(_3*i2 + b2);
					if (c>0){
						if (c + d>0)
							Dni = d;
						else if (d == -c)
							Dni = d;
						else if (d<-c)
							Dni = -d - _2*c;
					}
					else if (c == 0)
						Dni = _abs(d);
					else if (c < 0){
						if (c + d>0)
							Dni = d + _2*c;
						else if (d == -c)
							Dni = -d;
						else if (d < -c)
							Dni = -d;
					}
					Dni = b0 - Dni;
					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		// Cubic Partial BS Real
		else if (m_nFractalType == 11){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;
					floatexp r2 = r*r;
					floatexp i2 = i*i;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp ar = a*r;
					floatexp ib = i*b;
					floatexp ab;

					if (r>0){
						if (r + a>0)
							Dnr = a;
						else if (a == -r)
							Dnr = a;
						else if (a<-r)
							Dnr = -a - _2*r;
					}
					else if (r == 0)
						Dnr = a.abs();
					else if (r < 0){
						if (r + a>0)
							Dnr = a + _2*r;
						else if (a == -r)
							Dnr = -a;
						else if (a < -r)
							Dnr = -a;
					}
					ab = r + a;
					Dnr = (r2 - _3*i2) * Dnr + (_2*ar + a2 - _6*ib - _3*b2)*ab.abs() + a0;
					Dni = _6*r*(i*a + a*b) + _3*i*(a2 - b2) + _3*b*(r2 - i2) + b*(_3*a2 - b2) + b0;

					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		// Cubic Partial BS Imag
		else if (m_nFractalType == 12){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;
					floatexp r2 = r*r;
					floatexp i2 = i*i;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp ar = a*r;
					floatexp ib = i*b;
					floatexp ab;

					Dnr = _3*r2*a + _3*r*a2 - _6*r*i*b - _3*r*b2 + a*a2 - _3*i2*a - _6*i*a*b - _3*a*b2 + a0;

					if (i>0){
						if (i + b>0)
							Dni = b;
						else if (b == -i)
							Dni = b;
						else if (b<-i)
							Dni = -b - _2*i;
					}
					else if (i == 0)
						Dni = b.abs();
					else if (i < 0){
						if (i + b>0)
							Dni = b + _2*i;
						else if (b == -i)
							Dni = -b;
						else if (b < -i)
							Dni = -b;
					}
					ab = i + b;
					Dni = (_3*r2 - i2) * Dni + (_6*ar + _3*a2 - _2*ib - b2) * ab.abs() + b0;

					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		// Cubic Flying Squirrel
		else if (m_nFractalType == 13){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;
					floatexp r2 = r*r;
					floatexp i2 = i*i;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					Dnr = _3*r2*a + _3*r*a2 - _6*r*i*b - _3*r*b2 + a*a2 - _3*i2*a - _6*i*a*b - _3*a*b2 + a0;

					floatexp c = i*(_3*r2 - i2);
					floatexp d = _3*i*(_2*r*a + a2 - b2) + _3*b*(r2 + _2*r*a + a2) - b*(_3*i2 + b2);
					if (c>0){
						if (c + d>0)
							Dni = d;
						else if (d == -c)
							Dni = d;
						else if (d<-c)
							Dni = -d - _2*c;
					}
					else if (c == 0)
						Dni = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dni = d + _2*c;
						else if (d == -c)
							Dni = -d;
						else if (d < -c)
							Dni = -d;
					}
					Dni = Dni + b0;

					Di = Dni;
					Dr = Dnr;
				}
			}
		}
		// Cubic Quasi Perpendicular
		else if (m_nFractalType == 14){
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}

					floatexp &r = m_dxr[antal];
					floatexp &i = m_dxi[antal];
					floatexp &a = Dr;
					floatexp &b = Di;
					floatexp &a0 = dbD0r;
					floatexp &b0 = dbD0i;
					floatexp r2 = r*r;
					floatexp i2 = i*i;
					floatexp a2 = a*a;
					floatexp b2 = b*b;
					floatexp ar = a*r;
					floatexp ib = i*b;
					floatexp ab;

					if (r>0){
						if (r + a>0)
							Dnr = a;
						else if (a == -r)
							Dnr = a;
						else if (a<-r)
							Dnr = -a - _2*r;
					}
					else if (r == 0)
						Dnr = a.abs();
					else if (r < 0){
						if (r + a>0)
							Dnr = a + _2*r;
						else if (a == -r)
							Dnr = -a;
						else if (a < -r)
							Dnr = -a;
					}
					ab = r + a;
					Dnr = (r2 - _3*i2) * Dnr + (_2*ar + a2 - _6*ib - _3*b2)*ab.abs() + a0;

					floatexp c = _3*r2 - i2;
					floatexp d = _6*r*a + _3*a2 - _2*i*b - b2;
					if (c>0){
						if (c + d>0)
							Dni = d;
						else if (d == -c)
							Dni = d;
						else if (d<-c)
							Dni = -d - _2*c;
					}
					else if (c == 0)
						Dni = d.abs();
					else if (c < 0){
						if (c + d>0)
							Dni = d + _2*c;
						else if (d == -c)
							Dni = -d;
						else if (d < -c)
							Dni = -d;
					}
					ab = _3*r2 + _6*r*a + _3*a2 - i2 - _2*i*b - b2;
					Dni = b0 - Dni*i - ab.abs()*b;

					Di = Dni;
					Dr = Dnr;
				}
			}
		}
			// 4th Burning Ship Partial Imag
			else if (m_nFractalType == 15){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func2
						Dnr = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2 + a0;

						//func1
						floatexp c = y;
						floatexp d = b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(y+b)*(_12*x2*a+_12*x*a2+_4*a2*a - _4*a*y2-_8*b*x*y-_8*a*b*y-_4*b2*x-_4*a*b2) + Dni*(_4*x2*x - _4*x*y2) + b0;
						
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Burning Ship Partial Real
			else if (m_nFractalType == 16){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func2
						Dnr = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2 + a0;

						//func3
						floatexp c = x;
						floatexp d = a;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(x+a)*(_4*x2*b+_8*x*a*y+_8*x*a*b+_4*a2*y+_4*a2*b - _12*b*y2-_12*b2*y-_4*b2*b) + Dni*(_4*x2*y - _4*y2*y) + b0;
						
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Burning Ship Partial Real Mbar
			else if (m_nFractalType == 17){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func2
						Dnr = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2 + a0;

						//func4
						floatexp c = x;
						floatexp d = a;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(x+a)*(_12*y2*b+_12*y*b2+_4*b2*b - _8*a*x*y-_4*a2*y-_4*b*x2-_8*a*b*x-_4*a2*b) + Dni*(_4*y2*y - _4*x2*y) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic Burning Ship Partial Imag
			else if (m_nFractalType == 18){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func5
						floatexp c = x2*x2 + y2*y2 - _6*x2*y2;
						floatexp d = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 + _4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2 - _12*a*x*y2-_6*a2*y2-_12*b*x2*y-_24*a*b*x*y-_12*a2*b*y-_6*b2*x2-_12*a*b2*x-_6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						//func1
						c = y;
						d = b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(y+b)*(_12*x2*a+_12*x*a2+_4*a2*a - _4*a*y2-_8*b*x*y-_8*a*b*y-_4*b2*x-_4*a*b2) + Dni*(_4*x2*x - _4*x*y2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic Burning Ship Partial Real
			else if (m_nFractalType == 19){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func5
						floatexp c = x2*x2 + y2*y2 - _6*x2*y2;
						floatexp d = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 + _4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2 - _12*a*x*y2-_6*a2*y2-_12*b*x2*y-_24*a*b*x*y-_12*a2*b*y-_6*b2*x2-_12*a*b2*x-_6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						//func3
						c = x;
						d = a;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(x+a)*(_4*x2*b+_8*x*a*y+_8*x*a*b+_4*a2*y+_4*a2*b - _12*b*y2-_12*b2*y-_4*b2*b) + Dni*(_4*x2*y - _4*y2*y) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic Burning Ship Partial Real Mbar
			else if (m_nFractalType == 20){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func5
						floatexp c = x2*x2 + y2*y2 - _6*x2*y2;
						floatexp d = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 + _4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2 - _12*a*x*y2-_6*a2*y2-_12*b*x2*y-_24*a*b*x*y-_12*a2*b*y-_6*b2*x2-_12*a*b2*x-_6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						//func4
						c = x;
						d = a;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(x+a)*(_12*y2*b+_12*y*b2+_4*b2*b - _8*a*x*y-_4*a2*y-_4*b*x2-_8*a*b*x-_4*a2*b) + Dni*(_4*y2*y - _4*x2*y) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Buffalo Partial Imag
			else if (m_nFractalType == 21){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;
						//func2
						Dnr = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2 + a0;

						//func6
						floatexp c = _4*x2*x*y-_4*x*y2*y;
						floatexp d = -_4*a*y2*y-_12*b*x*y2-_12*a*b*y2+_12*a*x2*y-_12*b2*x*y+_12*a2*x*y-_12*a*b2*y+_4*a2*a*y+_4*b*x2*x+_12*a*b*x2-_4*b2*b*x+_12*a2*b*x-_4*a*b2*b+_4*a2*a*b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni=Dni+b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic Mbar
			else if (m_nFractalType == 22){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func5
						floatexp c = x2*x2 + y2*y2 - _6*x2*y2;
						floatexp d = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 + _4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2 - _12*a*x*y2-_6*a2*y2-_12*b*x2*y-_24*a*b*x*y-_12*a2*b*y-_6*b2*x2-_12*a*b2*x-_6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						//func8
						Dni = b0 - (_12*x2*y*a+_12*x*y*a2-_12*x*y2*b-_12*x*y*b2+_4*x2*x*b+_12*x2*b*a+_12*x*b*a2-_4*x*b2*b+_4*a2*a*y-_4*a*y2*y-_12*a*y2*b-_12*a*y*b2+_4*a2*a*b-_4*a*b2*b);
						
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th False Quasi Perpendicular
			else if (m_nFractalType == 23){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;
						//func2
						Dnr = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2 + a0;

						//func9
						floatexp c = x2-y2;
						floatexp d = -_2*b*y+_2*a*x-b2+a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = -(_4*x*y)*Dni - (_4*x*b + _4*a*y + _4*a*b)*_abs(-y2-_2*b*y+x2+_2*a*x-b2+a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th False Quasi Heart
			else if (m_nFractalType == 24){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;
						//func2
						Dnr = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2 + a0;

						//func10
						floatexp c = x2-y2;
						floatexp d = -_2*b*y+_2*a*x-b2+a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = (_4*x*y)*Dni + (_4*x*b + _4*a*y + _4*a*b)*_abs(-y2-_2*b*y+x2+_2*a*x-b2+a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic False Quasi Perpendicular
			else if (m_nFractalType == 25){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func5
						floatexp c = x2*x2 + y2*y2 - _6*x2*y2;
						floatexp d = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 + _4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2 - _12*a*x*y2-_6*a2*y2-_12*b*x2*y-_24*a*b*x*y-_12*a2*b*y-_6*b2*x2-_12*a*b2*x-_6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						//func9
						c = x2-y2;
						d = -_2*b*y+_2*a*x-b2+a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = -(_4*x*y)*Dni - (_4*x*b + _4*a*y + _4*a*b)*_abs(-y2-_2*b*y+x2+_2*a*x-b2+a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic False Quasi Heart
			else if (m_nFractalType == 26){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;
						//func5
						floatexp c = x2*x2 + y2*y2 - _6*x2*y2;
						floatexp d = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 + _4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2 - _12*a*x*y2-_6*a2*y2-_12*b*x2*y-_24*a*b*x*y-_12*a2*b*y-_6*b2*x2-_12*a*b2*x-_6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;
						//func10
						c = x2-y2;
						d = -_2*b*y+_2*a*x-b2+a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = (_4*x*y)*Dni + (_4*x*b + _4*a*y + _4*a*b)*_abs(-y2-_2*b*y+x2+_2*a*x-b2+a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Burning Ship Partial
			else if (m_nFractalType == 27){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func14
						floatexp c = x;
						floatexp d = a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = (Dnr) * (x2*x2- _10*x2*y2 + _5*y2*y2) + _abs(x+a) * (_4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 -_20*x2*y*b-_10*x2*b2-_20*x*a*y2-_40*x*a*y*b-_20*x*a*b2-_10*a2*y2-_20*a2*y*b-_10*a2*b2 + _20*y2*y*b+_30*y2*b2+_20*y*b2*b+_5*b2*b2) + a0;

						//func15
						Dni = _20*y*x2*x*a+_30*y*x2*a2+_20*y*x*a2*a+_5*y*a2*a2-_30*y2*x2*b-_30*y*x2*b2-_20*y2*y*x*a-_60*y2*x*a*b-_60*y*x*a*b2-_10*y2*y*a2-_30*y2*a2*b-_30*y*a2*b2+_5*y2*y2*b+_10*y2*y*b2+_10*y2*b2*b+_5*y*b2*b2+_5*b*x2*x2+_20*b*x2*x*a+_30*b*x2*a2+_20*b*x*a2*a+_5*b*a2*a2-_10*b2*b*x2-_20*b2*b*x*a-_10*b2*b*a2+b2*b2*b + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Burning Ship Partial Mbar
			else if (m_nFractalType == 28){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func14
						floatexp c = x;
						floatexp d = a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = (Dnr) * (x2*x2- _10*x2*y2 + _5*y2*y2) + _abs(x+a) * (_4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 -_20*x2*y*b-_10*x2*b2-_20*x*a*y2-_40*x*a*y*b-_20*x*a*b2-_10*a2*y2-_20*a2*y*b-_10*a2*b2 + _20*y2*y*b+_30*y2*b2+_20*y*b2*b+_5*b2*b2) + a0;

						//func16
						Dni = b0 - (_20*y*x2*x*a+_30*y*x2*a2+_20*y*x*a2*a+_5*y*a2*a2-_30*y2*x2*b-_30*y*x2*b2-_20*y2*y*x*a-_60*y2*x*a*b-_60*y*x*a*b2-_10*y2*y*a2-_30*y2*a2*b-_30*y*a2*b2+_5*y2*y2*b+_10*y2*y*b2+_10*y2*b2*b+_5*y*b2*b2+_5*b*x2*x2+_20*b*x2*x*a+_30*b*x2*a2+_20*b*x*a2*a+_5*b*a2*a2-_10*b2*b*x2-_20*b2*b*x*a-_10*b2*b*a2+b2*b2*b);

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Celtic Mbar
			else if (m_nFractalType == 29){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func17
						floatexp c = _5*x*y2*y2-_10*x2*x*y2+x2*x2*x;
						floatexp d = _20*x*b*y2*y-_30*x2*a*y2+_30*x*b2*y2-_30*x*a2*y2-_20*x2*x*b*y-_60*x2*a*b*y+_20*x*b2*b*y-_60*x*a2*b*y+_5*x2*x2*a-_10*x2*x*b2+_10*x2*x*a2-_30*x2*a*b2+_10*x2*a2*a+_5*x*b2*b2-_30*x*a2*b2+_5*x*a2*a2+_5*a*y2*y2+_20*a*b*y2*y+_30*a*b2*y2-_10*a2*a*y2+_20*a*b2*b*y-_20*a2*a*b*y+_5*a*b2*b2-_10*a2*a*b2+a2*a2*a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr+=a0;

						//func16
						Dni = b0 - (_20*y*x2*x*a+_30*y*x2*a2+_20*y*x*a2*a+_5*y*a2*a2-_30*y2*x2*b-_30*y*x2*b2-_20*y2*y*x*a-_60*y2*x*a*b-_60*y*x*a*b2-_10*y2*y*a2-_30*y2*a2*b-_30*y*a2*b2+_5*y2*y2*b+_10*y2*y*b2+_10*y2*b2*b+_5*y*b2*b2+_5*b*x2*x2+_20*b*x2*x*a+_30*b*x2*a2+_20*b*x*a2*a+_5*b*a2*a2-_10*b2*b*x2-_20*b2*b*x*a-_10*b2*b*a2+b2*b2*b);

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Quasi Burning Ship (BS/Buffalo Hybrid)
			else if (m_nFractalType == 30){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func14
						floatexp c = x;
						floatexp d = a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = (Dnr) * (x2*x2- _10*x2*y2 + _5*y2*y2) + _abs(x+a) * (_4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 -_20*x2*y*b-_10*x2*b2-_20*x*a*y2-_40*x*a*y*b-_20*x*a*b2-_10*a2*y2-_20*a2*y*b-_10*a2*b2 + _20*y2*y*b+_30*y2*b2+_20*y*b2*b+_5*b2*b2) + a0;

						//func18
						c = y2*y2*y-_10*y2*y*x2+_5*y*x2*x2;
						d = _5*y2*y2*b-_20*y2*y*a*x+_10*y2*y*b2-_10*y2*y*a2-_30*y2*b*x2-_60*y2*a*b*x+_10*y2*b2*b-_30*y2*a2*b+_20*y*a*x2*x-_30*y*b2*x2+_30*y*a2*x2-_60*y*a*b2*x+_20*y*a2*a*x+_5*y*b2*b2-_30*y*a2*b2+_5*y*a2*a2+_5*b*x2*x2+_20*b*a*x2*x-_10*b2*b*x2+_30*b*a2*x2-_20*b2*b*a*x+_20*b*a2*a*x+b2*b2*b-_10*b2*b*a2+_5*b*a2*a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni=b0-Dni;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Quasi Perpendicular
			else if (m_nFractalType == 31){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func14
						floatexp c = x;
						floatexp d = a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = (Dnr) * (x2*x2- _10*x2*y2 + _5*y2*y2) + _abs(x+a) * (_4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 -_20*x2*y*b-_10*x2*b2-_20*x*a*y2-_40*x*a*y*b-_20*x*a*b2-_10*a2*y2-_20*a2*y*b-_10*a2*b2 + _20*y2*y*b+_30*y2*b2+_20*y*b2*b+_5*b2*b2) + a0;

						//func19
						c = _5*x2*x2 - _10*x2*y2 + y2*y2;
						d = _4*b*y2*y-_20*a*x*y2+_6*b2*y2-_10*a2*y2-_20*b*x2*y-_40*a*b*x*y+_4*b2*b*y-_20*a2*b*y+_20*a*x2*x-_10*b2*x2+_30*a2*x2-_20*a*b2*x+_20*a2*a*x+b2*b2-_10*a2*b2+_5*a2*a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = -y * Dni - b * _abs(y2*y2+_4*b*y2*y-_10*x2*y2-_20*a*x*y2+_6*b2*y2-_10*a2*y2-_20*b*x2*y-_40*a*b*x*y+_4*b2*b*y-_20*a2*b*y+_5*x2*x2+_20*a*x2*x-_10*b2*x2+_30*a2*x2-_20*a*b2*x+_20*a2*a*x+b2*b2-_10*a2*b2+_5*a2*a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Quasi Heart
			else if (m_nFractalType == 32){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						//func14
						floatexp c = x;
						floatexp d = a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - _2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + _2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = (Dnr) * (x2*x2- _10*x2*y2 + _5*y2*y2) + _abs(x+a) * (_4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2 -_20*x2*y*b-_10*x2*b2-_20*x*a*y2-_40*x*a*y*b-_20*x*a*b2-_10*a2*y2-_20*a2*y*b-_10*a2*b2 + _20*y2*y*b+_30*y2*b2+_20*y*b2*b+_5*b2*b2) + a0;

						//func19
						c = _5*x2*x2 - _10*x2*y2 + y2*y2;
						d = _4*b*y2*y-_20*a*x*y2+_6*b2*y2-_10*a2*y2-_20*b*x2*y-_40*a*b*x*y+_4*b2*b*y-_20*a2*b*y+_20*a*x2*x-_10*b2*x2+_30*a2*x2-_20*a*b2*x+_20*a2*a*x+b2*b2-_10*a2*b2+_5*a2*a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - _2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + _2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = y * Dni + b * _abs(y2*y2+_4*b*y2*y-_10*x2*y2-_20*a*x*y2+_6*b2*y2-_10*a2*y2-_20*b*x2*y-_40*a*b*x*y+_4*b2*b*y-_20*a2*b*y+_5*x2*x2+_20*a*x2*x-_10*b2*x2+_30*a2*x2-_20*a*b2*x+_20*a2*a*x+b2*b2-_10*a2*b2+_5*a2*a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//FT_Simon100A_plain
			else if (m_nFractalType == 33){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;


						//Dnr = b*b*_abs(b*b)-_4*b*_abs(a*b)*a-a*a*_abs(b*b)-b*b*_abs(a*a)+a*a*_abs(a*a) + a0;
						//Dni = _2*a*a*_abs(a*b)+_2*a*b*_abs(a*a) - _2*a*b*_abs(b*b)-_2*b*b*_abs(a*b) + b0;

						Dnr = (y2)*lb_abs_exp(y2,_2*b*y+b2)-_4*y*x*lb_abs_exp(x*y,x*b+a*y+a*b)-x2*lb_abs_exp(y2,_2*b*y+b2)-y2*lb_abs_exp(x2,_2*x*a+a2)+x2*lb_abs_exp(x2,_2*x*a+a2) 
							+ (_2*b*y+b2)*_abs(y2+_2*b*y+b2)-_4*(y*a+b*x+b*a)*_abs(x*y+x*b+a*y+a*b)-(_2*x*a+a2)*_abs(y2+_2*b*y+b2)-(_2*b*y+b2)*_abs(x2+_2*x*a+a2)+(_2*x*a+a2)*_abs(x2+_2*x*a+a2) + a0;


						Dni = _2*x2*lb_abs_exp(x*y,x*b+a*y+a*b)+_2*x*y*lb_abs_exp(x2,_2*x*a+a2)-_2*x*y*lb_abs_exp(y2,_2*b*y+b2)-_2*y2*lb_abs_exp(x*y,x*b+a*y+a*b) 
							+ _2*(_2*x*a+a2)*_abs(x*y+x*b+a*y+a*b) +_2*(x*b+a*y+a*b)*_abs(x2+_2*x*a+a2)-_2*(x*b+a*y+a*b)*_abs(y2+_2*b*y+b2)-_2*(_2*b*y+b2)*_abs(x*y+x*b+a*y+a*b)  + b0;
							

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Imag Quasi Perpendicular / Heart
			else if (m_nFractalType == 34){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2 +a0;
						Dni = _4 * (a) * _abs(- y2*y-_3*b*y2+x2*y+_2*a*x*y-_3*b2*y+a2*y+b*x2+_2*a*b*x-b2*b+a2*b) + _4 * x * lb_abs_exp(- y2*y+x2*y,-_3*b*y2+_2*a*x*y-_3*b2*y+a2*y+b*x2+_2*a*b*x-b2*b+a2*b) + b0;	//func21

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Real Quasi Perpendicular
			else if (m_nFractalType == 35){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2 +a0;
						Dni = -_4*y*lb_abs_exp(x2*x-x*y2,-a*y2-_2*b*x*y-_2*a*b*y+_3*a*x2-b2*x+_3*a2*x-a*b2+a2*a) - _4*b*_abs(- x*y2-a*y2-_2*b*x*y-_2*a*b*y+x2*x+_3*a*x2-b2*x+_3*a2*x-a*b2+a2*a) + b0;	//func22

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Real Quasi Heart
			else if (m_nFractalType == 36){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr = _4*x2*x*a+_6*x2*a2+_4*x*a2*a+a2*a2+_4*y2*y*b+_6*y2*b2+_4*y*b2*b+b2*b2-_12*x2*y*b-_6*x2*b2-_12*x*a*y2-_24*x*a*y*b-_12*x*a*b2-_6*a2*y2-_12*a2*y*b-_6*a2*b2 +a0;
						Dni = _4*y*lb_abs_exp(x2*x-x*y2,-a*y2-_2*b*x*y-_2*a*b*y+_3*a*x2-b2*x+_3*a2*x-a*b2+a2*a) + _4*b*_abs(- x*y2-a*y2-_2*b*x*y-_2*a*b*y+x2*x+_3*a*x2-b2*x+_3*a2*x-a*b2+a2*a) + b0;	//func22

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Celtic Imag Quasi Perpendicular / Heart
			else if (m_nFractalType == 37){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr = lb_abs_exp(x2*x2 + y2*y2 - _6*x2*y2,_4*y2*y*b-_12*y2*a*x+_6*y2*b2-_6*y2*a2-_12*x2*y*b-_24*x*y*a*b+_4*b2*b*y-_12*b*y*a2+_4*x2*x*a-_6*x2*b2+_6*x2*a2-_12*b2*x*a+_4*a2*a*x+b2*b2-_6*b2*a2+a2*a2) +a0;
						Dni = _4 * a * _abs(- y2*y-_3*b*y2+x2*y+_2*a*x*y-_3*b2*y+a2*y+b*x2+_2*a*b*x-b2*b+a2*b) + _4 * x * lb_abs_exp(- y2*y+x2*y,-_3*b*y2+_2*a*x*y-_3*b2*y+a2*y+b*x2+_2*a*b*x-b2*b+a2*b) + b0;	//func21

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Celtic Real Quasi Perpendicular
			else if (m_nFractalType == 38){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr = lb_abs_exp(x2*x2 + y2*y2 - _6*x2*y2,_4*y2*y*b-_12*y2*a*x+_6*y2*b2-_6*y2*a2-_12*x2*y*b-_24*x*y*a*b+_4*b2*b*y-_12*b*y*a2+_4*x2*x*a-_6*x2*b2+_6*x2*a2-_12*b2*x*a+_4*a2*a*x+b2*b2-_6*b2*a2+a2*a2) +a0;
						Dni = -_4*y*lb_abs_exp(x2*x-x*y2,-a*y2-_2*b*x*y-_2*a*b*y+_3*a*x2-b2*x+_3*a2*x-a*b2+a2*a) - _4*b*_abs(- x*y2-a*y2-_2*b*x*y-_2*a*b*y+x2*x+_3*a*x2-b2*x+_3*a2*x-a*b2+a2*a) + b0;	//func22

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Celtic Real Quasi Heart
			else if (m_nFractalType == 39){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr = lb_abs_exp(x2*x2 + y2*y2 - _6*x2*y2,_4*y2*y*b-_12*y2*a*x+_6*y2*b2-_6*y2*a2-_12*x2*y*b-_24*x*y*a*b+_4*b2*b*y-_12*b*y*a2+_4*x2*x*a-_6*x2*b2+_6*x2*a2-_12*b2*x*a+_4*a2*a*x+b2*b2-_6*b2*a2+a2*a2) +a0;
						Dni = _4*y*lb_abs_exp(x2*x-x*y2,-a*y2-_2*b*x*y-_2*a*b*y+_3*a*x2-b2*x+_3*a2*x-a*b2+a2*a) + _4*b*_abs(- x*y2-a*y2-_2*b*x*y-_2*a*b*y+x2*x+_3*a*x2-b2*x+_3*a2*x-a*b2+a2*a) + b0;	//func22

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//Cubic FT_Simon100A_plain
			else if (m_nFractalType == 40){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr= _3*lb_abs_exp(x2*y,x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b)*(y2*y) - lb_abs_exp(y2*y,_3*y2*b+_3*y*b2+b2*b)*(y2*y) + _9*lb_abs_exp(x*y2,_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2)*(x*y2) - _3*lb_abs_exp(x2*x,_3*x2*a+_3*x*a2+a2*a)*(x*y2) + _3*lb_abs_exp(y2*y,_3*y2*b+_3*y*b2+b2*b)*(x2*y) - _9*lb_abs_exp(x2*y,x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b)*(x2*y) - _3*lb_abs_exp(x*y2,_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2)*(x2*x) + lb_abs_exp(x2*x,_3*x2*a+_3*x*a2+a2*a)*(x2*x)
							+ _3*_abs(x2*y+x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b)*(_3*y2*b+_3*y*b2+b2*b) - _abs(y2*y+_3*y2*b+_3*y*b2+b2*b)*(_3*y2*b+_3*y*b2+b2*b) + _9*_abs(x*y2+_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2)*(_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2) - _3*_abs(x2*x+_3*x2*a+_3*x*a2+a2*a)*(_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2) + _3*_abs(y2*y+_3*y2*b+_3*y*b2+b2*b)*(x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b) -_9*_abs(x2*y+x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b)*(x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b) - _3*_abs(x*y2+_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2)*(_3*x2*a+_3*x*a2+a2*a) + _abs(x2*x+_3*x2*a+_3*x*a2+a2*a)*(_3*x2*a+_3*x*a2+a2*a)
							+ a0;
						Dni= _3*lb_abs_exp(x*y2,_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2)*(y2*y) - lb_abs_exp(x2*x,_3*x2*a+_3*x*a2+a2*a)*(y2*y) -_9*lb_abs_exp(x2*y,x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b)*(x*y2) -_9*lb_abs_exp(x*y2,_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2)*(x2*y) + _3*lb_abs_exp(x2*x,_3*x2*a+_3*x*a2+a2*a)*(x2*y) + _3*lb_abs_exp(y2*y,_3*y2*b+_3*y*b2+b2*b)*(x*y2) - lb_abs_exp(y2*y,_3*y2*b+_3*y*b2+b2*b)*(x2*x) + _3*lb_abs_exp(x2*y,x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b)*(x2*x)
							+ _3*_abs(x*y2+_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2)*(_3*y2*b+_3*y*b2+b2*b) - _abs(x2*x+_3*x2*a+_3*x*a2+a2*a)*(_3*y2*b+_3*y*b2+b2*b) -_9*_abs(x2*y+x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b)*(_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2) -_9*_abs(x*y2+_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2)*(x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b) + _3*_abs(x2*x+_3*x2*a+_3*x*a2+a2*a)*(x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b) + _3*_abs(y2*y+_3*y2*b+_3*y*b2+b2*b)*(_2*x*y*b+x*b2+a*y2+_2*a*y*b+a*b2) - _abs(y2*y+_3*y2*b+_3*y*b2+b2*b)*(_3*x2*a+_3*x*a2+a2*a) + _3*_abs(x2*y+x2*b+_2*x*a*y+_2*x*a*b+a2*y+a2*b)*(_3*x2*a+_3*x*a2+a2*a)
							+ b0;

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//HPDZ Buffalo
			else if (m_nFractalType == 41){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr = (-_2*b*y+_2*a*x-b2+a2) - lb_abs_exp(x,a) + a0;
						Dni = lb_abs_exp(x*y,x*b+a*y+a*b) * 2.0 - lb_abs_exp(y,b) + b0;

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//TheRedshiftRider 1
			else if (m_nFractalType == 42){
				complex<floatexp> a(g_FactorAR,g_FactorAI);
				complex<floatexp> _2(2,0),_3(3,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];

						complex<floatexp> z(x,y);
						complex<floatexp> d(Dr,Di);
						complex<floatexp> d0(dbD0r,dbD0i);
						complex<floatexp> d2=d*d;
						d = a*(_2*z*d+d2)+_3*z*z*d+_3*z*d2+d2*d + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 2
			else if (m_nFractalType == 43){
				complex<floatexp> a(g_FactorAR,g_FactorAI);
				complex<floatexp> _2(2,0),_3(3,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];

						complex<floatexp> z(x,y);
						complex<floatexp> d(Dr,Di);
						complex<floatexp> d0(dbD0r,dbD0i);
						complex<floatexp> d2=d*d;
						d = a*(_2*z*d+d2)-_3*z*z*d-_3*z*d2-d2*d + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 3
			else if (m_nFractalType == 44){
				complex<floatexp> a(2,0);
				complex<floatexp> _2(2,0),_3(3,0),_4(4,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];

						complex<floatexp> z(x,y);
						complex<floatexp> d(Dr,Di);
						complex<floatexp> d0(dbD0r,dbD0i);
						complex<floatexp> d2=d*d;
						d = a*(_2*z*d+d2)-_3*z*z*d-_3*z*d2-d2*d + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 4
			else if (m_nFractalType == 45){
				complex<floatexp> a(g_FactorAR,g_FactorAI);
				complex<floatexp> _2(2,0),_4(4,0),_6(6,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];

						complex<floatexp> z(x,y);
						complex<floatexp> d(Dr,Di);
						complex<floatexp> d2=d*d;
						complex<floatexp> d0(dbD0r,dbD0i);
						d = a*(_2*z*d+d2)+_4*(z^3)*d+_6*(z^2)*d2+_4*z*(d2*d)+(d2^2) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 5
			else if (m_nFractalType == 46){
				complex<floatexp> a(g_FactorAR,g_FactorAI);
				complex<floatexp> _2(2,0),_4(4,0),_6(6,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];

						complex<floatexp> z(x,y);
						complex<floatexp> d(Dr,Di);
						complex<floatexp> d2=d*d;
						complex<floatexp> d0(dbD0r,dbD0i);
						d = a*(_2*z*d+d2)-_4*(z^3)*d-_6*(z^2)*d2-_4*z*d2*d-(d2^2) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 6
			else if (m_nFractalType == 47){
				complex<floatexp> a(g_FactorAR,g_FactorAI);
				complex<floatexp> _2(2,0),_5(5,0),_10(10,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];

						complex<floatexp> z(x,y);
						complex<floatexp> d(Dr,Di);
						complex<floatexp> d2=d*d;
						complex<floatexp> d0(dbD0r,dbD0i);
						d = a*(_2*z*d+d2)+_5*(z^4)*d+_10*(z^3)*d2+_10*(z^2)*d2*d+_5*z*(d2^2)+(d2*d2*d) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 7
			else if (m_nFractalType == 48){
				complex<floatexp> a(g_FactorAR,g_FactorAI);
				complex<floatexp> _2(2,0),_5(5,0),_10(10,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];

						complex<floatexp> z(x,y);
						complex<floatexp> d(Dr,Di);
						complex<floatexp> d2=d*d;
						complex<floatexp> d0(dbD0r,dbD0i);
						d = a*(_2*z*d+d2)-_5*(z^4)*d-_10*(z^3)*d2-_10*(z^2)*d2*d-_5*z*(d2^2)-(d2*d2*d) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 8
			else if (m_nFractalType == 49){
				complex<floatexp> a(g_FactorAR,g_FactorAI);
				complex<floatexp> _2(2,0),_6(6,0),_15(15,0),_20(20,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];

						complex<floatexp> z(x,y);
						complex<floatexp> d(Dr,Di);
						complex<floatexp> d2=d*d;
						complex<floatexp> d0(dbD0r,dbD0i);
						d = a*(_2*z*d+d2)+_6*(z^5)*d+_15*(z^4)*d2+_20*(z^3)*(d2*d)+_15*(z^2)*(d2^2)+_6*z*(d2*d2*d)+(d2^3) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 9
			else if (m_nFractalType == 50){
				complex<floatexp> a(g_FactorAR,g_FactorAI);
				complex<floatexp> _2(2,0),_6(6,0),_15(15,0),_20(20,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];

						complex<floatexp> z(x,y);
						complex<floatexp> d(Dr,Di);
						complex<floatexp> d0(dbD0r,dbD0i);
						d = a*(_2*z*d+(d^2))-_6*(z^5)*d-_15*(z^4)*(d^2)-_20*(z^3)*(d^3)-_15*(z^2)*(d^4)-_6*z*(d^5)-(d^6) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			// SimonBrot2 4th
			else if (m_nFractalType == 51){
				floatexp _2(2);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_dxr[antal] + Dr;
						yi = m_dxi[antal] + Di;
						test2 = test1;
						test1 = (real*yr*yr + imag*yi*yi).todouble();
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						floatexp &x = m_dxr[antal];
						floatexp &y = m_dxi[antal];
						floatexp &a = Dr;
						floatexp &b = Di;
						floatexp &a0 = dbD0r;
						floatexp &b0 = dbD0i;
						floatexp x2 = x*x;
						floatexp y2 = y*y;
						floatexp a2 = a*a;
						floatexp b2 = b*b;

						Dnr=(x2-y2)*lb_abs_exp(x2-y2,_2*x*a+a2-_2*y*b-b2) + (_2*x*a+a2-_2*y*b-b2)*_abs(x2+_2*x*a+a2-y2-_2*y*b-b2) 
							- (_2*x*y)*lb_abs_exp(_2*x*y,_2*x*b+_2*a*y+_2*a*b) - (_2*x*b+_2*a*y+_2*a*b)*_abs(_2*x*y+_2*x*b+_2*a*y+_2*a*b)
							+ a0;
						Dni=(x2-y2)*lb_abs_exp(_2*x*y,_2*x*b+_2*a*y+_2*a*b) + (_2*x*a+a2-_2*y*b-b2)*_abs(_2*x*y+_2*x*b+_2*a*y+_2*a*b) 
							+ (_2*x*y)*lb_abs_exp(x2-y2,_2*x*a+a2-_2*y*b-b2) + (_2*x*b+_2*a*y+_2*a*b)*_abs(x2-y2+_2*x*a+a2-_2*y*b-b2) 
							+ b0;
						Dr=Dnr;
						Di=Dni;
					}
				}
			}
		else
		{
#define P2(t,p) perturbation_floatexp_##t##_##p(m_nFractalType, m_nPower, m_dxr, m_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, Dr, Di, D0r, D0i)
#define P(t,p) P2(t,p)
			P(0,2) || P(0,3) || P(0,4) || P(0,5) || P(0,6) || P(0,7) || P(0,8) || P(0,9) || P(0,10) || // P(0,p) ||
			P(1,2) || P(1,3) || P(1,4) || P(1,5)
			;
#undef P2
#undef P
    }
#if 0
		else{
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
					yr = m_dxr[antal] + Dr;
					yi = m_dxi[antal] + Di;
					test2 = test1;
					test1 = (real*yr*yr + imag*yi*yi).todouble();
					if (test1<m_db_z[antal]){
						if (!m_bNoGlitchDetection)
							test1 = m_nBailout2 * 2;
						bGlitch = TRUE;
					}
					complex<floatexp> X(m_dxr[antal], m_dxi[antal]);
					complex<floatexp> D(Dr, Di);
					complex<floatexp> D0(D0r, D0i);
					complex<floatexp> c(m_pnExpConsts[0], 0);
					int nXExp = m_nPower - 2, nDExp = 2, ci = 1;
					complex<floatexp> Dn = c*(X^m_nPower - 1)*D;
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
#endif

		InterlockedIncrement((LPLONG)&m_nDone);
		if (antal == m_nGlitchIter)
			bGlitch = TRUE;
		if (antal == m_nMaxIter){
			m_nPixels[x][y] = antal;
			m_nTrans[x][y] = 0;
			m_lpBits[nIndex] = 0;
			m_lpBits[nIndex + 1] = 0;
			m_lpBits[nIndex + 2] = 0;
			if (!nPStep && (!bGlitch || g_bShowGlitches)){
				int q;
				int nE = nStepSize*nStepSize;
				for (q = 0; q<nE; q++){
					int tx = x + q%nStepSize;
					int ty = y + q / nStepSize;
					if (tx<m_nX - 1 && ty<m_nY - 1 && m_nPixels[tx][ty] == -1){
						int nIndex1 = tx * 3 + (m_bmi->biHeight - 1 - ty)*m_row;
						m_lpBits[nIndex1] = m_lpBits[nIndex];
						m_lpBits[nIndex1 + 1] = m_lpBits[nIndex + 1];
						m_lpBits[nIndex1 + 2] = m_lpBits[nIndex + 2];
					}
				}
			}
		}
		else{
			m_nPixels[x][y] = antal;
			if (!bGlitch && m_nSmoothMethod == 1){
				double div = sqrt(test1) - sqrt(test2);
				if (div != 0)
					m_nTrans[x][y] = (sqrt(test1) - m_nBailout) / div;
				else
					m_nTrans[x][y] = 0;
			}
			else if (!bGlitch && m_nSmoothMethod == 0){
				m_nTrans[x][y] = log(log(sqrt(test1))) / log((double)m_nPower);
				if (!ISFLOATOK(m_nTrans[x][y]))
					m_nTrans[x][y] = 0;
				while (m_nTrans[x][y]<0){
					int offs = 1 + (int)m_nTrans[x][y];
					m_nPixels[x][y] += offs;
					m_nTrans[x][y] += offs;
				}
				while (m_nTrans[x][y]>1){
					int offs = (int)m_nTrans[x][y];
					m_nPixels[x][y] -= offs;
					m_nTrans[x][y] -= offs;
				}
			}
			if (bGlitch && !m_bNoGlitchDetection){
				m_nTrans[x][y] = 2;
				m_nPixels[x][y] = m_nMaxIter - 1;//(m_nMaxApproximation?m_nMaxApproximation-1:0);
			}
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			if (m_bMirrored)
				Mirror(x, y);
			if (!nPStep && (!bGlitch || g_bShowGlitches)){
				int q;
				int nE = nStepSize*nStepSize;
				for (q = 0; q<nE; q++){
					int tx = x + q%nStepSize;
					int ty = y + q / nStepSize;
					if (tx<m_nX - 1 && ty<m_nY - 1 && m_nPixels[tx][ty] == -1){
						int nIndex1 = tx * 3 + (m_bmi->biHeight - 1 - ty)*m_row;
						m_lpBits[nIndex1] = m_lpBits[nIndex];
						m_lpBits[nIndex1 + 1] = m_lpBits[nIndex + 1];
						m_lpBits[nIndex1 + 2] = m_lpBits[nIndex + 2];
					}
				}
			}
		}
	}
}
