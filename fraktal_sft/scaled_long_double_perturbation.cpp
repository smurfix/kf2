#include "fraktal_sft.h"
#include "complex.h"
#include "../formula/formula.h"

void CFraktalSFT::MandelCalcSLDouble()
{
	m_bIterChanged = TRUE;
	if (m_nFractalType == 0 && m_nPower == 2)
	{

		floatexp ldr = 0, ldi = 0;
		int antal, x, y, w, h;
	
		while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
			int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
			if (m_nPixels[x][y] != -1){
				SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
				continue;
			}
	    if (GuessPixel(x, y, w, h))
	      continue;
	
			// series approximation
			floatexp c = m_C;
			floatexp s = m_S;
			floatexp dbD0r = m_DX[m_nX / 2] + c*(m_DX[x] - m_DX[m_nX / 2]) + s*(m_DY[y] - m_DY[m_nY / 2]);
			floatexp dbD0i = m_DY[m_nY / 2] - s*(m_DX[x] - m_DX[m_nX / 2]) + c*(m_DY[y] - m_DY[m_nY / 2]);
			floatexp D0r = dbD0r;//(cr-rref);
			floatexp D0i = dbD0i;
			floatexp TDnr;
			floatexp TDni;
			floatexp TDDnr;
			floatexp TDDni;
			DoApproximation(antal, D0r, D0i, TDnr, TDni, TDDnr, TDDni);
	
			double test1 = 0, test2 = 0;
			BOOL bGlitch = FALSE;
			int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);
	
			sldouble sdbD0r(D0r);
			sldouble sdbD0i(D0i);
			sldouble Dr(TDnr);
			sldouble Di(TDni);
			floatexp ldcr(TDDnr);
			floatexp ldci(TDDni);
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter; antal++){
					long double yr = m_ldxr[antal] + Dr;
					long double yi = m_ldxi[antal] + Di;
					test2 = test1;
					test1 = g_real*yr*yr + g_imag*yi*yi;
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
					floatexp ldcnr = 2 * (ldcr * yr - ldci * yi) + 1;
					floatexp ldcni = 2 * (ldcr * yi + ldci * yr);
					sldouble Dnr = (2 * m_ldxr[antal] + Dr)*Dr - (2 * m_ldxi[antal] + Di)*Di + sdbD0r;
					sldouble Dni = 2 * ((m_ldxr[antal] + Dr)*Di + m_ldxi[antal] * Dr) + sdbD0i;
					ldcr = ldcnr;
					ldci = ldcni;
					Di = Dni;
					Dr = Dnr;
				}
			}
			ldr = ldcr;
			ldi = ldci;

			floatexp pixel_spacing = m_fPixelSpacing;
			ldr = ldr * pixel_spacing;
			ldi = ldi * pixel_spacing;
			double de = sqrt(test1) * log(test1) / double(sqrt(ldr * ldr + ldi * ldi));
	
			OutputIterationData(x, y, bGlitch, antal, test1, test2, de);
			InterlockedIncrement((LPLONG)&m_nDone);
	    OutputPixelData(x, y, w, h, bGlitch);
		}

  }
  else
  {
		assert(! "sldouble for fractaltype/power != 0/2");
	}
}
