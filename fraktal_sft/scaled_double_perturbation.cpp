#include "fraktal_sft.h"
#include "complex.h"
#include "../formula/formula.h"

void CFraktalSFT::MandelCalcSDouble()
{
	m_bIterChanged = TRUE;
	if (m_nFractalType == 0 && m_nPower == 2)
	{

		long double ldr = 0, ldi = 0;
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
			floatexp D0r, D0i;
			{
				long double lD0r, mmr, lD0i, mmi, c, s, t1, t2;
				c = m_C;
				s = m_S;
				mmr = m_lDX[x] - m_lDX[m_nX / 2];
				mmi = m_lDY[y] - m_lDY[m_nY / 2];
		
				t1 = c * mmr;
				t2 = s * mmi;
				lD0r = m_lDX[m_nX / 2] + t1;
				lD0r = lD0r + t2;
		
				t1 = s * mmr;
				t2 = c * mmi;
				lD0i = m_lDY[m_nY / 2] - t1;
				lD0i = lD0i + t2;
	
				D0r = lD0r;
				D0i = lD0i;
	    }
			floatexp TDnr;
			floatexp TDni;
			floatexp TDDnr;
			floatexp TDDni;
			DoApproximation(antal, D0r, D0i, TDnr, TDni, TDDnr, TDDni);
	
			double test1 = 0, test2 = 0;
			BOOL bGlitch = FALSE;
			int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);
	
			sdouble sdbD0r(D0r);
			sdouble sdbD0i(D0i);
			sdouble Dr(TDnr);
			sdouble Di(TDni);
			long double ldcr(TDDnr);
			long double ldci(TDDni);
			if (antal<nMaxIter && test1 <= m_nBailout2){
				for (; antal<nMaxIter; antal++){
					double yr = m_db_dxr[antal] + Dr;
					double yi = m_db_dxi[antal] + Di;
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
					long double ldcnr = 2 * (ldcr * yr - ldci * yi) + 1;
					long double ldcni = 2 * (ldcr * yi + ldci * yr);
					sdouble Dnr = (2 * m_db_dxr[antal] + Dr)*Dr - (2 * m_db_dxi[antal] + Di)*Di + sdbD0r;
					sdouble Dni = 2 * ((m_db_dxr[antal] + Dr)*Di + m_db_dxi[antal] * Dr) + sdbD0i;
					ldcr = ldcnr;
					ldci = ldcni;
					Di = Dni;
					Dr = Dnr;
				}
			}
			ldr = ldcr;
			ldi = ldci;

			long double pixel_spacing = m_lPixelSpacing;
			ldr = ldr * pixel_spacing;
			ldi = ldi * pixel_spacing;
			double de = sqrt(test1) * log(test1) / sqrt(ldr * ldr + ldi * ldi);
	
			OutputIterationData(x, y, bGlitch, antal, test1, test2, de);
			InterlockedIncrement((LPLONG)&m_nDone);
	    OutputPixelData(x, y, w, h, bGlitch);
		}

  }
  else
  {
		assert(! "sdouble for fractaltype/power != 0/2");
	}
}
