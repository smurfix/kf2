#include "fraktal_sft.h"
#include "floatexp.h"
#include "complex.h"
#include "../formula/formula.h"

static int Perturbation_Var(int antal,const long double *dxr,const long double *dxi, long double Dr, long double Di, long double D0r, long double D0i,double &test1, double &test2, int m_nBailout2, int m_nMaxIter,const double *m_db_z,BOOL &bGlitch,int m_nPower,const int *m_pnExpConsts)
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
                test1 = m_nBailout2*2;
            bGlitch=TRUE;
        }
      if(test1 > m_nBailout2)
        break;
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

void CFraktalSFT::MandelCalcLDBL()
{
	m_bIterChanged = TRUE;
	int antal, x, y;
	int nPStep, nStepSize;

	while (!m_bStop && m_P.GetPixel(x, y)){
		nStepSize = nPStep = m_P.GetStep();
		if (nPStep>1)
			nPStep = 0;
		else
			nPStep = 1;
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
		if (m_nPixels[x][y] != -1){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			continue;
		}
#ifdef GUESS
		if (nPStep && nStepSize==1){
			if (x && x<m_nX - 1 && m_nPixels[x - 1][y] != -1 && m_nPixels[x - 1][y] == m_nPixels[x + 1][y]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y] + m_nTrans[x + 1][y])*.5;
				if (m_nTrans[x][y] == TRANS_GLITCH)
					m_nTrans[x - 1][y] = TRANS_GLITCH;
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
		// Series approximation - Start
		floatexp D0r;
		floatexp D0i;

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
		floatexp TDnr;
		floatexp TDni;
		if (m_nMaxApproximation){
			antal = m_nMaxApproximation - 1;
			TDnr = m_APr[0] * D0r - m_APi[0] * D0i;
			TDni = m_APr[0] * D0i + m_APi[0] * D0r;
			floatexp D_r = D0r*D0r - D0i*D0i;
			floatexp D_i = (D0r*D0i).mul2();
			TDnr += m_APr[1] * D_r - m_APi[1] * D_i;
			TDni += m_APr[1] * D_i + m_APi[1] * D_r;
			int k;
			for (k = 2; k<m_nTerms; k++){
				floatexp  t = D_r*D0r - D_i*D0i;
				D_i = D_r*D0i + D_i*D0r;
				D_r = t;
				TDnr += m_APr[k] * D_r - m_APi[k] * D_i;
				TDni += m_APr[k] * D_i + m_APi[k] * D_r;
			}
		}
		else{
			antal = 0;
			TDnr = D0r;
			TDni = D0i;
		}
		long double Dr, Di;
		Dr = TDnr.toLongDouble();
		Di = TDni.toLongDouble();
		double test1 = 0, test2 = 0;
		BOOL bGlitch = FALSE;
		int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);

    if (m_nFractalType == 0 && m_nPower > 10)
    {

			// FIXME check this is still ok around long double vs scaled double zoom threshold e600
			antal = Perturbation_Var(antal, m_ldxr, m_ldxi, Dr, Di, lD0r, lD0i, test1, test2, m_nBailout2, nMaxIter, m_db_z, bGlitch, m_nPower, m_pnExpConsts);

		}
		else
		{
			int antal2 = antal;
			bool ok = perturbation_long_double(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, antal2, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, lD0r, lD0i);
			assert(ok && "perturbation_long_double");
			antal = antal2;
		}

		OutputIterationData(x, y, bGlitch, antal, test1, test2);

		InterlockedIncrement((LPLONG)&m_nDone);
		if (!nPStep && (!bGlitch || GetShowGlitches())){
			int q;
			int nE = nStepSize*nStepSize;
			for (q = 0; q<nE; q++){
				int tx = x + q%nStepSize;
				int ty = y + q / nStepSize;
				if (tx<m_nX && ty<m_nY && m_nPixels[tx][ty] == -1){
					int nIndex1 = tx * 3 + (m_bmi->biHeight - 1 - ty)*m_row;
					m_lpBits[nIndex1] = m_lpBits[nIndex];
					m_lpBits[nIndex1 + 1] = m_lpBits[nIndex + 1];
					m_lpBits[nIndex1 + 2] = m_lpBits[nIndex + 2];
				}
			}
		}
	}
}
