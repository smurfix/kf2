#include "fraktal_sft.h"
#include "floatexp.h"
#include "complex.h"
#include "../formula/formula.h"

static int Perturbation_Var(int antal,const long double *dxr,const long double *dxi, long double Dr, long double Di, long double D0r, long double D0i,double &test1, double &test2, int m_nBailout2, int m_nMaxIter,const double *m_db_z,BOOL &bGlitch,int m_nPower,const int *m_pnExpConsts, long double &dr, long double &di)
{ // FIXME derivative
(void) dr, di;
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
	int antal, x, y, w, h;

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

		// series approximation
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

		floatexp D0r = lD0r;
		// D0r *= m_nScaling;
		floatexp D0i = lD0i;
		// D0i *= m_nScaling;
		floatexp TDnr;
		floatexp TDni;
		floatexp TDDnr;
		floatexp TDDni;
		if (m_nMaxApproximation){
			antal = m_nMaxApproximation - 1;
			TDnr = m_APr[0] * D0r - m_APi[0] * D0i;
			TDni = m_APr[0] * D0i + m_APi[0] * D0r;
			TDDnr = m_APr[0];
			TDDni = m_APi[0];
			floatexp D_r = D0r;
			floatexp D_i = D0i;
			for (int k = 1; k < m_nTerms; k++)
			{
				TDDnr += (m_APr[k] * D_r - m_APi[k] * D_i) * floatexp(k + 1.0);
				TDDni += (m_APr[k] * D_i + m_APi[k] * D_r) * floatexp(k + 1.0);
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
			TDDnr = 1.0;
			TDDni = 0.0;
		}


		long double Dr, Di, dr, di;
		Dr = TDnr.toLongDouble();
		Di = TDni.toLongDouble();
		dr = TDDnr.toLongDouble();
		di = TDDni.toLongDouble();
		double test1 = 0, test2 = 0;
		BOOL bGlitch = FALSE;
		int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);

		if (m_nFractalType == 0 && m_nPower > 10)
		{

			// FIXME check this is still ok around long double vs scaled double zoom threshold e600
			antal = Perturbation_Var(antal, m_ldxr, m_ldxi, Dr, Di, lD0r, lD0i, test1, test2, m_nBailout2, nMaxIter, m_db_z, bGlitch, m_nPower, m_pnExpConsts, dr, di);

		}
		else
		{
			bool ok = perturbation_long_double(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, lD0r, lD0i, dr, di);
			assert(ok && "perturbation_long_double");
		}

		long double pixel_spacing = m_lPixelSpacing;
		dr *= pixel_spacing;
		di *= pixel_spacing;
		double de = sqrt(test1) * log(test1) / sqrt(dr * dr + di * di);
		OutputIterationData(x, y, bGlitch, antal, test1, test2, de);

		InterlockedIncrement((LPLONG)&m_nDone);
		OutputPixelData(x, y, w, h, bGlitch);
	}
}
