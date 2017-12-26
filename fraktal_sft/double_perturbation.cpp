#include "fraktal_sft.h"
#include "complex.h"
#include "../formula/formula.h"

void CFraktalSFT::MandelCalc()
{
	m_bIterChanged = TRUE;
	double Dnr, Dni, yr, yi, dr, di;
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

		// Series approximation
		double dbD0r = m_pDX[m_nX / 2] + m_C*(m_pDX[x] - m_pDX[m_nX / 2]) + m_S*(m_pDY[y] - m_pDY[m_nY / 2]);
		double dbD0i = m_pDY[m_nY / 2] - m_S*(m_pDX[x] - m_pDX[m_nX / 2]) + m_C*(m_pDY[y] - m_pDY[m_nY / 2]);
		if (m_nInflections)
		{
			int inf;
			complex<CFixedFloat> c(dbD0r,dbD0i);
			for(inf=m_nInflections-1;inf>=0;inf--){
				complex<CFixedFloat> d = c-m_pInflections[inf];
				c=m_pInflections[inf]+d*d;
			}
			dbD0r=c.m_r.ToDouble();
			dbD0i=c.m_i.ToDouble();
		}
		floatexp D0r0 = dbD0r;
		floatexp D0r = D0r0 * m_nScaling;
		floatexp D0i0 = dbD0i;
		floatexp D0i = D0i0 * m_nScaling;
		floatexp TDnr;
		floatexp TDni;
		floatexp TDDnr;
		floatexp TDDni;
		DoApproximation(antal, D0r, D0i, TDnr, TDni, TDDnr, TDDni);

		double test1 = 0, test2 = 0;
		BOOL bGlitch = FALSE;
		int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);
		if (m_nScalingOffset){
			double Dr = TDnr.todouble(m_nScalingOffset);
			double Di = TDni.todouble(m_nScalingOffset);
			long double ldcr = TDDnr.toLongDouble();
			long double ldci = TDDni.toLongDouble();
			if (m_nPower == 2){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter; antal++){
						yr = m_db_dxr[antal] + Dr*m_nScaling;
						yi = m_db_dxi[antal] + Di*m_nScaling;
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
						Dnr = (2 * m_db_dxr[antal] + Dr*m_nScaling)*Dr - (2 * m_db_dxi[antal] + Di*m_nScaling)*Di + dbD0r;
						Dni = 2 * ((m_db_dxr[antal] + Dr*m_nScaling)*Di + m_db_dxi[antal] * Dr) + dbD0i;
						ldcr = ldcnr;
						ldci = ldcni;
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			if (m_nPower == 3){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter; antal++){
						yr = m_db_dxr[antal] + Dr*m_nScaling;
						yi = m_db_dxi[antal] + Di*m_nScaling;
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
						//Dnr=3*((m_db_dxr[antal]*m_db_dxr[antal]-m_db_dxi[antal]*m_db_dxi[antal])*Dr+m_db_dxr[antal]*(Dr*Dr*m_nScaling-Di*Di*m_nScaling)-Di*(2*m_db_dxi[antal]*(m_db_dxr[antal]+Dr*m_nScaling)+Dr*m_nScaling*Di*m_nScaling))+Dr*m_nScaling*Dr*m_nScaling*Dr+dbD0r;
						//Dni=3*((m_db_dxr[antal]*m_db_dxr[antal]-m_db_dxi[antal]*m_db_dxi[antal])*Di+m_db_dxi[antal]*(Dr*Dr*m_nScaling-Di*Di*m_nScaling)+Dr*(2*m_db_dxr[antal]*(m_db_dxi[antal]+Di)+Dr*m_nScaling*Di*m_nScaling))-Di*Di*m_nScaling*Di*m_nScaling+dbD0i;
						long double ldcnr = 3 * (ldcr * (yr * yr - yi * yi) - ldci * (2 * yr * yi)) + 1;
						long double ldcni = 3 * (ldci * (yr * yr - yi * yi) + ldcr * (2 * yr * yi));
						Dnr = 3 * m_db_dxr[antal] * m_db_dxr[antal] * Dr - 6 * m_db_dxr[antal] * m_db_dxi[antal] * Di - 3 * m_db_dxi[antal] * m_db_dxi[antal] * Dr + 3 * m_db_dxr[antal] * Dr*Dr*m_nScaling - 3 * m_db_dxr[antal] * Di*Di*m_nScaling - 3 * m_db_dxi[antal] * 2 * Dr*Di*m_nScaling + Dr*Dr*Dr*m_nScaling*m_nScaling - 3 * Dr*Di*Di*m_nScaling*m_nScaling + dbD0r;
						Dni = 3 * m_db_dxr[antal] * m_db_dxr[antal] * Di + 6 * m_db_dxr[antal] * m_db_dxi[antal] * Dr - 3 * m_db_dxi[antal] * m_db_dxi[antal] * Di + 3 * m_db_dxr[antal] * 2 * Dr*Di*m_nScaling + 3 * m_db_dxi[antal] * Dr*Dr*m_nScaling - 3 * m_db_dxi[antal] * Di*Di*m_nScaling + 3 * Dr*Dr*Di*m_nScaling*m_nScaling - Di*Di*Di*m_nScaling*m_nScaling + dbD0i;
						ldcr = ldcnr;
						ldci = ldcni;
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			ldr = ldcr;
			ldi = ldci;
		}
		else
		{

			double Dr = TDnr.todouble();
			double Di = TDni.todouble();
			dr = TDDnr.todouble();
			di = TDDni.todouble();

			if (m_nFractalType == 0 && m_nPower > 10) // FIXME derivative
			{

				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_db_dxr[antal] + Dr;
						yi = m_db_dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						complex<double> X(m_db_dxr[antal], m_db_dxi[antal]);
						complex<double> D(Dr, Di);
						complex<double> D0(dbD0r, m_pDY[y]);
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

			}
			else
			{

				bool ok = perturbation_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, dr, di);
				assert(ok && "perturbation_double");

			}
		}

		long double pixel_spacing = m_lPixelSpacing;
		if (m_nScalingOffset)
		{
			ldr = ldr * pixel_spacing;
			ldi = ldi * pixel_spacing;
		}
		else
		{
			ldr = dr * pixel_spacing;
			ldi = di * pixel_spacing;
		}
		double de = sqrt(test1) * log(test1) / sqrt(ldr * ldr + ldi * ldi);

		OutputIterationData(x, y, bGlitch, antal, test1, test2, de);
		InterlockedIncrement((LPLONG)&m_nDone);
    OutputPixelData(x, y, w, h, bGlitch);
  }
}
