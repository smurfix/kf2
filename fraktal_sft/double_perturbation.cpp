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
		floatexp D0r = 0;
		floatexp D0i = 0;
		floatexp daa0 = 1;
		floatexp dab0 = 0;
		floatexp dba0 = 0;
		floatexp dbb0 = 1;
		GetPixelCoordinates(x, y, D0r, D0i, daa0, dab0, dba0, dbb0);

		floatexp TDnr;
		floatexp TDni;
		floatexp TDDnr;
		floatexp TDDni;
		DoApproximation(antal, D0r, D0i, TDnr, TDni, TDDnr, TDDni);

		double test1 = 0, test2 = 0;
		BOOL bGlitch = FALSE;
		int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);
		if (m_nScalingOffset){ // FIXME matrix derivatives
			double dbD0r = D0r.todouble(m_nScalingOffset);
			double dbD0i = D0i.todouble(m_nScalingOffset);
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
							long double Xr = m_db_dxr[antal];
							long double Xi = m_db_dxi[antal];
							long double xr = ((long double)(Dr)) * m_nScaling;
							long double xi = ((long double)(Di)) * m_nScaling;
							long double cr = ((long double)(dbD0r)) * m_nScaling;
							long double ci = ((long double)(dbD0i)) * m_nScaling;
							if (type_0_power_2_pixel_has_glitched(cr, ci, xr, xi, Xr, Xi, ldcr, ldci, (long double) (m_epsilon), m_lPixelSpacing)){
								bGlitch = TRUE;
								if (! m_bNoGlitchDetection)
									break;
							}
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
							bGlitch = TRUE;
							if (! m_bNoGlitchDetection)
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

			double dbD0r = D0r.todouble();
			double dbD0i = D0i.todouble();
			double Dr = TDnr.todouble();
			double Di = TDni.todouble();
			dr = TDDnr.todouble();
			di = TDDni.todouble();

			if (m_nFractalType == 0 && m_nPower > 10)
			{

				complex<double> d(dr, di);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter; antal++){
						yr = m_db_dxr[antal] + Dr;
						yi = m_db_dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							bGlitch = TRUE;
							if (! m_bNoGlitchDetection)
								break;
						}
						if (test1 > m_nBailout2)
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
				dr = d.m_r;
				di = d.m_i;

			}
			else // FIXME matrix derivatives
			{
				double daa = daa0.todouble();
				double dab = dab0.todouble();
				double dba = dba0.todouble();
				double dbb = dbb0.todouble();
				dr *= m_dPixelSpacing;
				di *= m_dPixelSpacing;
				bool ok = perturbation_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, dr, di, m_epsilon, m_dPixelSpacing, daa, dab, dba, dbb);
				assert(ok && "perturbation_double");

			}
		}

		long double pixel_spacing = m_lPixelSpacing;
		if (m_nScalingOffset)
		{
			ldr = ldr * pixel_spacing;
			ldi = ldi * pixel_spacing;
		}
		else if (m_nPower > 10)
		{
			ldr = dr * pixel_spacing;
			ldi = di * pixel_spacing;
		}
		else
		{
			ldr = dr;
			ldi = di;
		}
		double de = sqrt(test1) * log(test1) / sqrt(ldr * ldr + ldi * ldi);

		OutputIterationData(x, y, bGlitch, antal, test1, test2, de);
		InterlockedIncrement((LPLONG)&m_nDone);
    OutputPixelData(x, y, w, h, bGlitch);
  }
}
