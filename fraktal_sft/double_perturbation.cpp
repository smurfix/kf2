/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2019 Claude Heiland-Allen

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

	// vectorization
	double16 Dr16, Di16, dbD0r16, dbD0i16, test116, test216;
	int16 antal16, bGlitch16, x16, y16, w16, h16;
	int k = 0;
	const int chunksize = GetSIMDChunkSize();
	const int vectorsize = GetSIMDVectorSize();
	const bool vectorized = ! GetDerivatives() && (m_nFractalType == 0 ? ! (m_nPower > 10) : true) && vectorsize >= 1;

	int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);
	while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
		if (m_nPixels[x][y] != PIXEL_UNEVALUATED){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y, w, h);
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
		floatexp dxa1, dxb1, dya1, dyb1;
		DoApproximation(antal, D0r, D0i, TDnr, TDni, dxa1, dxb1, dya1, dyb1);
		floatexp TDDnr = dxa1;
		floatexp TDDni = dya1;

		double test1 = 0, test2 = 0;
		BOOL bGlitch = FALSE;
#if 0
		if (m_nScalingOffset){ // FIXME matrix derivatives
			double dbD0r = D0r.todouble(m_nScalingOffset);
			double dbD0i = D0i.todouble(m_nScalingOffset);
			double Dr = TDnr.todouble(m_nScalingOffset);
			double Di = TDni.todouble(m_nScalingOffset);
			long double ldcr = TDDnr.toLongDouble();
			long double ldci = TDDni.toLongDouble();
			if (m_nPower == 2){
				if (GetDerivatives())
				{
				if (antal<nMaxIter && test1 <= m_nBailout2){
					bool no_g = g_real == 1.0 && g_imag == 1.0;
					for (; antal<nMaxIter; antal++){
						yr = m_db_dxr[antal] + Dr*m_nScaling;
						yi = m_db_dxi[antal] + Di*m_nScaling;
						test2 = test1;
						if (no_g)
							test1 = yr*yr + yi*yi;
						else
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
				} else {
				if (antal<nMaxIter && test1 <= m_nBailout2){
					bool no_g = g_real == 1.0 && g_imag == 1.0;
					for (; antal<nMaxIter; antal++){
						yr = m_db_dxr[antal] + Dr*m_nScaling;
						yi = m_db_dxi[antal] + Di*m_nScaling;
						test2 = test1;
						if (no_g)
							test1 = yr*yr + yi*yi;
						else
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
						Dnr = (2 * m_db_dxr[antal] + Dr*m_nScaling)*Dr - (2 * m_db_dxi[antal] + Di*m_nScaling)*Di + dbD0r;
						Dni = 2 * ((m_db_dxr[antal] + Dr*m_nScaling)*Di + m_db_dxi[antal] * Dr) + dbD0i;
						Di = Dni;
						Dr = Dnr;
					}
				}
			  }
			}
			if (m_nPower == 3){
				if (GetDerivatives())
				{
				if (antal<nMaxIter && test1 <= m_nBailout2){
					bool no_g = g_real == 1.0 && g_imag == 1.0;
					for (; antal<nMaxIter; antal++){
						yr = m_db_dxr[antal] + Dr*m_nScaling;
						yi = m_db_dxi[antal] + Di*m_nScaling;
						test2 = test1;
						if (no_g)
							test1 = yr*yr + yi*yi;
						else
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
				} else {
				if (antal<nMaxIter && test1 <= m_nBailout2){
					bool no_g = g_real == 1.0 && g_imag == 1.0;
					for (; antal<nMaxIter; antal++){
						yr = m_db_dxr[antal] + Dr*m_nScaling;
						yi = m_db_dxi[antal] + Di*m_nScaling;
						test2 = test1;
						if (no_g)
							test1 = yr*yr + yi*yi;
						else
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
						Dnr = 3 * m_db_dxr[antal] * m_db_dxr[antal] * Dr - 6 * m_db_dxr[antal] * m_db_dxi[antal] * Di - 3 * m_db_dxi[antal] * m_db_dxi[antal] * Dr + 3 * m_db_dxr[antal] * Dr*Dr*m_nScaling - 3 * m_db_dxr[antal] * Di*Di*m_nScaling - 3 * m_db_dxi[antal] * 2 * Dr*Di*m_nScaling + Dr*Dr*Dr*m_nScaling*m_nScaling - 3 * Dr*Di*Di*m_nScaling*m_nScaling + dbD0r;
						Dni = 3 * m_db_dxr[antal] * m_db_dxr[antal] * Di + 6 * m_db_dxr[antal] * m_db_dxi[antal] * Dr - 3 * m_db_dxi[antal] * m_db_dxi[antal] * Di + 3 * m_db_dxr[antal] * 2 * Dr*Di*m_nScaling + 3 * m_db_dxi[antal] * Dr*Dr*m_nScaling - 3 * m_db_dxi[antal] * Di*Di*m_nScaling + 3 * Dr*Dr*Di*m_nScaling*m_nScaling - Di*Di*Di*m_nScaling*m_nScaling + dbD0i;
						Di = Dni;
						Dr = Dnr;
					}
				}
				}
			}
			ldr = ldcr;
			ldi = ldci;
		}
		else
#endif

		{

			double dbD0r = D0r.todouble();
			double dbD0i = D0i.todouble();
			double Dr = TDnr.todouble();
			double Di = TDni.todouble();
			dr = TDDnr.todouble();
			di = TDDni.todouble();

			if (m_nFractalType == 0 && m_nPower > 10)
			{
				if (GetDerivatives())
				{
					complex<double> d(dr, di);
					bool no_g = g_real == 1.0 && g_imag == 1.0;
					if (antal<nMaxIter && test1 <= m_nBailout2){
						for (; antal<nMaxIter; antal++){
							yr = m_db_dxr[antal] + Dr;
							yi = m_db_dxi[antal] + Di;
							test2 = test1;
							if (no_g)
								test1 = yr*yr + yi*yi;
							else
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
				else
				{
					if (antal<nMaxIter && test1 <= m_nBailout2){
						bool no_g = g_real == 1.0 && g_imag == 1.0;
						for (; antal<nMaxIter; antal++){
							yr = m_db_dxr[antal] + Dr;
							yi = m_db_dxi[antal] + Di;
							test2 = test1;
							if (no_g)
								test1 = yr*yr + yi*yi;
							else
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
				}

			}
			else // FIXME matrix derivatives
			{
				if (vectorized)
				{
				  x16[k] = x;
				  y16[k] = y;
				  w16[k] = w;
				  h16[k] = h;
				  Dr16[k] = m_nScalingOffset ? TDnr.todouble(m_nScalingOffset) : Dr;
				  Di16[k] = m_nScalingOffset ? TDni.todouble(m_nScalingOffset) : Di;
				  dbD0r16[k] = m_nScalingOffset ? D0r.todouble(m_nScalingOffset) : dbD0r;
				  dbD0i16[k] = m_nScalingOffset ? D0i.todouble(m_nScalingOffset) : dbD0i;
				  antal16[k] = antal;
				  test116[k] = test1;
				  test216[k] = test2;
				  bGlitch16[k] = bGlitch;
				  k = k + 1;
				  if (k == vectorsize)
				  {
				    if (vectorsize == 1)
				    {
				      int antalv, bGlitchv;
				      double test1v, test2v, Drv, Div, dbD0rv, dbD0iv;
				      for (int q = 0; q < vectorsize; ++q)
				      {
					antalv = antal16[q];
					bGlitchv = bGlitch16[q];
					test1v = test116[q];
					test2v = test216[q];
					Drv = Dr16[q];
					Div = Di16[q];
					dbD0rv = dbD0r16[q];
					dbD0iv = dbD0i16[q];
				      }
				      bool ok = m_nScalingOffset
				        ? perturbation_scaled_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, m_nScaling, 1 / m_nScaling)
					: perturbation_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv)
					;
				      assert(ok && "perturbation_double");
				      for (int q = 0; q < vectorsize; ++q)
				      {
					antal16[q] = antalv;
					bGlitch16[q] = bGlitchv;
					test116[q] = test1v;
					test216[q] = test2v;
					Dr16[q] = Drv;
					Di16[q] = Div;
					dbD0r16[q] = dbD0rv;
					dbD0i16[q] = dbD0iv;
				      }
				    }
				    else if (vectorsize == 2)
				    {
				      int2 antalv, bGlitchv;
				      double2 test1v, test2v, Drv, Div, dbD0rv, dbD0iv;
				      for (int q = 0; q < vectorsize; ++q)
				      {
					antalv[q] = antal16[q];
					bGlitchv[q] = bGlitch16[q];
					test1v[q] = test116[q];
					test2v[q] = test216[q];
					Drv[q] = Dr16[q];
					Div[q] = Di16[q];
					dbD0rv[q] = dbD0r16[q];
					dbD0iv[q] = dbD0i16[q];
				      }
				      bool ok = m_nScalingOffset
				        ? perturbation2_scaled_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize, m_nScaling, 1 / m_nScaling)
					: perturbation2_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize)
					;
				      assert(ok && "perturbation_double2");
				      for (int q = 0; q < vectorsize; ++q)
				      {
					antal16[q] = antalv[q];
					bGlitch16[q] = bGlitchv[q];
					test116[q] = test1v[q];
					test216[q] = test2v[q];
					Dr16[q] = Drv[q];
					Di16[q] = Div[q];
					dbD0r16[q] = dbD0rv[q];
					dbD0i16[q] = dbD0iv[q];
				      }
				    }
				    else if (vectorsize == 4)
				    {
				      int4 antalv, bGlitchv;
				      double4 test1v, test2v, Drv, Div, dbD0rv, dbD0iv;
				      for (int q = 0; q < vectorsize; ++q)
				      {
					antalv[q] = antal16[q];
					bGlitchv[q] = bGlitch16[q];
					test1v[q] = test116[q];
					test2v[q] = test216[q];
					Drv[q] = Dr16[q];
					Div[q] = Di16[q];
					dbD0rv[q] = dbD0r16[q];
					dbD0iv[q] = dbD0i16[q];
				      }
				      bool ok = m_nScalingOffset
				        ? perturbation4_scaled_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize, m_nScaling, 1 / m_nScaling)
					: perturbation4_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize)
					;
				      assert(ok && "perturbation_double4");
				      for (int q = 0; q < vectorsize; ++q)
				      {
					antal16[q] = antalv[q];
					bGlitch16[q] = bGlitchv[q];
					test116[q] = test1v[q];
					test216[q] = test2v[q];
					Dr16[q] = Drv[q];
					Di16[q] = Div[q];
					dbD0r16[q] = dbD0rv[q];
					dbD0i16[q] = dbD0iv[q];
				      }
				    }
				    else if (vectorsize == 8)
				    {
				      int8 antalv, bGlitchv;
				      double8 test1v, test2v, Drv, Div, dbD0rv, dbD0iv;
				      for (int q = 0; q < vectorsize; ++q)
				      {
					antalv[q] = antal16[q];
					bGlitchv[q] = bGlitch16[q];
					test1v[q] = test116[q];
					test2v[q] = test216[q];
					Drv[q] = Dr16[q];
					Div[q] = Di16[q];
					dbD0rv[q] = dbD0r16[q];
					dbD0iv[q] = dbD0i16[q];
				      }
				      bool ok = m_nScalingOffset
				        ? perturbation8_scaled_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize, m_nScaling, 1 / m_nScaling)
					: perturbation8_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize)
					;
				      assert(ok && "perturbation_double8");
				      for (int q = 0; q < vectorsize; ++q)
				      {
					antal16[q] = antalv[q];
					bGlitch16[q] = bGlitchv[q];
					test116[q] = test1v[q];
					test216[q] = test2v[q];
					Dr16[q] = Drv[q];
					Di16[q] = Div[q];
					dbD0r16[q] = dbD0rv[q];
					dbD0i16[q] = dbD0iv[q];
				      }
				    }
				    else if (vectorsize == 16)
				    {
				      int16 antalv, bGlitchv;
				      double16 test1v, test2v, Drv, Div, dbD0rv, dbD0iv;
				      for (int q = 0; q < vectorsize; ++q)
				      {
					antalv[q] = antal16[q];
					bGlitchv[q] = bGlitch16[q];
					test1v[q] = test116[q];
					test2v[q] = test216[q];
					Drv[q] = Dr16[q];
					Div[q] = Di16[q];
					dbD0rv[q] = dbD0r16[q];
					dbD0iv[q] = dbD0i16[q];
				      }
				      bool ok = m_nScalingOffset
				        ? perturbation16_scaled_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize, m_nScaling, 1 / m_nScaling)
					: perturbation16_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antalv, test1v, test2v, bGlitchv, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Drv, Div, dbD0rv, dbD0iv, chunksize)
					;
				      assert(ok && "perturbation_double16");
				      for (int q = 0; q < vectorsize; ++q)
				      {
					antal16[q] = antalv[q];
					bGlitch16[q] = bGlitchv[q];
					test116[q] = test1v[q];
					test216[q] = test2v[q];
					Dr16[q] = Drv[q];
					Di16[q] = Div[q];
					dbD0r16[q] = dbD0rv[q];
					dbD0i16[q] = dbD0iv[q];
				      }
				    }
				    else
				    {
				      assert(! "valid vectorsize");
				    }
				  }
				}
				else
				{
				  double daa = daa0.todouble();
				  double dab = dab0.todouble();
				  double dba = dba0.todouble();
				  double dbb = dbb0.todouble();
				  dr *= m_dPixelSpacing;
				  di *= m_dPixelSpacing;
				  bool ok = GetDerivatives()
				    ? perturbation_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, dr, di, m_epsilon, m_dPixelSpacing, daa, dab, dba, dbb)
				    : perturbation_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i)
				    ;
				  assert(ok && "perturbation_double");
				}
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
		double de = GetDerivatives()
		  ? sqrt(test1) * log(test1) / sqrt(ldr * ldr + ldi * ldi)
		  : 0
		  ;

                if (vectorized)
		{
		  if (k == vectorsize)
		  {
		    for (k = 0; k < vectorsize; ++k)
		    {
		      OutputIterationData(x16[k], y16[k], w16[k], h16[k], bGlitch16[k], antal16[k], test116[k], test216[k], 0);
		      InterlockedIncrement((LPLONG)&m_nDone);
		      OutputPixelData(x16[k], y16[k], w16[k], h16[k], bGlitch16[k]);
		    }
		    k = 0;
		  }
		}
		else
		{
		  OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, de);
		  InterlockedIncrement((LPLONG)&m_nDone);
		  OutputPixelData(x, y, w, h, bGlitch);
		}
	}
	if (vectorized)
	{
		int leftover = k;
		for (k = 0; k < leftover; ++k)
		{
			x = x16[k];
			y = y16[k];
			w = w16[k];
			h = h16[k];
			antal = antal16[k];
			double test1 = test116[k];
			double test2 = test216[k];
			int bGlitch = bGlitch16[k];
			double Dr = Dr16[k];
			double Di = Di16[k];
			double dbD0r = dbD0r16[k];
			double dbD0i = dbD0i16[k];
			bool ok = m_nScalingOffset
			  ? perturbation_scaled_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i, m_nScaling, 1 / m_nScaling)
			  : perturbation_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i)
			  ;
			assert(ok && "perturbation_double");
			OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, 0);
			InterlockedIncrement((LPLONG)&m_nDone);
			OutputPixelData(x, y, w, h, bGlitch);
		}
	}
}
