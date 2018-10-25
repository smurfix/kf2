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
#include "floatexp.h"
#include "complex.h"
#include "../formula/formula.h"

static int Perturbation_Var(int antal,const long double *dxr,const long double *dxi, long double Dr, long double Di, long double D0r, long double D0i,double &test1, double &test2, int m_nBailout2, int m_nMaxIter,const double *m_db_z,BOOL &bGlitch,int m_nPower,const int *m_pnExpConsts, const bool m_bNoGlitchDetection)
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
        bGlitch=TRUE;
        if (! m_bNoGlitchDetection)
          break;
      }
      complex<long double> y(yr, yi);
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

static int Perturbation_Var(int antal,const long double *dxr,const long double *dxi, long double Dr, long double Di, long double D0r, long double D0i,double &test1, double &test2, int m_nBailout2, int m_nMaxIter,const double *m_db_z,BOOL &bGlitch,int m_nPower,const int *m_pnExpConsts, long double &dr, long double &di, const bool m_bNoGlitchDetection)
{
  complex<long double> d(dr, di);
  long double yr, yi;
  bGlitch=FALSE;
  if(antal<m_nMaxIter && test1 <= m_nBailout2){
    for(;antal<m_nMaxIter;antal++){
      yr=dxr[antal]+Dr;
      yi=dxi[antal]+Di;
      test2=test1;
      test1 = g_real*yr*yr + g_imag*yi*yi;
      if(test1<m_db_z[antal]){
        bGlitch=TRUE;
        if (! m_bNoGlitchDetection)
          break;
      }
      complex<long double> y(yr, yi);
      d = m_nPower * d * (y ^ (m_nPower - 1)) + 1;
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
  dr = d.m_r;
  di = d.m_i;
  return antal;
}

void CFraktalSFT::MandelCalcLDBL()
{
	m_bIterChanged = TRUE;
	int antal, x, y, w, h;
	long double dr = 0, di = 0;
	floatexp ldcr = 0, ldci = 0;

	while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
		if (m_nPixels[x][y] != PIXEL_UNEVALUATED){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			if (m_bMirrored)
				Mirror(x, y);
			continue;
		}
		if (GuessPixel(x, y, w, h))
			continue;
		// Series approximation - Start
		floatexp D0r = 0;
		floatexp D0i = 0;
		floatexp daa = 1;
		floatexp dab = 0;
		floatexp dba = 0;
		floatexp dbb = 1;
		GetPixelCoordinates(x, y, D0r, D0i, daa, dab, dba, dbb);

		floatexp TDnr;
		floatexp TDni;
		floatexp dxa1, dxb1, dya1, dyb1;
		DoApproximation(antal, D0r, D0i, TDnr, TDni, dxa1, dxb1, dya1, dyb1);
		floatexp TDDnr = dxa1;
		floatexp TDDni = dya1;

		double test1 = 0, test2 = 0;
		BOOL bGlitch = FALSE;
		int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);

		if (m_nScalingOffsetL){ // FIXME matrix derivatives
			ldcr = TDDnr;
			ldci = TDDni;
			long double lD0r = D0r.toLongDouble(m_nScalingOffsetL);
			long double lD0i = D0i.toLongDouble(m_nScalingOffsetL);
			long double Dr = TDnr.toLongDouble(m_nScalingOffsetL);
			long double Di = TDni.toLongDouble(m_nScalingOffsetL);
			if (m_nPower == 2){
			if (GetDerivatives())
			{
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter; antal++){
						double yr = m_ldxr[antal] + Dr*m_nScalingL;
						double yi = m_ldxi[antal] + Di*m_nScalingL;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							floatexp Xr(m_ldxr[antal]);
							floatexp Xi(m_ldxi[antal]);
							floatexp xr = (floatexp(Dr)) * m_nScalingL;
							floatexp xi = (floatexp(Di)) * m_nScalingL;
							floatexp cr = (floatexp(lD0r)) * m_nScalingL;
							floatexp ci = (floatexp(lD0i)) * m_nScalingL;
							if (type_0_power_2_pixel_has_glitched(cr, ci, xr, xi, Xr, Xi, ldcr, ldci, floatexp(m_epsilon), m_fPixelSpacing)){
								bGlitch = TRUE;
								if (! m_bNoGlitchDetection)
									break;
							}
							bGlitch = TRUE;
							if (! m_bNoGlitchDetection)
								break;
						}
						if (test1 > m_nBailout2)
						{
							break;
						}
						floatexp ldcnr = 2 * (ldcr * yr - ldci * yi) + 1;
						floatexp ldcni = 2 * (ldcr * yi + ldci * yr);
						long double Dnr = (2 * m_ldxr[antal] + Dr*m_nScalingL)*Dr - (2 * m_ldxi[antal] + Di*m_nScalingL)*Di + lD0r;
						long double Dni = 2 * ((m_ldxr[antal] + Dr*m_nScalingL)*Di + m_ldxi[antal] * Dr) + lD0i;
						ldcr = ldcnr;
						ldci = ldcni;
						Di = Dni;
						Dr = Dnr;
					}
				}
			} else {
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter; antal++){
						double yr = m_ldxr[antal] + Dr*m_nScalingL;
						double yi = m_ldxi[antal] + Di*m_nScalingL;
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
						long double Dnr = (2 * m_ldxr[antal] + Dr*m_nScalingL)*Dr - (2 * m_ldxi[antal] + Di*m_nScalingL)*Di + lD0r;
						long double Dni = 2 * ((m_ldxr[antal] + Dr*m_nScalingL)*Di + m_ldxi[antal] * Dr) + lD0i;
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
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						double yr = m_ldxr[antal] + Dr*m_nScalingL;
						double yi = m_ldxi[antal] + Di*m_nScalingL;
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
						floatexp ldcnr = 3 * (ldcr * (yr * yr - yi * yi) - ldci * (2 * yr * yi)) + 1;
						floatexp ldcni = 3 * (ldci * (yr * yr - yi * yi) + ldcr * (2 * yr * yi));
						//Dnr=3*((m_ldxr[antal]*m_ldxr[antal]-m_ldxi[antal]*m_ldxi[antal])*Dr+m_ldxr[antal]*(Dr*Dr*m_nScalingL-Di*Di*m_nScalingL)-Di*(2*m_ldxi[antal]*(m_ldxr[antal]+Dr*m_nScalingL)+Dr*m_nScalingL*Di*m_nScalingL))+Dr*m_nScalingL*Dr*m_nScalingL*Dr+dbD0r;
						//Dni=3*((m_ldxr[antal]*m_ldxr[antal]-m_ldxi[antal]*m_ldxi[antal])*Di+m_ldxi[antal]*(Dr*Dr*m_nScalingL-Di*Di*m_nScalingL)+Dr*(2*m_ldxr[antal]*(m_ldxi[antal]+Di)+Dr*m_nScalingL*Di*m_nScalingL))-Di*Di*m_nScalingL*Di*m_nScalingL+dbD0i;
						long double Dnr = 3 * m_ldxr[antal] * m_ldxr[antal] * Dr - 6 * m_ldxr[antal] * m_ldxi[antal] * Di - 3 * m_ldxi[antal] * m_ldxi[antal] * Dr + 3 * m_ldxr[antal] * Dr*Dr*m_nScalingL - 3 * m_ldxr[antal] * Di*Di*m_nScalingL - 3 * m_ldxi[antal] * 2 * Dr*Di*m_nScalingL + Dr*Dr*Dr*m_nScalingL*m_nScalingL - 3 * Dr*Di*Di*m_nScalingL*m_nScalingL + lD0r;
						long double Dni = 3 * m_ldxr[antal] * m_ldxr[antal] * Di + 6 * m_ldxr[antal] * m_ldxi[antal] * Dr - 3 * m_ldxi[antal] * m_ldxi[antal] * Di + 3 * m_ldxr[antal] * 2 * Dr*Di*m_nScalingL + 3 * m_ldxi[antal] * Dr*Dr*m_nScalingL - 3 * m_ldxi[antal] * Di*Di*m_nScalingL + 3 * Dr*Dr*Di*m_nScalingL*m_nScalingL - Di*Di*Di*m_nScalingL*m_nScalingL + lD0i;
						ldcr = ldcnr;
						ldci = ldcni;
						Di = Dni;
						Dr = Dnr;
					}
				}
			} else {
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						double yr = m_ldxr[antal] + Dr*m_nScalingL;
						double yi = m_ldxi[antal] + Di*m_nScalingL;
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
						//Dnr=3*((m_ldxr[antal]*m_ldxr[antal]-m_ldxi[antal]*m_ldxi[antal])*Dr+m_ldxr[antal]*(Dr*Dr*m_nScalingL-Di*Di*m_nScalingL)-Di*(2*m_ldxi[antal]*(m_ldxr[antal]+Dr*m_nScalingL)+Dr*m_nScalingL*Di*m_nScalingL))+Dr*m_nScalingL*Dr*m_nScalingL*Dr+dbD0r;
						//Dni=3*((m_ldxr[antal]*m_ldxr[antal]-m_ldxi[antal]*m_ldxi[antal])*Di+m_ldxi[antal]*(Dr*Dr*m_nScalingL-Di*Di*m_nScalingL)+Dr*(2*m_ldxr[antal]*(m_ldxi[antal]+Di)+Dr*m_nScalingL*Di*m_nScalingL))-Di*Di*m_nScalingL*Di*m_nScalingL+dbD0i;
						long double Dnr = 3 * m_ldxr[antal] * m_ldxr[antal] * Dr - 6 * m_ldxr[antal] * m_ldxi[antal] * Di - 3 * m_ldxi[antal] * m_ldxi[antal] * Dr + 3 * m_ldxr[antal] * Dr*Dr*m_nScalingL - 3 * m_ldxr[antal] * Di*Di*m_nScalingL - 3 * m_ldxi[antal] * 2 * Dr*Di*m_nScalingL + Dr*Dr*Dr*m_nScalingL*m_nScalingL - 3 * Dr*Di*Di*m_nScalingL*m_nScalingL + lD0r;
						long double Dni = 3 * m_ldxr[antal] * m_ldxr[antal] * Di + 6 * m_ldxr[antal] * m_ldxi[antal] * Dr - 3 * m_ldxi[antal] * m_ldxi[antal] * Di + 3 * m_ldxr[antal] * 2 * Dr*Di*m_nScalingL + 3 * m_ldxi[antal] * Dr*Dr*m_nScalingL - 3 * m_ldxi[antal] * Di*Di*m_nScalingL + 3 * Dr*Dr*Di*m_nScalingL*m_nScalingL - Di*Di*Di*m_nScalingL*m_nScalingL + lD0i;
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			}
			dr = (ldcr * m_fPixelSpacing).toLongDouble();
			di = (ldci * m_fPixelSpacing).toLongDouble();
		}
		else if (m_nFractalType == 0 && m_nPower > 10) // FIXME matrix derivatives
		{
			dr = TDDnr.toLongDouble();
			di = TDDni.toLongDouble();
			long double lD0r = D0r.toLongDouble();
			long double lD0i = D0i.toLongDouble();
			long double Dr = TDnr.toLongDouble();
			long double Di = TDni.toLongDouble();
			// FIXME check this is still ok around long double vs scaled double zoom threshold e600
			antal = GetDerivatives()
			  ? Perturbation_Var(antal, m_ldxr, m_ldxi, Dr, Di, lD0r, lD0i, test1, test2, m_nBailout2, nMaxIter, m_db_z, bGlitch, m_nPower, m_pnExpConsts, dr, di, m_bNoGlitchDetection)
			  : Perturbation_Var(antal, m_ldxr, m_ldxi, Dr, Di, lD0r, lD0i, test1, test2, m_nBailout2, nMaxIter, m_db_z, bGlitch, m_nPower, m_pnExpConsts, m_bNoGlitchDetection)
			  ;
			long double pixel_spacing = m_lPixelSpacing;
			dr *= pixel_spacing;
			di *= pixel_spacing;
		}
		else
		{
			dr = TDDnr.toLongDouble();
			di = TDDni.toLongDouble();
			long double lD0r = D0r.toLongDouble();
			long double lD0i = D0i.toLongDouble();
			long double Dr = TDnr.toLongDouble();
			long double Di = TDni.toLongDouble();
			long double ldaa = daa.toLongDouble();
			long double ldab = dab.toLongDouble();
			long double ldba = dba.toLongDouble();
			long double ldbb = dbb.toLongDouble();
			int antal2 = antal;
			dr *= m_lPixelSpacing;
			di *= m_lPixelSpacing;
			bool ok = GetDerivatives()
			  ? perturbation_long_double(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, antal2, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, lD0r, lD0i, dr, di, m_epsilon, m_lPixelSpacing, ldaa, ldab, ldba, ldbb)
			  : perturbation_long_double(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, antal2, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, lD0r, lD0i)
			  ;
			antal = antal2;
			assert(ok && "perturbation_long_double");
		}

		double de = GetDerivatives()
		  ? sqrt(test1) * log(test1) / hypotl(dr, di)
		  : 0
		  ;
		OutputIterationData(x, y, bGlitch, antal, test1, test2, de);

		InterlockedIncrement((LPLONG)&m_nDone);
		OutputPixelData(x, y, w, h, bGlitch);
	}
}
