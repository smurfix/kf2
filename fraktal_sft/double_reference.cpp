/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2020 Claude Heiland-Allen

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

void CFraktalSFT::CalculateReference()
{
	const bool derivatives = GetDerivatives();
	Precision prec(m_rref.m_f.precision());

	int64_t i;
	if (m_db_dxr)
		delete[] m_db_dxr;
	m_db_dxr = new double [m_nMaxIter];
	if (m_db_dxi)
		delete[] m_db_dxi;
	m_db_dxi = new double [m_nMaxIter];
	if (m_db_z)
		delete[] m_db_z;
	m_db_z = new double [m_nMaxIter];

	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;
	double terminate = SMOOTH_BAILOUT*SMOOTH_BAILOUT;
	const double nBailout = GetBailoutRadius();
	const double p = GetBailoutNorm();
	const double nBailout2 = p < 1.0/0.0 ? pow(nBailout, p) : nBailout;

	if (m_Inflections.size() > 0)
	{
		int inf;
		complex<CFixedFloat> c(m_rref,m_iref);
		for(inf=m_Inflections.size()-1;inf>=0;inf--){
			complex<CFixedFloat> d = c-m_Inflections[inf];
			c=m_Inflections[inf]+d*d;
		}
		m_rref=c.m_r;
		m_iref=c.m_i;
	}

	m_nGlitchIter = m_nMaxIter + 1;
	int64_t nMaxIter = m_nMaxIter;

	int64_t antal = 0;
	double test1 = 0;
	double test2 = 0;
	double phase = 0;
	double xxr = 0, xxi = 0;

	double dr = 1, di = 0;
	long double ldr = 1, ldi = 0;

	if (m_nFractalType == 0 && m_nPower > 10) // FIXME matrix derivatives, option to disable derivatives
	{

		double threashold = 0.0001;
		for (i = 7; i <= m_nPower; i += 2)
			threashold *= 10;
		if (GetGlitchLowTolerance()) {
			threashold = sqrt(threashold);
		}
		if (threashold>.5)
			threashold = .5;
		complex<CFixedFloat> r(m_rref, m_iref);
		complex<CFixedFloat> X(g_SeedR, g_SeedI);
		complex<double> d(1.0, 0.0);
		bool stored = false;
		double old_absval = 0;
		double abs_val = 0;
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			complex<double> x(X.m_r.ToDouble(), X.m_i.ToDouble());
			d = m_nPower * d * (x^(m_nPower - 1)) + 1;
			X = (X^m_nPower) + r;
			m_db_dxr[i] = X.m_r.ToDouble();
			m_db_dxi[i] = X.m_i.ToDouble();
			old_absval = abs_val;
			abs_val = (m_db_dxr[i] * m_db_dxr[i] + m_db_dxi[i] * m_db_dxi[i]);
			m_db_z[i] = abs_val*threashold;
			if (abs_val >= 4)
			{
				if (terminate == 4 && !stored)
				{
					stored = true;
					antal = i;
					test1 = abs_val;
					test2 = old_absval;
				}
			}
			if (abs_val >= terminate){
				if (terminate > 4 && !stored)
				{
					stored = true;
					antal = i;
					test1 = abs_val;
					test2 = old_absval;
				}
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
			m_nRDone++;
		}
		dr = d.m_r;
		di = d.m_i;
		ldr = dr * m_lPixelSpacing;
		ldi = di * m_lPixelSpacing;

	}
	else if (m_nScalingOffset && scaling_supported(m_nFractalType, m_nPower, derivatives))
	{

		floatexp _x, _y, daa, dab, dba, dbb;
		GetPixelCoordinates(g_nAddRefX, g_nAddRefY, _x, _y, daa, dab, dba, dbb);
		long double ddaa = daa.todouble();
		long double ddab = dab.todouble();
		long double ddba = dba.todouble();
		long double ddbb = dbb.todouble();
		ldr *= m_lPixelSpacing;
		ldi *= m_lPixelSpacing;
		bool ok = derivatives
		  ? reference(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, p, GetGlitchLowTolerance(), antal, test1, test2, xxr, xxi, ldr, ldi, ddaa, ddab, ddba, ddbb)
		  : reference(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, p, GetGlitchLowTolerance(), antal, test1, test2, xxr, xxi)
		  ;
		assert(ok && "reference_scaled_double");

	}
	else
	{

		floatexp _x, _y, daa, dab, dba, dbb;
		GetPixelCoordinates(g_nAddRefX, g_nAddRefY, _x, _y, daa, dab, dba, dbb);
		double ddaa = daa.todouble();
		double ddab = dab.todouble();
		double ddba = dba.todouble();
		double ddbb = dbb.todouble();
		dr *= m_dPixelSpacing;
		di *= m_dPixelSpacing;
		bool ok = derivatives
		  ? reference(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, p, GetGlitchLowTolerance(), antal, test1, test2, xxr, xxi, dr, di, ddaa, ddab, ddba, ddbb)
		  : reference(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, p, GetGlitchLowTolerance(), antal, test1, test2, xxr, xxi)
		  ;
		assert(ok && "reference_double");
		ldr = dr;
		ldi = di;
	}
}
