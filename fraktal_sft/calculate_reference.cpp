/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2021 Claude Heiland-Allen

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
#include "reference.h"
#include "complex.h"
#include "../formula/formula.h"
#include "../common/barrier.h"

void CFraktalSFT::CalculateReference(const enum Reference_Type reftype)
{
	Precision prec(m_rref.m_f.precision());

	if (m_Reference)
	{
		reference_delete(m_Reference);
		m_Reference = nullptr;
	}
	m_Reference = reference_new(m_nMaxIter, m_ReferenceStrictZero, reftype, m_UseHybridFormula ? false : is_convergent(m_nFractalType, m_nPower), m_UseHybridFormula ? 1 : reference_glitches(m_nFractalType, m_nPower));

	double terminate = SMOOTH_BAILOUT*SMOOTH_BAILOUT;
	int64_t nMaxIter = m_nMaxIter;

	if (m_UseHybridFormula)
	{
		bool ok = reference_hybrid(m_HybridFormula, m_Reference, m_bStop, m_nRDone, m_nMaxIter, m_rref, m_iref, m_SeedR, m_SeedI, terminate, GetGlitchLowTolerance());
		assert(ok && "calculate_reference_hybrid");
	}

	else if (m_nZoom > 300 && CalculateReferenceThreaded())
	{
		// nop
	}

	else if (m_nFractalType == 0 && m_nPower > 10)
	{
		CFixedFloat xr = m_SeedR, xi = m_SeedI;
		double threshold = 0.0001;
		for (int i = 7; i <= m_nPower; i += 2)
			threshold *= 10;
		threshold = std::exp(std::log(threshold) * (1 - GetGlitchLowTolerance() / 2));
		if (threshold>.5)
			threshold = .5;
		for (int64_t i = 0; i<nMaxIter && !m_bStop; i++){
			complex<CFixedFloat> X(xr, xi), r(m_rref, m_iref);
			complex<CFixedFloat> Xn = (X^m_nPower) + r;
			xr = Xn.m_r;
			xi = Xn.m_i;
			floatexp X0 = floatexp(xr);
			floatexp Y0 = floatexp(xi);
			floatexp abs_val = X0 * X0 + Y0 * Y0;
			floatexp Z0 = abs_val*threshold;
			reference_append(m_Reference, X0, Y0, Z0);
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 10;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
				}
			}
			m_nRDone++;
		}
	}

	else
	{
		bool ok = reference(m_nFractalType, m_nPower, m_Reference, m_bStop, m_nRDone, m_nMaxIter, m_rref, m_iref, m_SeedR, m_SeedI, m_FactorAR, m_FactorAI, terminate, GetGlitchLowTolerance());
		assert(ok && "calculate_reference");
	}
}
