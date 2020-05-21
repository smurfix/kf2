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
#include "nanomb1.h"
#include "nanomb1.inc"
extern int64_t g_period;

void CFraktalSFT::CalculateReferenceNANOMB1()
{
	if (! (m_nFractalType == 0 && m_nPower == 2))
		return;

	Precision prec(m_rref.m_f.precision());
	complex<decNumber> c(m_rref.m_f, m_iref.m_f);
	int m = GetOrderM();
	int n = GetOrderN() / 2;
	int64_t period = g_period ? g_period : 1;
	const double er = GetBailoutRadius();
	const double er2 = er * er;
	floatexp r0(m_fPixelSpacing * hypot(m_nX, m_nY));

	if (m_NanoMB1Ref)
	{
		delete m_NanoMB1Ref;
		m_NanoMB1Ref = nullptr;
	}
	m_NanoMB1Ref = NanoMB1_Reference_Calculate(c, m, n, period, m_nMaxIter, r0, er2, GetGlitchLowTolerance(), m_bStop, m_nRDone);
}
