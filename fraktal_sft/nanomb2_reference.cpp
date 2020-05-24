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
#include "nanomb2.h"
#include "nanomb2.inc"
extern int64_t g_period;

void CFraktalSFT::CalculateReferenceNANOMB2()
{
	if (! (m_nFractalType == 0 && m_nPower == 2))
		return;

	Precision prec(m_rref.m_f.precision());
	complex<decNumber> c(m_rref.m_f, m_iref.m_f);
	int m = GetOrderM();
	int n = GetOrderN();
	int64_t maxperiod = g_period ? g_period : m_nMaxIter;
	floatexp r0(m_fPixelSpacing * hypot(m_nX, m_nY));
	floatexp radius_scale(GetRadiusScale());

	if (m_NanoMB2Ref)
	{
		delete m_NanoMB2Ref;
		m_NanoMB2Ref = nullptr;
	}
	m_NanoMB2Ref = NanoMB2_Reference_Calculate(c, m, n, maxperiod, r0, m_bStop, m_nRDone, radius_scale);
}
