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
#include "nanomb2.h"

void CFraktalSFT::MandelCalcNANOMB2()
{
	bool interior_checking = GetInteriorChecking();
	m_bIterChanged = TRUE;
	int x, y, w, h;
	while (!m_bStop && m_P.GetPixel(x, y, w, h, m_bMirrored)){
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
		if (m_nPixels[x][y] != PIXEL_UNEVALUATED){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y, w, h);
			if (m_bMirrored)
				Mirror(x, y);
			continue;
		}
		if (GuessPixel(x, y, w, h))
			continue;

		floatexp D0r = 0;
		floatexp D0i = 0;
		floatexp daa = 1;
		floatexp dab = 0;
		floatexp dba = 0;
		floatexp dbb = 1;
		GetPixelCoordinates(x, y, D0r, D0i, daa, dab, dba, dbb);
		
		complex<floatexp> dc(D0r, D0i);
		int bGlitch = 0, antal = 0;
		double test1 = 0, test2 = 0, de = 0;

		int maxsi = m_nMaxIter; // FIXME
		if (m_NanoMB2Ref)
			NanoMB2_Pixel(m_NanoMB2Ref, dc, m_fPixelSpacing, maxsi, m_nMaxIter, bGlitch, antal, test1, test2, de, interior_checking);
		if (antal > m_nMaxIter) antal = m_nMaxIter;

		OutputIterationData(x, y, w, h, bGlitch, antal, test1, test2, de);
		InterlockedIncrement((LPLONG)&m_nDone);
		OutputPixelData(x, y, w, h, bGlitch);
	}
}
