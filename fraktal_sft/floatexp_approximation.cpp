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

void CFraktalSFT::CalculateApproximation(int nType)
{
	m_nApprox = 0;
	if (GetNoApprox() || GetApproximationType() == SeriesType_None || m_nZoom < 3 /* g_nRefZero */)
	{
		m_nMaxApproximation = 0;
		return;
	}
	bool isC = GetApproximationType() == SeriesType_Complex;
	bool isR = GetApproximationType() == SeriesType_Real;
	const double nBailout = 2; // FIXME hardcoded
	const double nBailout2 = nBailout * nBailout;

#if 0
	// disabled for now because of severe underskipping problems
	double nProbeRatio = 1.0 / 65536.0;
	double r = sqrt(nProbeRatio);
	int nProbeX = ceil(fabs(m_rApprox.right - m_rApprox.left) * r);
	int nProbeY = ceil(fabs(m_rApprox.bottom - m_rApprox.top) * r);
	if (nProbeX < 3) nProbeX = 3;
	if (nProbeY < 3) nProbeY = 3;
#else
	int nProbeX = 3;
	int nProbeY = 3;
#endif
	int nProbe = nProbeX * nProbeY - 1;
	floatexp _1 = 1;
	floatexp _3 = 3;
	floatexp *dbTr = new floatexp[nProbe];
	floatexp *dbTi = new floatexp[nProbe];
	floatexp *dbTr0 = new floatexp[nProbe];
	floatexp *dbTi0 = new floatexp[nProbe];

	POINT *p = new POINT[nProbe];
	int k = 0;
	if (GetExponentialMap())
	{
		// probe points equally spaced along top edge
		for (int i = 0; i < nProbe; ++i)
		{
			p[k].x = i * (m_nX / nProbe);
			p[k].y = 0;
			++k;
		}
	}
	else
	{
		// probe points equally spaced across image
		for (int j = 0; j < nProbeY; ++j)
		{
			int y = m_rApprox.top + j * (m_rApprox.bottom - m_rApprox.top) / (nProbeY - 1);
			if (y < m_rApprox.top) y = m_rApprox.top;
			if (y >= m_rApprox.bottom) y = m_rApprox.bottom - 1;
			for (int i = 0; i < nProbeX; ++i)
			{
				if (i == nProbeX/2 && j == nProbeY/2)
					continue;
				int x = m_rApprox.left + i * (m_rApprox.right - m_rApprox.left) / (nProbeX - 1);
				if (x < m_rApprox.left) x = m_rApprox.left;
				if (x >= m_rApprox.right) x = m_rApprox.right - 1;
				p[k].x = x;
				p[k].y = y;
				++k;
			}
		}
	}
	assert(k == nProbe);

	for (int j = 0; j<nProbe; j++){
		GetPixelCoordinates(p[j].x, p[j].y, dbTr0[j], dbTi0[j]);
		dbTr[j] = dbTr0[j];
		dbTi[j] = dbTi0[j];
	}
	int64_t maxIter = reference_size_x(m_Reference);
	m_nMaxApproximation = maxIter;
	//floatexp mindiff = (m_nBailout==2?0.000001:0.001);
	floatexp mindiff = std::exp(std::log(0.001) + (std::log(0.00001) - std::log(0.001)) * GetApproxLowTolerance());
	//	if(dbTr[0]<0 && dbTi[0]<1e-16 && dbTi[0]>-1e-16)
	//		mindiff = 0.0000001;
	if (GetNoApprox())
		mindiff = 0;
	if (GetAutoApproxTerms()){
		int nT = sqrt((double)m_count_queued)*0.021;
		if ((GetFractalType() == 0 && GetPower() > 2) || (GetFractalType() == 1 && GetPower() == 2))
			nT = 3;
		SetApproxTerms(nT);
	}
	int m_nTerms = GetApproxTerms();

	// clear approximation
	if (isC)
		for (int i = 0; i < m_nTerms; i++)
		{
			m_APr[i] = 0;
			m_APi[i] = 0;
		}
	if (isR)
		for (int i = 0; i <= m_nTerms; ++i)
			for (int j = 0; j <= m_nTerms - i; ++j)
			{
				m_APs->s[i][j] = 0;
				m_APs->t[i][j] = 0;
			}

	floatexp xr;
	floatexp xi;
	floatexp *APr = isC ? new floatexp[m_nTerms] : nullptr;
	floatexp *APi = isC ? new floatexp[m_nTerms] : nullptr;
	SeriesR2<double, int64_t> *APs = isR ? new SeriesR2<double, int64_t> : nullptr;

	const bool rescaled = isC && m_nFractalType == 0 && m_nPower == 2 && GetUseRescaledSeries();
	floatexp T = 1;
	double one_over_t = double(floatexp(1) / T);
	complex<double> a[2][MAX_APPROX_TERMS+1];
	complex<floatexp> A[2][MAX_APPROX_TERMS+1];
	int src = 0, dst = 1;
	// copy approximation
	if (isC)
	{
		if (rescaled)
		{
			for (int k = 0; k < m_nTerms; ++k)
			{
				a[src][k] = 0;
				a[dst][k] = 0;
				A[src][k] = complex<floatexp>(0);
				A[dst][k] = complex<floatexp>(0);
			}
		}
		else
		{
			for (int i = 0; i < m_nTerms; i++)
			{
				APr[i] = m_APr[i];
				APi[i] = m_APi[i];
			}
		}
	}
	if (isR)
		for (int i = 0; i <= m_nTerms; ++i)
			for (int j = 0; j <= m_nTerms - i; ++j)
			{
				APs->s[i][j] = m_APs->s[i][j];
				APs->t[i][j] = m_APs->t[i][j];
			}

	int64_t K = 0;
	for (int64_t iteration = 0; iteration<maxIter - 1 && !m_bStop; iteration++){
		m_nApprox++;

		// get reference
		int64_t n = iteration - 1;
		if (iteration <= 0)
			xr = xi = 0;
		else
		{
			reference_get(m_Reference, K, n, xr, xi);
		}

		// copy approximation
		if (isC)
		{
			if (rescaled)
			{
				std::swap(src, dst);
			}
			else
			{
				for (int i = 0; i < m_nTerms; i++)
				{
					APr[i] = m_APr[i];
					APi[i] = m_APi[i];
				}
			}
		}
		if (isR)
			for (int i = 0; i <= m_nTerms; ++i)
				for (int j = 0; j <= m_nTerms - i; ++j)
				{
					APs->s[i][j] = m_APs->s[i][j];
					APs->t[i][j] = m_APs->t[i][j];
				}

		// step approximation
		if (m_nFractalType == 0 && isC)
		{
			if (m_nPower == 2)
			{
				if (rescaled)
				{
					const complex<floatexp> Z(xr, xi);
					const complex<double> z((double(xr)), (double(xi)));
					bool ok = false;
					bool retry_full = false;
					do
					{
						ok = true;
						// do a full iteration if any of z and a[src][k] are small
						bool full = retry_full || (std::abs(z.m_r) < 1e-300 && std::abs(z.m_i) < 1e-300);
						for (int k = 0; k < m_nTerms; ++k)
						{
							if (full)
							{
								break;
							}
							full |= std::abs(a[src][k].m_r) < 1e-300 && std::abs(a[src][k].m_i) < 1e-300;
						}
						if (full)
						{
							// full iteration
							for (int k = 0; k < m_nTerms; ++k)
							{
								A[dst][k] = complex<floatexp>(0);
								int q = k / 2;
								int i = 0;
								int j = k - 1;
								while (q)
								{
									A[dst][k] += A[src][i] * A[src][j];
									q--;
									i++;
									j--;
								}
								A[dst][k].m_r = A[dst][k].m_r.mul2();
								A[dst][k].m_i = A[dst][k].m_i.mul2();
								if (k % 2)
								{
									A[dst][k] += A[src][k/2] * A[src][k/2];
								}
								A[dst][k] += 2 * Z * A[src][k];
							}
							A[dst][0].m_r += 1;
							// rescale
							T = 0;
							for (int k = 0; k < m_nTerms; ++k)
							{
								floatexp Ak2 = A[dst][k].m_r * A[dst][k].m_r + A[dst][k].m_i * A[dst][k].m_i;
								if (Ak2.val > 0)
								{
									T = std::max(T, exp(log(Ak2) / (2 * (k + 1))));
								}
							}
							one_over_t = double(floatexp(1) / T);
							floatexp Tk = 1;
							for (int k = 0; k < m_nTerms; ++k)
							{
								Tk *= T;
								a[dst][k] = complex<double>(A[dst][k] / Tk);
							}
						}
						else
						{
							// rescaled iteration
							for (int k = 0; k < m_nTerms; ++k)
							{
								a[dst][k] = 0;
								int q = k / 2;
								int i = 0;
								int j = k - 1;
								while (q)
								{
									a[dst][k] += a[src][i] * a[src][j];
									q--;
									i++;
									j--;
								}
								a[dst][k] *= 2;
								if (k % 2)
								{
									a[dst][k] += a[src][k/2] * a[src][k/2];
								}
								a[dst][k] += 2 * z * a[src][k];
							}
							a[dst][0].m_r += one_over_t;
							// rescale
							floatexp Tk = 1;
							for (int k = 0; k < m_nTerms; ++k)
							{
								Tk *= T;
								A[dst][k] = complex<floatexp>(a[dst][k]) * Tk;
							}
							T = 0;
							for (int k = 0; k < m_nTerms; ++k)
							{
								floatexp Ak2 = A[dst][k].m_r * A[dst][k].m_r + A[dst][k].m_i * A[dst][k].m_i;
								if (Ak2.val > 0)
								{
									T = std::max(T, exp(log(Ak2) / (2 * (k + 1))));
								}
							}
							one_over_t = double(floatexp(1) / T);
							Tk = 1;
							for (int k = 0; k < m_nTerms; ++k)
							{
								Tk *= T;
								a[dst][k] = complex<double>(A[dst][k] / Tk);
							}
							// some a[dst][k] can underflow at small Z even with rescaling
							// if that happens, do a full iteration instead
							for (int k = 0; k < m_nTerms; ++k)
							{
								if (retry_full)
								{
									break;
								}
								retry_full |= std::abs(a[dst][k].m_r) < 1e-300 && std::abs(a[dst][k].m_i) < 1e-300;
							}
							if (retry_full)
							{
								ok = false; // retry
							}
						}
					} while (! ok);
				}
				else
				{
					m_APr[0] = (xr*APr[0] - xi*APi[0]).mul2() + _1;
					m_APi[0] = (xi*APr[0] + xr*APi[0]).mul2();
					for (int k = 1; k<m_nTerms; k++){
						m_APr[k] = (xr*APr[k] - xi*APi[k]).mul2();
						m_APi[k] = (xi*APr[k] + xr*APi[k]).mul2();
						int n = k / 2;
						int q = n;
						int f = 0;
						int r = k - 1;
						while (q){
							m_APr[k] += (APr[f] * APr[r] - APi[f] * APi[r]).mul2();
							m_APi[k] += (APi[f] * APr[r] + APr[f] * APi[r]).mul2();
							q--;
							f++;
							r--;
						}
						if (k % 2){
							m_APr[k] += APr[n] * APr[n] - APi[n] * APi[n];
							m_APi[k] += (APr[n] * APi[n]).mul2();
						}
					}
				}
			}
			else
			{
				int P = m_nPower;
				complex<floatexp> X(xr, xi), A(APr[0], APi[0]), B(APr[1], APi[1]), C(APr[2], APi[2]);
				complex<floatexp> An = P * A * (X^(P-1)) + 1;
				complex<floatexp> Bn = P * B * (X^(P-1)) + ((P*(P-1))/2) * (A^2) * (X^(P-2));
				complex<floatexp> Cn = P * C * (X^(P-1)) + (P*(P-1)) * A * B * (X^(P-2)) + ((P*(P-1)*(P-2))/6) * (A^3) * (X^(P-3));
				m_APr[0] = An.m_r;
				m_APi[0] = An.m_i;
				m_APr[1] = Bn.m_r;
				m_APi[1] = Bn.m_i;
				m_APr[2] = Cn.m_r;
				m_APi[2] = Cn.m_i;
			}
		}
		else if (m_nFractalType == 1 && isR)
		{
			if (m_nPower == 2)
			{

	enum { Q0, Q1, Q2, Q3 } q;
	if ((xr * xi).val >= 0) q = Q0; else q = Q3;
	for (int i = 0; i <= m_nTerms; ++i)
	{
		for (int j = 0; j <= m_nTerms - i; ++j)
		{
			// s
			floatexp total = 0;
			if (i == 1 && j == 0) total += 1;
			total += 2 * (xr * APs->s[i][j] - xi * APs->t[i][j]);
			for (int k = 0; k <= i; ++k)
			{
				const int ik = i - k;
				for (int l = 0; l <= j; ++l)
				{
					const int jl = j - l;
					total += APs->s[k][l] * APs->s[ik][jl] - APs->t[k][l] * APs->t[ik][jl];
				}
			}
			m_APs->s[i][j] = total;
			// t;
			total = 0;
			total += xr * APs->t[i][j] + xi * APs->s[i][j];
			for (int k = 0; k <= i; ++k)
			{
				const int ik = i - k;
				for (int l = 0; l <= j; ++l)
				{
					const int jl = j - l;
					total += APs->s[k][l] * APs->t[ik][jl];
				}
			}
			if (i == 0 && j == 0 && (q == Q1 || q == Q2)) total += 2 * xr * xi;
			total = total.mul2();
			if (q == Q1 || q == Q3) total = -total;
			if (i == 0 && j == 1) total += 1;
			m_APs->t[i][j] = total;
		}
	}

			}
		}

			/*
			An+1 = 2XnAn + 1
			Bn+1 = 2XnBn + An2
			Cn+1 = 2XnCn + 2AnBn (33097)
			Dn+1 = 2XnDn + 2AnCn + Bn2
			En+1 = 2XnEn + 2AnDn + 2BnCn (38613)
			Fn+1 = 2XnFn + 2AnEn + 2BnDn + Cn2 (38613)
			Gn+1 = 2XnGn + 2AnFn + 2BnEn + 2CnDn
			Hn+1 = 2XnHn + 2AnGn + 2BnFn + 2CnEn + Dn2
			In+1 = 2XnIn + 2AnHn + 2BnGn + 2CnFn + 2DnEn
			*/
			if (iteration<m_nMaxApproximation)
		  {
				floatexp dxr, dxi;
				int64_t old_K = K;
			  reference_get(m_Reference, K, iteration, dxr, dxi);
			  K = old_K;
				int j = 0;
				for (j = 0; j<nProbe; j++){

					// do approximation
					floatexp Dnr, Dni;
					if (isC)
					{
						if (rescaled)
						{
							if (m_nMaxApproximation)
							{
								const complex<floatexp> C(dbTr0[j], dbTi0[j]);
								complex<floatexp> Z(0);
								for (int k = m_nTerms - 1; k >= 0; --k)
								{
									Z += A[dst][k];
									Z *= C;
								}
								Dnr = Z.m_r;
								Dni = Z.m_i;
							}
							else
							{
								Dnr = dbTr0[j];
								Dni = dbTi0[j];
							}
						}
						else
						{
							int64_t antal_;
							floatexp DDnr_, DDni_;
							DoApproximation(antal_, dbTr0[j], dbTi0[j], Dnr, Dni, DDnr_, DDni_);
						}
					}
					if (isR)
						DoApproximation(dbTr0[j], dbTi0[j], Dnr, Dni);

					// compare with probe point
					floatexp diff = (Dnr - dbTr[j]) / dbTr[j];
					if (diff>mindiff || diff<-mindiff){
						m_nMaxApproximation = iteration;
						break;
					}
					diff = (Dni - dbTi[j]) / dbTi[j];
					if (diff>mindiff || diff<-mindiff){
						m_nMaxApproximation = iteration;
						break;
					}
					double yr = double(dxr + Dnr);
					double yi = double(dxi + Dni);
					if (yr*yr + yi*yi>nBailout2){
						m_nMaxApproximation = iteration;
						break;
					}

					// step probe point using perturbation
					if (m_nFractalType == 0)
					{

						if (m_nPower == 2){
							Dnr = (dxr*dbTr[j] - dxi*dbTi[j]).mul2() + dbTr[j] * dbTr[j] - dbTi[j] * dbTi[j] + dbTr0[j];
							Dni = (dxr*dbTi[j] + dxi*dbTr[j] + dbTr[j] * dbTi[j]).mul2() + dbTi0[j];
						}
						else if (m_nPower == 3){
							Dnr = _3*((dxr*dxr - dxi*dxi)*dbTr[j] + dxr*(dbTr[j] * dbTr[j] - dbTi[j] * dbTi[j]) - dbTi[j] * (dxi*(dxr + dbTr[j]).mul2() + dbTr[j] * dbTi[j])) + dbTr[j] * dbTr[j] * dbTr[j] + dbTr0[j];
							Dni = _3*((dxr*dxr - dxi*dxi)*dbTi[j] + dxi*(dbTr[j] * dbTr[j] - dbTi[j] * dbTi[j]) + dbTr[j] * (dxr*(dxi + dbTi[j]).mul2() + dbTr[j] * dbTi[j])) - dbTi[j] * dbTi[j] * dbTi[j] + dbTi0[j];
						}
						else if (m_nPower == 4){
							complex<floatexp> X(dxr, dxi);
							complex<floatexp> D(dbTr[j], dbTi[j]);
							complex<floatexp> D0(dbTr0[j], dbTi0[j]);
							complex<floatexp> _4(4, 0), _6(6, 0);
							complex<floatexp> Dn = _4*(X ^ 3)*D + _6*(X ^ 2)*(D ^ 2) + _4*X*(D ^ 3) + (D ^ 4) + D0;
							Dni = Dn.m_i;
							Dnr = Dn.m_r;
						}
						else if (m_nPower == 5){
							complex<floatexp> X(dxr, dxi);
							complex<floatexp> D(dbTr[j], dbTi[j]);
							complex<floatexp> D0(dbTr0[j], dbTi0[j]);
							complex<floatexp> _5(5, 0), _10(10, 0);
							complex<floatexp> Dn = _5*(X ^ 4)*D + _10*(X ^ 3)*(D ^ 2) + _10*(X ^ 2)*(D ^ 3) + _5*X*(D ^ 4) + (D ^ 5) + D0;
							Dni = Dn.m_i;
							Dnr = Dn.m_r;
						}
						else if (m_nPower == 6){
							complex<floatexp> X(dxr, dxi);
							complex<floatexp> D(dbTr[j], dbTi[j]);
							complex<floatexp> D0(dbTr0[j], dbTi0[j]);
							complex<floatexp> _6(6, 0), _15(15, 0), _20(20, 0);
							complex<floatexp> Dn = _6*(X ^ 5)*D + _15*(X ^ 4)*(D ^ 2) + _20*(X ^ 3)*(D ^ 3) + _15*(X ^ 2)*(D ^ 4) + _6*X*(D ^ 5) + (D ^ 6) + D0;
							Dni = Dn.m_i;
							Dnr = Dn.m_r;
						}
						else if (m_nPower == 7){
							complex<floatexp> X(dxr, dxi);
							complex<floatexp> D(dbTr[j], dbTi[j]);
							complex<floatexp> D0(dbTr0[j], dbTi0[j]);
							complex<floatexp> _7(7, 0), _21(21, 0), _35(35, 0);
							complex<floatexp> Dn = _7*(X ^ 6)*D + _21*(X ^ 5)*(D ^ 2) + _35*(X ^ 4)*(D ^ 3) + _35*(X ^ 3)*(D ^ 4) + _21*(X ^ 2)*(D ^ 5) + _7*X*(D ^ 6) + (D ^ 7) + D0;
							Dni = Dn.m_i;
							Dnr = Dn.m_r;
						}
						else if (m_nPower == 8){
							complex<floatexp> X(dxr, dxi);
							complex<floatexp> D(dbTr[j], dbTi[j]);
							complex<floatexp> D0(dbTr0[j], dbTi0[j]);
							complex<floatexp> _8(8, 0), _28(28, 0), _56(56, 0), _70(70, 0);
							complex<floatexp> Dn = _8*(X ^ 7)*D + _28*(X ^ 6)*(D ^ 2) + _56*(X ^ 5)*(D ^ 3) + _70*(X ^ 4)*(D ^ 4) + _56*(X ^ 3)*(D ^ 5) + _28*(X ^ 2)*(D ^ 6) + _8*X*(D ^ 7) + (D ^ 8) + D0;
							Dni = Dn.m_i;
							Dnr = Dn.m_r;
						}
						else if (m_nPower == 9){
							complex<floatexp> X(dxr, dxi);
							complex<floatexp> D(dbTr[j], dbTi[j]);
							complex<floatexp> D0(dbTr0[j], dbTi0[j]);
							complex<floatexp> _9(9, 0), _36(36, 0), _84(84, 0), _126(126, 0);
							complex<floatexp> Dn = _9*(X ^ 8)*D + _36*(X ^ 7)*(D ^ 2) + _84*(X ^ 6)*(D ^ 3) + _126*(X ^ 5)*(D ^ 4) + _126*(X ^ 4)*(D ^ 5) + _84*(X ^ 3)*(D ^ 6) + _36*(X ^ 2)*(D ^ 7) + _9*X*(D ^ 8) + (D ^ 9) + D0;
							Dni = Dn.m_i;
							Dnr = Dn.m_r;
						}
						else if (m_nPower == 10){
							complex<floatexp> X(dxr, dxi);
							complex<floatexp> D(dbTr[j], dbTi[j]);
							complex<floatexp> D0(dbTr0[j], dbTi0[j]);
							complex<floatexp> _10(10, 0), _45(45, 0), _120(120, 0), _210(210, 0), _252(252, 0);
							complex<floatexp> Dn = _10*(X ^ 9)*D + _45*(X ^ 8)*(D ^ 2) + _120*(X ^ 7)*(D ^ 3) + _210*(X ^ 6)*(D ^ 4) + _252*(X ^ 5)*(D ^ 5) + _210*(X ^ 4)*(D ^ 6) + _120*(X ^ 3)*(D ^ 7) + _45*(X ^ 2)*(D ^ 8) + _10*X*(D ^ 9) + (D ^ 10) + D0;
							Dni = Dn.m_i;
							Dnr = Dn.m_r;
						}
						else{
							complex<floatexp> X(dxr, dxi);
							complex<floatexp> D(dbTr[j], dbTi[j]);
							complex<floatexp> D0(dbTr0[j], dbTi0[j]);
							complex<floatexp> c(m_pnExpConsts[0], 0);
							int nXExp = m_nPower - 2, nDExp = 2, ci = 1;
							complex<floatexp> Dn = c*(X^(m_nPower - 1))*D;
							while (nXExp){
								c.m_r = m_pnExpConsts[ci++];
								Dn += c*(X^nXExp)*(D^nDExp);
								nXExp--;
								nDExp++;
							}
							Dn += (D^m_nPower) + D0;
							Dni = Dn.m_i;
							Dnr = Dn.m_r;
						}

					}
					else if (m_nFractalType == 1)
					{

						if (m_nPower == 2)
						{
							Dnr = (dxr*dbTr[j] - dxi*dbTi[j]).mul2() + dbTr[j] * dbTr[j] - dbTi[j] * dbTi[j] + dbTr0[j];
							Dni = diffabs(dxr * dxi, dxr*dbTi[j] + dxi*dbTr[j] + dbTr[j] * dbTi[j]).mul2() + dbTi0[j];
						}

					}
					dbTr[j] = Dnr;
					dbTi[j] = Dni;
				}
			if (j<nProbe)
				break;
		}
	}

	// store last-but-one approximation
	if (isC)
	{
		if (rescaled)
		{
			for (int k = 0; k < m_nTerms; ++k)
			{
				m_APr[k] = A[src][k].m_r;
				m_APi[k] = A[src][k].m_i;
			}
		}
		else
		{
			for (int i = 0; i<m_nTerms; i++){
				m_APr[i] = APr[i];
				m_APi[i] = APi[i];
			}
		}
	}
	if (isR)
	{
//		bool isZero = true;
		for (int i = 0; i <= m_nTerms; ++i)
			for (int j = 0; j <= m_nTerms - i; ++j)
			{
				m_APs->s[i][j] = APs->s[i][j];
				m_APs->t[i][j] = APs->t[i][j];
//				isZero &= m_APs->s[i][j] == 0;
//				isZero &= m_APs->t[i][j] == 0;
			}
/*
		for (int i = 0; i <= m_nTerms; ++i)
		{
			for (int j = 0; j <= m_nTerms - i; ++j)
				std::cerr << m_APs->s[i][j].toLongDouble() << "\t";
			std::cerr << std::endl;
		}
		for (int i = 0; i <= m_nTerms; ++i)
		{
			for (int j = 0; j <= m_nTerms - i; ++j)
				std::cerr << m_APs->t[i][j].toLongDouble() << "\t";
			std::cerr << std::endl;
		}
*/
	}

	if (isC)
	{
		delete[] APr;
		delete[] APi;
	}
	if (isR)
		delete APs;
	delete[] dbTr;
	delete[] dbTi;
	delete[] dbTr0;
	delete[] dbTi0;
	delete[] p;
}

void CFraktalSFT::DoApproximation(int64_t &antal, const floatexp &D0r, const floatexp &D0i, floatexp &TDnr, floatexp &TDni, floatexp &TDDnr, floatexp &TDDni)
{
	if (m_nMaxApproximation)
	{
		antal = m_nMaxApproximation - 1;
		TDnr = 0; TDni = 0; TDDnr = 0; TDDni = 0;
		for (int k = GetApproxTerms() - 1; k >= 0; --k)
		{
			floatexp tr = TDnr * D0r - TDni * D0i + m_APr[k];
			floatexp ti = TDnr * D0i + TDni * D0r + m_APi[k];
			TDnr = tr;
			TDni = ti;
			tr = TDDnr * D0r - TDDni * D0i + m_APr[k] * floatexp(k + 1.0);
			ti = TDDnr * D0i + TDDni * D0r + m_APi[k] * floatexp(k + 1.0);
			TDDnr = tr;
			TDDni = ti;
		}
		floatexp tr = TDnr * D0r - TDni * D0i;
		floatexp ti = TDnr * D0i + TDni * D0r;
		TDnr = tr;
		TDni = ti;
	}
	else
	{
		antal = 0;
		TDnr = D0r;
		TDni = D0i;
		TDDnr = 1.0;
		TDDni = 0.0;
	}
}

void CFraktalSFT::DoApproximation
( int64_t &antal
, const floatexp &a
, const floatexp &b
, floatexp &x
, floatexp &y
, floatexp &dxa
, floatexp &dxb
, floatexp &dya
, floatexp &dyb
)
{
	switch (GetApproximationType())
	{
		case SeriesType_Complex:
		{
			floatexp dx, dy;
			DoApproximation(antal, a, b, x, y, dx, dy);
			// Cauchy-Riemann
			dxa =  dx;
			dxb = -dy;
			dya =  dy;
			dyb =  dx;
			break;
		}
		case SeriesType_Real:
		{
			if (m_nMaxApproximation)
			{
				antal = m_nMaxApproximation - 1;
				auto order = GetApproxTerms();
				floatexp an[order + 1];
				floatexp bn[order + 1];
				floatexp dan[order + 1];
				floatexp dbn[order + 1];
				an[0] = 1;
				bn[0] = 1;
				dan[0] = 0;
				dbn[0] = 0;
				for (auto i = 1; i <= order; ++i)
				{
					an[i] = an[i - 1] * a;
					bn[i] = bn[i - 1] * b;
					dan[i] = i * an[i - 1];
					dbn[i] = i * bn[i - 1];
				}
				// x   = sum ApproxSeriesR2X[i][j]   a^i       b^j
				// dxa = sum ApproxSeriesR2X[i][j] i a^{i-1}   b^j
				// dxb = sum ApproxSeriesR2X[i][j]   a^i     j b^{j-1}
				floatexp totalX = 0;
				floatexp totalY = 0;
				floatexp totalDXA = 0;
				floatexp totalDXB = 0;
				floatexp totalDYA = 0;
				floatexp totalDYB = 0;
				for (auto i = 0; i <= order; ++i)
				{
					floatexp subtotalX = 0;
					floatexp subtotalY = 0;
					floatexp subtotalDXB = 0;
					floatexp subtotalDYB = 0;
					for (auto j = 0; j <= order - i; ++j)
					{
						subtotalX   += m_APs->s[i][j] *  bn[j];
						subtotalY   += m_APs->t[i][j] *  bn[j];
						subtotalDXB += m_APs->s[i][j] * dbn[j];
						subtotalDYB += m_APs->t[i][j] * dbn[j];
					}
					totalX   += subtotalX   *  an[i];
					totalY   += subtotalY   *  an[i];
					totalDXA += subtotalX   * dan[i];
					totalDYA += subtotalY   * dan[i];
					totalDXB += subtotalDXB *  an[i];
					totalDYB += subtotalDYB *  an[i];
				}
				x   = totalX;
				y   = totalY;
				dxa = totalDXA;
				dxb = totalDXB;
				dya = totalDYA;
				dyb = totalDYB;
			}
			else
			{
				antal = 0;
				x = a;
				y = b;
				dxa = 1.0;
				dxb = 0.0;
				dya = 0.0;
				dyb = 1.0;
			}
			break;
		}
		case SeriesType_None:
		{
			antal = 0;
			x = a;
			y = b;
			dxa = 1.0;
			dxb = 0.0;
			dya = 0.0;
			dyb = 1.0;
		}
	}
}

void CFraktalSFT::DoApproximation
( const floatexp &a
, const floatexp &b
, floatexp &x
, floatexp &y
)
{
	int order = GetApproxTerms();
	floatexp an[order + 1];
	floatexp bn[order + 1];
	an[0] = 1;
	bn[0] = 1;
	for (int i = 1; i <= order; ++i)
	{
		an[i] = an[i - 1] * a;
		bn[i] = bn[i - 1] * b;
	}
	floatexp totalX = 0;
	floatexp totalY = 0;
	for (int i = 0; i <= order; ++i)
	{
		floatexp subtotalX = 0;
		floatexp subtotalY = 0;
		for (int j = 0; j <= order - i; ++j)
		{
			subtotalX += m_APs->s[i][j] * bn[j];
			subtotalY += m_APs->t[i][j] * bn[j];
		}
		totalX += subtotalX * an[i];
		totalY += subtotalY * an[i];
	}
	x = totalX;
	y = totalY;
}
