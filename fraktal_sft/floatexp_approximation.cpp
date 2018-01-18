#include "fraktal_sft.h"
#include "complex.h"

void CFraktalSFT::CalculateApproximation(int nType)
{
	m_nApprox = 0;
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
	CFixedFloat cr, ci;
	int i, j, k = 0;
	floatexp *dbTr = new floatexp[nProbe];
	floatexp *dbTi = new floatexp[nProbe];
	floatexp *dbTr0 = new floatexp[nProbe];
	floatexp *dbTi0 = new floatexp[nProbe];
	cr = m_rstart;
	ci = m_istart;

	POINT *p = new POINT[nProbe];
	for (j = 0; j < nProbeY; ++j)
	{
		int y = m_rApprox.top + j * (m_rApprox.bottom - m_rApprox.top) / (nProbeY - 1);
		if (y < m_rApprox.top) y = m_rApprox.top;
		if (y >= m_rApprox.bottom) y = m_rApprox.bottom - 1;
		for (i = 0; i < nProbeX; ++i)
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
	assert(k == nProbe);

	if (nType == 0){
		for (j = 0; j<nProbe; j++){
			dbTr0[j] = m_pDX[p[j].x];
			dbTi0[j] = m_pDY[p[j].y];
			if (m_nScalingOffset){
				dbTr0[j] *= m_nScaling;
				dbTi0[j] *= m_nScaling;
			}
			dbTr[j] = dbTr0[j];
			dbTi[j] = dbTi0[j];
		}
	}
	else if (nType == 1){
		for (j = 0; j<nProbe; j++){
			dbTr0[j] = m_lDX[p[j].x];
			dbTr[j] = dbTr0[j];
			dbTi0[j] = m_lDX[p[j].y];
			dbTi[j] = dbTi0[j];
		}
	}
	else{
		for (j = 0; j<nProbe; j++){
			dbTr[j] = dbTr0[j] = m_DX[p[j].x];
			dbTi[j] = dbTi0[j] = m_DY[p[j].y];
		}
	}
	m_nMaxApproximation = m_nMaxIter;
	//floatexp mindiff = (m_nBailout==2?0.000001:0.001);
	floatexp mindiff;
	if (m_bLowTolerance)
		mindiff = 0.00001;
	else
		mindiff = 0.001;
	//	if(dbTr[0]<0 && dbTi[0]<1e-16 && dbTi[0]>-1e-16)
	//		mindiff = 0.0000001;
	if (GetNoApprox())
		mindiff = 0;
	m_APr[0] = 1;
	m_APi[0] = 0;
	if (GetAutoApproxTerms()){
		int nT = sqrt((double)m_nTotal)*0.021;
		if (nT<5)
			nT = 5;
		if (nT>60)
			nT = 60;
		SetApproxTerms(nT);
	}
	int m_nTerms = GetApproxTerms();
	for (i = 1; i<m_nTerms; i++){
		m_APr[i] = 0;
		m_APi[i] = 0;
	}

	if (m_nFractalType>0 || m_nZoom<g_nRefZero){
		m_nMaxApproximation = 0;
		return;
	}
	floatexp xr;
	floatexp xi;
	floatexp *APr = new floatexp[m_nTerms];
	floatexp *APi = new floatexp[m_nTerms];
	for (i = 0; i<m_nTerms; i++){
		APr[i] = m_APr[i];
		APi[i] = m_APi[i];
	}

	for (i = 0; i<m_nMaxIter - 1 && !m_bStop; i++){
		m_nApprox++;
		// Series approximation
		int n = i - 1;
		if (i == 0)
			xr = xi = 0;
		else if (nType == 0){
			xr = m_db_dxr[n];
			xi = m_db_dxi[n];
		}
		else if (nType == 1){
			xr = m_ldxr[n];
			xi = m_ldxi[n];
		}
		else{
			xr = m_dxr[n];
			xi = m_dxi[n];
		}
		int k;
		for (k = 0; k<m_nTerms; k++){
			APr[k] = m_APr[k];
			APi[k] = m_APi[k];
		}
		if (m_nPower == 2){
			int k;
			m_APr[0] = (xr*APr[0] - xi*APi[0]).mul2() + _1;
			m_APi[0] = (xi*APr[0] + xr*APi[0]).mul2();
			for (k = 1; k<m_nTerms; k++){
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
		else{
			complex<floatexp> X(xr, xi), A(APr[0], APi[0]), B(APr[1], APi[1]), C(APr[2], APi[2]), fa(m_nPower, 0), fa_2(m_nPower / 2, 0), fa_6(m_nPower / 6, 0), fa1(m_nPower - 1, 0), fa2(m_nPower - 2, 0), f1(1, 0), f2(2, 0), f3(3, 0), f6(6, 0);
			//(4-6971)
			// p*X^(p-1)*A+1
			complex<floatexp> An = fa*(X ^ (m_nPower - 1))*A + f1;
			m_APr[0] = An.m_r;
			m_APi[0] = An.m_i;

			//(4-999)
			//                B=(p/2)* X^(p-2)         *((p-1)*A^2  + X*2*B)
			complex<floatexp> Bn = fa_2*(X ^ (m_nPower - 2)) * (fa1*(A ^ 2) + X*f2*B);
			m_APr[1] = Bn.m_r;
			m_APi[1] = Bn.m_i;

			//(4-999)
			//                C=(p/6) *  X^(p-3) *       ((p-1)*A* ((p-2)*  A^2 +   6*X*B) + X^2 *   6 *  C)
			complex<floatexp> Cn = fa_6 * (X ^ (m_nPower - 3))* (fa1* A * (fa2 * (A ^ 2) + f6*X*B) + (X ^ 2) * f6 * C);
			m_APr[2] = Cn.m_r;
			m_APi[2] = Cn.m_i;
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
		if (i<m_nMaxApproximation){
			floatexp dxr, dxi;
			if (nType == 0){
				dxr = m_db_dxr[i];
				dxi = m_db_dxi[i];
			}
			else if (nType == 1){
				dxr = m_ldxr[i];
				dxi = m_ldxi[i];
			}
			else{
				dxr = m_dxr[i];
				dxi = m_dxi[i];
			}
			for (j = 0; j<nProbe; j++){
				floatexp Dnr = m_APr[0] * dbTr0[j] - m_APi[0] * dbTi0[j];
				floatexp Dni = m_APr[0] * dbTi0[j] + m_APi[0] * dbTr0[j];
				floatexp D_r = dbTr0[j] * dbTr0[j] - dbTi0[j] * dbTi0[j];
				floatexp D_i = (dbTr0[j] * dbTi0[j]).mul2();
				Dnr += m_APr[1] * D_r - m_APi[1] * D_i;
				Dni += m_APr[1] * D_i + m_APi[1] * D_r;
				int k;
				for (k = 2; k<m_nTerms; k++){
					floatexp  t = D_r*dbTr0[j] - D_i*dbTi0[j];
					D_i = D_r*dbTi0[j] + D_i*dbTr0[j];
					D_r = t;
					Dnr += m_APr[k] * D_r - m_APi[k] * D_i;
					Dni += m_APr[k] * D_i + m_APi[k] * D_r;
				}
				floatexp diff = (Dnr - dbTr[j]) / dbTr[j];
				if (diff>mindiff || diff<-mindiff){
					m_nMaxApproximation = i;
					break;
				}
				diff = (Dni - dbTi[j]) / dbTi[j];
				if (diff>mindiff || diff<-mindiff){
					m_nMaxApproximation = i;
					break;
				}
				double yr = (dxr + Dnr).todouble();
				double yi = (dxi + Dni).todouble();
				if (g_real*yr*yr + g_imag*yi*yi>m_nBailout2){
					m_nMaxApproximation = i;
					break;
				}

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

				dbTr[j] = Dnr;
				dbTi[j] = Dni;
			}
			if (j<nProbe)
				break;
		}
	}
	for (i = 0; i<m_nTerms; i++){
		m_APr[i] = APr[i];
		m_APi[i] = APi[i];
	}
	if (GetNoApprox())
		m_nMaxApproximation = 0;
	delete[] APr;
	delete[] APi;
	delete[] dbTr;
	delete[] dbTi;
	delete[] dbTr0;
	delete[] dbTi0;
	delete[] p;
}
