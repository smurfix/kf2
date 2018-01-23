#include "fraktal_sft.h"
#include "../common/StringVector.h"
#include "../common/bitmap.h"
#include "../common/parallell.h"

static int WINAPI ThRenderFractal(CFraktalSFT *p)
{
#ifndef _DEBUG
	try{
#endif
		p->RenderFractal();
#ifndef _DEBUG
	}
	catch (...) {
		p->m_bRunning=FALSE;
//MessageBox(GetActiveWindow(),"Krash - 2","Krash",MB_OK);
	}
#endif
	return 0;
}

static int ThMandelCalc(TH_PARAMS *pMan)
{
#ifndef _DEBUG
	try{
#endif
		pMan->p->MandelCalc();
#ifndef _DEBUG
	}
	catch (...) {
//		pMan->p->m_bRunning=FALSE;
MessageBox(GetActiveWindow(),"Krash - 1","Krash",MB_OK);
	}
#endif
	return 0;
}

static int ThMandelCalcEXP(TH_PARAMS *pMan)
{
	pMan->p->MandelCalcEXP();
	return 0;
}

static int ThMandelCalcLDBL(TH_PARAMS *pMan)
{
	pMan->p->MandelCalcLDBL();
	return 0;
}

static int ThMandelCalcSDouble(TH_PARAMS *pMan)
{
	pMan->p->MandelCalcSDouble();
	return 0;
}

static int ThMandelCalcSLDouble(TH_PARAMS *pMan)
{
	pMan->p->MandelCalcSLDouble();
	return 0;
}

void CFraktalSFT::RenderFractal(int nX, int nY, int nMaxIter, HWND hWnd, BOOL bNoThread, BOOL bResetOldGlitch)
{
	m_bStop = TRUE;
	double counter = 0;
	while(m_bRunning)
	{
		Sleep(4);
		counter += 4;
	}
#ifdef KF_DEBUG_SLEEP
	if (counter > 0)
		std::cerr << "RenderFractal() slept for " << counter << "ms" << std::endl;
#endif
	m_bStop = FALSE;
	if (hWnd)
		m_hWnd = hWnd;
	m_nX = nX;
	m_nY = nY;
	SetImageWidth(nX);
	SetImageHeight(nY);
	m_nMaxIter = nMaxIter;
	m_nRDone = m_nDone = m_nGuessed = 0;
	if (m_pDX){
		delete[] m_pDX;
		m_pDX = NULL;
	}
	if (m_pDY){
		delete[] m_pDY;
		m_pDY = NULL;
	}
	if (m_DX){
		delete[] m_DX;
		m_DX = NULL;
	}
	if (m_DY){
		delete[] m_DY;
		m_DY = NULL;
	}
	if (m_lDX){
		delete[] m_lDX;
		m_lDX = NULL;
	}
	if (m_lDY){
		delete[] m_lDY;
		m_lDY = NULL;
	}
	if (bResetOldGlitch)
		memset(m_pOldGlitch, -1, sizeof(m_pOldGlitch));
	if (m_nPower>10 && m_nPrevPower != m_nPower){
		m_nPrevPower = m_nPower;
		if (m_pnExpConsts){
			delete[] m_pnExpConsts;
			m_pnExpConsts = NULL;
		}
		CStringTable stVal("4,6", "", ",");
		int i, k;
		char szTmp[20];
		for (i = 5; i <= m_nPower; i++){
			stVal[0].InsertString(0, itoa(i, szTmp, 10));
			for (k = 1; k<stVal.GetCount(); k++)
				stVal[k].InsertString(0, itoa(atoi(stVal[k - 1][1]) + atoi(stVal[k][0]), szTmp, 10));
			if (i % 2 == 0){
				stVal.AddRow();
				stVal.AddInt(stVal.GetCount() - 1, atoi(stVal[stVal.GetCount() - 2][1]) * 2);
			}
		}
		m_pnExpConsts = new int[m_nPower - 1];
		k = 0;
		for (i = 0; i<stVal.GetCount(); i++)
			m_pnExpConsts[k++] = atoi(stVal[i][0]);
		if (m_nPower % 2 == 0)
			i--;
		for (i--; i >= 0; i--)
			m_pnExpConsts[k++] = atoi(stVal[i][0]);
	}
	else if (m_nPower <= 10 && m_pnExpConsts){
		delete[] m_pnExpConsts;
		m_pnExpConsts = NULL;
	}

	WaitForSingleObject(m_hMutex, INFINITE);
	bool resize = m_nXPrev != m_nX || m_nYPrev != m_nY;
	if (resize){
		SetImageSize(m_nX, m_nY);
	}
	if (m_bResized || resize)
	{
		DeleteObject(m_bmBmp);
		m_bmBmp = NULL;
	}
	HDC hDC = GetDC(NULL);
	if (!m_bmBmp)
		m_bmBmp = create_bitmap(hDC, m_nX, m_nY);

	if (!m_bAddReference){
		if (m_bmi)
			free(m_bmi);
		m_bmi = (BITMAPINFOHEADER *)malloc(sizeof(BITMAPINFOHEADER)+sizeof(RGBQUAD)* 256);
		memset(m_bmi, 0, sizeof(BITMAPINFOHEADER)+sizeof(RGBQUAD)* 256);
		m_bmi->biSize = sizeof(BITMAPINFOHEADER);
		if (!GetDIBits(hDC, m_bmBmp, 0, 0, NULL, (LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
			{ /*Beep(1000,10)*/ }
		m_bmi->biCompression = m_bmi->biClrUsed = m_bmi->biClrImportant = 0;
		m_bmi->biBitCount = 24;
		if (m_bmi->biBitCount != 24)
			m_bmi->biClrUsed = 1 << m_bmi->biBitCount;
		m_row = ((((m_bmi->biWidth*(DWORD)m_bmi->biBitCount) + 31)&~31) >> 3);
		m_bmi->biSizeImage = m_row*m_bmi->biHeight;
		if (!m_lpBits || (int)m_bmi->biSizeImage != m_nSizeImage){
			m_nSizeImage = m_bmi->biSizeImage;
			if (m_lpBits)
				delete[] m_lpBits;
			m_lpBits = new BYTE[m_bmi->biSizeImage];

			if (!GetDIBits(hDC, m_bmBmp, 0, m_bmi->biHeight, m_lpBits,
				(LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
				{ /*Beep(1000,10)*/ }
		}
	}
	ReleaseDC(NULL, hDC);
	ReleaseMutex(m_hMutex);

	CFixedFloat pixel_spacing = (m_istop - m_istart) / m_nY;
	m_fPixelSpacing = pixel_spacing;
	m_dPixelSpacing = double(m_fPixelSpacing);
	m_lPixelSpacing = (long double)(m_fPixelSpacing);

	if (bNoThread){
		SetTimer(m_hWnd, 0, 100, NULL);
		ThRenderFractal(this);
	}
	else{
		m_bRunning = TRUE;
		DWORD dw;
		HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThRenderFractal, (LPVOID)this, 0, &dw);
		CloseHandle(hThread);
	}
}

void CFraktalSFT::RenderFractal()
{
	m_C = cos(g_Degree);
	m_S = sin(g_Degree);

	CFixedFloat div = m_istop - m_istart;

	int nZeroes = 0;
	std::string sszZoom = m_istop.ToText();
	const char *szZoom = sszZoom.c_str();
	if (*szZoom == '-')
		szZoom++;
	if (*szZoom == '0'){
		szZoom++;
		if (*szZoom == '.'){
			szZoom++;
			while (*szZoom == '0'){
				nZeroes++;
				szZoom++;
			}
		}
	}

	{
		Precision q(LOW_PRECISION);
		FixedFloat f(div.m_f);
		f.precision(LOW_PRECISION);
		ToZoom(CDecNumber(FixedFloat(4 / f)), m_nZoom);
	}
	if (m_bAddReference){
		int x, y;
		m_nTotal = 0;
		for (x = 0; x<m_nX; x++)
		for (y = 0; y<m_nY; y++)
		if (m_nPixels[x][y] == -1)
			m_nTotal++;
	}
	else
		m_nTotal = m_nX*m_nY;

	if (m_db_dxr){
		delete[] m_db_dxr;
		m_db_dxr = NULL;
	}
	if (m_db_dxi){
		delete[] m_db_dxi;
		m_db_dxi = NULL;
	}
	if (m_ldxr){
			delete[] m_ldxr;
		m_ldxr = NULL;
	}
	if (m_ldxi){
		delete[] m_ldxi;
		m_ldxi = NULL;
	}
	if (m_dxr){
		delete[] m_dxr;
		m_dxr = NULL;
	}
	if (m_dxi){
		delete[] m_dxi;
		m_dxi = NULL;
	}

	if (m_nZoom < THRESHOLD_DOUBLE)
		m_CalcType = CalcType_Double;
	else if (m_nZoom < THRESHOLD_SCALED_DOUBLE && m_nFractalType == 0 && m_nPower == 2)
		m_CalcType = CalcType_ScaledDouble;
	else if (m_nZoom < THRESHOLD_LONG_DOUBLE)
		m_CalcType = CalcType_LongDouble;
	else if (m_nZoom < THRESHOLD_SCALED_LONG_DOUBLE && m_nFractalType == 0 && m_nPower == 2)
		m_CalcType = CalcType_ScaledLongDouble;
	else
		m_CalcType = CalcType_FloatExp;
	if (GetLongDoubleAlways()) m_CalcType = CalcType_LongDouble;
	if (GetFloatExpAlways()) m_CalcType = CalcType_FloatExp;
	switch (m_CalcType)
	{
		case CalcType_Double: RenderFractalDouble(); break;
		case CalcType_ScaledDouble: RenderFractalSDouble(); break;
		case CalcType_LongDouble: RenderFractalLDBL(); break;
		case CalcType_ScaledLongDouble: RenderFractalSLDouble(); break;
		case CalcType_FloatExp: RenderFractalEXP(); break;
  }
}

void CFraktalSFT::RenderFractalDouble()
{
	m_P.Init(m_nX, m_nY);
	int i;
	if (!GetReuseReference() || !m_db_dxr || m_nZoom<g_nRefZero){
		if (m_bAddReference != 1 || m_nZoom<g_nRefZero){
			if (m_nZoom >= g_nRefZero){
				m_rref = (m_rstop + m_rstart)*.5;
				m_iref = (m_istop + m_istart)*.5;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
			else{
				m_rref = 0;
				m_iref = 0;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
		}
		CalculateReference();
	}
	int x, y;
	if (!m_pDX || !m_pDY){
		CFixedFloat c = m_rstart;
		CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
		m_pDX = new double[m_nX];
		for (x = 0; x<m_nX; x++, c += step)
			m_pDX[x] = (c - m_rref).ToDouble();
		c = m_istart;
		step = (m_istop - m_istart)*(1 / (double)m_nY);
		m_pDY = new double[m_nY];
		for (y = 0; y<m_nY; y++, c += step)
			m_pDY[y] = (c - m_iref).ToDouble();
	}

	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	CalculateApproximation(CalcType_Double);

       if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
               m_bNoGlitchDetection = FALSE;
       else
               m_bNoGlitchDetection = TRUE;

		// CalcStart
		if (!m_bAddReference){
			for (x = 0; x<m_nX; x++){
				for (y = 0; y<m_nY; y++){
					m_nPixels[x][y] = -1;
					m_nTrans[x][y] = 0;
				}
			}
		}

		SYSTEM_INFO sysinfo;
		GetSystemInfo(&sysinfo);
		int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors;
		int nStep = m_nX / nParallel;
		int nXStart = 0;

		CParallell P(
#ifdef _DEBUG
			1
#else
			nParallel
#endif
			);
		TH_PARAMS *pMan = new TH_PARAMS[nParallel];
		nXStart = 0;
		for (i = 0; i<nParallel; i++){
			pMan[i].p = this;
			pMan[i].nXStart = nXStart;
			nXStart += nStep;
			if (nXStart>m_nX)
				nXStart = m_nX;
			if (i == nParallel - 1)
				pMan[i].nXStop = m_nX;
			else
				pMan[i].nXStop = nXStart;
			P.AddFunction((LPEXECUTE)ThMandelCalc, &pMan[i]);
			if (pMan[i].nXStop == m_nX && pMan[i].nXStop - pMan[i].nXStart>1 && i<nParallel - 1){
				pMan[i].nXStop--;
				nXStart = pMan[i].nXStop;
			}
			if (pMan[i].nXStop == m_nX){
				break;
			}
		}
		P.Execute();
		P.Reset();
		delete[] pMan;

	m_bAddReference = FALSE;
	if (!m_bNoPostWhenDone)
		PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
	m_bNoPostWhenDone = FALSE;
	m_bRunning = FALSE;
}

void CFraktalSFT::RenderFractalSDouble()
{
	m_P.Init(m_nX, m_nY);
	if (!GetReuseReference() || !m_ldxr){
		if (m_bAddReference != 1 || m_nZoom<g_nRefZero){
			if (m_nZoom >= g_nRefZero){
				m_rref = (m_rstop + m_rstart)*.5;
				m_iref = (m_istop + m_istart)*.5;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
			else{
				m_rref = 0;
				m_iref = 0;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
		}
		CalculateReferenceSDouble();
	}
	int i;
	int x, y;
	if (!m_lDX || !m_lDY){
		CFixedFloat c = m_rstart;
		CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
		m_lDX = new long double[m_nX];
		CFixedFloat tmp;
		for (x = 0; x<m_nX; x++, c += step){
			tmp = c - m_rref;
			m_lDX[x] = mpfr_get_ld(tmp.m_f.backend().data(), MPFR_RNDN);
		}
		c = m_istart;
		step = (m_istop - m_istart)*(1 / (double)m_nY);
		m_lDY = new long double[m_nY];
		for (y = 0; y<m_nY; y++, c += step){
			tmp = c - m_iref;
			m_lDY[y] = mpfr_get_ld(tmp.m_f.backend().data(), MPFR_RNDN);
		}
	}
	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	CalculateApproximation(CalcType_ScaledDouble);

	// CalcStart
	if (!m_bAddReference){
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				m_nPixels[x][y] = -1;
				m_nTrans[x][y] = 0;
			}
		}
	}

	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors;

	CParallell P(
#ifdef _DEBUG
		1
#else
		nParallel
#endif
		);
	TH_PARAMS *pMan = new TH_PARAMS[nParallel];
	int nStep = m_nX / nParallel;
	if (nStep<2)
		nStep = 2;
	else
		nStep++;
	int nXStart = 0;
	for (i = 0; i<nParallel; i++){
		pMan[i].p = this;
		pMan[i].nXStart = nXStart;
		nXStart += nStep;
		if (nXStart>m_nX)
			nXStart = m_nX;
		if (i == nParallel - 1)
			pMan[i].nXStop = m_nX;
		else
			pMan[i].nXStop = nXStart;
		P.AddFunction((LPEXECUTE)ThMandelCalcSDouble, &pMan[i]);
		if (pMan[i].nXStop == m_nX && pMan[i].nXStop - pMan[i].nXStart>1 && i<nParallel - 1){
			pMan[i].nXStop--;
			nXStart = pMan[i].nXStop;
		}
		if (pMan[i].nXStop == m_nX){
			break;
		}
	}
	P.Execute();
	P.Reset();
	delete[] pMan;
	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	if (!m_bNoPostWhenDone)
		PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
	m_bNoPostWhenDone = FALSE;
	m_bRunning = FALSE;
}

void CFraktalSFT::RenderFractalLDBL()
{
	m_P.Init(m_nX, m_nY);
	if (!GetReuseReference() || !m_ldxr){
		if (m_bAddReference != 1 || m_nZoom<g_nRefZero){
			if (m_nZoom >= g_nRefZero){
				m_rref = (m_rstop + m_rstart)*.5;
				m_iref = (m_istop + m_istart)*.5;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
			else{
				m_rref = 0;
				m_iref = 0;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
		}
		CalculateReferenceLDBL();
	}
	int i;
	int x, y;
	if (!m_lDX || !m_lDY){
		CFixedFloat c = m_rstart;
		CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
		m_lDX = new long double[m_nX];
		CFixedFloat tmp;
		for (x = 0; x<m_nX; x++, c += step){
			tmp = c - m_rref;
			m_lDX[x] = mpfr_get_ld(tmp.m_f.backend().data(), MPFR_RNDN);
		}
		c = m_istart;
		step = (m_istop - m_istart)*(1 / (double)m_nY);
		m_lDY = new long double[m_nY];
		for (y = 0; y<m_nY; y++, c += step){
			tmp = c - m_iref;
			m_lDY[y] = mpfr_get_ld(tmp.m_f.backend().data(), MPFR_RNDN);
		}
	}
	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	CalculateApproximation(CalcType_LongDouble);

	// CalcStart
	if (!m_bAddReference){
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				m_nPixels[x][y] = -1;
				m_nTrans[x][y] = 0;
			}
		}
	}

	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors;

	CParallell P(
#ifdef _DEBUG
		1
#else
		nParallel
#endif
		);
	TH_PARAMS *pMan = new TH_PARAMS[nParallel];
	int nStep = m_nX / nParallel;
	if (nStep<2)
		nStep = 2;
	else
		nStep++;
	int nXStart = 0;
	for (i = 0; i<nParallel; i++){
		pMan[i].p = this;
		pMan[i].nXStart = nXStart;
		nXStart += nStep;
		if (nXStart>m_nX)
			nXStart = m_nX;
		if (i == nParallel - 1)
			pMan[i].nXStop = m_nX;
		else
			pMan[i].nXStop = nXStart;
		P.AddFunction((LPEXECUTE)ThMandelCalcLDBL, &pMan[i]);
		if (pMan[i].nXStop == m_nX && pMan[i].nXStop - pMan[i].nXStart>1 && i<nParallel - 1){
			pMan[i].nXStop--;
			nXStart = pMan[i].nXStop;
		}
		if (pMan[i].nXStop == m_nX){
			break;
		}
	}
	P.Execute();
	P.Reset();
	delete[] pMan;
	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	if (!m_bNoPostWhenDone)
		PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
	m_bNoPostWhenDone = FALSE;
	m_bRunning = FALSE;
}

void CFraktalSFT::RenderFractalSLDouble()
{
	m_P.Init(m_nX, m_nY);
	if (!GetReuseReference() || !m_dxr){
		if (m_bAddReference != 1 || m_nZoom<g_nRefZero){
			if (m_nZoom >= g_nRefZero){
				m_rref = (m_rstop + m_rstart)*.5;
				m_iref = (m_istop + m_istart)*.5;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
			else{
				m_rref = 0;
				m_iref = 0;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
		}
		CalculateReferenceSLDouble();
	}
	int i;
	int x, y;
	if (!m_DX || !m_DY){
		CFixedFloat c = m_rstart;
		CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
		m_DX = new floatexp[m_nX];
		for (x = 0; x<m_nX; x++, c += step)
			m_DX[x] = (c - m_rref);
		c = m_istart;
		step = (m_istop - m_istart)*(1 / (double)m_nY);
		m_DY = new floatexp[m_nY];
		for (y = 0; y<m_nY; y++, c += step)
			m_DY[y] = (c - m_iref);
	}
	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	CalculateApproximation(CalcType_ScaledLongDouble);

	// CalcStart
	if (!m_bAddReference){
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				m_nPixels[x][y] = -1;
				m_nTrans[x][y] = 0;
			}
		}
	}

	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors;

	CParallell P(
#ifdef _DEBUG
		1
#else
		nParallel
#endif
		);
	TH_PARAMS *pMan = new TH_PARAMS[nParallel];
	int nStep = m_nX / nParallel;
	if (nStep<2)
		nStep = 2;
	else
		nStep++;
	int nXStart = 0;
	for (i = 0; i<nParallel; i++){
		pMan[i].p = this;
		pMan[i].nXStart = nXStart;
		nXStart += nStep;
		if (nXStart>m_nX)
			nXStart = m_nX;
		if (i == nParallel - 1)
			pMan[i].nXStop = m_nX;
		else
			pMan[i].nXStop = nXStart;
		P.AddFunction((LPEXECUTE)ThMandelCalcSLDouble, &pMan[i]);
		if (pMan[i].nXStop == m_nX && pMan[i].nXStop - pMan[i].nXStart>1 && i<nParallel - 1){
			pMan[i].nXStop--;
			nXStart = pMan[i].nXStop;
		}
		if (pMan[i].nXStop == m_nX){
			break;
		}
	}
	P.Execute();
	P.Reset();
	delete[] pMan;
	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	if (!m_bNoPostWhenDone)
		PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
	m_bNoPostWhenDone = FALSE;
	m_bRunning = FALSE;
}

void CFraktalSFT::RenderFractalEXP()
{
	m_P.Init(m_nX, m_nY);
	if (!GetReuseReference() || !m_dxr){
		if (m_bAddReference != 1 || m_nZoom<g_nRefZero){
			if (m_nZoom >= g_nRefZero){
				m_rref = (m_rstop + m_rstart)*.5;
				m_iref = (m_istop + m_istart)*.5;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
			else{
				m_rref = 0;
				m_iref = 0;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
		}
		CalculateReferenceEXP();
	}
	int i;
	int x, y;
	if (!m_DX || !m_DY){
		CFixedFloat c = m_rstart;
		CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
		m_DX = new floatexp[m_nX];
		for (x = 0; x<m_nX; x++, c += step)
			m_DX[x] = (c - m_rref);
		c = m_istart;
		step = (m_istop - m_istart)*(1 / (double)m_nY);
		m_DY = new floatexp[m_nY];
		for (y = 0; y<m_nY; y++, c += step)
			m_DY[y] = (c - m_iref);
	}
	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	CalculateApproximation(CalcType_FloatExp);

	// CalcStart
	if (!m_bAddReference){
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				m_nPixels[x][y] = -1;
				m_nTrans[x][y] = 0;
			}
		}
	}

	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors;

	CParallell P(
#ifdef _DEBUG
		1
#else
		nParallel
#endif
		);
	TH_PARAMS *pMan = new TH_PARAMS[nParallel];
	int nStep = m_nX / nParallel;
	if (nStep<2)
		nStep = 2;
	else
		nStep++;
	int nXStart = 0;
	for (i = 0; i<nParallel; i++){
		pMan[i].p = this;
		pMan[i].nXStart = nXStart;
		nXStart += nStep;
		if (nXStart>m_nX)
			nXStart = m_nX;
		if (i == nParallel - 1)
			pMan[i].nXStop = m_nX;
		else
			pMan[i].nXStop = nXStart;
		P.AddFunction((LPEXECUTE)ThMandelCalcEXP, &pMan[i]);
		if (pMan[i].nXStop == m_nX && pMan[i].nXStop - pMan[i].nXStart>1 && i<nParallel - 1){
			pMan[i].nXStop--;
			nXStart = pMan[i].nXStop;
		}
		if (pMan[i].nXStop == m_nX){
			break;
		}
	}
	P.Execute();
	P.Reset();
	delete[] pMan;
	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	if (!m_bNoPostWhenDone)
		PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
	m_bNoPostWhenDone = FALSE;
	m_bRunning = FALSE;
}
