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
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
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
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
	return 0;
}

static int ThMandelCalcEXP(TH_PARAMS *pMan)
{
	pMan->p->MandelCalcEXP();
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
	return 0;
}

static int ThMandelCalcLDBL(TH_PARAMS *pMan)
{
	pMan->p->MandelCalcLDBL();
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
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

	if (bNoThread){
		if (m_hWnd)
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
	if (m_nZoom>=g_nLDBL && g_LDBL && m_nZoom <= g_nEXP && m_nPower<8){// && !(m_nFractalType==1 && m_nPower==3)){
		if (m_db_dxr){
			delete[] m_db_dxr;
			m_db_dxr = NULL;
		}
		if (m_db_dxi){
			delete[] m_db_dxi;
			m_db_dxi = NULL;
		}
		if (m_dxr){
			delete[] m_dxr;
			m_dxr = NULL;
		}
		if (m_dxi){
			delete[] m_dxi;
			m_dxi = NULL;
		}
#ifdef KF_OPENCL
		if (cl)
		{
			RenderFractalOpenCLEXP();
		}
		else
#endif
		{
			RenderFractalLDBL();
		}
		return;
	}
	else if (m_nZoom>=g_nLDBL){
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
#ifdef KF_OPENCL
		if (cl)
		{
			RenderFractalOpenCLEXP();
		}
		else
#endif
		{
			RenderFractalEXP();
		}
		return;
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
		m_nScalingOffset = 0;
		m_nScaling = 1;
		for (i = 300; i<m_nZoom; i++){
			m_nScalingOffset++;
			m_nScaling = m_nScaling*.1;
		}
		CalculateReference();
	}
	int x, y;

	CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
	m_pixel_step_x = step;
	m_pixel_center_x = m_rstart + (m_nX/2) * step - m_rref;
	step = (m_istop - m_istart)*(1 / (double)m_nY);
	m_pixel_step_y = step;
	m_pixel_center_y = m_istart + (m_nY/2) * step - m_iref;

	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
/*
	if(m_bAddReference){
		m_rApprox.left = m_nX/2;
		m_rApprox.top = m_nY/2;
		m_rApprox.right = m_nX/2;
		m_rApprox.bottom = m_nY/2;
		for (y = 0; y<m_nY; y++){
			for (x = 0; x<m_nX; x++){
				if(m_nPixels[x][y]==-1){
					if(m_rApprox.left>x)
						m_rApprox.left=x;
					if(m_rApprox.top>y)
						m_rApprox.top=y;
					if(m_rApprox.right<x)
						m_rApprox.right=x;
					if(m_rApprox.bottom<y)
						m_rApprox.bottom=y;
				}
			}
		}
	}
	else{
		m_rApprox.left = 0;
		m_rApprox.top = 0;
		m_rApprox.right = m_nX;
		m_rApprox.bottom = m_nY;
	}
*/
	CalculateApproximation(0);

       if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
               m_bNoGlitchDetection = FALSE;
       else
               m_bNoGlitchDetection = TRUE;

#ifdef KF_OPENCL
	if (cl)
	{
		RenderFractalOpenCL();
	}
	else
#endif
	{
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
	}
	m_bAddReference = FALSE;
	if (!m_bNoPostWhenDone)
		if (m_hWnd)
			PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
	m_bNoPostWhenDone = FALSE;
	m_bRunning = FALSE;
}

void CFraktalSFT::RenderFractalLDBL()
{
	m_P.Init(m_nX, m_nY);
	int i;
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
		m_nScalingOffsetL = 0;
		m_nScalingL = 1;
		for (i = 4900; i<m_nZoom; i++){
			m_nScalingOffsetL++;
			m_nScalingL = m_nScalingL*.1L;
		}
		CalculateReferenceLDBL();
	}
	int x, y;

	CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
	m_pixel_step_x = step;
	m_pixel_center_x = m_rstart + (m_nX/2) * step - m_rref;
	step = (m_istop - m_istart)*(1 / (double)m_nY);
	m_pixel_step_y = step;
	m_pixel_center_y = m_istart + (m_nY/2) * step - m_iref;

	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	CalculateApproximation(1);

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

	CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
	m_pixel_step_x = step;
	m_pixel_center_x = m_rstart + (m_nX/2) * step - m_rref;
	step = (m_istop - m_istart)*(1 / (double)m_nY);
	m_pixel_step_y = step;
	m_pixel_center_y = m_istart + (m_nY/2) * step - m_iref;

	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	CalculateApproximation(2);

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
