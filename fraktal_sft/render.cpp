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
#include "newton.h"
#include "../common/StringVector.h"
#include "../common/bitmap.h"
#include "../common/parallell.h"
#include "../common/timer.h"
#include "main.h"

#include <cstring>

#ifndef WINVER
int ThRenderFractal(CFraktalSFT *p)
#else
static int WINAPI ThRenderFractal(CFraktalSFT *p)
#endif
{
	try{
		p->RenderFractal();
	}
#ifdef KF_OPENCL
	catch (OpenCLException &e)
	{
#ifdef WINVER
		p->m_bRunning = false;
#endif
		p->SetOpenCLDeviceIndex(-1);
#ifdef WINVER
		OpenCLErrorDialog(&p->cl_error, p->m_hWnd, p->m_hWnd ? false : true);
#endif
	}
#endif
	catch (...)
	{
#ifdef WINVER
		p->m_bRunning=FALSE;
#endif
//MessageBox(GetActiveWindow(),"Krash - 2","Krash",MB_OK);
	}
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
	return 0;
}

static int ThMandelCalc(TH_PARAMS *pMan)
{
#ifndef _DEBUG
	try{
#endif
		pMan->p->MandelCalc(pMan->reftype);
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

static int ThMandelCalcNANOMB1(TH_PARAMS *pMan)
{
	pMan->p->MandelCalcNANOMB1();
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
	return 0;
}

static int ThMandelCalcNANOMB2(TH_PARAMS *pMan)
{
	pMan->p->MandelCalcNANOMB2();
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
	return 0;
}

void CFraktalSFT::Render(BOOL bNoThread, BOOL bResetOldGlitch)
{
	m_bStop = TRUE;
#ifdef WINVER
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
	m_bRunning = TRUE;

#else // !Windows
	if(m_renderThread.joinable())
		m_renderThread.join();
#endif
	m_bStop = FALSE;
	m_nRDone = 0;
	if (bResetOldGlitch)
		memset(m_pOldGlitch, -1, sizeof(m_pOldGlitch));

#ifdef WINVER
	WaitForMutex(m_hMutex);
#else
	m_mutex.lock();
#endif
	if (m_bResized)
	    FreeBitmap();
	AllocateBitmap();
#ifdef WINVER
	ReleaseMutex(m_hMutex);
#else
	m_mutex.unlock();
#endif

	CFixedFloat pixel_spacing = (m_ZoomRadius * 2) / m_nY; // FIXME skew
	m_fPixelSpacing = floatexp(pixel_spacing);

	if (bNoThread || (GetUseOpenCL() && ! GetOpenCLThreaded())){
#ifdef WINVER
		if (m_hWnd)
			SetTimer(m_hWnd, 0, 100, NULL);
#endif
		ThRenderFractal(this);
	}
	else{
#ifdef WINVER
		DWORD dw;
		HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThRenderFractal, (LPVOID)this, 0, &dw);
		CloseHandle(hThread);
#else
		m_renderThread = std::thread(ThRenderFractal, this);
#endif
	}
}

void CFraktalSFT::RenderFractal()
{
	m_bIsRendering = true;
	if (! m_bAddReference)
	{
		m_count_queued = m_nX * m_nY;
		m_count_good_guessed = 0;
		m_count_good = 0;
		m_count_bad = 0;
		m_count_bad_guessed = 0;
	}
	{
		CFixedFloat div = m_ZoomRadius * 2;
		Precision q(LOW_PRECISION);
		FixedFloat f(div.m_f);
		f.precision(LOW_PRECISION);
		ToZoom(CDecNumber(FixedFloat(4 / f)), m_nZoom);
	}
	Reference_Type reftype = GetReferenceType(m_nZoom);
	if (GetUseNanoMB1() && GetFractalType() == 0 && GetPower() == 2 && ! m_bAddReference)
	{
		RenderFractalNANOMB1();
		m_bIsRendering = false;
		return;
	}
	else if (GetUseNanoMB2() && GetFractalType() == 0 && GetPower() == 2 && ! m_bAddReference)
	{
		RenderFractalNANOMB2();
		m_bIsRendering = false;
		return;
	}

	if (m_ReferenceReuse && (! GetReuseReference() || N.g_bJustDidNewton || reftype != reference_type(m_ReferenceReuse)))
	{
		if (m_Reference == m_ReferenceReuse)
		{
			m_Reference = nullptr;
		}
		reference_delete(m_ReferenceReuse);
		m_ReferenceReuse = nullptr;
		N.g_bJustDidNewton = false;
	}
	if (m_Reference)
	{
		if (m_Reference != m_ReferenceReuse)
		{
			reference_delete(m_Reference);
		}
		m_Reference = nullptr;
	}
	if (m_bAddReference == 0 && GetReuseReference())
	{
		m_Reference = m_ReferenceReuse;
		m_rref = m_rrefReuse;
		m_iref = m_irefReuse;
	}
	m_P.Init(m_nX, m_nY, m_bInteractive);
	int i;
	if (! m_Reference)
	{
		if (m_bAddReference == 0)
		{
			m_rref = m_CenterRe;
			m_iref = m_CenterIm;
			m_nAddRefX = -1;
			m_nAddRefY = -1;
		}
		double wall = get_wall_time();
		double cpu = get_cpu_time();
		CalculateReference(reftype);
		if (! m_ReferenceReuse && GetReuseReference())
		{
			m_ReferenceReuse = m_Reference;
			m_rrefReuse = m_rref;
			m_irefReuse = m_iref;
		}
		m_timer_reference_wall += get_wall_time() - wall;
		m_timer_reference_cpu += get_cpu_time() - cpu;
	}

	m_pixel_center_x = floatexp(m_CenterRe - m_rref);
	m_pixel_center_y = floatexp(m_CenterIm - m_iref);
	m_pixel_scale = floatexp((m_ZoomRadius * 2) / m_nY);

	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;

#if 0  // XXX for m_rApprox
	if(m_bAddReference)
	{
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
	else
	{
		m_rApprox.left = 0;
		m_rApprox.top = 0;
		m_rApprox.right = m_nX;
		m_rApprox.bottom = m_nY;
	}
#endif

	{
		double wall = get_wall_time();
		double cpu = get_cpu_time();
		CalculateApproximation();
		m_timer_approximation_wall += get_wall_time() - wall;
		m_timer_approximation_cpu += get_cpu_time() - cpu;
	}
	double wall = get_wall_time();
	double cpu = get_cpu_time();

	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;

#ifdef KF_OPENCL
	if (cl)
	{
		m_count_good_guessed = 0; // FIXME OpenCL progress doesn't track guessing
		m_count_good = m_nX * m_nY - m_count_queued;
		RenderFractalOpenCL(reftype);
		if (GetGlitchCenterMethod() != 0)
		{
			m_count_good += m_count_queued - m_OpenCL_Glitched_Count;
			m_count_queued = m_OpenCL_Glitched_Count;
		}
	}
	else
#endif
	{
		CalcStart();

		SYSTEM_INFO sysinfo;
		GetSystemInfo(&sysinfo);
		int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors - GetThreadsReserveCore();
		if (nParallel < 1) nParallel = 1;
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
			pMan[i].reftype = reftype;
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
	{
#ifdef WINVER
		if (m_hWnd)
			PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
#endif
	}
	m_bNoPostWhenDone = FALSE;
#ifdef WINVER
	m_bRunning = FALSE;
#endif

	m_bIsRendering = false;
	m_timer_perturbation_wall += get_wall_time() - wall;
	m_timer_perturbation_cpu += get_cpu_time() - cpu;
}

void CFraktalSFT::RenderFractalNANOMB1()
{
	m_P.Init(m_nX, m_nY, m_bInteractive);
	if (! GetReuseReference() || ! m_NanoMB1Ref || N.g_bJustDidNewton)
	{
		m_rref = m_CenterRe;
		m_iref = m_CenterIm;
		m_nAddRefX = -1;
		m_nAddRefY = -1;
		N.g_bJustDidNewton = false;
		double wall = get_wall_time();
		double cpu = get_cpu_time();
		CalculateReferenceNANOMB1();
		m_timer_reference_wall += get_wall_time() - wall;
		m_timer_reference_cpu += get_cpu_time() - cpu;
	}
	double wall = get_wall_time();
	double cpu = get_cpu_time();
	int i;
	m_pixel_center_x = floatexp(m_CenterRe - m_rref);
	m_pixel_center_y = floatexp(m_CenterIm - m_iref);
	m_pixel_scale = floatexp((m_ZoomRadius * 2) / m_nY);
	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	//CalculateApproximation();
	CalcStart();
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors - GetThreadsReserveCore();
	if (nParallel < 1) nParallel = 1;
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
		P.AddFunction((LPEXECUTE)ThMandelCalcNANOMB1, &pMan[i]);
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
#ifdef WINVER
	m_bRunning = FALSE;
#endif
	m_timer_perturbation_wall += get_wall_time() - wall;
	m_timer_perturbation_cpu += get_cpu_time() - cpu;
}

void CFraktalSFT::RenderFractalNANOMB2()
{
	m_P.Init(m_nX, m_nY, m_bInteractive);
	if (! GetReuseReference() || ! m_NanoMB2Ref || N.g_bJustDidNewton)
	{
		m_rref = m_CenterRe;
		m_iref = m_CenterIm;
		m_nAddRefX = -1;
		m_nAddRefY = -1;
		N.g_bJustDidNewton = false;
		double wall = get_wall_time();
		double cpu = get_cpu_time();
		CalculateReferenceNANOMB2();
		m_timer_reference_wall += get_wall_time() - wall;
		m_timer_reference_cpu += get_cpu_time() - cpu;
	}
	double wall = get_wall_time();
	double cpu = get_cpu_time();
	int i;
	m_pixel_center_x = floatexp(m_CenterRe - m_rref);
	m_pixel_center_y = floatexp(m_CenterIm - m_iref);
	m_pixel_scale = floatexp((m_ZoomRadius * 2) / m_nY);
	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	//CalculateApproximation();
	CalcStart();
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors - GetThreadsReserveCore();
	if (nParallel < 1) nParallel = 1;
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
		P.AddFunction((LPEXECUTE)ThMandelCalcNANOMB2, &pMan[i]);
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
#ifdef WINVER
	m_bRunning = FALSE;
#endif

	m_timer_perturbation_wall += get_wall_time() - wall;
	m_timer_perturbation_cpu += get_cpu_time() - cpu;
}

void CFraktalSFT::CalcStart(int x0, int x1, int y0, int y1)
{
	for (int x = x0; x < x1; x++)
	{
		for (int y = y0; y < y1; ++y)
			m_nPixels[x][y] = PIXEL_UNEVALUATED;
		memset(&m_nTrans[x][y0], 0, sizeof(m_nTrans[x][y0]) * (y1 - y0));
	}
}

static int ThCalcStart(TH_PARAMS *pMan)
{
	pMan->p->CalcStart(pMan->nXStart, pMan->nXStop, 0, pMan->p->GetHeight());
	return 0;
}

void CFraktalSFT::CalcStart()
{
	if (!m_bAddReference){
		SYSTEM_INFO sysinfo;
		GetSystemInfo(&sysinfo);
		int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors - GetThreadsReserveCore();
		if (nParallel < 1) nParallel = 1;
		CParallell P(
#ifdef _DEBUG
			1
#else
			nParallel
#endif
			);
		TH_PARAMS *pMan = new TH_PARAMS[nParallel];
// TODO split by Y, not X, due to cache locality
		int nXStart = 0;
		int nXStep = (m_nX + nParallel - 1) / nParallel;
		for (int i = 0; i < nParallel; i++)
		{
			pMan[i].p = this;
			pMan[i].nXStart = nXStart;
			nXStart += nXStep;
			if (nXStart > m_nX) nXStart = m_nX;
			pMan[i].nXStop = nXStart;
			P.AddFunction((LPEXECUTE)ThCalcStart, &pMan[i]);
		}
		P.Execute();
		P.Reset();
		delete[] pMan;
	}
}
