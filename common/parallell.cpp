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

// Kalles Fraktaler 2
//
// © 2014 Karl Runmo ,runmo@hotmail.com
//
// COVERED CODE IS PROVIDED UNDER THIS LICENSE ON AN "AS IS" BASIS, WITHOUT WARRANTY
// OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTIES
// THAT THE COVERED CODE IS FREE OF DEFECTS, MERCHANTABLE, FIT FOR A PARTICULAR PURPOSE
// OR NON-INFRINGING. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE COVERED
// CODE IS WITH YOU. SHOULD ANY COVERED CODE PROVE DEFECTIVE IN ANY RESPECT, YOU (NOT
// THE INITIAL DEVELOPER OR ANY OTHER CONTRIBUTOR) ASSUME THE COST OF ANY NECESSARY
// SERVICING, REPAIR OR CORRECTION. THIS DISCLAIMER OF WARRANTY CONSTITUTES AN ESSENTIAL
// PART OF THIS LICENSE. NO USE OF ANY COVERED CODE IS AUTHORIZED HEREUNDER EXCEPT UNDER
// THIS DISCLAIMER.
#include <windows.h>
#include "parallell.h"
#include <mpfr.h>

#ifndef THREAD_MODE_BACKGROUND_BEGIN
#define THREAD_MODE_BACKGROUND_BEGIN PROCESS_MODE_BACKGROUND_BEGIN
#define THREAD_MODE_BACKGROUND_END PROCESS_MODE_BACKGROUND_END
#endif

#ifdef KF_EMBED
void Parallell_ThExecute(LPVOID pParameter)
#else
ULONG WINAPI Parallell_ThExecute(LPVOID pParameter)
#endif
{
	CParallell::EXECUTE *pE = (CParallell::EXECUTE *)pParameter;
#ifndef KF_EMBED
	SetThreadPriority(GetCurrentThread(), THREAD_MODE_BACKGROUND_BEGIN);
	SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST);
#endif
#ifndef _DEBUG
try{
#endif
	pE->lpfnExecute(pE->pParameter);
#ifndef _DEBUG
}catch(...){
}
#endif
#ifndef KF_EMBED
	SetThreadPriority(GetCurrentThread(), THREAD_MODE_BACKGROUND_END);
	SetEvent(pE->hDone);
#endif
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
#ifndef KF_EMBED
	return 0;
#endif
}

CParallell::CParallell(int nParallell)
{
	m_dwStackSize = 0;
	m_nParallell = nParallell;
	m_ppExecute = NULL;
	m_nExecute = 0;
	m_lpfnTotalDone = NULL;
	m_pDone = NULL;
}
CParallell::~CParallell()
{
	Reset();
}
void CParallell::Reset()
{
	int i;
	for(i=0;i<m_nExecute;i++)
		delete m_ppExecute[i];
	free(m_ppExecute);
	m_nExecute=0;
	m_ppExecute=NULL;
}

void CParallell::SetTotalDone(LPEXECUTE lpfnTotalDone,LPVOID pDone)
{
	m_lpfnTotalDone = lpfnTotalDone;
	m_pDone = pDone;
}
int CParallell::AddFunction(LPEXECUTE lpfnExecute,LPVOID pParameter,LPEXECUTE lpfnDone)
{
	int i = m_nExecute++;
	m_ppExecute = (EXECUTE**)realloc(m_ppExecute,sizeof(EXECUTE*)*m_nExecute);
	m_ppExecute[i] = new EXECUTE;
#ifndef KF_EMBED
	m_ppExecute[i]->hThread = NULL;
	m_ppExecute[i]->hDone = CreateEvent(NULL,0,0,NULL);
#endif
	m_ppExecute[i]->lpfnExecute = lpfnExecute;
	m_ppExecute[i]->lpfnDone = lpfnDone;
	m_ppExecute[i]->pParameter = pParameter;
	return m_nExecute;
}

void CParallell::SetStackSize(DWORD dwStackSize)
{
	m_dwStackSize = dwStackSize;
}
int CParallell::Execute()
{
	int i, j;
#ifndef KF_EMBED
	DWORD dw;
#endif
	for(i=0;i<m_nParallell && i<m_nExecute;i++){
#ifdef KF_EMBED
		m_ppExecute[i]->hThread = std::thread(Parallell_ThExecute,m_ppExecute[i]);
#else
		m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
		while(m_ppExecute[i]->hThread==INVALID_HANDLE_VALUE || m_ppExecute[i]->hThread==NULL){
			m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
			Sleep(20);
		}
#endif
	}
	for(j=0;j<m_nExecute;j++){
#ifdef KF_EMBED
		m_ppExecute[j]->hThread.join();
#else
		if(WaitForSingleObject(m_ppExecute[j]->hDone,INFINITE)==WAIT_TIMEOUT)
			TerminateThread(m_ppExecute[j]->hThread,0);
#endif
		if(m_ppExecute[j]->lpfnDone)
			m_ppExecute[j]->lpfnDone(m_ppExecute[j]->pParameter);
#ifndef KF_EMBED
		CloseHandle(m_ppExecute[j]->hThread);
		CloseHandle(m_ppExecute[j]->hDone);
#endif
		if(i<m_nExecute){
#ifdef KF_EMBED
			m_ppExecute[i]->hThread = std::thread(Parallell_ThExecute,m_ppExecute[i]);
#else
			m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
			while(m_ppExecute[i]->hThread==INVALID_HANDLE_VALUE || m_ppExecute[i]->hThread==NULL){
				m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
				Sleep(20);
			}
#endif
			i++;
		}
	}
	if(m_lpfnTotalDone)
		m_lpfnTotalDone(m_pDone);
	return 1;
}

