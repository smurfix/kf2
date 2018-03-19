/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2018 Claude Heiland-Allen

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

ULONG WINAPI Parallell_ThExecute(LPVOID pParameter)
{
	CParallell::EXECUTE *pE = (CParallell::EXECUTE *)pParameter;
#ifndef _DEBUG
try{
#endif
	pE->lpfnExecute(pE->pParameter);
#ifndef _DEBUG
}catch(...){
}
#endif
	SetEvent(pE->hDone);
	mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE);
	return 0;
}

CParallell::CParallell(int nParallell, int nTimeout,int nInitWait)
{
	m_dwStackSize = 0;
	m_nParallell = nParallell;
	m_nTimeout = nTimeout;
	m_nInitWait = nInitWait;
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
	m_ppExecute[i]->hThread = NULL;
	m_ppExecute[i]->hDone = CreateEvent(NULL,0,0,NULL);
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
	DWORD dw;
	for(i=0;i<m_nParallell && i<m_nExecute;i++){
		m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
		Sleep(m_nInitWait);
		while(m_ppExecute[i]->hThread==INVALID_HANDLE_VALUE || m_ppExecute[i]->hThread==NULL){
			m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
			Sleep(20);
		}
		SetThreadPriority(m_ppExecute[i]->hThread,THREAD_PRIORITY_LOWEST);
	}
	for(j=0;j<m_nExecute;j++){
		if(WaitForSingleObject(m_ppExecute[j]->hDone,m_nTimeout)==WAIT_TIMEOUT)
			TerminateThread(m_ppExecute[j]->hThread,0);
		if(m_ppExecute[j]->lpfnDone)
			m_ppExecute[j]->lpfnDone(m_ppExecute[j]->pParameter);
		CloseHandle(m_ppExecute[j]->hThread);
		CloseHandle(m_ppExecute[j]->hDone);
		if(i<m_nExecute){
			m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
			while(m_ppExecute[i]->hThread==INVALID_HANDLE_VALUE || m_ppExecute[i]->hThread==NULL){
				m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
				Sleep(20);
			}
			SetThreadPriority(m_ppExecute[i]->hThread,THREAD_PRIORITY_LOWEST);
			i++;
		}
	}
	if(m_lpfnTotalDone)
		m_lpfnTotalDone(m_pDone);
	return 1;
}

int CParallell::ExecuteNoOrder()
{
	int i, j;
	DWORD dw;
	HANDLE *hDone = new HANDLE[m_nParallell];
	int *nIndex = new int[m_nParallell];
	int nEnd=m_nParallell;
	for(i=0;i<m_nParallell && i<m_nExecute;i++){
		hDone[i]=m_ppExecute[i]->hDone;
		nIndex[i]=i;
		m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
		Sleep(m_nInitWait);
		while(m_ppExecute[i]->hThread==INVALID_HANDLE_VALUE || m_ppExecute[i]->hThread==NULL){
			m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
			Sleep(20);
		}
		SetThreadPriority(m_ppExecute[i]->hThread,THREAD_PRIORITY_LOWEST);
		nEnd--;
	}
	j=0;
	while(j<m_nExecute){
		int nWRet;
		if((nWRet=WaitForMultipleObjects(m_nParallell-nEnd,hDone,FALSE,m_nTimeout))==WAIT_TIMEOUT){
			nWRet=0;
			TerminateThread(m_ppExecute[nIndex[nWRet]]->hThread,0);
		}
		else{
			nWRet-=WAIT_OBJECT_0;
		}
		int k = nIndex[nWRet];
		if(m_ppExecute[k]->lpfnDone)
			m_ppExecute[k]->lpfnDone(m_ppExecute[k]->pParameter);
		CloseHandle(m_ppExecute[k]->hThread);
		CloseHandle(m_ppExecute[k]->hDone);
		if(i<m_nExecute){
			m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
			while(m_ppExecute[i]->hThread==INVALID_HANDLE_VALUE || m_ppExecute[i]->hThread==NULL){
				m_ppExecute[i]->hThread = CreateThread(NULL,m_dwStackSize,Parallell_ThExecute,m_ppExecute[i],0,&dw);
				Sleep(20);
			}
			SetThreadPriority(m_ppExecute[i]->hThread,THREAD_PRIORITY_LOWEST);
			nIndex[nWRet] = i;
			hDone[nWRet] = m_ppExecute[i]->hDone;
			i++;
		}
		else{
			for(;nWRet<m_nParallell-1;nWRet++){
				hDone[nWRet]=hDone[nWRet+1];
				nIndex[nWRet]=nIndex[nWRet+1];
			}
			nEnd++;
		}

		j++;
	}
	if(m_lpfnTotalDone)
		m_lpfnTotalDone(m_pDone);

	delete[] hDone;
	delete[] nIndex;
	return 1;
}
