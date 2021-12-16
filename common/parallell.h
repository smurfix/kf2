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

#ifndef COMMON_PARALLALL_H
#define COMMON_PARALLALL_H

#ifdef KF_EMBED
#include <thread>
#endif

typedef int (*LPEXECUTE)(LPVOID pParameter);

#ifdef KF_EMBED
void Parallell_ThExecute(LPVOID pParameter);
#else
ULONG WINAPI Parallell_ThExecute(LPVOID pParameter);
#endif

class CParallell
{
	struct EXECUTE{
#ifdef KF_EMBED
		std::thread hThread;
#else
		HANDLE hThread;
		HANDLE hDone;
#endif
		LPEXECUTE lpfnExecute;
		LPEXECUTE lpfnDone;
		LPVOID pParameter;
	}**m_ppExecute;
	int m_nExecute;
	LPEXECUTE m_lpfnTotalDone;
	LPVOID m_pDone;

	int m_nParallell;
	DWORD m_dwStackSize;

public:
	CParallell(int nParallell);
	~CParallell();

	void SetTotalDone(LPEXECUTE lpfnTotalDone,LPVOID pDone);
	int AddFunction(LPEXECUTE lpfnExecute, LPVOID pParameter, LPEXECUTE lpfnDone=NULL);
	int Execute();
	void Reset();
	void SetStackSize(DWORD dwStackSize);

#ifdef KF_EMBED
	friend void Parallell_ThExecute(LPVOID pParameter);
#else
	friend ULONG WINAPI Parallell_ThExecute(LPVOID pParameter);
#endif
};

#endif // COMMON_PARALLALL_H
