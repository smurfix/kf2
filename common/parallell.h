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

#include "kf-task.h"

typedef int (*LPEXECUTE)(LPVOID pParameter);

class CParallell
{
	struct EXECUTE{
		std::thread hThread;
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

	friend void Parallell_ThExecute(CParallell::EXECUTE *pE);
};

#endif // COMMON_PARALLALL_H
