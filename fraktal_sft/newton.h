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

#ifndef KF_NEWTON_H
#define KF_NEWTON_H 1

#include "defs.h"

struct CNewton {
	ABOOL stop;
	bool g_bNewtonRunning;
	bool g_bNewtonExit;
	bool g_bJustDidNewton;

	int g_nr_zoom_target;
	int g_nr_folding;
	double g_nr_folding_custom;
	int g_nr_size_power;
	double g_nr_size_power_custom;
	int g_nr_size_factor;
	double g_nr_size_factor_custom;
	int g_nr_action;
	bool g_nr_ball_method;

	floatexp g_fNewtonDelta2[2];
	int g_nNewtonETA;
	int64_t g_iterations;

	double g_skew[4];
	int64_t g_period;

	CNewton();
	~CNewton();
};

extern int WINAPI NewtonProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);

struct progress_t
{
	int counters[4];
	ABOOL stop;
#ifdef WINVER
	HWND hWnd;
	HANDLE hDone;
#else
	void *SFT;
#endif
	double start_time, elapsed_time;
};

const struct formula *get_formula(int type, int power);

#endif
