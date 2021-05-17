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

extern bool g_bNewtonRunning;
extern bool g_bNewtonStop;
extern int64_t g_period;
extern bool g_bJustDidNewton;

extern int WINAPI NewtonProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);

struct progress_t
{
	int counters[4];
	volatile bool running;
	HWND hWnd;
	HANDLE hDone;
	double start_time, elapsed_time;
};

const struct formula *get_formula(int type, int power);

#endif
