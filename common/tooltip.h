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

#ifndef __TOOLTIP_H__
#define __TOOLTIP_H__

typedef char * (*LPGETTEXT)(int nID,LPARAM lParam);
BOOL InitToolTip(HWND hWnd,HINSTANCE hInst,LPGETTEXT lpfnGetText,LPARAM lParam,BOOL bCenter=FALSE,char *szFont=NULL, int nFont=0);
long WINAPI ButtonProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
void ExitToolTip(HWND hWnd);

#endif//__TOOLTIP_H__
