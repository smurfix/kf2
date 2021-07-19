/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2020 Claude Heiland-Allen

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

#ifndef KF_MAIN_TRANSFORMATION_H
#define KF_MAIN_TRANSFORMATION_H 1

#include <windows.h>
#include "../common/matrix.h"

extern INT_PTR WINAPI TransformationProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
extern void TransformImage(HBITMAP bmBmp);
extern void TransformImage(HBITMAP bmBkg, HBITMAP bmBkgDraw, POINT pm);
extern void TransformApply(const polar2 &P);
extern void TransformRefresh(const polar2 &P);
extern void TransformBlit(HDC hDC, int w, int h);
extern polar2 TransformUpdateRotation(const polar2 &P0, const double x0, const double y0, const double x1, const double y1);
extern polar2 TransformUpdateStretch(const polar2 &P0, const double x0, const double y0, const double x1, const double y1);

extern HWND g_hwTransformationDialog;
extern bool g_bTransformationDialogIsOpen;

#endif
