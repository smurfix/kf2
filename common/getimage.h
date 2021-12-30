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

#ifndef KF_GETIMAGE_H
#define KF_GETIMAGE_H 1

#include <string>

#include <windows.h>

#ifdef WINVER
HBITMAP GetImageFromData(char *szImgData,int nImgData);
HBITMAP GetImage(const std::string &szFile);
void FillRectShade(HDC hDC, RECT r, int nR1, int nG1, int nB1,int nR2, int nG2, int nB2,int nType=0);
void SkuggadRect(HDC pDC, RECT wr, BOOL bUpp,BOOL bDark);
void SkuggadCirkle(HDC pDC, RECT wr, BOOL bUpp,BOOL bDark);

HBITMAP GetImageFromResource(char *szResourceType, char *szResourceName,HINSTANCE hInstance);

void ResizeBitmap(HBITMAP *bmBitmap,int nWidth,int nHeight,int nNewWidth,int nNewHeight,BOOL bHalftone=TRUE,__int64 *pnData=NULL);
#endif

#endif
