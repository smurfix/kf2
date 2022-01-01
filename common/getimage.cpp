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
// � 2014 Karl Runmo ,runmo@hotmail.com
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

#include "getimage.h"

#include <stdio.h>
#define sprintf_s snprintf

#include <olectl.h>
#include <ocidl.h>
#include "bitmap.h"

HBITMAP GetImageFromData(char *szImgData,int nImgData)
{
	BITMAP bm;
	HBITMAP bmBmp, bmRet, bmBmpOld, bmRetOld;
	HDC dcBmp, dcRet, hDC;
	HRESULT hr;
	char *szData;
	IStream *pStream;
	IPicture *pPicture;
	HANDLE hMem;

	hMem = GlobalAlloc(GMEM_FIXED,nImgData);
	szData = (char*)GlobalLock(hMem);
	memcpy(szData,szImgData,nImgData);

	if(FAILED(hr = CreateStreamOnHGlobal(hMem,FALSE,&pStream))){
		GlobalFree(hMem);
		return NULL;
	}
	hr = OleLoadPicture(pStream,nImgData,TRUE,IID_IPicture,(void**)&pPicture);

	pStream->Release();
	GlobalFree(hMem);
	if(FAILED(hr))
		return NULL;

	if(FAILED(hr = pPicture->get_Handle((OLE_HANDLE*)&bmBmp))){
		pPicture->Release();
		return NULL;
	}
	GetObject(bmBmp,sizeof(BITMAP),&bm);

	hDC = GetDC(NULL);
	bmRet = create_bitmap(hDC,bm.bmWidth,bm.bmHeight);
	dcBmp = CreateCompatibleDC(hDC);
	dcRet = CreateCompatibleDC(hDC);
	bmBmpOld = (HBITMAP)SelectObject(dcBmp,bmBmp);
	bmRetOld = (HBITMAP)SelectObject(dcRet,bmRet);
	BitBlt(dcRet,0,0,bm.bmWidth,bm.bmHeight,dcBmp,0,0,SRCCOPY);
	SelectObject(dcBmp,bmBmpOld);
	SelectObject(dcRet,bmRetOld);
	DeleteDC(dcBmp);
	DeleteDC(dcRet);
	ReleaseDC(NULL,hDC);
	pPicture->Release();

	return bmRet;
}
void UpsideBitmap(HBITMAP bmBitmap)
{
	HDC hDC = GetDC(NULL);
	BYTE *lpBits, *lpBitsNew;
	int row;
	BITMAPINFOHEADER bmi={sizeof(BITMAPINFOHEADER)};
	if(!GetDIBits(hDC,bmBitmap,0,0,NULL,(LPBITMAPINFO)&bmi,DIB_RGB_COLORS))
		Beep(1000,10);
	bmi.biCompression=bmi.biClrUsed=bmi.biClrImportant=0;
	bmi.biBitCount = 24;
	row = ((((bmi.biWidth*(DWORD)bmi.biBitCount)+31)&~31) >> 3);
	bmi.biSizeImage=row*bmi.biHeight;
	lpBitsNew = new BYTE[bmi.biSizeImage];
	lpBits = new BYTE[bmi.biSizeImage];
	if(!GetDIBits(hDC,bmBitmap,0,bmi.biHeight,lpBits,
			(LPBITMAPINFO)&bmi,DIB_RGB_COLORS))
		Beep(1000,10);
	int nX, nY;
	for(nX=0;nX<bmi.biWidth;nX++)
		for(nY=0;nY<bmi.biHeight;nY++){
			lpBitsNew[nX*BM_WIDTH + (bmi.biHeight-nY-1)*row] = lpBits[nX*BM_WIDTH + nY*row];
			lpBitsNew[nX*BM_WIDTH + (bmi.biHeight-nY-1)*row + 1] = lpBits[nX*BM_WIDTH + nY*row + 1];
			lpBitsNew[nX*BM_WIDTH + (bmi.biHeight-nY-1)*row + 2] = lpBits[nX*BM_WIDTH + nY*row + 2];
		}
	if(!SetDIBits(hDC,bmBitmap,0,bmi.biHeight,lpBitsNew,
			(LPBITMAPINFO)&bmi,DIB_RGB_COLORS))
		Beep(1000,10);
	delete lpBitsNew;
	delete lpBits;
}
HBITMAP GetImage(const std::string &szFile)
{
	BITMAP bm;
	HBITMAP bmBmp, bmRet, bmBmpOld, bmRetOld;
	HDC dcBmp, dcRet, hDC;
	HRESULT hr;
	int nLen;
	char *szData;
	IStream *pStream;
	IPicture *pPicture;
	DWORD dwRead;
	HANDLE hFile, hMem;

	if((hFile = CreateFileA(szFile.c_str(),GENERIC_READ,0,NULL,OPEN_EXISTING,0,NULL))==INVALID_HANDLE_VALUE)
		return NULL;
	nLen = GetFileSize(hFile,NULL);
	hMem = GlobalAlloc(GMEM_FIXED,nLen);
	szData = (char*)GlobalLock(hMem);
	ReadFile(hFile,szData,nLen,&dwRead,NULL);
	CloseHandle(hFile);

	if(FAILED(hr = CreateStreamOnHGlobal(hMem,FALSE,&pStream))){
		GlobalFree(hMem);
		return (HBITMAP)LoadImageA(GetModuleHandle(NULL),szFile.c_str(),IMAGE_BITMAP,0,0,LR_LOADFROMFILE);
	}
	hr = OleLoadPicture(pStream,nLen,TRUE,IID_IPicture,(void**)&pPicture);

	pStream->Release();
	GlobalFree(hMem);
	if(FAILED(hr))
		return (HBITMAP)LoadImageA(GetModuleHandle(NULL),szFile.c_str(),IMAGE_BITMAP,0,0,LR_LOADFROMFILE);

	short nType;
	pPicture->get_Type(&nType);
	if(nType==2){
		short nType;
		OLE_XSIZE_HIMETRIC nWidth, nHeight;
		pPicture->get_Type(&nType);
		pPicture->get_Width(&nWidth);
		pPicture->get_Height(&nHeight);
		hDC = GetDC(NULL);
		HDC dcBmp = CreateCompatibleDC(hDC);
		HBITMAP bmBmp = create_bitmap(hDC,nWidth/4,nHeight/4);
		HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,bmBmp);
		RECT r={0,0,nWidth/4,nHeight/4};
		FillRect(dcBmp,&r,(HBRUSH)GetStockObject(WHITE_BRUSH));
		pPicture->Render(dcBmp,0,0,nWidth/4,nHeight/4,0,nHeight,nWidth,-nHeight,&r);

		SelectObject(dcBmp,bmOld);
		DeleteDC(dcBmp);
		ReleaseDC(NULL,hDC);
		pPicture->Release();
		return bmBmp;
	}
	if(FAILED(hr = pPicture->get_Handle((OLE_HANDLE*)&bmBmp))){
		pPicture->Release();
		return (HBITMAP)LoadImageA(GetModuleHandle(NULL),szFile.c_str(),IMAGE_BITMAP,0,0,LR_LOADFROMFILE);
	}
	GetObject(bmBmp,sizeof(BITMAP),&bm);

	hDC = GetDC(NULL);
	bmRet = create_bitmap(hDC,bm.bmWidth,bm.bmHeight);
	dcBmp = CreateCompatibleDC(hDC);
	dcRet = CreateCompatibleDC(hDC);
	bmBmpOld = (HBITMAP)SelectObject(dcBmp,bmBmp);
	bmRetOld = (HBITMAP)SelectObject(dcRet,bmRet);
	BitBlt(dcRet,0,0,bm.bmWidth,bm.bmHeight,dcBmp,0,0,SRCCOPY);
	SelectObject(dcBmp,bmBmpOld);
	SelectObject(dcRet,bmRetOld);
	DeleteDC(dcBmp);
	DeleteDC(dcRet);
	ReleaseDC(NULL,hDC);
	pPicture->Release();

	return bmRet;
}

HBITMAP GetImageFromResource(char *szResourceType, char *szResourceName,HINSTANCE hInstance)
{
	(void) hInstance;
	HGLOBAL hGlobal;
	HRSRC hResource;
	if(!(hResource=FindResourceA(GetModuleHandle(NULL),szResourceName,szResourceType)))
		return NULL;
	if(!(hGlobal=LoadResource(GetModuleHandle(NULL),hResource)))
		return NULL;
	int nLen = SizeofResource(GetModuleHandle(NULL),hResource);
	char *szData = (char*)GlobalLock(hGlobal);
	return GetImageFromData(szData,nLen);
}
void FillRectShade(HDC hDC, RECT r, int nR1, int nG1, int nB1,int nR2, int nG2, int nB2,int nType)
{
	RECT rFill=r;
	HBRUSH br;
	int i, nStep = (r.right-r.left>r.bottom-r.top?r.right-r.left:r.bottom-r.top);
	if(nType>3){
		HRGN rr = CreateRectRgn(r.left,r.top,r.right,r.bottom);
		int nWidth = r.right-r.left;
		int nHeight = r.bottom-r.top;
		r.left-=nWidth/4;
		r.right+=nWidth/4;
		r.top-=nHeight/4;
		r.bottom+=nHeight/4;
		rFill = r;
		nStep = (r.right-r.left>r.bottom-r.top?r.right-r.left:r.bottom-r.top);
		for(i=0;i<=nStep;i++){
			if((nType & 1)==0)
				rFill.right = r.right - (r.right-r.left)*i/nStep;
			else
				rFill.right = r.right;
			if((nType & 2)==0)
				rFill.bottom = r.bottom - (r.bottom-r.top)*i/nStep;
			else
				rFill.bottom = r.bottom;
			HRGN rgn = CreateEllipticRgn(rFill.left,rFill.top,rFill.right,rFill.bottom);
			CombineRgn(rgn,rgn,rr,RGN_AND);
			br = CreateSolidBrush(RGB(nR2 - (nR2-nR1)*i/nStep,nG2 - (nG2-nG1)*i/nStep,nB2 - (nB2-nB1)*i/nStep));
			FillRgn(hDC,rgn,br);
//			FillRect(hDC,&rFill,br);
			DeleteObject(br);
			DeleteObject(rgn);
		}
		return;
	}
	for(i=0;i<=nStep;i++){
		if((nType & 1)==0)
			rFill.right = r.right - (r.right-r.left)*i/nStep;
		else
			rFill.right = r.right;
		if((nType & 2)==0)
			rFill.bottom = r.bottom - (r.bottom-r.top)*i/nStep;
		else
			rFill.bottom = r.bottom;
		br = CreateSolidBrush(RGB(nR2 - (nR2-nR1)*i/nStep,nG2 - (nG2-nG1)*i/nStep,nB2 - (nB2-nB1)*i/nStep));
		FillRect(hDC,&rFill,br);
		DeleteObject(br);
	}
}

void ResizeBitmap(HBITMAP *bmBitmap,int nWidth,int nHeight,int nNewWidth,int nNewHeight,BOOL bHalftone,__int64 *pnData)
{
	int row, rowNew;
	if(pnData)
		*pnData=0;

	BITMAP bb;
	GetObject(*bmBitmap,sizeof(BITMAP),&bb);
	if(nWidth==-1)
		nWidth = bb.bmWidth;
	if(nHeight==-1)
		nHeight = bb.bmHeight;

	BYTE *lpBits, *lpBitsNew;
	BITMAPINFOHEADER bmi={sizeof(BITMAPINFOHEADER)};
	BITMAPINFOHEADER bmiNew={sizeof(BITMAPINFOHEADER)};

	HDC hDC = GetDC(NULL);
	HDC dcNew = CreateCompatibleDC(hDC);
	HBITMAP bmNew = create_bitmap(hDC,nNewWidth,nNewHeight);
	HBITMAP bmOld = (HBITMAP)SelectObject(dcNew,bmNew);

	if(!GetDIBits(dcNew,bmNew,0,0,NULL,(LPBITMAPINFO)&bmiNew,DIB_RGB_COLORS))
		Beep(1000,10);
	bmiNew.biCompression=bmiNew.biClrUsed=bmiNew.biClrImportant=0;
	bmiNew.biBitCount = 8*BM_WIDTH;
	rowNew = ((((bmiNew.biWidth*(DWORD)bmiNew.biBitCount)+31)&~31) >> 3);
	bmiNew.biSizeImage=rowNew*bmiNew.biHeight;
	lpBitsNew = new BYTE[bmiNew.biSizeImage];

	if(!GetDIBits(hDC,*bmBitmap,0,0,NULL,(LPBITMAPINFO)&bmi,DIB_RGB_COLORS))
		Beep(1000,10);
	bmi.biCompression=bmi.biClrUsed=bmi.biClrImportant=0;
	bmi.biBitCount = 24;
	row = ((((bmi.biWidth*(DWORD)bmi.biBitCount)+31)&~31) >> 3);
	bmi.biSizeImage=row*bmi.biHeight;
	lpBits = new BYTE[bmi.biSizeImage];
	if(!GetDIBits(hDC,*bmBitmap,0,bmi.biHeight,lpBits,
			(LPBITMAPINFO)&bmi,DIB_RGB_COLORS))
		Beep(1000,10);
	int nXStart,nYStart;
	int nXNew, nYNew;
	if(bHalftone){
		for(nXNew=0;nXNew<nNewWidth;nXNew++)
			for(nYNew=0;nYNew<nNewHeight;nYNew++){
				int Colors[3]={0};
				int nDiv=0;
				for(nXStart=nXNew*nWidth/nNewWidth;nXStart<(nXNew+1)*nWidth/nNewWidth;nXStart++)
					for(nYStart=nYNew*nHeight/nNewHeight;nYStart<(nYNew+1)*nHeight/nNewHeight;nYStart++) {
						int pixOff = nXStart*BM_WIDTH + nYStart*row;
						if(pixOff + 2 < (int)bmi.biSizeImage){
							nDiv++;
							Colors[0]+=lpBits[pixOff];
							Colors[1]+=lpBits[pixOff + 1];
							Colors[2]+=lpBits[pixOff + 2];
						}
					}

				if(!nDiv){
					nXStart=nXNew*nWidth/nNewWidth;
					nYStart=nYNew*nHeight/nNewHeight;
					int pixOff = nXStart*BM_WIDTH + nYStart*row;
					Colors[0]+=lpBits[pixOff];
					Colors[1]+=lpBits[pixOff + 1];
					Colors[2]+=lpBits[pixOff + 2];
					nDiv++;
					if(nXNew && nXStart==(nXNew-1)*nWidth/nNewWidth){
						nXNew--;
						pixOff = nXNew*BM_WIDTH + nYNew*rowNew;
						Colors[0]+=lpBitsNew[pixOff];
						Colors[1]+=lpBitsNew[pixOff + 1];
						Colors[2]+=lpBitsNew[pixOff + 2];
						nXNew++;
						nDiv++;
					}
					if(nYNew && nYStart==(nYNew+1)*nHeight/nNewHeight){
						nYNew--;
						pixOff = nXNew*BM_WIDTH + nYNew*rowNew;
						Colors[0]+=lpBitsNew[pixOff];
						Colors[1]+=lpBitsNew[pixOff + 1];
						Colors[2]+=lpBitsNew[pixOff + 2];
						nYNew++;
						nDiv++;
					}
					pixOff = nXNew*BM_WIDTH + nYNew*rowNew;
					if(pixOff + 2 < (int)bmiNew.biSizeImage){
						lpBitsNew[pixOff] = Colors[0]/nDiv;
						lpBitsNew[pixOff + 1] = Colors[1]/nDiv;
						lpBitsNew[pixOff + 2] = Colors[2]/nDiv;
						if(pnData)
							*pnData+=lpBitsNew[pixOff]+lpBitsNew[pixOff + 1]+lpBitsNew[pixOff + 2];
					}
				}
				else{
					Colors[0]/=nDiv;
					Colors[1]/=nDiv;
					Colors[2]/=nDiv;
					int pixOff = nXNew*BM_WIDTH + nYNew*rowNew;
					if(pixOff + 2 < (int)bmiNew.biSizeImage){
						lpBitsNew[pixOff] = Colors[0];
						lpBitsNew[pixOff + 1] = Colors[1];
						lpBitsNew[pixOff + 2] = Colors[2];
						if(pnData)
							*pnData+=lpBitsNew[pixOff]+lpBitsNew[pixOff + 1]+lpBitsNew[pixOff + 2];
					}
				}
			}
	}
	else{
		for(nXNew=0;nXNew<nNewWidth;nXNew++)
			for(nYNew=0;nYNew<nNewHeight;nYNew++){
				nXStart=nXNew*nWidth/nNewWidth;
				nYStart=nYNew*nHeight/nNewHeight;
				int pixOff = nXStart*BM_WIDTH + nYStart*row;
				int pixOffNew = nXNew*BM_WIDTH + nYNew*rowNew;
				lpBitsNew[pixOffNew] = lpBits[pixOff];
				lpBitsNew[pixOffNew + 1] = lpBits[pixOff + 1];
				lpBitsNew[pixOffNew + 2] = lpBits[pixOff + 2];
				if(pnData)
					*pnData+=lpBitsNew[pixOffNew]+lpBitsNew[pixOffNew + 1]+lpBitsNew[pixOffNew + 2];
			}
	}
	if(!SetDIBits(hDC,bmNew,0,bmiNew.biHeight,lpBitsNew,
			(LPBITMAPINFO)&bmiNew,DIB_RGB_COLORS))
		Beep(1000,10);
	delete [] lpBits;
	delete [] lpBitsNew;
	SelectObject(dcNew,bmOld);
	DeleteObject(*bmBitmap);
	*bmBitmap = bmNew;
	DeleteDC(dcNew);
	ReleaseDC(NULL,hDC);
	return;
}


void SkuggadRect(HDC pDC, RECT wr, BOOL bUpp,BOOL bDark)
{
	HPEN wp=CreatePen(PS_SOLID,1,bDark?RGB(192,192,192):RGB(255,255,255)), 
		 gp=CreatePen(PS_SOLID,1,bDark?RGB(0,0,0):RGB(128,128,128)), op;

	// Rita v�nstra och �vre med ena f�rgen
	if(bUpp)
		op = (HPEN)SelectObject(pDC,gp);
	else
		op = (HPEN)SelectObject(pDC,wp);
	MoveToEx(pDC,wr.left,wr.bottom-1,NULL);
	LineTo(pDC,wr.right-1,wr.bottom-1);
	LineTo(pDC,wr.right-1,wr.bottom-1);
	LineTo(pDC,wr.right-1,wr.top);
	LineTo(pDC,wr.right-1,wr.top);

	// Rita h�gra och undre med andra f�rgen
	if(bUpp)
		SelectObject(pDC,wp);
	else
		SelectObject(pDC,gp);
	MoveToEx(pDC,wr.right-1,wr.top,NULL);
	LineTo(pDC,wr.left,wr.top);
	LineTo(pDC,wr.left,wr.top);
	LineTo(pDC,wr.left,wr.bottom-1);
	LineTo(pDC,wr.left,wr.bottom-1);

	// Frig�r GDI-minne
	SelectObject(pDC,op);
	DeleteObject(wp);
	DeleteObject(gp);
}

void SkuggadCirkle(HDC pDC, RECT wr, BOOL bUpp,BOOL bDark)
{
	HPEN wp=CreatePen(PS_SOLID,1,bDark?RGB(192,192,192):RGB(255,255,255)), 
		 gp=CreatePen(PS_SOLID,1,bDark?RGB(0,0,0):RGB(128,128,128)), op;

	// Rita v�nstra och �vre med ena f�rgen
	if(bUpp)
		op = (HPEN)SelectObject(pDC,gp);
	else
		op = (HPEN)SelectObject(pDC,wp);
	Arc(pDC,wr.left,wr.top,wr.right,wr.bottom,wr.left,wr.bottom,wr.right,wr.top);
	// Rita h�gra och undre med andra f�rgen
	if(bUpp)
		SelectObject(pDC,wp);
	else
		SelectObject(pDC,gp);
	Arc(pDC,wr.left,wr.top,wr.right,wr.bottom,wr.right,wr.top,wr.left,wr.bottom);
	// Frig�r GDI-minne
	SelectObject(pDC,op);
	DeleteObject(wp);
	DeleteObject(gp);
}
