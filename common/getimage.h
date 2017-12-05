#ifndef KF_GETIMAGE_H
#define KF_GETIMAGE_H 1

#include <string>

#include <windows.h>

HBITMAP GetImageFromData(char *szImgData,int nImgData);
HBITMAP GetImage(const std::string &szFile);
void FillRectShade(HDC hDC, RECT r, int nR1, int nG1, int nB1,int nR2, int nG2, int nB2,int nType=0);
void SkuggadRect(HDC pDC, RECT wr, BOOL bUpp,BOOL bDark, int nCorner=0);
void SkuggadCirkle(HDC pDC, RECT wr, BOOL bUpp,BOOL bDark);

HBITMAP GetImageFromResource(char *szResourceType, char *szResourceName,HINSTANCE hInstance);

// Denna funktion skall anropas med hDC=NULL för att frigöra minnet
COLORREF GetPixelDIB(HDC hDC, HBITMAP bmBitmap,int x, int y);
// Denna funktion skall avslutas med bCreate=TRUE för att skapa bilden och frigöra minnet
void SetPixelDIB(HDC hDC, HBITMAP bmBitmap,int x, int y,COLORREF col,BOOL bCreate);

void ResizeBitmap(HBITMAP *bmBitmap,int nWidth,int nHeight,int nNewWidth,int nNewHeight,BOOL bHalftone=TRUE,__int64 *pnData=NULL);

#endif
