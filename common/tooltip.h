#ifndef __TOOLTIP_H__
#define __TOOLTIP_H__

typedef char * (*LPGETTEXT)(int nID,LPARAM lParam);
BOOL InitToolTip(HWND hWnd,HINSTANCE hInst,LPGETTEXT lpfnGetText,LPARAM lParam,BOOL bCenter=FALSE,char *szFont=NULL, int nFont=0);
long WINAPI ButtonProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
void ExitToolTip(HWND hWnd);

#endif//__TOOLTIP_H__
