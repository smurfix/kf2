#ifndef KF_MAIN_H
#define KF_MAIN_H 1

#include <windows.h>

extern int SaveImage(char *szFileName, HBITMAP bmBmp, int nQuality, const char *comment);
extern char *GetToolText(int nID,LPARAM lParam);
extern double GetDlgItemFloat(HWND hWnd,int nID);
extern void SetDlgItemFloat(HWND hWnd,int nID,double val);

extern bool g_bExamineDirty;
extern HICON g_hIcon;
extern HWND g_hwStatus;

#endif
