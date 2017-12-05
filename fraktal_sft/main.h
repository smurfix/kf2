#ifndef KF_MAIN_H
#define KF_MAIN_H 1

#include <string>

#include <windows.h>

extern int SaveImage(const std::string &szFileName, HBITMAP bmBmp, int nQuality, const std::string &comment);
extern char *GetToolText(int nID,LPARAM lParam);
extern double GetDlgItemFloat(HWND hWnd,int nID);
extern void SetDlgItemFloat(HWND hWnd,int nID,double val);
extern int FileExists(char *szFind);

extern bool g_bExamineDirty;
extern bool g_bAnim;
extern bool g_bAddReference;
extern bool g_bAddMainReference;
extern bool g_bEraser;
extern bool g_bWaitRead;

extern HICON g_hIcon;

extern HWND g_hwStatus;
extern HWND g_hwExamine;

#endif
