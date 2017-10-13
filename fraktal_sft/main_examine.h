#ifndef KF_MAIN_EXAMINE_H
#define KF_MAIN_EXAMINE_H 1

#include <windows.h>

extern bool Examine(HWND hWnd);
extern int WINAPI ExamineProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
extern const char *ExamineToolTip(int nID);

#endif

