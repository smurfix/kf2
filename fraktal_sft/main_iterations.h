#ifndef KF_MAIN_ITERATIONS_H
#define KF_MAIN_ITERATIONS_H 1

#include <windows.h>

extern int WINAPI IterationProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
extern const char *IterationToolTip(int nID);

#endif
