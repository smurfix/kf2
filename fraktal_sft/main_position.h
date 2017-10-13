#ifndef KF_MAIN_POSITION_H
#define KF_MAIN_POSITION_H 1

#include <windows.h>

extern int WINAPI PositionProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
extern const char *PositionToolTip(int nID);

#endif
