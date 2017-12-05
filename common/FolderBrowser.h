#ifndef KF_FOLDERBROWSER_H
#define KF_FOLDERBROWSER_H 1

#include <cinttypes>
#include <string>

#include <windows.h>

intptr_t Browse(HWND hWnd,std::string &szFolder);
int BrowseFile(HWND hwParent,BOOL bOpen, const char *szTitle,const char *szExt,std::string &szFile);

#endif
