#ifndef KF_FOLDERBROWSER_H
#define KF_FOLDERBROWSER_H 1

#include <cinttypes>
#include <string>

#include <windows.h>

BOOL SkapaKatalog(char *szFile);
int ValidatePath(char *szPath);
intptr_t Browse(HWND hWnd,std::string &szFolder);
#if 0
intptr_t Browse(HWND hWnd,char *szFolder, int nFolder,char *szLabel,char *szValue,int nValue);
#endif
int BrowseFile(HWND hwParent,BOOL bOpen, const std::string &szTitle,const std::string &szExt,std::string &szFile);
#if 0
#include "StringVector.h"
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraName,int &nExtraValue,int nExtraMin, int nExtraMax);
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraCheck,int &nExtraCheck);
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraCheck,int &nExtraCheck,char *szExtraCheck1,int &nExtraCheck1);
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraCheck,int &nExtraCheck,CStringVektor &svExtraList,int &nExtraListSelectedIndex);
#endif

#endif
