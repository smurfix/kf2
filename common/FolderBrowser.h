#include "StringVector.h"

BOOL SkapaKatalog(char *szFile);
int ValidatePath(char *szPath);
intptr_t Browse(HWND hWnd,char *szFolder, int nFolder);
intptr_t Browse(HWND hWnd,char *szFolder, int nFolder,char *szLabel,char *szValue,int nValue);
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile);
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraName,int &nExtraValue,int nExtraMin, int nExtraMax);
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraCheck,int &nExtraCheck);
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraCheck,int &nExtraCheck,char *szExtraCheck1,int &nExtraCheck1);
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraCheck,int &nExtraCheck,CStringVektor &svExtraList,int &nExtraListSelectedIndex);
