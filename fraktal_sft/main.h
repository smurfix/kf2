/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2020 Claude Heiland-Allen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifndef KF_MAIN_H
#define KF_MAIN_H 1

#include <string>

#include <windows.h>

extern int SaveImage(const std::string &szFileName, HBITMAP bmBmp, int nQuality, const std::string &comment);
extern int SaveImage(const std::string &szFileName, const BYTE *lpBits, int biWidth, int biHeight, int nQuality, const std::string &comment);
extern char *GetToolText(int nID,LPARAM lParam);
extern double GetDlgItemFloat(HWND hWnd,int nID);
extern void SetDlgItemFloat(HWND hWnd,int nID,double val);
extern int FileExists(const std::string &szFind);

extern std::string replace_path_filename(const std::string &path, const std::string &file);
extern std::string replace_path_extension(const std::string &path, const std::string &ext);
extern std::string get_filename_path(const std::string &file);
extern std::string get_filename_file(const std::string &file);
extern std::string get_filename_extension(const std::string &file);
extern std::string get_filename_zoom_string(const std::string &file);

extern std::string g_szFile;
extern bool g_bExamineDirty;
extern bool g_bAnim;
extern bool g_bAddReference;
extern bool g_bAddMainReference;
extern bool g_bEraser;
extern bool g_bWaitRead;

extern HICON g_hIcon;
extern HFONT g_monospaced_font;

extern HWND g_hwStatus;
extern HWND g_hwExamine;
extern HWND g_hwColors;

extern void OpenCLErrorDialog(HWND hWnd, bool fatal);

#endif
