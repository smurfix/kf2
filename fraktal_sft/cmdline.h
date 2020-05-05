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

#ifndef KF_CMDLINE_H
#define KF_CMDLINE_H 1

#include <iostream>
#include <string>

enum LogLevel
{
  LogLevel_Debug = 0,
  LogLevel_Status = 1,
  LogLevel_Info = 2,
  LogLevel_Warn = 3,
  LogLevel_Error = 4
};

extern LogLevel g_log_level;
#define output_log_message(level, message) \
  do { \
    if((LogLevel_ ## level) >= g_log_level) \
      ((LogLevel_ ## level) >= LogLevel_Status ? std::cerr : std::cout) \
        << message << std::endl; \
  } while(0)

class CommandLineArguments
{
public:
  bool        bLoadMap;
  std::string sLoadMap;
  bool        bLoadPalette;
  std::string sLoadPalette;
  bool        bLoadLocation;
  std::string sLoadLocation;
  bool        bLoadSettings;
  std::string sLoadSettings;
  bool        bSaveEXR;
  std::string sSaveEXR;
  bool        bSaveTIF;
  std::string sSaveTIF;
  bool        bSavePNG;
  std::string sSavePNG;
  bool        bSaveJPG;
  std::string sSaveJPG;
  bool        bSaveMap;
  std::string sSaveMap;
  bool        bSaveKFR;
  std::string sSaveKFR;
  bool        bZoomOut;
  int         nZoomOut;
  bool        bVersion;
  bool        bHelp;
  bool        bError;
  CommandLineArguments(const std::string &commandline);
};

extern const std::string usage;
extern const std::string version;
extern const int kfr_version_number;
extern const int kfs_version_number;

#endif
