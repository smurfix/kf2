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
  bool        bLoadLocation;
  std::string sLoadLocation;
  bool        bLoadSettings;
  std::string sLoadSettings;
  bool        bSavePNG;
  std::string sSavePNG;
  bool        bSaveJPG;
  std::string sSaveJPG;
  bool        bSaveMap;
  std::string sSaveMap;
  bool        bVersion;
  bool        bHelp;
  bool        bError;
  CommandLineArguments(const std::string &commandline);
};

extern const std::string usage;
extern const std::string version;

#endif
