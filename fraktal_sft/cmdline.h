#ifndef KF_CMDLINE_H
#define KF_CMDLINE_H 1

#include <string>

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
