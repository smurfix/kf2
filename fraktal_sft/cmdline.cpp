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

#include "cmdline.h"
#include <vector>
#include <cctype>

LogLevel g_log_level = LogLevel_Status;

static std::vector<std::string> SplitCommandLine(const std::string&commandline)
{
	std::vector<std::string> args;
	bool backslash = false;
	char quote = 0;
	int n = commandline.length();
	std::string arg = "";
	for (int i = 0; i < n; ++i)
	{
		if (!backslash && quote == '\'' && commandline[i] == '\'')
		{
			quote = 0;
		}
		else if (!backslash && quote == '"' && commandline[i] == '"')
		{
			quote = 0;
		}
		else if (!backslash && (commandline[i] == '\'' || commandline[i] == '"'))
		{
			quote = commandline[i];
		}
		else if (!backslash && !quote && isspace(commandline[i]))
		{
			if (i > 0 && !isspace(commandline[i - 1]))
				args.push_back(arg);
			arg = "";
		}
		else if (backslash)
		{
			arg += commandline[i];
			backslash = false;
		}
		else if (commandline[i] == '\\')
		{
			backslash = true;
		}
		else
		{
			arg += commandline[i];
		}		
	}
	if (n > 0 && !isspace(commandline[n - 1]))
		args.push_back(arg);
	return args;
}

CommandLineArguments::CommandLineArguments(const std::string &commandline)
: bLoadMap(false)
, sLoadMap("")
, bLoadPalette(false)
, sLoadPalette("")
, bLoadLocation(false)
, sLoadLocation("")
, bLoadSettings(false)
, sLoadSettings("")
, bSaveEXR(false)
, sSaveEXR("")
, bSaveTIF(false)
, sSaveTIF("")
, bSavePNG(false)
, sSavePNG("")
, bSaveJPG(false)
, sSaveJPG("")
, bSaveMap(false)
, sSaveMap("")
, bSaveKFR(false)
, sSaveKFR("")
, bZoomOut(false)
, nZoomOut(0)
, bVersion(false)
, bHelp(false)
, bError(false)
{
	std::vector<std::string> args = SplitCommandLine(commandline);
  for (size_t i = 0; i < args.size(); ++i)
  {
		if ("-o" == args[i] || "--load-map" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bLoadMap = true;
				sLoadMap = args[i];
			}
			else
			{
				bError = true;
			}
		}
		else if ("-c" == args[i] || "--load-palette" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bLoadPalette = true;
				sLoadPalette = args[i];
			}
			else
			{
				bError = true;
			}
		}
		else if ("-l" == args[i] || "--load-location" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bLoadLocation = true;
				sLoadLocation = args[i];
			}
			else
			{
				bError = true;
			}
		}
		else if ("-s" == args[i] || "--load-settings" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bLoadSettings = true;
				sLoadSettings = args[i];
			}
			else
			{
				bError = true;
			}
		}
		else if ("-x" == args[i] || "--save-exr" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bSaveEXR = true;
				sSaveEXR = args[i];
			}
			else
			{
				bError = true;
			}
		}
		else if ("-t" == args[i] || "--save-tif" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bSaveTIF = true;
				sSaveTIF = args[i];
			}
			else
			{
				bError = true;
			}
		}
		else if ("-p" == args[i] || "--save-png" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bSavePNG = true;
				sSavePNG = args[i];
			}
			else
			{
				bError = true;
			}
		}
		else if ("-j" == args[i] || "--save-jpg" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bSaveJPG = true;
				sSaveJPG = args[i];
			}
			else
			{
				bError = true;
			}
		}
		else if ("-m" == args[i] || "--save-map" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bSaveMap = true;
				sSaveMap = args[i];
			}
			else
			{
				bError = true;
			}
		}
		else if ("--save-kfr" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bSaveKFR = true;
				sSaveKFR = args[i];
			}
			else
			{
				bError = true;
			}
		}
		else if ("-z" == args[i] || "--zoom-out" == args[i])
		{
			++i;
			if (i < args.size())
			{
				bZoomOut = true;
				nZoomOut = atoi(args[i].c_str());
			}
			else
			{
				bError = true;
			}
		}
		else if ("--log" == args[i])
		{
			++i;
			if (i < args.size())
			{
				if ("debug" == args[i]) g_log_level = LogLevel_Debug;
				else if ("status" == args[i]) g_log_level = LogLevel_Status;
				else if ("info" == args[i]) g_log_level = LogLevel_Info;
				else if ("warn" == args[i]) g_log_level = LogLevel_Warn;
				else if ("error" == args[i]) g_log_level = LogLevel_Error;
				else bError = true;
			}
			else
			{
				bError = true;
			}
		}
		else if ("-v" == args[i] || "-V" == args[i] || "--version" == args[i])
		{
			bVersion = true;
		}
		else if ("-h" == args[i] || "-H" == args[i] || "--help" == args[i] || "-?" == args[i])
		{
			bHelp = true;
		}
		else
		{
			bError = true;
		}
	}
}

const std::string usage =
"kf.exe [options]\n"
"    -o, --load-map      [FILE.kfb]  load map file\n"
"    -c, --load-palette  [FILE.kfp]  load palette file\n"
"    -l, --load-location [FILE.kfr]  load location file\n"
"    -s, --load-settings [FILE.kfs]  load settings file\n"
"    -x, --save-exr      [FILE.exr]  save EXR\n"
"    -t, --save-tif      [FILE.tif]  save TIFF\n"
"    -p, --save-png      [FILE.png]  save PNG\n"
"    -j, --save-jpg      [FILE.jpg]  save JPEG\n"
"    -m, --save-map      [FILE.kfb]  save KFB\n"
"        --save-kfr      [FILE.kfr]  save KFR\n"
"    -z, --zoom-out      [NFRAMES]   zoom sequence\n"
"    --log (debug|status|info|warn|error)\n"
"                                    logging verbosity\n"
"    -v, -V, --version               show version\n"
"    -h, -H, -?, --help              show this help\n"
;

const std::string version = "2.14.10+git1";

// bump these when changing what is saved in KFR/KFS
const int kfr_version_number = 2;
const int kfs_version_number = 2;
