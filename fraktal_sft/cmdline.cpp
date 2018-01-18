#include "cmdline.h"
#include <vector>
#include <cctype>

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
: bLoadLocation(false)
, sLoadLocation("")
, bLoadSettings(false)
, sLoadSettings("")
, bSavePNG(false)
, sSavePNG("")
, bSaveJPG(false)
, sSaveJPG("")
, bSaveMap(false)
, sSaveMap("")
, bVersion(false)
, bHelp(false)
, bError(false)
{
	std::vector<std::string> args = SplitCommandLine(commandline);
  for (size_t i = 0; i < args.size(); ++i)
  {
		if ("-l" == args[i] || "--load-location" == args[i])
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
"    -l, --load-location [FILE.kfr]  load location file\n"
"    -s, --load-settings [FILE.kfs]  load settings file\n"
"    -p, --save-png      [FILE.png]  save PNG\n"
"    -j, --save-jpg      [FILE.jpg]  save JPEG\n"
"    -m, --save-map      [FILE.kfb]  save KFB\n"
"    -v, -V, --version               show version\n"
"    -h, -H, -?, --help              show this help\n"
;

const std::string version = "2.13.1";
