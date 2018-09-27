/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2018 Claude Heiland-Allen

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

#include <windows.h>
#include <tchar.h>
#include <urlmon.h>
#include <wininet.h>

#include <sstream>

#include "check_for_update.h"

std::string CheckForUpdate()
{
  std::string sret = "unknown";
  const auto url = TEXT("https://mathr.co.uk/kf/VERSION.txt");
  const auto prefix = TEXT("KF-VERSION");
  TCHAR dir[MAX_PATH];
  DWORD dret = GetTempPath(MAX_PATH, dir);
  if (0 < dret && dret < MAX_PATH)
  {
    TCHAR path[MAX_PATH];
    UINT uret = GetTempFileName(dir, prefix, 0, path);
    if (uret)
    {
      DeleteUrlCacheEntry(url);
      HRESULT hr = URLDownloadToFile(nullptr, url, path, 0, nullptr);
      if (SUCCEEDED(hr))
      {
        HANDLE f = CreateFile(path, GENERIC_READ, 0, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
        if (f != INVALID_HANDLE_VALUE)
        {
#define BUFFERSIZE 64
          char buffer[BUFFERSIZE];
          DWORD bytes = 0;
          if (ReadFile(f, buffer, BUFFERSIZE, &bytes, nullptr))
          {
            if (bytes < BUFFERSIZE - 1)
            {
              buffer[bytes] = 0;
              int v[5] = { 0, 0, 0, 0, 0 };
              if (4 == sscanf(buffer, "%d.%d.%d.%d\n", v+0, v+1, v+2, v+3)
               || 3 == sscanf(buffer, "%d.%d.%d\n", v+0, v+1, v+2))
              {
                std::ostringstream os;
                os << v[0] << "." << v[1] << "." << v[2];
                if (v[3]) os << "." << v[3];
                sret = os.str();
              }
            }
          }
          CloseHandle(f);
        }
        DeleteFile(path);
        DeleteUrlCacheEntry(url);
      }
    }
  }
  return sret;
}
