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

#ifndef BARRIER_H
#define BARRIER_H 1

#include <windows.h>

// centralized barrier from
// https://www.jlab.org/hpc/papers/hpcasia07.pdf

class barrier
{

private:

  volatile LONG release;
  LONG padding1[63];
  volatile LONG counter;
  LONG padding2[63];
  LONG num_thread;
  bool yielding;

public:

  inline barrier(LONG n)
  : release(0), counter(n), num_thread(n)
  {
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    yielding = LONG(sysinfo.dwNumberOfProcessors) < num_thread;
  };

  inline bool wait(volatile bool *stop)
  {
    LONG flag = release;
    LONG count = InterlockedDecrement(&counter);
    if (count == 0)
    {
      counter = num_thread;
      InterlockedIncrement(&release);
    }
    else
      while (flag == release)
      {
        if (*stop)
          return true;
        if (yielding)
          SwitchToThread();
      }
    return false;
  };

};

#endif
