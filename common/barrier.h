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

  inline BOOL wait(volatile BOOL *stop)
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
          return 1;
        if (yielding)
          SwitchToThread();
      }
    return 0;
  };

};

#endif
