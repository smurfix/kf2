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

public:

  inline barrier(LONG n)
  : release(0), counter(n), num_thread(n)
  { };

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
        SwitchToThread();
      }
    return 0;
  };

};

#endif
