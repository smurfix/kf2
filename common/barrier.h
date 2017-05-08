#ifndef BARRIER_H
#define BARRIER_H 1


#if 0

#include <windows.h>

// https://web.archive.org/web/20160310235613/http://greenteapress.com/semaphores/downey08semaphores.pdf
// p44 (56) 3.6.7 Barrier objects

class barrier
{

private:

  LONG n;
  LONG volatile a;
  LONG volatile b;
  LONG volatile c;

public:

  inline barrier(LONG n)
  : n(n), a(0), b(0), c(0)
  { };

  inline BOOL wait(volatile BOOL *stop)
  {
    if (n == InterlockedIncrement(&a))
      InterlockedAdd(&b, n);
    while (InterlockedDecrement(&b) < 0)
    {
      InterlockedIncrement(&b);
      if (*stop) return 1;
      SwitchToThread();
    }
    if (0 == InterlockedDecrement(&a))
      InterlockedAdd(&c, n);
    while (InterlockedDecrement(&c) < 0)
    {
      InterlockedIncrement(&c);
      if (*stop) return 1;
      SwitchToThread();
    }
    return 0;
  };

};

#endif


#if 1

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


#endif
