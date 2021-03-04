/*
Kalles Fraktaler 2
Copyright (C) 2017-2021 Claude Heiland-Allen

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

#ifndef KF_FIFO_H
#define KF_FIFO_H 1

#include <queue>
#ifdef __clang__
#include <mutex>
#include <condition_variable>
#else
#include <mingw-std-threads/mingw.mutex.h>
#include <mingw-std-threads/mingw.condition_variable.h>
#endif

template <typename T>
struct fifo
{
  std::mutex lock;
  std::condition_variable cond;
  std::queue<T> queue;
};

template <typename T>
void fifo_write(fifo<T> &f, T &m)
{
  std::unique_lock<std::mutex> lock(f.lock);
  f.queue.push(m);
  f.cond.notify_all();
}

template <typename T>
bool fifo_read(fifo<T> &f, T &m)
{
  std::unique_lock<std::mutex> lock(f.lock);
  while (f.queue.empty())
  {
    f.cond.wait(lock);
  }
  m = f.queue.front();
  f.queue.pop();
  return true;
}

#endif
