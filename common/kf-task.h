#ifndef KF_TASK_H
#define KF_TASK_H

#if defined(__clang__) || !defined(WINVER)
#include <mutex>
#include <shared_mutex>
#include <thread>
#include <condition_variable>
#else
#include <mingw-std-threads/mingw.mutex.h>
#include <mingw-std-threads/mingw.shared_mutex.h>
#include <mingw-std-threads/mingw.thread.h>
#include <mingw-std-threads/mingw.condition_variable.h>
#endif

#endif
