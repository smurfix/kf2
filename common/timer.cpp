/*
Kalles Fraktaler 2
Copyright (C) 2021 Claude Heiland-Allen

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

// <https://stackoverflow.com/a/17440673>

#include <windows.h>

#ifndef WINVER
#include <time.h>
#endif

double get_wall_time(){
#ifdef WINVER
    LARGE_INTEGER time,freq;
    if (!QueryPerformanceFrequency(&freq)){
        //  Handle error
        return 0;
    }
    if (!QueryPerformanceCounter(&time)){
        //  Handle error
        return 0;
    }
    return (double)time.QuadPart / freq.QuadPart;
#else
    struct timespec t;
    if(clock_gettime(CLOCK_MONOTONIC_RAW, &t) == -1)
        return 0;
    return t.tv_sec + (double)t.tv_nsec/1000000000.;
#endif
}

double get_cpu_time(){
#ifdef WINVER
    FILETIME a,b,c,d;
    if (GetProcessTimes(GetCurrentProcess(),&a,&b,&c,&d) != 0){
        //  Returns total user time.
        //  Can be tweaked to include kernel times as well.
        return
            (double)(d.dwLowDateTime |
            ((unsigned long long)d.dwHighDateTime << 32)) * 0.0000001;
    }else{
        //  Handle error
        return 0;
    }
#else
    struct timespec t;
    if(clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t) == -1)
        return 0;
    return t.tv_sec + (double)t.tv_nsec/1000000000.;
    
#endif
}
