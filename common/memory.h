/*
Kalles Fraktaler 2
Copyright (C) 2020 Claude Heiland-Allen

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

#ifndef MEMORY_H
#define MEMORY_H 1

// aligned memory (de)allocation

#include <cstdlib>

#if 0

// C++17 feature not yet supported by MinGW in Debian testing

template <typename T>
T *new_aligned(size_t count)
{
	const size_t alignment = 1 << 12;
	size_t bytes = count * sizeof(T);
	// size must be multiple of alignment
	bytes += alignment - 1;
	bytes /= alignment;
	bytes *= alignment;
	return static_cast<T *>(std::aligned_alloc(alignment, bytes));
}

template <typename T>
void delete_aligned(T *ptr)
{
	if (ptr)
	{
		std::free(ptr);
	}
}

#else

// store malloc'd pointer just before the aligned memory pointer

#include <memory>
// #include <cstdio>

template <typename T>
T *new_aligned(size_t count)
{
	const size_t alignment = 1 << 12;
	size_t bytes = count * sizeof(T) + sizeof(void *);
	bytes += 2 * alignment - 1;
	bytes &= ~(alignment - 1);
	void *ptr = std::malloc(bytes);
	if (! ptr)
	{
		return nullptr;
	}
	void *ptr2 = ((char *) ptr) + sizeof(void *);
	size_t space = bytes - sizeof(void *);
	void **ret = (void **) std::align(alignment, count * sizeof(T), ptr2, space);
	if (! ret)
	{
		return nullptr;
	}
	ret[-1] = ptr;
// std::fprintf(stderr, "new_aligned(%p): %p %p %p\n", (void *) (uintptr_t) bytes, ptr, ret, &ret[-1]);
	return (T *) ret;
}

template <typename T>
void delete_aligned(T *ptr)
{
	void **ret = (void **) ptr;
// std::fprintf(stderr, "delete_aligned(): %p %p %p\n", ret[-1], ptr, &ret[-1]);
	std::free(ret[-1]);
}

#endif

#endif
