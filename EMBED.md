# Building a KF2 library

Use these steps to build a `libkf2-embed.so` dynamic library.

The following steps assume Linux/Debian (should also work on Ubuntu).
Additions for other distributions, \*BSD, OS-X, etc., are welcome.


## Install development libraries

Rather than running `prepare.sh`, we use our distribution's libraries.

On Debian, install these packages:

- build-essential
- libpixman-dev
- libopenexr-dev
- libgsl-dev
- libjpeg62-turbo-dev
- libmpc-dev

using `sudo apt install`.

## Build

- make clean
- make embed
- make embed  # you most likely need to do this twice

## install

- sudo cp libkf2-embed.so /usr/local/lib

If `/usr/local/lib` is not in your `/etc/ld.so.conf` (or in a file included
by that, on newer Linux distributions; most typically in `/etc/ld.so.conf.d/libc.conf`),
add a line with that path, then run `sudo ldconfig`.

If you do not want to install the library yet(?), do

- `export LD_LIBRARY_PATH=$(pwd)`

instead.

# Build the Python wrapper

## Get the source

- git clone https://github.com/smurfix/kf2-py.git python
- cd python

## install modules

- On Debian: `sudo dpkg -i $(grep -v '#' REQUIREMENTS)`

## Build

- make ext

## Test

- scripts/kf2 --version

If that shows your KF2 source's version number, success! You have cleared
the first hurdle.

## Debugging

When built with embedding, this library manages to find a GDB bug.

Workaround:

- get GDB sources
- edit gdb/gdbtypes.c
- patch internal_type_vptr_fieldno function:

	   type = check_typedef (type);
    -   gdb_assert (type->code () == TYPE_CODE_STRUCT
    -              || type->code () == TYPE_CODE_UNION);

	+  if (type->code () != TYPE_CODE_STRUCT
	+      && type->code () != TYPE_CODE_UNION) {
	+    return -1;
	+  }

- build, install, and use this version.

