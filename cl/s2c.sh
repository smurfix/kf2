#!/bin/bash
echo "/* machine-generated file, do not edit */"
echo "const char *$1 ="
sed 's|//.*||' |
sed 's|^|"|' |
sed 's|$|\\n"|'
echo ";"
