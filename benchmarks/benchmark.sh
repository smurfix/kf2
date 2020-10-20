#!/bin/bash
# Kalles Fraktaler 2
# Copyright (C) 2013-2017 Karl Runmo
# Copyright (C) 2017-2020 Claude Heiland-Allen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
out="${HOSTNAME}-$(../kf.exe --version | tr -d '\r')"
mkdir -p "${out}"
for ocl in 0 1
do
  for loc in 1e14 1e50 5e113 5e227 4e533 1e1086
  do
    echo >> "${out}/${loc}_${ocl}.log" "# dimension refcount realsecs usersecs syssecs peakrsskb"
    for dim in 256 1024 4096
    do
      cat "fast.kfs" |
      sed "s/^UseOpenCL: .*\r$/UseOpenCL: $ocl\r/" |
      sed "s/^ImageWidth: .*\r$/ImageWidth: $dim\r/" |
      sed "s/^ImageHeight: .*\r$/ImageHeight: $dim\r/" > "${out}/${loc}_${ocl}_${dim}.kfs"
      echo "START ${loc} ${ocl} ${dim}"
      /usr/bin/time --append --output "${out}/${loc}_${ocl}.log" --format="${dim} 0 %e %U %S %M" \
        ../kf.exe --log info -s "${out}/${loc}_${ocl}_${dim}.kfs" -l "${loc}.kfr" -p "${out}/${loc}_${ocl}_${dim}.png"
      echo "END   ${loc} ${ocl} ${dim}"
    done
  done
done 2>&1 | ts | tee -a "${out}/benchmark.log"
