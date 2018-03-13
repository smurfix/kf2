#!/bin/bash
out="kf-$(../kf.exe --version | tr -d '\r')"
mkdir -p "${out}"
for low in fast best
do
  for loc in 1e14 1e50 5e113 5e227 4e533 1e1086
  do
    echo >> "${out}/${loc}_${low}.log" "# dimension refcount realsecs usersecs syssecs peakrsskb"
    for dim in 1 2 4 8 16 32 64 128 256 512 1024 2048 4096
    do
      sed "s/^ImageWidth: .*\r$/ImageWidth: $dim\r/" < "${low}.kfs" |
      sed "s/^ImageHeight: .*\r$/ImageHeight: $dim\r/" > "${out}/${loc}_${low}_${dim}.kfs"
      echo "START ${loc} ${low} ${dim}"
      /usr/bin/time --append --output "${out}/${loc}_${low}.log" --format="${dim} 0 %e %U %S %M" \
        ../kf.exe -s "${out}/${loc}_${low}_${dim}.kfs" -l "${loc}.kfr" -p "${out}/${loc}_${low}_${dim}.png" -j "${out}/${loc}_${low}_${dim}.jpg" -m "${out}/${loc}_${low}_${dim}.kfb"
      echo "END   ${loc} ${low} ${dim}"
      kfb-pseudo-de < "${out}/${loc}_${low}_${dim}.kfb" > "${out}/${loc}_${low}_${dim}.pgm"
    done
  done
done 2>&1 | ts | tee -a "${out}/benchmark.log"
