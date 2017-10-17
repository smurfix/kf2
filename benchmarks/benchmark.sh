#!/bin/bash
for loc in 1e14 1e50 5e113 5e227 4e533 1e1086
do
  for low in 0 1
  do
    echo >> "${loc}_${low}.log" "# dimension refcount realsecs usersecs syssecs peakrsskb"
    for dim in 1 2 4 8 16 32 64 128 256 512 1024 2048 4096
    do
      cat <<EOF | sed "s/$/\r/" > "${loc}_${low}_${dim}.kfs"
ZoomSize: 2
MaxReferences: 10000
GlitchLowTolerance: $low
ApproxLowTolerance: 1
AutoApproxTerms: 1
ApproxTerms: 10
WindowWidth: 512
WindowHeight: 512
WindowTop: 249
WindowLeft: 698
WindowBottom: 582
WindowRight: 524
ImageWidth: $dim
ImageHeight: $dim
AnimateZoom: 1
ArbitrarySize: 1
ReuseReference: 0
AutoSolveGlitches: 1
SolveGlitchNear: 0
NoApprox: 0
Mirror: 0
LongDoubleAlways: 0
FloatExpAlways: 0
AutoIterations: 1
ShowGlitches: 1
EOF
      echo "START ${loc} ${low} ${dim}"
      /usr/bin/time --append --output "${loc}_${low}.log" --format="${dim} 0 %e %U %S %M" \
        ../kf.exe -s "${loc}_${low}_${dim}.kfs" -l "${loc}.kfr" -p "${loc}_${low}_${dim}.png" -j "${loc}_${low}_${dim}.jpg" -m "${loc}_${low}_${dim}.kfb"
      echo "END   ${loc} ${low} ${dim}"
      kfb-pseudo-de < "${loc}_${low}_${dim}.kfb" > "${loc}_${low}_${dim}.pgm"
    done
  done
done 2>&1 | ts | tee -a benchmark.log
