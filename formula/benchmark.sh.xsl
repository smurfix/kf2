<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" />
<xsl:template match="/">#!/bin/bash
set -euo pipefail
kf="${1:-../kf.exe}"
platform="${2:--1}"
opencl=$((platform &gt;= 0))
platformname=cpu
if (( opencl ))
then
  platformname="cl$platform"
fi
out="$("$kf" --version | tr -d '\r')/${HOSTNAME:-unknown}/$platformname"
mkdir -p "$out"
echo "================================================================="
echo "== benchmarking built-in formulas                              =="
echo "================================================================="
(
  echo "# fractaltype power derivatives totalwall totalcpu refwall refcpu apxwall apxcpu ptbwall ptbcpu"
  cat &lt;&lt; EOF |
<xsl:for-each select="formulas/group/formula"><xsl:value-of select="../@type" /><xsl:text> </xsl:text><xsl:value-of select="@power" /><xsl:text>
</xsl:text></xsl:for-each>EOF
  while read type power
  do
    for derivatives in 0 1
    do
      n="builtin-$type-$power-$derivatives"
      echo -e "OpenResetsParameters: 0\r\nImageWidth: 1024\r\nImageHeight: 1024\r\nJitterSeed: 1\r\nDerivatives: $derivatives\r\nUseOpenCL: $opencl\r\nOpenCLPlatform: $platform\r\nGuessing: $((1 - opencl))\r" &gt; "$out/$n.kfs"
      echo -e "Re: 0.000001\r\nIm: 0.000000001\r\nIterations: 10100\r\nFractalType: $type\r\nPower: $power\r\nDifferences: $((3 + 4 * $derivatives))\r" &gt; "$out/$n.kfr"
      "$kf" --log info -s "${out}/$n.kfs" -l "${out}/$n.kfr" -t "${out}/$n.tif" 2&gt;&amp;1 |
      tail -n 4 |
      tr -d '\r' |
      (
        read junk1 junk2 totalwall totalcpu
        read junk1 junk2 refwall refcpu
        read junk1 junk2 apxwall apxcpu
        read junk1 junk2 ptbwall ptbcpu
        echo "$type $power $derivatives $totalwall $totalcpu $refwall $refcpu $apxwall $apxcpu $ptbwall $ptbcpu"
      )
    done
  done
) |
tee -a "$out/builtin.dat"
echo "================================================================="
echo "== benchmarking Hybrid formulas                                =="
echo "================================================================="
(
  echo "# fractaltype power derivatives totalwall totalcpu refwall refcpu apxwall apxcpu ptbwall ptbcpu"
  for hybrid in ../formulas/*.kfr
  do
    type="$(basename "$hybrid" | sed "s/-.*$//" | sed "s/^0//")"
    power="$(basename "$hybrid" | sed "s/^..-//" | sed "s/-.*$//" | sed "s/^0//")"
    for derivatives in 0 1
    do
      n="hybrid-$type-$power-$derivatives"
      echo -e "OpenResetsParameters: 0\r\nImageWidth: 1024\r\nImageHeight: 1024\r\nJitterSeed: 1\r\nDerivatives: $derivatives\r\nUseOpenCL: $opencl\r\nOpenCLPlatform: $platform\r\nGuessing: $((1 - opencl))\r" &gt; "$out/$n.kfs"
      ( cat "$hybrid" &amp;&amp; echo -e "Re: 0.000001\r\nIm: 0.000000001\r\nIterations: 10100\r\nDifferences: $((3 + 4 * $derivatives))\r" ) &gt; "$out/$n.kfr"
      "$kf" --log info -s "${out}/$n.kfs" -l "${out}/$n.kfr" -t "${out}/$n.tif" 2&gt;&amp;1 |
      tail -n 4 |
      tr -d '\r' |
      (
        read junk1 junk2 totalwall totalcpu
        read junk1 junk2 refwall refcpu
        read junk1 junk2 apxwall apxcpu
        read junk1 junk2 ptbwall ptbcpu
        echo "$type $power $derivatives $totalwall $totalcpu $refwall $refcpu $apxwall $apxcpu $ptbwall $ptbcpu"
      )
    done
  done
) |
tee -a "$out/hybrid.dat"
echo "================================================================="
echo "== benchmarking deep Mandelbrot set power 2                    =="
echo "================================================================="
echo "# location dimension derivatives totalwall totalcpu refwall refcpu apxwall apxcpu ptbwall ptbcpu"
for loc in 1e14 1e50 5e113 5e227 4e533 1e1086
do
  for dim in 256 1024 4096
  do
    for derivatives in 0 1
    do
      n="mandelbrot-$loc-$dim-$derivatives"
      cat $loc.kfr |
      sed "s/Differences: .\r/Differences: $((3 + 4 * derivatives))\r/" &gt; "${out}/$n.kfr"
      cat "fast.kfs" |
      sed "s/^UseOpenCL: .*\r$/UseOpenCL: $opencl\r/" |
      sed "s/^OpenCLPlatform: .*\r$/OpenCLPlatform: $platform\r/" |
      sed "s/^Guessing: .*\r$/Guessing: $((1 - opencl))\r/" |
      sed "s/^ImageWidth: .*\r$/ImageWidth: $dim\r/" |
      sed "s/^ImageHeight: .*\r$/ImageHeight: $dim\r/" &gt; "${out}/$n.kfs"
      "$kf" --log info -s "${out}/$n.kfs" -l "$out/$n.kfr" -t "${out}/$n.tif" 2&gt;&amp;1 |
      tail -n 4 |
      tr -d '\r' |
      (
        read junk1 junk2 totalwall totalcpu
        read junk1 junk2 refwall refcpu
        read junk1 junk2 apxwall apxcpu
        read junk1 junk2 ptbwall ptbcpu
        echo "$loc $dim $derivatives $totalwall $totalcpu $refwall $refcpu $apxwall $apxcpu $ptbwall $ptbcpu"
      )
    done
  done
done |
tee -a "${out}/mandelbrot.dat"
</xsl:template>
</xsl:stylesheet>
