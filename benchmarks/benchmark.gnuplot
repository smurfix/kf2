# Kalles Fraktaler 2
# Copyright (C) 2013-2017 Karl Runmo
# Copyright (C) 2017-2018 Claude Heiland-Allen
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

set term pngcairo enhanced round dashed dashlength 5 linewidth 1 font "LMSans10" fontscale 2 size 3840,2160
set output "benchmark.png"

set title "Mandelbrot set deep zoom rendering cost"
set xlabel "pixel count"
set ylabel "CPU seconds per pixel"
set format "%h"
set log
set grid lt 0 lw 1
set key reverse Left samplen 8 opaque

set pointsize 3

set dashtype 1 (100,0)
set dashtype 2 (2,4)
set dashtype 3 (6,6)
set dashtype 10 (2,2)
set dashtype 20 (100,0)
set dashtype 30 (0,100)

set arrow 1 from 1,0.01 to 100,0.0001
set arrow 1 heads size 1,30 filled linewidth 1 linecolor 0 dashtype 20
set label 1 "constant cost per image" at 10,0.0015 center rotate by -24 textcolor rgb "black" nopoint

set arrow 2 from 1,0.00001 to 100,0.00001
set arrow 2 heads size 1,30 filled linewidth 1 linecolor 0 dashtype 20
set label 2 "constant cost per pixel" at 10,0.000015 center textcolor rgb "black" nopoint

set label 3 "1×1" at 1,2e-6 center textcolor rgb "black" nopoint
set label 4 "2×2" at 4,2e-6 center textcolor rgb "black" nopoint
set label 5 "4×4" at 16,2e-6 center textcolor rgb "black" nopoint
set label 6 "8×8" at 64,2e-6 center textcolor rgb "black" nopoint
set label 7 "16×16" at 256,2e-6 center textcolor rgb "black" nopoint
set label 8 "32×32" at 1024,2e-6 center textcolor rgb "black" nopoint
set label 9 "64×64" at 4096,2e-6 center textcolor rgb "black" nopoint
set label 10 "128×128" at 16384,2e-6 center textcolor rgb "black" nopoint
set label 11 "256×256" at 65536,2e-6 center textcolor rgb "black" nopoint
set label 12 "512×512" at 262144,2e-6 center textcolor rgb "black" nopoint
set label 13 "1024×1024" at 1048576,2e-6 center textcolor rgb "black" nopoint
set label 14 "2048×2048" at 4194304,2e-6 center textcolor rgb "black" nopoint
set label 15 "4096×4096" at 16777216,2e-6 center textcolor rgb "black" nopoint

set label 16 "1000" at 3.16,316 center rotate by -24 textcolor rgb "black" nopoint
set label 17 "10000" at 31.6,316 center rotate by -24 textcolor rgb "black" nopoint
set label 18 "100000" at 316,316 center rotate by -24 textcolor rgb "black" nopoint
set label 19 "1×10^{6}" at 3160,316 center rotate by -24 textcolor rgb "black" nopoint

plot [0.5:5e7] \
  0 t "zoom\t\titerations\tskipping" lc rgb "white" dt 30 lw 10 w l, \
  0 t "1×10^{1086}\t540445\t493126" lc 1 dt 20 lw 10 w l, \
  0 t "5×10^{227}\t1172649\t1028485" lc 2 dt 20 lw 10 w l, \
  0 t "4×10^{533}\t105891\t99110"  lc 3 dt 20 lw 10 w l, \
  0 t "1×10^{50}\t192862\t131318"   lc 4 dt 20 lw 10 w l, \
  0 t "5×10^{113}\t24086\t20060"   lc 7 dt 20 lw 10 w l, \
  0 t "1×10^{14}\t1070\t\t297"     lc 6 dt 20 lw 10 w l, \
  0 t "software\t\t\tthreshold" lc rgb "white" dt 30 lw 10 w l, \
  0 t "Kalles Fraktaler 2.12.11\tbest"  w lp lc 0 dt 1 lw 1 pt 9, \
  0 t "Kalles Fraktaler 2.12.11\tfast"  w lp lc 0 dt 1 lw 1 pt 10 ps 4.25, \
  0 t "Kalles Fraktaler 2.12.5\t3×10^{-4}" w lp lc 0 dt 2 lw 2 pt 13, \
  0 t "Kalles Fraktaler 2.12.5\t1×10^{-7}" w lp lc 0 dt 2 lw 2 pt 4, \
  0 t "MDZ 0.1.3\t\t\tn/a"                      w lp lc 0 dt 3 lw 1.5 pt 3, \
  "grid.log" t "" lt 0 lw 1 lc 0 w l, \
  "kf-2.12.11/1e1086_best.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)  t "" lc 1 pt 9  dt 1 w lp, \
  "kf-2.12.11/1e1086_fast.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)  t "" lc 1 pt 10 ps 4.25 dt 1 w lp, \
  "1e1086_kf1.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 1 pt 13 dt 2 lw 2 w lp, \
  "1e1086_kf0.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 1 pt 4 dt 2 lw 2 w lp, \
  "1e1086_mdz.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 1 pt 3 dt 3 lw 1.5 w lp, \
  "1e1086_mdz_extrapolate.log" u ($1**2):($2) t "" lc 1 dt 10 lw 0.5 w l, \
  "kf-2.12.11/5e227_best.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)  t "" lc 2 pt 9  dt 1 w lp, \
  "kf-2.12.11/5e227_fast.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)  t "" lc 2 pt 10 ps 4.25 dt 1 w lp, \
  "5e227_kf1.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 2 pt 13 dt 2 lw 2 w lp, \
  "5e227_kf0.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 2 pt 4 dt 2 lw 2 w lp, \
  "5e227_mdz.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 2 pt 3 dt 3 lw 1.5 w lp, \
  "5e227_mdz_extrapolate.log" u ($1**2):($2) t "" lc 2 dt 10 lw 0.5 w l, \
  "kf-2.12.11/4e533_best.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)   t "" lc 3 pt 9  dt 1 w lp, \
  "kf-2.12.11/4e533_fast.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)   t "" lc 3 pt 10 ps 4.25 dt 1 w lp, \
  "4e533_kf1.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 3 pt 13 dt 2 lw 2 w lp, \
  "4e533_kf0.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 3 pt 4 dt 2 lw 2 w lp, \
  "4e533_mdz.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 3 pt 3 dt 3 lw 1.5 w lp, \
  "4e533_mdz_extrapolate.log" u ($1**2):($2) t "" lc 3 dt 10 lw 0.5 w l, \
  "kf-2.12.11/1e50_best.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)   t "" lc 4 pt 9  dt 1 w lp, \
  "kf-2.12.11/1e50_fast.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)   t "" lc 4 pt 10 ps 4.25 dt 1 w lp, \
  "1e50_kf1.log" u ($1**2):(($4+$5)/$1**2)   t "" lc 4 pt 13 dt 2 lw 2 w lp, \
  "1e50_kf0.log" u ($1**2):(($4+$5)/$1**2)   t "" lc 4 pt 4 dt 2 lw 2 w lp, \
  "1e50_mdz.log" u ($1**2):(($4+$5)/$1**2)   t "" lc 4 pt 3 dt 3 lw 1.5 w lp, \
  "1e50_mdz_extrapolate.log" u ($1**2):($2)  t "" lc 4 dt 10 lw 0.5 w l, \
  "kf-2.12.11/5e113_best.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)   t "" lc 7 pt 9  dt 1 w lp, \
  "kf-2.12.11/5e113_fast.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)   t "" lc 7 pt 10 ps 4.25 dt 1 w lp, \
  "5e113_kf1.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 7 pt 13 dt 2 lw 2 w lp, \
  "5e113_kf0.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 7 pt 4 dt 2 lw 2 w lp, \
  "5e113_mdz.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 7 pt 3 dt 3 lw 1.5 w lp, \
  "5e113_mdz_extrapolate.log" u ($1**2):($2) t "" lc 7 dt 10 lw 0.5 w l, \
  "kf-2.12.11/1e14_best.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)   t "" lc 6 pt 9  dt 1 w lp, \
  "kf-2.12.11/1e14_fast.log" u ($1**2):(($4+$5)/$1**2 * 3.0/2.3)   t "" lc 6 pt 10 ps 4.25 dt 1 w lp, \
  "1e14_kf1.log" u ($1**2):(($4+$5)/$1**2)   t "" lc 6 pt 13 dt 2 lw 2 w lp, \
  "1e14_kf0.log" u ($1**2):(($4+$5)/$1**2)   t "" lc 6 pt 4 dt 2 lw 2 w lp, \
  "1e14_mdz.log" u ($1**2):(($4+$5)/$1**2)   t "" lc 6 pt 3 dt 3 lw 1.5 w lp, \
  "1e14_mdz_extrapolate.log" u ($1**2):($2)  t "" lc 6 dt 10 lw 0.5 w l \

#  0 t "mandelbrot-perturbator\t1×10^{-3}"  w lp lc 0 dt 1 lw 1 pt 9, \
#  0 t "mandelbrot-perturbator\t1×10^{-6}"  w lp lc 0 dt 1 lw 1 pt 10 ps 4.25, \
#  "1e1086_mp3.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 1 pt 9 dt 1 w lp, \
#  "1e1086_mp6.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 1 pt 10 ps 4.25 dt 1 w lp, \
#  "5e227_mp3.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 2 pt 9 dt 1 w lp, \
#  "5e227_mp6.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 2 pt 10 ps 4.25 dt 1 w lp, \
#  "4e533_mp3.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 3 pt 9 dt 1 w lp, \
#  "4e533_mp6.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 3 pt 10 ps 4.25 dt 1 w lp, \
#  "1e50_mp3.log" u ($1**2):(($4+$5)/$1**2)   t "" lc 4 pt 9 dt 1 w lp, \
#  "1e50_mp6.log" u ($1**2):(($4+$5)/$1**2)   t "" lc 4 pt 10 ps 4.25 dt 1 w lp, \
#  "5e113_mp3.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 7 pt 9 dt 1 w lp, \
#  "5e113_mp6.log" u ($1**2):(($4+$5)/$1**2)  t "" lc 7 pt 10 ps 4.25 dt 1 w lp, \
#  "1e14_mp3.log" u ($1**2):(($4+$5)/$1**2)   t "" lc 6 pt 9 dt 1 w lp, \
#  "1e14_mp6.log" u ($1**2):(($4+$5)/$1**2)   t "" lc 6 pt 10 ps 4.25 dt 1 w lp, \

