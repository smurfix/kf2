set key top left
unset xtics
set log y 2
set ylabel "seconds"
set grid
set term pngcairo enhanced size 788,432
set output "builtin.png"
set title "Built-in"
set xlabel "formula"
plot [-1:130][:] "cpu/builtin.dat" u ($0 + floor($0/2) + 1):($4) w i t "CPU", "cl1/builtin.dat" u ($0 + floor($0/2) + 1 + 0.5):($4-$11) w i t "GPU"
set output "hybrid.png"
set title "Hybrid"
set xlabel "formula"
plot [-1:272][:] "cpu/hybrid.dat" u ($0 + floor($0/4) + 1):($5) w i t "CPU", "cl1/hybrid.dat" u ($0 + floor($0/4) + 1 + 0.5):($5-$12) w i t "GPU"
set output "mandelbrot.png"
set title "Mandelbrot"
set xlabel "location"
plot [-1:43][:] "cpu/mandelbrot.dat" u ($0 + floor($0/6) + 1):($4) w i t "CPU", "cl1/mandelbrot.dat" u ($0 + floor($0/6) + 1 + 0.5):($4-$11) w i t "GPU"
