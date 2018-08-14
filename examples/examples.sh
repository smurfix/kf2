#!/bin/bash
rm examples.html
xsltproc examples.xsl ../formula/formula.xml | tee /dev/stderr |
while read FractalType Power Name
do
    stem="${FractalType}-${Power}"
    echo >> examples.html "<a href='formula/${stem}.kfr' title='${Name} (power ${Power})'><img src='formula/${stem}.png' alt='${Name} (power ${Power})' /></a>"
    cat <<EOF | sed -s 's/$/\r/g' > "${stem}.kfr"
Re: 0
Im: 0
Zoom: 1
Iterations: 1000
IterDiv: 0.010000
SmoothMethod: 0
ColorMethod: 7
Differences: 3
ColorOffset: 0
Rotate: 0.000000
Ratio: 360.000000
Colors: 255,255,255,128,0,64,160,0,0,192,128,0,64,128,0,0,255,255,64,128,255,0,0,255,
InteriorColor: 0,0,0,
Smooth: 1
MultiColor: 0
BlendMC: 0
MultiColors: 
Power: ${Power}
FractalType: ${FractalType}
Slopes: 1
SlopePower: 50
SlopeRatio: 20
SlopeAngle: 45
imag: 1
real: 1
SeedR: 0
SeedI: 0
FactorAR: 1
FactorAI: 0
EOF
    ../kf.exe -s examples.kfs -l "${stem}.kfr" -p "${stem}.png" --log warn
    pngtopnm -text "${stem}.txt" "${stem}.png" > "${stem}.ppm"
    pnmscale -reduce 4 "${stem}.ppm" |
    pnmtopng -text "${stem}.txt" -force -interlace -compression 9 > "${stem}.png"
    rm "${stem}.ppm" "${stem}.txt"
done
