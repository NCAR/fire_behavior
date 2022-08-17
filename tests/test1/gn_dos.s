#! /bin/sh
#
#helpini
# _______________________________________________________________________
#
#     Script gn_uno
#
#     Proposito:   Pinta unha serie con gnplot
#     Metodo:      obvio
#     Comentarios: 
#
# _______________________________________________________________________
#helpfin	
#
cte=1.0
tit=$3
cat>plot.1<<m1
set size 1,1
set title "$3" font 'Times-Roman,25'
set ylabel "Values" font 'Times-Roman,25'
#set yrange [-20:20]
set ytic autofreq font 'Times-Roman,25'
#set grid y
#
set xlabel 'Time [s]' font 'Times-Roman,25'
set xrange [240:360]
set xtic autofreq font 'Times-Roman,25'
#set xtic ("7" 7, "72 (half a day)" 72, "144 (a day)" 144, "216" 216) font 'Times-Roman,20'
#set xtic (" " 1)
#set grid x
#
set datafile missing '-9999.9004'
set style line 1 lt 1 lw 3 pt 7 ps 1
set style line 2 lt 3 lw 1 pt 2 ps 0.5
#set logscale y
#set logscale x
#plot '$1' u (\$2) notitle w linespoints linestyle 1
plot '$1' u (\$1):(\$2) title 'Stand alone' w lines linestyle 1, '$2' u (\$1):(\$2) title 'WRF' w lines linestyle 2
#plot '$1' u (\$1-1):(\$7) notitle w linespoints linestyle 1
#plot '$1' u (\$1):(\$7-\$8)/2 notitle w linespoints linestyle 1
#plot '$1' u (\$7):(\$8) notitle w points linestyle 1
#plot '$1' u (\$1):(\$3 ) title 'GN' w linespoints linestyle 1, '$2' u (\$1):(\$3 ) title 'INM' w linespoints linestyle 2, '$3' u (\$1):(\$3) title 'RN' w linespoints linestyle 3
#plot  0 notitle w l linestyle 3, '$1' u (\$1):(\$7) notitle w l linestyle 1
#pause 40
set output "plot.ps"
#set term post landscape "Times-Roman" 14
set term post landscape color solid  "Times-Roman" 14
replot
m1
gnuplot< plot.1








