set terminal png
set output 'magnVSt.png'

set ylabel 'num species'
set xlabel 't'
plot 'Results.dat' u 1:2 w l t 'P','' u 1:3 w l t 'M','' u 1:4 w l t 'DNA','' u 1:5 w l t 'PDNA'

set terminal png
set output 'M_freq.png'

set ylabel 'num species'
set xlabel 'M_st'
plot 'hist_m.dat' u 1:2 w boxes t 'M'


set terminal png
set output 'P_freq.png'

set ylabel 'freq'
set xlabel 'P_st'
plot 'hist_p.dat' u 1:2 w boxes t 'P' 

set terminal png
set output 'evo_promig.png'

plot 'Results_repes.dat' u 1:2 w l t 'P','' u 1:3 w l t 'M','' u 1:4 w l t 'DNA',\
      'Resultats_determinista.dat' u 1:2 w l t 'P','' u 1:3 w l t 'M','' u 1:4 w l t 'DNA'
