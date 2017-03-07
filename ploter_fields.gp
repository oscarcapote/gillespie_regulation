set xlabel 'P'
set ylabel 'm'

ap = 20
am = 600
dp = 1
dm = 10

dpdt(x,y) = am*y-dp*x
dmdt(x,y) = -dm*y+ap*x 
set xrange[0:10]
set yrange[0:10]
plot '++' u 1:2:(dpdt($1,$2)):(dmdt($1,$2)) w vec
pause(-1)
