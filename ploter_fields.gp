set xlabel 'P'
set ylabel 'm'

ap = 20
am = 600
dp = 1
dm = 10

# only integer x-cordinates
set samples 20
# only integer y-cordinates
set isosamples 20
dpdt(x,y) = am*y-dp*x
dmdt(x,y) = -dm*y+ap*x 
nulP(x) = -(dp/am)*x
nulM(x) = -(dm/ap)*x
set xrange[0:20]
set yrange[0:20]
plot '++' u 1:2:(dpdt($1,$2)/1000):(dmdt($1,$2)/1000) w vec,nulP(x),nulM(x)
pause(-1)
