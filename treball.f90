program gillespie
implicit none
real(8) :: k1,km1,am,ap,dm,dp,a0!Ratios
real(8),dimension(6) :: Pr!Ratios i probabilitats de donar una reaccio
real(8) ::P,M,DNA,PDNA!Magnituds dinamiques
real(8) :: suma
real(8) :: t,tau,rnd!Temps actual,temps estocastic i variable aleatoria entre 0 i 1
integer(8) :: mu,i,MaxItt,itt

!----------------------INICIALITZACIO---------------------
!---Condicions inicials
    P=0.0d0
    M=0.0d0
    DNA = 2.0d0
    PDNA = 0.0d0
!---Ritmes
    k1=5.0d0
    km1=20.0d0
    am=600.0d0
    ap=20.0d0
    dm=10.0d0
    dp=1.0d0
t=0
itt=1
MaxItt=2000
!call srand(9)
!----------------------------------------------------------
do
!------------------UPDATE DE PROBABILITATS-----------------
call prob_compute(Pr,a0)
!----------------------------------------------------------

!-------------------MONTECARLO TIME STEP-------------------
tau = -log(rand())/a0!Temps per que passi una reaccio
rnd = rand()
mu = 1!Reaccio qua pasara
suma =Pr(mu)
do while(rnd>suma)
    mu = mu+1
    suma = suma+Pr(mu)
enddo
t = t+tau
call reaction(mu)
print*,t,P,M,DNA,PDNA,Pr,mu
if(itt==MaxItt)then
    stop
endif
itt=itt+1
enddo
!----------------------------------------------------------
contains

subroutine prob_compute(Pr,a0)
    real(8),dimension(6) :: Pr!Ratios i probabilitats de donar una reaccio
    real(8) :: a0
    real(8) :: a0
    Pr(1) = k1*P*DNA
    Pr(2) = km1*(PDNA)
    Pr(3) = am*DNA
    Pr(4) = ap*M
    Pr(5) = dm*M
    Pr(6) = dp*P
    a0 = sum(Pr)
    Pr = Pr/a0
end subroutine

subroutine reaction(mu)
!Feim sa reaccio triada
    integer(8),intent(in) :: mu
    if(mu==1)then
        P = P-1
        DNA = DNA-1
        PDNA = PDNA+1
    elseif(mu==2)then
        P = P+1
        DNA = DNA+1
        PDNA = PDNA-1
    elseif(mu==3)then
        M=M+1
    elseif(mu==4)then
        P=P+1
    elseif(mu==5)then
        M=M-1
    elseif(mu==6)then
        P=P-1        
    endif
end subroutine

end program
