program gillespie
implicit none
real(8) :: a1,a2,a3,a4,a5,a6,a0!Ratios
real(8),dimension(6) :: a,Pr!Ratios i probabilitats de donar una reaccio
real(8) ::P,M,DNA,PDNA!Magnituds dinamiques
real(8) :: suma
real(8) :: t,tau,rnd!Temps actual,temps estocastic i variable aleatoria entre 0 i 1
integer(8) :: mu,i,MaxItt,itt

!----------------------INICIALITZACIO---------------------
P=0.0d0
M=0.0d0
DNA = 2.0d0
PDNA = 0.0d0
a1=5.0d0
a2=20.0d0
a3=600.0d0
a4=20.0d0
a5=10.0d0
a6=1.0d0
t=0
itt=1
MaxItt=100000
!call srand(9)
a = (/a1,a2,a3,a4,a5,a6/)
a0 = a1+a2+a3+a4+a5+a6
!----------------------------------------------------------
do
!------------------UPDATE DE PROBABILITATS-----------------
call prob_compute(Pr,a0)
!----------------------------------------------------------

!-------------------MONTECARLO TIME STEP-------------------
tau = -log(rand())/a0!Temps per que passi una reaccio
rnd = rand()!Reaccio qua pasara
!print*,'on entra?',Pr
!print*,'Random',rnd
mu = 1
suma =0.0d0
do while(rnd>suma)
    !print*,mu,suma,rnd,Pr(mu)+suma
    !if((rnd.gt.suma).and.(rnd.lt.Pr(mu)+suma))then
    !    exit
    !endif
    !print*,mu,suma,rnd,Pr(mu)+suma
    suma = suma+Pr(mu)
    mu = mu+1
enddo
!print*,Pr
!read(*,*)
!mu = mu+1
t = t+tau
call reaction(mu)
!read(*,*)
print*,t,P,M,DNA,PDNA,mu
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
    Pr = a
    !print*,a
    Pr(1) = Pr(1)*PDNA
    Pr(2) = Pr(2)*(P*DNA)
    Pr(3) = Pr(3)*DNA
    Pr(4) = Pr(4)*M
    Pr(5) = Pr(5)*M
    Pr(6) = Pr(6)*P
    a0 = sum(Pr)
    Pr = Pr/a0
end subroutine

subroutine reaction(mu)
!Feim sa reaccio triada
    integer(8),intent(in) :: mu
    !print*,'mu',mu
    if(mu==1)then
        !print*,itt,'mu=1'
        !read(*,*)
        P = P-1
        DNA = DNA-1
        PDNA = PDNA+1
    elseif(mu==2)then
        !print*,itt,'mu=2',Pr
        !read(*,*)
        P = P+1
        DNA = DNA+1
        PDNA = PDNA-1
    elseif(mu==3)then
        M = M+1
    elseif(mu==4)then
        P=P+1
        M=M-1
    elseif(mu==5)then
        M=M-1
    elseif(mu==6)then
        P=P-1        
    endif
end subroutine

end program
