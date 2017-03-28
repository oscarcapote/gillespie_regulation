program histogram
implicit none
integer(8) :: N!Nombre de mostres
integer(8) :: i,ios
real(8),dimension(:),allocatable :: M,P
real(8),dimension(:),allocatable :: H,X
real(8) :: t

!LLEGIM ARXIU
open(unit=100, file='Results.dat', iostat=ios, status="old", action="read")
if ( ios /= 0 ) stop "Error opening file "
N = 0
do while ( ios==0 )
  read(100,*, iostat=ios)
  N = N+1
end do
close(unit=100, iostat=ios)
if ( ios /= 0 ) stop "Error closing file unit 1"


open(unit=100, file='Results.dat', iostat=ios,action="read")
allocate(M(N))
allocate(P(N))
if ( ios /= 0 ) stop "Error opening scratch file on unit 100"
do i = 1, N
  read(100,*, iostat=ios) t,P(i),M(i)
end do
call make_histogram(P,ceiling(sqrt(dble(N)),8),H,X)
do i = 1, ceiling(sqrt(dble(N)))
  print*, X(i),H(i)
end do


close(unit=100, iostat=ios)
if ( ios /= 0 ) stop "Error closing file unit 100"
contains

subroutine make_histogram(D,N,L,X)
    real(8),dimension(:),intent(in) :: D
    integer(8),intent(in) :: N
    real(8),dimension(:),allocatable,intent(out) :: L,X
    integer(8) :: k
    real(8) :: h,x0,xf
    x0 = minval(D,1)
    xf = maxval(D,1)
    h = (xf-x0)/N !Amplada dels bins
    ! Omplim ses mostres i les fiquem en es bin corresponent
    allocate(L(N))
    allocate(X(N))
    L = 0.0d0
    X = (/(x0+i*h, i =0, N)/)
    do i=1,size(D)
      k = ((D(i)-x0)/(xf-x0))*N+1
      L(k) = L(k) + 1
    enddo
end subroutine
end program
