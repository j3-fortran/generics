program main
   use GenericIntrinsics_mod
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none


   type(GenericInteger) :: ia, ib, ic ! default integer kind
   type(GenericInteger(kind=INT64)) :: i64a, i64b, i64c

   type(GenericReal) :: sa, sb, sc ! default real kind
   type(GenericReal(kind=kind(1.d0))) :: da, db, dc ! double precision

   type(GenericComplex) :: za, zb, zc

   ia%value = 2
   ib%value = 3

   i64a%value = 5
   i64b%value = 7

   sa%value = 11.
   sb%value = 13.

   da%value = 17.
   db%value = 19.

   za%value = (23.,29.)
   zb%value = (31.,37.)


   ic = ia+ib
   print*,'Expected ',2+3,' and got', ic%value
   ic = ia*ib
   print*,'Expected ',2*3,' and got', ic%value

   i64c = i64a+i64b
   print*,'Expected ',5+7,' and got', i64c%value
   i64c = i64a*i64b
   print*,'Expected ',5*7,' and got', i64c%value

   ! mixed
   i64c = ia+i64b
   print*,'Expected ',2+7,' and got', i64c%value
   i64c = i64a*ib
   print*,'Expected ',7*3,' and got', i64c%value


end program main
