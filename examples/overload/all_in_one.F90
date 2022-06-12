module all_in_one
   use MPI_Support_mod, only: GatherTmpl
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   implicit none
   private

   public :: array_gather


   INSTANTIATE GatherTmp(real(kind=real32), 1)
   INSTANTIATE GatherTmp(real(kind=real32), 2)
   INSTANTIATE GatherTmp(real(kind=real64), 1)
   INSTANTIATE GatherTmp(real(kind=real64), 2)

   INSTANTIATE GatherTmp(integer(kind=int32), 1)
   INSTANTIATE GatherTmp(integer(kind=int32), 2)
   INSTANTIATE GatherTmp(integer(kind=int64), 1)
   INSTANTIATE GatherTmp(integer(kind=int64), 2)

end module all_in_one
