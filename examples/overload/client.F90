subroutine mysub(field)
   use MPI_Support_mod, only: GatherTmpl
   use, intrinsic :: iso_fortran_env, only: REAL32
   use esmf
   implicit none
   type(ESMF_Field), intent(in) :: field

   REAL(KIND=REAL32), allocatable :: local(:,:)
   REAL(KIND=REAL32), allocatable :: global(:,:)
   INTEGER, ALLOCATABLE :: shp(:)
   integer :: status

   INSTANTIATE(GatherTmpl(real(KIND=REAL32)), 2)

   local = get_local(field)
   global_shp = get_global_shape(field)

   allocate(global(global_shp(1), global_shp(2)))

   call array_gather(local, global, grid, mask, depe, hw, rc=status)

end subroutine mysub
