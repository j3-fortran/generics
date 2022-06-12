module MPI_Support_mod
   implicit none
   private

   public :: GatherTmpl

   template GatherTmpl(T, N)
      type :: T; end type T
      integer, parameter :: N     ! rank

      interface array_gather
         module procedure array_gather_t
      end interface array_gather

   contains

      subroutine array_gather_t(local_array, global_array, grid, mask, depe, hw, rc)
         use ESMF, only: ESMF_Grid
         use ESMF, only: ESMF_DELayout
         use ESMF, only: ESMF_DistGrid
         use ESMF, only: ESMF_VM
         type(T), intent(IN),  RANK(N) :: local_array
         type(T), intent(OUT), RANK(N) :: global_array
         type (ESMF_Grid)      :: grid
         integer, optional,  intent(IN   )   :: mask(:)
         integer, optional,  intent(IN   )   :: depe
         integer, optional,  intent(IN   )   :: hw
         integer, optional, intent(OUT)     :: rc
    
         ! Local variables
         
         integer                               :: status
         character(len=ESMF_MAXSTR)            :: IAm='ArrayGather'
         
         type (ESMF_DELayout)  :: layout
         type (ESMF_DistGrid)  :: distGrid
         integer,               allocatable    :: AL(:,:)
         integer,               allocatable    :: AU(:,:)
         integer, allocatable, dimension(:) :: recvcounts, displs, kk
         integer                                       :: nDEs
         integer                                       :: sendcount
         
         integer                                       :: I, J, K, II
         integer                                       :: LX, JJ
         integer                                       :: de, deId
         integer                                       :: I1, IN
         integer                                       :: ibeg,iend
         integer                                       :: gridRank
         integer                                       :: J1, JN
         integer                                       :: jbeg,jend
         integer                                       :: ISZ, JSZ
         integer                                       :: destPE, myhw
         type(T), allocatable :: var(:)
         integer                               :: deList(1)
         type(ESMF_VM) :: vm

         integer :: L_BEG(N), L_END(N)
    
         ! Works only on 1D and 2D arrays
         ! Note: for tile variables the gridRank is 1 
         ! and the case RANK_=2 needs additional attention 

         if (N > 2) error stop 'procedure designed for just ranks 1 and 2'

         if(present(depe)) then
            destPE = depe 
         else
            destPE = MAPL_Root
         end if

         if(present(hw)) then
            myhw = hw
         else
            myhw = 0
         end if
         
         call ESMF_GridGet(GRID,dimCount=gridRank,rc=STATUS);VERIFY_(STATUS)
         call ESMF_GridGet(GRID,distGrid=distGrid,rc=STATUS);VERIFY_(STATUS)
         call ESMF_DistGridGet(distGRID, delayout=layout,rc=STATUS)
         VERIFY_(STATUS)
         call ESMF_DELayoutGet(layout, deCount =nDEs, localDeList=deList, &
              rc=status)
         VERIFY_(STATUS)
         deId = deList(1)
         call ESMF_DELayoutGet(layout, vm=vm, rc=status)
         VERIFY_(STATUS)
         
         allocate (AL(gridRank,0:nDEs-1),  stat=status)
         VERIFY_(STATUS)
         allocate (AU(gridRank,0:nDEs-1),  stat=status)
         VERIFY_(STATUS)
         
         call ESMF_DistGridGet(distgrid, &
              minIndexPDe=AL, maxIndexPDe=AU, rc=status)
         VERIFY_(STATUS)
         
         allocate (recvcounts(nDEs), displs(0:nDEs), stat=status)
         VERIFY_(STATUS)
         
         if (deId == destPE) then
            allocate(VAR(0:size(GLOBAL_ARRAY)-1), stat=status)
            VERIFY_(STATUS)
         else
            allocate(VAR(0), stat=status)
            VERIFY_(STATUS)
         end if
         
         displs(0) = 0
         if (N > 1) then
            if (gridRank == 1) then
               J1 = lbound(local_array,N)
               JN = ubound(local_array,N)
            endif
         end if

         do I = 1,nDEs
            J = I - 1
            de = J
            I1 = AL(1,J)
            IN = AU(1,J)
            if (rank > 1) then
               if (gridRank > 1) then
                  J1 = AL(2,J)
                  JN = AU(2,J)
               end if
               recvcounts(I) = (IN - I1 + 1) * (JN - J1 + 1)
            else
               recvcounts(I) = (IN - I1 + 1)
            end if

            if (de == deId) then
               sendcount = recvcounts(I)      ! Count I will send
               L_BEG(:) = 1 + myhw
               L_END(1) = IN - I1 + 1 + myhw
               if (N > 1) then
                  L_END(N) = JN - J1 + 1 + myhw
               end if
            end if
            displs(I) = displs(J) + recvcounts(I)
         enddo

         if (present(mask) .or. myHW == 0) then
            call MAPL_CommsGatherV(layout, local_array, sendcount, &
                 var, recvcounts, displs, destPE, status)
         else
            call MAPL_CommsGatherV(layout, local_array@(L_BEG:L_END), &
                 sendcount, var, recvcounts, displs, destPE, &
                 status)
            end if
         end if
         VERIFY_(STATUS)

         if (deId == destPE) then
            if (present(mask)) then
               ISZ = size(mask)
               if (N == 2) then
                  JSZ = size(GLOBAL_ARRAY,2)
               else
                  JSZ = 1
               end if
               allocate(KK (0:nDEs-1        ), stat=status)
               VERIFY_(STATUS)
               KK = DISPLS(0:nDEs-1)

               do I=1,ISZ
                  K = MASK(I)
                  II = KK(K)
                  if (N == 1) then
                     GLOBAL_ARRAY(I) = VAR(II)
                  else
                     LX = AU(1,K) - AL(1,K) + 1 
                     do J=1,JSZ
                        GLOBAL_ARRAY(I,J) = VAR(II+LX*(J-1))
                     end do
                  end if
                  KK(MASK(I)) = KK(MASK(I)) + 1 
               end do

               deallocate(KK, stat=status)
               VERIFY_(STATUS)
            else
               if (N == 1) then
                  global_array = var ! ALT: I am not sure if this is correct
               else
                  do I = 0,nDEs-1
                     I1 = AL(1,I)
                     IN = AU(1,I)
                     J1 = AL(2,I)
                     JN = AU(2,I)
                     
                     K = displs(I)
                     do JJ=J1,JN
                        do II=I1,IN
                           global_array(II,JJ) = var(K)
                           K = K+1
                        end do
                     end do
                  end do
               end if
            end if ! if (present(mask))
         end if

         deallocate(VAR, stat=status)
         VERIFY_(STATUS)
         deallocate(recvcounts, displs, AU, AL, stat=status)
         VERIFY_(STATUS)
         
         call ESMF_VmBarrier(vm, rc=status)
         VERIFY_(STATUS)
         RETURN_(ESMF_SUCCESS)
      end subroutine ARRAY_GATHER_T

   end template


end module MPI_Support_mod

