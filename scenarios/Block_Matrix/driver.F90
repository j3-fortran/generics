
program driver
  use GenericBlockMatrixMultiply_mod
  use SimpleBlock_mod
  use NestedBlock_mod
  implicit none

  type BlockStructure
     integer :: ni, nj, nk
  end type BlockStructure

  call simple_case(BlockStructure(3,2,4))
  call nested_case(BlockStructure(3,2,4), BlockStructure(2,3,7))
  call diabolical_case()

contains

  subroutine simple_case(bs)
    type(BlockStructure), intent(in) :: bs
    
    integer :: ni, nj, nk
    
    type(SimpleA), allocatable :: t_a(:,:)
    type(SimpleB), allocatable :: t_b(:,:)
    type(SimpleC), allocatable :: t_c(:,:)
    type(SimpleT), allocatable :: t_t(:,:)
    
    type(BlockMatrix) :: a, b, c
    integer :: i, j, k
    
    ni = bs%ni
    nj = bs%nj
    nk = bs%nk
    
    allocate(t_a(ni, nk))
    allocate(t_b(nk, nj))
    allocate(t_c(ni, nj))
    allocate(t_t(ni, nj))
    
    do k = 1, nk
       do i = 1, ni
          t_a(i,k)%element = 1
       end do
    end do
    
    do j = 1, nj
       do k = 1, nk
          t_b(k,j)%element = 1
       end do
    end do
    
    a = BlockMatrix(t_a, t_b, t_c, t_t)
    b = BlockMatrix(t_a, t_b, t_c, t_t)
    c = BlockMatrix(t_a, t_b, t_c, t_t)
    
    c = a .matmul. b
    
    ! Because seed values are all "1.", we expect result values to be
    ! nk
    
    do j = 1, nj
       do i = 1, ni
          select type (q => c%c(i,j))
          type is (SimpleC)
             print*, i, j, q%element == nk
          end select
       end do
    end do
  end subroutine simple_case

  subroutine nested_case(bs_inner, bs_outer)
    type(BlockStructure), intent(in) :: bs_inner
    type(BlockStructure), intent(in) :: bs_outer
    
    integer :: ni, nj, nk
    type(SimpleA), allocatable :: t_a(:,:)
    type(SimpleB), allocatable :: t_b(:,:)
    type(SimpleC), allocatable :: t_c(:,:)
    type(SimpleT), allocatable :: t_t(:,:)

    integer :: nb_i, nb_j, nb_k
    type(NestedA), allocatable :: n_a(:,:)
    type(NestedB), allocatable :: n_b(:,:)
    type(NestedC), allocatable :: n_c(:,:)
    type(NestedT), allocatable :: n_t(:,:)
    
    type(BlockMatrix) :: a, b, c

    integer :: i, j, k
    integer :: ii, jj
    
    ni = bs_inner%ni
    nj = bs_inner%nj
    nk = bs_inner%nk
    
    allocate(t_a(ni, nk))
    allocate(t_b(nk, nj))
    allocate(t_c(ni, nj))
    allocate(t_t(ni, nj))
    
    do k = 1, nk
       do i = 1, ni
          t_a(i,k)%element = 1
       end do
    end do
    
    do j = 1, nj
       do k = 1, nk
          t_b(k,j)%element = 1
       end do
    end do
    
    do j = 1, nj
       do i = 1, ni
          t_c(i,j)%element = 1
       end do
    end do
    
    do j = 1, nj
       do i = 1, ni
          t_t(i,j)%element = -9999
       end do
    end do

    nb_i = bs_outer%ni
    nb_j = bs_outer%nj
    nb_k = bs_outer%nk
    
    allocate(n_a(nb_i, nb_k))
    allocate(n_b(nb_k, nb_j))
    allocate(n_c(nb_i, nb_j))
    allocate(n_t(nb_i, nb_j))

    do k = 1, nb_k
       do i = 1, nb_i
          n_a(i,k)%matrix = BlockMatrix(t_a,t_b,t_c,t_t)
       end do
    end do
    
    do j = 1, nb_j
       do k = 1, nb_k
          n_b(k,j)%matrix = BlockMatrix(t_a,t_b,t_c,t_t)
       end do
    end do

    do j = 1, nb_j
       do i = 1, nb_i
          n_c(i,j)%matrix = BlockMatrix(t_a,t_b,t_c,t_t)
       end do
    end do
    
    do j = 1, nb_j
       do i = 1, nb_i
          n_t(i,j)%matrix = BlockMatrix(t_a,t_b,t_c,t_t)
       end do
    end do

    a = BlockMatrix(n_a, n_b, n_c, n_t)
    b = BlockMatrix(n_a, n_b, n_c, n_t)
    c = BlockMatrix(n_a, n_b, n_c, n_t)
    
    c = a .matmul. b

    ! Because seed values are all "1.", we expect result values to be
    ! (nk * nb_k)
    
    do j = 1, nb_j
       do i = 1, nb_i
          select type (q => c%c(i,j))
          type is (NestedC)
             do jj = 1, nj
                do ii = 1, ni
                   select type (qq => q%matrix%c(ii,jj))
                   type is (SimpleC)
                      print*, i, j, ii, jj, qq%element == nk*nb_k
                   end select
                end do
             end do
          end select
       end do
    end do

  end subroutine nested_case

  subroutine diabolical_case()
    integer :: ni, nj, nk
    type(SimpleA), allocatable :: t_a(:,:)
    type(SimpleB), allocatable :: t_b(:,:)
    type(SimpleC), allocatable :: t_c(:,:)
    type(SimpleT), allocatable :: t_t(:,:)

    integer :: nb_i, nb_j, nb_k
    type(NestedA), allocatable :: n_a(:,:)
    type(NestedB), allocatable :: n_b(:,:)
    type(NestedC), allocatable :: n_c(:,:)
    type(NestedT), allocatable :: n_t(:,:)

    integer :: nb2_i, nb2_j, nb2_k
    type(NestedA), allocatable :: tmp(:,:)
    type(NestedA), allocatable :: n2_a(:,:)
    type(NestedB), allocatable :: n2_b(:,:)
    type(NestedC), allocatable :: n2_c(:,:)
    type(NestedT), allocatable :: n2_t(:,:)
    
    type(BlockMatrix) :: a, b, c

    integer :: i, j, k
    integer :: ii, jj
    integer :: iii, jjj
    
    ni = 3
    nj = 2
    nk = 4
    
    allocate(t_a(ni, nk))
    allocate(t_b(nk, nj))
    allocate(t_c(ni, nj))
    allocate(t_t(ni, nj))
    
    do k = 1, nk
       do i = 1, ni
          t_a(i,k)%element = 1
       end do
    end do
    
    do j = 1, nj
       do k = 1, nk
          t_b(k,j)%element = 1
       end do
    end do
    
    do j = 1, nj
       do i = 1, ni
          t_c(i,j)%element = 1
       end do
    end do
    
    do j = 1, nj
       do i = 1, ni
          t_t(i,j)%element = -9999
       end do
    end do

    nb_i = 2
    nb_j = 3
    nb_k = 7
    
    allocate(n_a(nb_i, nb_k))
    allocate(n_b(nb_k, nb_j))
    allocate(n_c(nb_i, nb_j))
    allocate(n_t(nb_i, nb_j))
    
    do k = 1, nb_k
       do i = 1, nb_i
          n_a(i,k)%matrix = BlockMatrix(t_a,t_b,t_c,t_t)
       end do
    end do
    
    do j = 1, nb_j
       do k = 1, nb_k
          n_b(k,j)%matrix = BlockMatrix(t_a,t_b,t_c,t_t)
       end do
    end do

    do j = 1, nb_j
       do i = 1, nb_i
          n_c(i,j)%matrix = BlockMatrix(t_a,t_b,t_c,t_t)
       end do
    end do
    
    do j = 1, nb_j
       do i = 1, nb_i
          n_t(i,j)%matrix = BlockMatrix(t_a,t_b,t_c,t_t)
       end do
    end do


    nb2_i = 1
    nb2_j = 1
    nb2_k = 2
    
    allocate(n2_a(nb2_i, nb2_k))
    allocate(n2_b(nb2_k, nb2_j))
    allocate(n2_c(nb2_i, nb2_j))
    allocate(n2_t(nb2_i, nb2_j))
    
    do k = 1, nb2_k
       do i = 1, nb2_i
          n2_a(i,k)%matrix = BlockMatrix(n_a,n_b,n_c,n_t)
       end do
    end do
    
    do j = 1, nb2_j
       do k = 1, nb2_k
          n2_b(k,j)%matrix = BlockMatrix(n_a,n_b,n_c,n_t)
       end do
    end do

    do j = 1, nb2_j
       do i = 1, nb2_i
          n2_c(i,j)%matrix = BlockMatrix(n_a,n_b,n_c,n_t)
       end do
    end do
    
    do j = 1, nb2_j
       do i = 1, nb2_i
          n2_t(i,j)%matrix = BlockMatrix(n_a,n_b,n_c,n_t)
       end do
    end do
    print*,__FILE__,__LINE__


    
    allocate(a%a, source=n2_a)
    a = BlockMatrix(n2_a, n2_b, n2_c, n2_t)
    b = BlockMatrix(n2_a, n2_b, n2_c, n2_t)
    c = BlockMatrix(n2_a, n2_b, n2_c, n2_t)

    c = a .matmul. b

    ! Because seed values are all "1.", we expect result values to be
    ! (nk * nb_k)

    print*,__FILE__,__LINE__, nb2_j, nb_j, nb_i
    do j = 1, nb2_j
       do i = 1, nb2_i
          select type (q => c%c(i,j))
          type is (NestedC)
             do jj = 1, nb_j
                do ii = 1, nb_i
                   select type (qq => q%matrix%c(ii,jj))
                   type is (NestedC)
                      print*,__FILE__,__LINE__, ii,jj,i,j
                      do jjj = 1, nj
                         do iii = 1, ni
                            select type (qqq => qq%matrix%c(iii,jjj))
                            type is (SimpleC)
                               print*, i, j, ii, jj, qqq%element == nk*nb_k*nb2_k
                            end select
                         end do
                      end do
                   end select
                end do
             end do
          end select
       end do
    end do
  end subroutine diabolical_case

end program driver
