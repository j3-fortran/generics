module NeighborSearch
! Implements linked-list neighbor search, as commonly used in Molecular Dynamics (MD) codes
! We are given a periodic unit unit_cell in R^dim, and a bunch of points in that unit unit_cell
! We want to sort the points into a grid of sub-cells / grid cells to facilitate finding neighbors
! By neighboring points we mean points closer than a *Euclidean* cutoff distance cutoff
   
   ! What I really want is dim and wp to be template kind parameters
   integer, parameter :: max_dim=max_dim ! This only exists because of limitations in F202x/y
   integer, parameter :: wp ! What I really want is for this to be a template kind parameter

   ! What I really want is for this parameterized type to be a template parameter
   type :: UnitCell(wp,dim) ! This is an orthogonal unit unit_cell
      integer, kind :: wp,dim
      ! Every type consistent with a UnitCell should have this component
      ! It gives the size of the unit cell along each dimension in unit cell coordinates
      real(wp), dimension(dim) :: lengths ! We only need dim lengths along each dimension
   end type

   ! I cannot change the definition of the type UnitCell here in F202x!
   ! So I am forced to declare a second type here for illustration purposes
   ! But to make the rest of the code below work for this type of UnitCell,
   ! I would need to rename this type to GeneralUnitCell and the above one to OrthoUnitCell
   ! This is why I need a template feature
   type :: GeneralUnitCell(wp,dim) ! This is a general unit unit_cell
      integer, kind :: wp,dim
      real(wp), dimension(dim) :: lengths ! Every type consistent with a UnitCell should have this component
      real(wp), dimension(dim,dim) :: lattice ! dim vectors in R^dim (columns of lattice)
   end type
   
   type :: NeighborList(wp,dim)
      integer, kind :: wp,dim
      real(wp) :: cutoff=-1.0_wp
      integer :: grid_size(dim)=1 ! Number of grid cells along each dimension
      real(wp), dimension(dim) :: grid_lengths ! The length (in unit cell coordinates) of a grid cell 
         ! grid_lengths = unit_cell%lengths/list%grid_size
      real(wp), dimension(:), allocatable :: next ! Next in list "pointers"
      ! What I really want here is dimension(grid_size), i.e., for the rank of heads to be dim
      ! I do not think there is a way to do that yet in F202x/y
      ! So I declare this to be of dimension max_dim, the max possible dimension allowed
      real(wp), dimension(:,:,:), allocatable :: heads ! Heads of linked lists "pointers"   
   end type

   interface ! These routines could either be template parameters, or, 
             ! for an OOP design, type-bound procedures of type UnitCell
   
      ! This is just an example code so I do not implement here periodic wrapping/unwrapping
      ! That is, this is not the nearest distance commonly used in MD simulations
      function EuclideanDistance(unit_cell,x,y) result(d) ! Euclidean distance between points x and y
         type(UnitCell), intent(in) :: unit_cell
         ! What I really want here is dimension(dim) and real(wp) where dim and wp are kind parameters
         ! This is why I need a template feature
         real(wp), dimension(:), intent(in) :: x, y 
         real(wp) :: d ! Euclidean ||x-y||
         ! Example, for an orthogonal unit unit_cell:
         ! d=sum(((x-y)*unit_cell%lengths)**2)
         ! For a general unit unit_cell
         ! d=sum(matmul(unit_cell%lattice, x-y)**2)
         ! If dim is a compile-time constant, compiler can inline/vectorize all of this
      end function
      
      ! Computes what is the minimal length (in *unit unit_cell coordinates*) grid cells can have
      ! to ensure that points can be within a *Euclidean* distance cutoff only if in neighboring grid cells
      function GridCellSize(unit_cell,cutoff) result(lengths)
         type(UnitCell), intent(in) :: unit_cell
         ! Again what I really want here is wp to be a template parameter
         real(wp), intent(in) :: cutoff
         ! What I really want here is dimension(dim)
         real(wp), dimension(max_dim) :: lengths
         
         ! For example, for orthogonal cells:
         ! lengths=cutoff
         ! The formula for general cells is known but complicated so I omit it
      end function
            
   end interface

contains

   subroutine CreateNeighborList(list, unit_cell, cutoff, max_n_points, dim)
      integer, intent(in) :: dim ! What I really want is to be a compile time constant (template parameter)
      type(NeighborList(wp,dim)), intent(inout) :: list
      type(UnitCell(wp,dim)), intent(in) :: unit_cell
      ! Again what I really want here is real(wp)
      real(wp) intent(in) :: cutoff
      integer, intent(in) :: max_n_points (for allocating arrays)

      ! What I really want here is compile time constant dim for optimization
      real(wp), dimension(dim) :: lengths
      integer, dimension(dim) :: n_cells

      lengths=GridCellSize(unit_cell,cutoff)
      n_cells=unit_cell%lengths/lengths ! (1:dim) should be unnecessary
      n_cells(dim+1:)=1 ! Dummy dimensions because we don't have variable rank yet

      list%cutoff=cutoff
      list%grid_size=n_cells
      list%grid_lengths=unit_cell%lengths/list%grid_size

      allocate(list%heads(n_cells(1),n_cells(2),n_cells(3))) ! Not pretty without rank-generic features
      list%heads=0 ! zero means no particles in that grid cell
      allocate(list%next(max_n_points))
      list%next=0 ! zero means end of list
   
   end subroutine
   
   subroutine AddPoints(unit_cell,list,positions,n_points,dim)
      integer, intent(in) :: dim ! What I really want is to be a compile time constant (template parameter)
      type(UnitCell), intent(in) :: unit_cell
      type(NeighborList(wp,dim)), intent(inout) :: list
      real(wp), dimension(dim,n_points) :: positions ! Positions of points to add to the list
      
      integer :: point, next_point
      integer, dimension(dim) :: grid_cell
      
      ! One can of course reallocate here to increase the size of list%next but for now:
      if(n_points>size(list%next)) stop "Increase max_n_points"
      
      ! Insert points into linked lists done using integers instead of pointers (much more efficient)
      do point=1, n_points
         grid_cell=floor(points(:,particle)/list%grid_lengths)
         next_point=list%heads(grid_cell(1),grid_cell(2),grid_cell(3))
         list%heads(grid_cell(1),grid_cell(2),grid_cell(3))=point
         list%next(point)=next_point
      end do   
   
   end subroutine
   
         
  ! There would be another routine here to find all the neighbors of a given point,
  ! or all pairs of points, that are within a Euclidean distance list%cutoff
  ! This would use the test 
  ! if(EuclideanDistance(cell,point_1,point_2)<=cutoff) then
  ! add this pair of particles to the output
  ! Even better if one can construct an iterator over neighbors, but we don't have that yet either...
  
  ! Etc.

end module
