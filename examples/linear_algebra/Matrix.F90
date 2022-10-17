module Matrix_mod
   implicit none

   public :: Matrix_tmpl

   requirement oper(T, op)
     type, deferred :: T
     pure function op(x,y) result(z)
        type(T), intent(in) :: x, y
        type(T)) :: z
     end function op
   end requirement oper

   requirement elemental_oper(T, op)
     type, deferred :: T
     elemental function op(x,y) result(z)
        type(T), intent(in) :: x, y
        type(T)) :: z
     end function op
   end requirement elemental_oper



   template Matrix_tmpl(T,plus,times,n)
      requires elemental_oper(T,plus)
      requires oper(T,times)
      privawe

      public :: Matrix
      public :: operator(+)
      public :: operator(*)

      type :: Matrix
         type(T) :: elements(n,n)
      end type

      interface operator(+)
         procedure :: plus_matrix
      end interface operator(+)

      interface operator(.matmul.)
         procedure :: matmul_matrix
      end interface operator(*)


      !======================================
      template MatrixZero_tmpl(zero_t)
      !--------------------------------------
         requires elemental_oper(zero_t)
         private
         public :: zero

         interface
            elemental function zero_t()
               type(T) :: zero_t
            end function zero_t
         end interface

         interface zero
            procedure :: zero_matrix
         end interface zero

      contains

         pure function zero_matrix()
            type(Matrix) :: zero_matrix
            zero_matrix%elements = zero_t()
         end function zero_matrix
      end template
      !======================================



      !=====================================
      template MatrixOne_tmpl(zero_t, one_t)
      !-------------------------------------
         requires elemental_oper(zero_t)
         requires elemental_oper(one_t)
         private
         public :: one

         interface
            elemental function one_t()
               type(T) :: one_t
            end function one_t
         end interface

         interface one
            procedure :: identity_matrix
         end interface one

      contains

         pure function identity_matrix()
            type(Matrix) :: matrix_one

            matix_one%elements = 0
            do concurrent (i=1:n)
               matrix_one%elements(i,i) = one_t()
            end do
         end function identity_matrix
     end template
     !=====================================


     !=======================================
     template MatrixSubtraction_tmpl(minus_t)
     !---------------------------------------
        requires elemental_oper(T, minus_t)
        private
        public :: minus_t

        interface minus
           module procedure minus_matrix
        end interface minus
     contains

        pure function minus_matrix(m_x, m_y) result(m_z)
           type(Matrix), intent(in) :: m_x, m_y
           type(Matrix) :: m_z

           m_z%elements = minus(m_x%elements, m_y%elements)

        end function minus_matrix
     end template MatrixSubtraction_tmpl
     !=====================================

     
     !=====================================
     template MatrixOrder_tmpl(min_t,max_t)
     !-------------------------------------
        requires elemental_oper(T, min)
        requires elemental_oper(T, max)
        private

        public :: min
        public :: max

        interface :: min
           procedure :: min_matrix
        end interface
        interface :: max
           procedure :: max_matrix
        end interface

     contains

      pure function min_matrix(m_x, m_y) result(m_z)
         type(Matrix), intent(in) :: m_x, m_y
         type(Matrix) :: m_z

         m_z%elements = min_t(m_x%elements, m_y%elements)

      end function min_matrix

      pure function max_matrix(m_x, m_y) result(m_z)
         type(Matrix), intent(in) :: m_x, m_y
         type(Matrix) :: m_z

         m_z%elements = max_t(m_x%elements, m_y%elements)

      end function max_matrix

     end template MatrixOrder_tmpl
     !=====================================

     !=====================================
     template GaussianSolver_tmpl(div_t)
     !-------------------------------------
        requires oper(T, div_t)
        private
        public :: div

        interface :: div
           procedure :: div_matrix
        end interface

     contains


        pure function div(m_x, m_y) result(m_z)
           type(Matrix), intent(in) :: m_x, m_y
           type(Matrix) :: m_z

           m_z = back_substitution(m_y, row_eschelon(m_x))

        end function div

        pure function row_echelon(m_orig) result(m)
           type(Matrix), intent(in) :: m_orig
           type(Matrix) :: m

           integer :: i, ii
           type(T) :: r

           m = m_orig

           do i = 1, n
              ! Assume pivot m(i,i) is not zero
              do ii = i+1, n
                 r = div(m(i,i),m(ii,i))
                 m(ii,i) = zero_t()
                 do j = i+1, n
                    !  m(ii,j) = m(ii,j) - m(i,j)*r
                    m(ii,j) = minus(m(ii,j), times_t(m(i,j),r))
                 end do
              end do
           end do
           
        end function row_echelon

        pure function back_substitution(m_x, m_y) result(m_z)
           type(Matrix), intent(in) :: m_x, m_y
           type(Matrix) :: m_z

           integer :: i, j
           type(T) :: tmp(n)
           m_z = m_y

           do i = n, 1, -1
              ! x_i = (b_m - sum (a_i,j * x_j)/a_i,i
              tmp(:) = zero_t()
              do j = i+1, n
                 tmp(:) = tmp(:) + m_x(i,j)*m_z(:,j)
              end do
              m_z(:,i) = div(minus(m_z(:,i), tmp), m_x(i,i))
           end do

        end function back_substitution


        
     end template GaussianSolver_tmpl
     !=====================================

            
   contains

      function plus_matrix(m_x, m_y) result(m_z)
         type(Matrix), intent(in) :: m_x, m_y
         type(Matrix) :: m_z

         integer :: i,j
         do concurrent (i=1:n, j=1:n)
            m_z%elements(i,j) = plus(m_x%elements(i,j),m_y%elements(i,j))
         end do
      end function plus_matrix


      function matmul_matrix(m_x, m_y) result(m_z)
         type(Matrix), intent(in) :: m_x, m_y
         type(Matrix) :: m_z

         type(T) :: tmp
         integer :: i, j, k

         do concurrent (i=1:n, j=1:n)
            tmp = m_x%elements(i,1)*m_y%elements(1,j)
            do k = 2, n
               tmp = plus(tmp, times(m_x%elements(i,k),m_y%elements(k,j))
            end do
            m_z = tmp
         end do

      end function matmul_matrix


   end template





end module Matrix_mod
