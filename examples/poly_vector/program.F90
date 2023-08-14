program main
   use Vector_m
   use PolyVector_m
   implicit none


   type :: Base
   end type Base

   type, extends(Base) :: Child
   end type Child

   type(Base) :: b1, b2
   type(Child) :: c1, c2
   
   instantiate vector_tmpl(Base), only: mono_vector_base => vector
   instantiate vector_tmpl(Child), only: mono_vector_child => vector
   instantiate poly_vector_tmpl(Base), only: poly_vector_base => vector

   call mono_vector_base%push_back(b1)
   call mono_vector_base%push_back(b2)
   
   call mono_vector_child%push_back(c1)
   call mono_vector_child%push_back(c2)

   call mono_vector_base%push_back(c1) ! illegal requires type Base


   call poly_vector_base%push_back(b1)
   call poly_vector_base%push_back(c1)! Legal - type Child extends type Base
end program main

   
