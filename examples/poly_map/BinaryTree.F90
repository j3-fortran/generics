module BinaryTree_m
   implicit none

   template BinaryTree_tmpl(P, less)
      type, deferred :: P

      type :: BinaryTree
         type(P) :: node
         type(BinaryTree), allocatable :: left
         type(BinaryTree), allocatable :: right
      end type BinaryTree

   end template
      
end module BinaryTree_m
