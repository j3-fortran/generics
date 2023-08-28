module Pair_m
   implicit none

   template Pair_tmpl(K, T)
      type, deferred :: K ! key
      type, deferred :: T ! value
   end template

end module Pair_m
