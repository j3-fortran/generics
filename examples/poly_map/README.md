This is an attempt to demonstrate how to combine polymorphism with a
simple implementation of a Map template.

Here we have cases where either the key type, K, or the value type, T,
may be polymorphic or both.  We can reuse the "monomorphic" Map
template that uses wrapper types that encaplulate polymorphic objects.
This reduces the complexity considerably, but still requires unique
interfaces to the various operations on Map as the arguments must be
declare CLASS or TYPE depending on the case.

This results in 4 templates for MAP:
   - monomorphic K, monomorphic T
   - monomorphic K, polymorphic T
   - polymorphic K, monomorphic T
   - polymorphic K, polymorphic T


