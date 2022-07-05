This example demonstrates how to define and use a template that
implements SWAP for arbitrary types.  Two variants are provided that
emphasize different approaches for rank-agnostic.  Quite possibly the
2-parameter variant produces more efficient code as the ranks of
objects are then known at compilation time.
