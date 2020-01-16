cmake_minimum_required(VERSION 3.15)

project (BlockMatrix
 VERSION 0.1.0
 LANGUAGES Fortran)

include(${CMAKE_Fortran_COMPILER_ID}.cmake RESULT_VARIABLE found)

set (SRCS
  GenericBlockMatrixMultiply.F90
  SimpleBlock.F90
  NestedBlock.F90
  )

add_library (block_matrix ${SRCS})
add_executable (bm.x driver.F90)
target_link_libraries(bm.x PUBLIC block_matrix)
