# CMake4DSMKernelSuite is distributed under GNU General Public Licence (v3). See accompanying file LICENSE.txt

cmake_minimum_required(VERSION 3.9...3.23)
project(DSMKernelSuite LANGUAGES Fortran)
find_package(MPI COMPONENTS Fortran REQUIRED)
add_compile_options(-ffree-line-length-none  -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -O4)
add_subdirectory(src/SGTpsv)
add_subdirectory(src/SGTsh)
add_subdirectory(src/KernelMaker)

