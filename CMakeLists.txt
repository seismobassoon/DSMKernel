# CMake4DSMKernelSuite is distributed under GNU General Public Licence (v3). See accompanying file LICENSE.txt

# NF tries gtk-fortran
#set (gtk-fortran /Users/nobuaki/Github/gtk-fortran/build/modules)
#add_subdirectory(src/guiDSM)

cmake_minimum_required(VERSION 3.9...3.23)
project(DSMKernelSuite LANGUAGES Fortran)
#enable_language(Fortran)
find_package(MPI REQUIRED)
set(CMAKE_Fortran_COMPILER mpif90)
set(EXECUTABLE_OUTPUT_PATH ../../../bin)

# Strain Green Function dictionary should be stored in a bi space!!
set(SGT_ROOT "${PROJECT_SOURCE_DIR}/SGTdictionary")


#add_compile_options(-ffree-line-length-none  -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -O4)
add_compile_options(-Wall -fbacktrace -ffree-line-length-none -fimplicit-none -fbacktrace -O4 -ftree-vectorize)
#add_compile_options(-fc=ifort -O4 -heap-arrays -check nobounds -xAVX -ftz -assume buffered_io -assume byterecl  -implicitnone -warn truncated_source  -warn declarations -warn alignments -warn ignore_loc -warn usage -mcmodel=medium -shared-intel)
add_subdirectory(src/SGTpsv)
add_subdirectory(src/SGTsh)
add_subdirectory(src/KernelMaker)
add_subdirectory(share)

add_subdirectory(src/SGTpsvMax)
add_subdirectory(src/SGTpsvLChunk)
add_subdirectory(src/SGTshMax)
# some example information files
add_subdirectory(examples/single_kernel)
