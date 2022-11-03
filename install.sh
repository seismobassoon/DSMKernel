rm -rf build
#cmake -S . -B build -DCMAKE_FORTRAN_COMPILER=/opt/homebrew/bin/mpifort
cmake -S . -B build 
cmake --build build 
