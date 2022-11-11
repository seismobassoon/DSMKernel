add_library(KernelMakerlib
		bwfilt.f90
		calculateK.f90
		fourier.f90
		others.f90
	)

add_executable(xKernelMaker KernelMaker.f90)
target_link_libraries(xKernelMaker KernelMakerlib commonlib)
