add_library(KernelMakerlib 
		rotation.f90
		pinputKernel.f90
		moduleKernel.f90
		calculateK.f90
		others.f90
		localschema.f90
		fourier.f90
		)
add_executable(xKernelMaker 
		KernelMaker.f90)
target_link_libraries(xKernelMaker KernelMakerlib commonlib)
