add_library(KernelMakerlib INTERFACE
		pinputKernel.f90
		moduleKernel.f90
		calculateK.f90
		others.f90
		localschema.f90
		)
add_executable(xKernelMaker 
		KernelMaker.f90)
target_link_libraries(xKernelMaker KernelMakerlib commonlib)
