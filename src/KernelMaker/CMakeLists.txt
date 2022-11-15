add_library(KernelMakerlib SHARED
		dummy.f90
	)
add_library(KernelMakerSublib
		calculateK.f90
		others.f90
	)
target_link_libraries(KernelMakerlib 
		KernelMakerSublib
		commonlib
	)
add_executable(xKernelMaker KernelMaker.f90)
target_link_libraries(xKernelMaker KernelMakerlib commonlib)
