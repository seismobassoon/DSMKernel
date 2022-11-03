add_library(KernelMakerlib
		bwfilt.f90
		calculateK.f90
		fourier.f90
		module.f90
		others.f90
	)

#target_compile_options(KernelMakerlib PUBLIC ${DKS_options})
add_executable(xKernelMaker KernelMaker.f90)
#target_compile_options(xKernelMaker ${DKS_options})

