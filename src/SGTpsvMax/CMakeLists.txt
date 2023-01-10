add_library(sgtPSVMaxlib
		calmat.f90
		dcsymbdl.f90
		dcsymbdl3.f90
		glu2.f90
		modules.f90
		others.f90
		rk3.f90
                dsm_every_l.f90
		trial.f90
	)

add_executable(xSGTpsvMax SGTpsv-MPI.f90)
target_link_libraries(xSGTpsvMax sgtPSVMaxlib commonlib)

