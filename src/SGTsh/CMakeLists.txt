add_library(sgtSHlib
		calmat.f90
		dclib.f90
		dclib3.f90
		glu2.f90
		modules.f90
		others.f90
		trial.f90
	)
	
add_executable(xSGTsh SGTsh-MPI.f90)
target_link_libraries(xSGTsh sgtSHlib commonlib)
