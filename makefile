#linux makefile
#created 18.3.2014 
#Author: Karoline Block, University of Leipzig

#Compiler
#FC = /usr/bin/gfortran-4.6
FC = gfortran
#FC = /opt/intel/Compiler/11.1/056/bin/intel64//ifort
#FC = ifort

#Compiler flags
#FCFLAGS = -Wall -g -fbacktrace -fbounds-check
#FCFLAGS = -Wall -g -fbacktrace -fbounds-check -fcheck=all -ffpe-trap=zero,overflow,underflow,denormal,precision
FCFLAGS = -Wall -O3 -g -fbacktrace -fbounds-check
#FCFLAGS = -Ofast

#NetCDF libraries (necessary for USE netcdf in mo_netcdf_io)
NCFLAGS = -I/opt/netcdf-fortran/current/include -L/opt/netcdf-fortran/current/lib -lnetcdf -lnetcdff


#Executable
PROG = box_model_karo

#Source codes
SRCS =  mo_kind.f90 mo_box_settings.f90 mo_constants.f90 mo_box_input.f90 mo_ham_nenes.f90 \
	mo_box_netcdf_io.f90 mo_ham_ccnctl.f90 mo_box_ham_m7.f90 mo_box_updraft.f90 \
	mo_box_aerosols.f90 mo_box_ham_tools.f90 mo_ham_ccn.f90 mo_ham_activ.f90 box_model_karo.f90


#Objects (the same suffix as .f90 files just with .o as ending)
OBJS := $(SRCS:.f90=.o)


# Target
all: $(PROG)

# Link
$(PROG): $(OBJS)
	$(FC) $(FCFLAGS) -o $(PROG) $(OBJS) $(NCFLAGS)


# Compilation of individual modules
# (creates .mod and .o files from .f90 files with the same suffix)
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< $(NCFLAGS)

#Dependencies
mo_constants.o: 	mo_kind.o
mo_ham_ccnctl.o: 	mo_kind.o
mo_box_ham_m7.o: 	mo_kind.o
mo_box_input.o: 	mo_kind.o mo_constants.o mo_box_settings.o
mo_box_aerosols.o: 	mo_kind.o mo_constants.o mo_box_settings.o
mo_box_netcdf_io.o: 	mo_kind.o mo_box_aerosols.o mo_ham_ccnctl.o
mo_ham_nenes.o:         mo_box_aerosols.o
mo_ham_activ.o: 	mo_kind.o mo_constants.o mo_box_settings.o mo_box_aerosols.o mo_ham_ccnctl.o mo_box_updraft.o mo_ham_nenes.o 
mo_box_ham_tools.o: 	mo_kind.o mo_box_aerosols.o mo_box_ham_m7.o 
mo_ham_ccn.o: 		mo_kind.o mo_constants.o mo_box_settings.o mo_box_aerosols.o mo_box_ham_tools.o mo_ham_ccnctl.o
box_model_karo.o: 	mo_kind.o mo_constants.o mo_ham_ccnctl.o mo_box_settings.o \
			mo_box_input.o mo_box_netcdf_io.o \
			mo_box_aerosols.o mo_ham_activ.o mo_ham_ccn.o mo_box_updraft.o


# clean up 
clean:
	rm -f $(PROG) *.o *.mod



