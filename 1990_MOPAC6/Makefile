#
#   Makefile for making the executable of program MOPAC
#
#
#    Valid Commands of this makefile
#
#	make		Makes the MOPAC file
#	make clean	Clean up disk to minimum config
#
FFLAGS	        =     -c  -static
HDRS		= SIZES
SRCS:sh		= ls *.f 
OBJS		= $(SRCS:.f=.o)
SIZEDEPSRC:sh	= grep -l -i '      INCLUDE ' *.f; true
SIZEDEPEND	= $(SIZEDEPSRC:.f=.o)

mopac.exe:     	SIZES $(OBJS) 
		@echo -n "Loading mopac.exe ... "
		f77    $(OBJS) -o mopac.exe
		@echo "done"

clean:
	 	rm -f $(OBJS)


$(SIZEDEPEND):	$$< $(HDRS) 
		$(FC) $(FFLAGS) $<

###
