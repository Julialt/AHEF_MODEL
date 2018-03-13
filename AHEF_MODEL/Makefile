.SUFFIXES: .f .fi .h .o .d .a .mod
#---------------------------------------------
# declaration of definitions (macros)

# Makefile modified from GECKO-A box model

# remember to delete previous *.o files when you change the compiler
#!! use gfortran on supercomputers
#------------------------------------ 

#FC = gfortran
#OMPFLAGS =
#PRECFLAG = -cpp -M -O3 -finit-real=zero -mcmodel=medium -fdefault-real-8 -ffpe-trap=invalid,zero,overflow
#FFLAGS = $(OMPFLAGS) $(PRECFLAG) -c -s

#----------------------- 
#!! use pgf90 on juliapc
#----------------------- 
# -fpic puts code (not compiler) in charge of memory control
#  #  -C adds array bounds checking
#  -g generates symbolic debug information, with optimization level = 0
#  #  -mcmodel allows data sections to be > 2GB
#  -O2 sets optimisation level = 2 
#  #  -pg enables gprof-style sample-based profiling

FC = pgf90
#PRECFLAG = -r8
PRECFLAG = 
LNKFLAGS = 
INCLUDES = 
FFLAGS = $(INCLUDES) $(LNKFLAGS) $(OMPFLAGS) $(PRECFLAG) -c -s

#----------------------- 

LIBDIR = ./SOURCE
MAINDIR = ./SOURCE
OBJDIR = ./OBJ
INCDIR = ./SOURCE

#---------------------------------------------
# VPATH specifies directories to search, in order: VPATH = dir1:dir2:dir3
# so that dependency file.o : file.f is interpreted file.o : dir1/file.f
# vpath (lower case) allows selective specification, e.g.
# vpath %.f ../LIB means files *.f in dir ../LIB

vpath %.f $(LIBDIR) $(MAINDIR)
vpath %.h $(LIBDIR)
vpath %.d $(OBJDIR)
vpath %.a $(OBJDIR)
vpath %.o $(OBJDIR)


MAIN_SOURCE = AHEF.f
 
# find all sources *.f in LIB ##and subdirectories
SOURCES := $(wildcard $(LIBDIR)/*.f) ##$(wildcard $(LIBDIR)/**/*.f)

#create list of *.o files
OBJS := $(patsubst $(LIBDIR)/%.f,$(OBJDIR)/%.o,$(SOURCES))

# create dependencies list -> *.d files
$(OBJDIR)/%.d: %.f
	@set -e;
	$(eval DF := $(patsubst $(LIBDIR)/%.f,$(OBJDIR)/%.d,$<))
	$(eval OF := $(patsubst $(LIBDIR)/%.f,$(OBJDIR)/%.o,$<))
	@rm -f $(DF)
	$(FC) $(PRECFLAG) $(LNKFLAGS) $(INCLUDES) -c -I $(INCDIR) $< > $(DF).temp
	@sed '1s,.*\.o[ :],$(OF) $(DF) :,g' < $(DF).temp > $(DF); \
	rm -f $(DF).temp

include $(OBJS:$(OBJDIR)/%.o=$(OBJDIR)/%.d)

# rule to compile objs
$(OBJDIR)/%.o : %.f
	$(FC) $(FFLAGS) -c $< -o $@

#rule to compile aheflib.a
$(OBJDIR)/aheflib.a : $(OBJS)
	ar -r $@ $(OBJS)

AHEF : $(OBJDIR)/aheflib.a $(MAINDIR)/$(MAIN_SOURCE)
	$(FC) $(MAINDIR)/$(MAIN_SOURCE) $(PRECFLAG) $(LNKFLAGS) $(INCLUDES) $(OBJDIR)/aheflib.a -I $(INCDIR) -o$(MAINDIR)/$@


clean :
	rm -f $(OBJDIR)/*
	rm -f $(OBJDIR)/*.o
