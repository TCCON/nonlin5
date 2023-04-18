FC = ifort

FILES = \
  ggg_src/get_igram_run_parameters.o \
  ggg_src/get_opusigram_params.o \
  ggg_src/get_igram_data.o \
  ggg_src/get_opus_xx.o \
  ggg_src/getendian.o \
  ggg_src/julian.o \
  ggg_src/lnbc.o \
  ggg_src/fnbc.o \
  ggg_src/rbyte.o \
  get_ifg_opus.f90 \
  nonlin5.f90

all: nonlin5 pre-nonlin5 collate_params5
	
nonlin5: $(FILES)
	$(FC) $(FILES) -o nonlin5

pre-nonlin5: pre-nonlin5.f90
	$(FC) pre-nonlin5.f90 -o pre-nonlin5

collate_params5: collate_params5.f90
	$(FC) collate_params5.f90 -o collate_params5
