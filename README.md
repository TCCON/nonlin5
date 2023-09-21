# nonlin5

This software was written by Pauli Heikkinen (Pauli.Heikkinen@fmi.fi) and Rigel Kivi (Rigel.Kivi@fmi.fi) to compute coefficients that correct nonlinearity in 125HR spectra. The software is written based on the formulation by Frank Hase (frank.hase@kit.edu), described in his PhD thesis (https://www.imk-asf.kit.edu/downloads/bod/disshase.pdf) in Chapter 5.

SOFTWARE REQUIREMENTS:

 - Linux operating system
 - Intel Fortran Compiler installed
 - GNU Parallel installed

REQUIREMENTS FOR SPECTRA:

 - Spectra in OPUS format
 - Measurements in DC mode
 - One forward-backward scan in one file
 - When no light enters the instrument, interferogram values should be close to zero. No significant offset in zero level

 If the above conditions are not met, then the code needs modifications!

INSTALLATION:

 1. Untar nonlin5.tgz somewhere

 2. Go to the nonlin5 directory

 3. Compile GGG2020 functions & subroutines (for reading OPUS-files):
   [ftir@kenobi.fmi.fi nonlin5]$ cd ggg_src
   [ftir@kenobi.fmi.fi ggg_src]$ make

 4. Build executables, pre-nonlin5, nonlin5 and collate_results5:
   [ftir@kenobi.fmi.fi ggg_src]$ cd ..
   [ftir@kenobi.fmi.fi nonlin5]$ make

INSTALLATION TEST

 1. Modify the directory paths:

   nonlin5/pre-nonlin5.inp file: 
     % nonlin5 install directory
     /opt/GGG/run_ggg2020/nonlin/nonlin5/
     % opus-i2s input file directory
     /opt/GGG/run_ggg2020/nonlin/nonlin5/test/opus-i2s_in/

   nonlin5/test/opus-i2s_in/opus-i2s_20170906.in file:
     Parameter #1
     /opt/GGG/run_ggg2020/nonlin/nonlin5/test/spectra/

   Replace the part "/opt/GGG/run_ggg2020/nonlin/" in the paths above with the real one.

 2. Run pre-nonlin5
   [ftir@kenobi.fmi.fi nonlin5]$ ./pre-nonlin5

 3. Run nonlin5
   [ftir@kenobi.fmi.fi nonlin5]$ ./run_nonlin5.sh
   Try also:
   [ftir@kenobi.fmi.fi nonlin5]$ ./nonlin5

 4. Run collate_params 
   [ftir@kenobi.fmi.fi nonlin5]$ ./collate_params5 params/

 5. Compare the results with those found in test/sod_results

PROGRAMS:

### nonlin5 ###

nonlin5 calculates the correction coefficients for correcting interferograms affected by non-linearity. The correction function for the interferogram:
 I_corrected = I_original + a x I_original^2 + b x I_original^3 + c x I_original^4 (a,b and c positive)
The cost function is defined as a ratio of the signal sum in the out-of-band regions, 100-3600cm-1 and 14200-15750cm-1 to the signal sum on the in-band region 4100-9700cm-1. Coefficients a, b and c are found that minimize the cost function. The minimum value is approached iteratively adding decreasing random numbers to the coefficients. It starts finding coefficients around 0. The search area width (Initial step size) is 1.0E-1. If better coefficients are found, then the search continues around the new values with the same step size. If better coefficients are not found after 100 tries (Number of tries), then the step size is reduced by 0.25 (Step adjustment coefficient). The search ends when the step size is below the threshold (Min step size), 1.0E-6, or the total number of tries is 50 000 (Max number of cycles). If 50 000 is reached, then probably the search didn’t work. The search parameters are defined in the nonlin5 input file. The parameters might need some tuning.

For reading interferogram data from OPUS files, there are parameters ”Range of channels” and ”Selected channel” in the input file. Channel 1 is the slave and channel 2 is the master. For instruments that have simultaneous InGaAs and Si measurements, Range of channels is 1 2. Typically, Si is the master and InGaAs is the slave. Setting Selected channel to 1 reads InGaAs data. In the case of single-detector acquisition there is only master channel available. Range of channel is 2 2 and Selected channel is 2 then. In general, ifg values are positive and they increase with increasing signal. But if interferograms are inverted, ifg values are negative and they decrease with increasing signal, then “upside down?” parameter needs to be set to 1. It multiplies ifg values by -1 making them positive.

nonlin5 can be run by giving the input file name as an argument. Like this:
./nonlin5 /opt/GGG/run_ggg2020/nonlin/nonlin5/inputs/nonlin5_20170906_0125.inp
If it is run without the argument, then nonlin5.inp file must exist in the same directory as nonlin5.

Output files:

Parameter file (nonlin5/params/):
 Line 1: JD2000 date/time
 Line 2: SIA, FVSI
 Line 3: IFG centerburst position FWD, BWD
 Line 4: IFG centerburst value FWD, BWD
 Line 5: Final cost function value
 Line 6: Number of cycles
 Line 7: a, b, c coefficients

Original spectrum file (nonlin5/spec_orig/) &
Optimized spectrum file (nonlin5/spec_opti/):
 Column 1: Wavenumber
 Column 2: FWD spectrum
 Column 3: BWD spectrum

Cycles file (nonlin5/cycles):
 Column 1: Cycle no.
 Column 2: Cost function value
 Columns 3-5: a, b, c coefficients

### pre-nonlin5 ###

pre-nonlin5 reads all opus-i2s input files from the directory that is given in the pre-nonlin5.inp file. It creates nonlin5 input files for all spectra it can find from the i2s files. It puts them in nonlin5/inputs/. Min SIA, max FVSI and date interval can be defined to skip some spectra. pre-nonlin5 also makes script files in nonlin5/scripts/ directory for running nonlin5. run_nonlin5.sh feeds these scripts to parallel. Number of parallel jobs is set in pre-nonlin5.inp.

### collate_params5 ###

collate_params5 collects data from all parameter files to one file, collate_params5.out. Directory path to parameter files must be given as an argument:
 ./collate_params5 params/

## After computing coefficients ##

After computing the set of coefficients across several interferograms, compute a global set of coefficients by plotting the correction curves, taking the average correction curve and finding the coefficients that best fit the average curve. The software to do this part is not currently included in nonlin5. Here is an example of the method:

![correction](https://github.com/TCCON/nonlin5/assets/38021331/dccc98ad-d8a2-4c95-8cb5-e027d3d8ebdb)
