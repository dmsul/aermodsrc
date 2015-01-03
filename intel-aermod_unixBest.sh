ifort modules.f  -c -O2 -check format  -no-prec-div -traceback
ifort aermod.f   -c -O2 -check format  -no-prec-div -traceback 
ifort setup.f    -c -O2 -check format  -no-prec-div -traceback
ifort coset.f    -c -O2 -check format  -no-prec-div -traceback
ifort soset.f    -c -O2 -check format  -no-prec-div -traceback
ifort reset.f    -c -O2 -check format  -no-prec-div -traceback
ifort meset.f    -c -O2 -check format  -no-prec-div -traceback
ifort ouset.f    -c -O2 -check format  -no-prec-div -traceback
ifort inpsum.f   -c -O2 -check format  -no-prec-div -traceback
ifort metext.f   -c -O2 -check format  -no-prec-div -traceback
ifort iblval.f   -c -O2 -check format  -no-prec-div -traceback
ifort siggrid.f  -c -O2 -check format  -no-prec-div -traceback
ifort tempgrid.f -c -O2 -check format  -no-prec-div -traceback
ifort windgrid.f -c -O2 -check format  -no-prec-div -traceback
ifort calc1.f    -c -O2 -check format  -no-prec-div -traceback
ifort calc2.f    -c -O2 -check format  -no-prec-div -traceback
ifort prise.f    -c -O2 -check format  -no-prec-div -traceback
ifort prime.f    -c -O2 -check format  -no-prec-div -traceback
ifort sigmas.f   -c -O2 -check format  -no-prec-div -traceback
ifort pitarea.f  -c -O2 -check format  -no-prec-div -traceback
ifort output.f   -c -O2 -check format  -no-prec-div -traceback
ifort evset.f    -c -O2 -check format  -no-prec-div -traceback
ifort evcalc.f   -c -O2 -check format  -no-prec-div -traceback
ifort evoutput.f -c -O2 -check format  -no-prec-div -traceback
ifort -o aermod_unix -O2 -check format -no-prec-div -traceback modules.o aermod.o setup.o coset.o soset.o reset.o meset.o ouset.o inpsum.o metext.o iblval.o siggrid.o tempgrid.o windgrid.o calc1.o calc2.o prise.o prime.o sigmas.o pitarea.o output.o evset.o evcalc.o evoutput.o
rm *.o *.mod
