ifort modules.f  -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort aermod.f   -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback 
ifort setup.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort coset.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort soset.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort reset.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort meset.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort ouset.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort inpsum.f   -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort metext.f   -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort iblval.f   -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort siggrid.f  -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort tempgrid.f -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort windgrid.f -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort calc1.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort calc2.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort prise.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort prime.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort sigmas.f   -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort pitarea.f  -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort output.f   -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort evset.f    -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort evcalc.f   -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort evoutput.f -c -O2 -check format -ipo -no-prec-div -axSSE2 -traceback
ifort -o aermod_v13350_Intel.out -O2 -ipo -check format -no-prec-div -axSSE2 modules.o aermod.o setup.o coset.o soset.o reset.o meset.o ouset.o inpsum.o metext.o iblval.o siggrid.o tempgrid.o windgrid.o calc1.o calc2.o prise.o prime.o sigmas.o pitarea.o output.o evset.o evcalc.o evoutput.o

rm *.o *.mod