ifort modules.f  -c 
ifort aermod.f   -c  
ifort setup.f    -c 
ifort coset.f    -c 
ifort soset.f    -c 
ifort reset.f    -c 
ifort meset.f    -c 
ifort ouset.f    -c 
ifort inpsum.f   -c 
ifort metext.f   -c 
ifort iblval.f   -c 
ifort siggrid.f  -c 
ifort tempgrid.f -c 
ifort windgrid.f -c 
ifort calc1.f    -c 
ifort calc2.f    -c 
ifort prise.f    -c 
ifort prime.f    -c 
ifort sigmas.f   -c 
ifort pitarea.f  -c 
ifort output.f   -c 
ifort evset.f    -c 
ifort evcalc.f   -c 
ifort evoutput.f -c 
ifort -o aermod_v13350_Intel.out modules.o aermod.o setup.o coset.o soset.o reset.o meset.o ouset.o inpsum.o metext.o iblval.o siggrid.o tempgrid.o windgrid.o calc1.o calc2.o prise.o prime.o sigmas.o pitarea.o output.o evset.o evcalc.o evoutput.o

rm *.o *.mod