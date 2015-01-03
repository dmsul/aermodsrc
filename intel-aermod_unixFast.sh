ifort modules.f  -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort aermod.f   -c -O3 -xHost -check format  -no-prec-div --align-all  -ipo
ifort setup.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort coset.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort soset.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort reset.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort meset.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort ouset.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort inpsum.f   -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort metext.f   -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort iblval.f   -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort siggrid.f  -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort tempgrid.f -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort windgrid.f -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort calc1.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort calc2.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort prise.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort prime.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort sigmas.f   -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort pitarea.f  -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort output.f   -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort evset.f    -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort evcalc.f   -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort evoutput.f -c -O3 -xHost -check format  -no-prec-div --align-all -ipo
ifort -o aermod_faster -O3 -xHost -check format -no-prec-div --align-all -ipo modules.o aermod.o setup.o coset.o soset.o reset.o meset.o ouset.o inpsum.o metext.o iblval.o siggrid.o tempgrid.o windgrid.o calc1.o calc2.o prise.o prime.o sigmas.o pitarea.o output.o evset.o evcalc.o evoutput.o
rm *.o *.mod
