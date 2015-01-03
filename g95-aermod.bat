g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 modules.f  
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 aermod.f   
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 setup.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 coset.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 soset.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 reset.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 meset.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 ouset.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 inpsum.f   
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 metext.f   
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 iblval.f   
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 siggrid.f  
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 tempgrid.f 
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 windgrid.f 
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 calc1.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 calc2.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 prise.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 prime.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 sigmas.f   
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 pitarea.f  
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 output.f   
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 evset.f    
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 evcalc.f   
g95 -c -ftrace=full -fbounds-check -Wuninitialized -Wprecision-loss -O2 -march=pentium4 evoutput.f 

g95 -o aermod_v13350_g95.exe MODULES.o AERMOD.o SETUP.o COSET.o SOSET.o RESET.o MESET.o OUSET.o INPSUM.o METEXT.o IBLVAL.o SIGGRID.o TEMPGRID.o WINDGRID.o CALC1.o CALC2.o PRISE.o PRIME.o SIGMAS.o PITAREA.o OUTPUT.o EVSET.o EVCALC.o EVOUTPUT.o

del *.o *.mod