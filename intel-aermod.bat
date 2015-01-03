ifort modules.f  /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort aermod.f   /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace 
ifort setup.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort coset.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort soset.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort reset.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort meset.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort ouset.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort inpsum.f   /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort metext.f   /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort iblval.f   /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort siggrid.f  /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort tempgrid.f /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort windgrid.f /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort calc1.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort calc2.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort prise.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort prime.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort sigmas.f   /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort pitarea.f  /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort output.f   /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort evset.f    /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort evcalc.f   /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort evoutput.f /compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
ifort /exe:aermod_v13350_Intel.exe /O2 /Qipo /check:format /Qprec-div- /QaxSSE2 modules.obj aermod.obj setup.obj coset.obj soset.obj reset.obj meset.obj ouset.obj inpsum.obj metext.obj iblval.obj siggrid.obj tempgrid.obj windgrid.obj calc1.obj calc2.obj prise.obj prime.obj sigmas.obj pitarea.obj output.obj evset.obj evcalc.obj evoutput.obj

rm *.obj
rm *.mod

