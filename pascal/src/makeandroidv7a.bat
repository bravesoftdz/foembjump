del fpc.cfg
copy fpcv7a.cfg fpc.cfg
copy androidlibs\libc.so libc.so
copy androidlibs\libc.a libc.a
copy androidlibs\liblog.so liblog.so
copy androidlibs\libdl.so libdl.so
copy androidlibs\libGLESv2.so libGLESv2.so
del *.ppu
del *.s
del *.o
del link.res
SET PATH=d:\FPC\2.5.1\bin\i386-win32\;d:\FPC\2.5.1\bin\arm-linux\
SET nodosfilewarning=1
ppcrossarm -b -B -XX -Xc -XD -Tlinux -darm -dandroid -dBeRoXMRingBuffer -dMixerOwnDefine -dUnrolledLoops -dMixerNearest -dMixerLinear -dMixerCubicSpline -dMixerWindowedFIR -dUseMIDIMacrors -dUseAmigaMODLoader -dUseDPCM4 -dUseFilters -dUseMono -dUseStereo -dUseSaver -dmobile -olibjumpgame.so jumpgame.dpr
del fpc.cfg
del libc.so
del libc.a
del libdl.so
del libGLESv2.so
del liblog.so
del *.ppu
del *.s
del *.o
del link.res



