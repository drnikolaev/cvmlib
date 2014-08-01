#!/bin/bash
LIBDIR=../lib
MINGWPATH='C:\TDM-GCC-32'
FILESET='LIC* libcvm_ia32.* libcvm_ia32_debug.* regtest_cvm_ia32.exe regtest_cvm_ia32_debug.exe'
CVMVER=8.1
#LD_LIBRARY_PATH_ORIG=${LD_LIBRARY_PATH}
PATHORIG=$PATH

pushd ${LIBDIR}/mingw/
cp *.a ../
popd

make clean
make
pushd $LIBDIR
rm -rf mingw32
mkdir mingw32
cp ${MINGWPATH}/bin/libgfortran-3.dll mingw32
cp ${MINGWPATH}/bin/libquadmath-0.dll mingw32
cp ${MINGWPATH}/bin/libgcc_s_dw2-1.dll mingw32
cp ${MINGWPATH}/bin/libgcc_s_sjlj-1.dll mingw32
export PATH=mingw32
./regtest_cvm_ia32 -t2 -r5
if test "$?" != 0
then
  export PATH=${PATHORIG}
  exit
fi
export PATH=${PATHORIG}
rm -f ../cvmlib.$CVMVER.lapack.ia32.mingw.tar
rm -f ../cvmlib.$CVMVER.lapack.ia32.mingw.tar.gz
tar -cf ../cvmlib.$CVMVER.lapack.ia32.mingw.tar ${FILESET} mingw32/*
gzip ../cvmlib.$CVMVER.lapack.ia32.mingw.tar
popd
