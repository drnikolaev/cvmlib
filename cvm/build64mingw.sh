#!/bin/bash
CVMVER=8.1
LIBDIR=../lib64
MINGWPATH='C:\TDM-GCC-64'
FILESET='LIC* libcvm_em64t.* libcvm_em64t_debug.* regtest_cvm_em64t.exe regtest_cvm_em64t_debug.exe'
FILESET_ILP64='LIC* libcvm_em64t_ilp64.* libcvm_em64t_ilp64_debug.* regtest_cvm_em64t_ilp64.exe regtest_cvm_em64t_ilp64_debug.exe'
#LD_LIBRARY_PATH_ORIG=${LD_LIBRARY_PATH}
PATHORIG=$PATH

pushd ${LIBDIR}/mingw
cp *.a ../
popd

pushd ${LIBDIR}
rm -rf mingw64
mkdir mingw64
cp ${MINGWPATH}/bin/libgfortran_64-3.dll mingw64
cp ${MINGWPATH}/bin/libquadmath_64-0.dll mingw64
cp ${MINGWPATH}/bin/libgcc_s_seh_64-1.dll mingw64
popd

#make clean EM64T=1
make       EM64T=1
pushd ${LIBDIR}
export PATH=mingw64
./regtest_cvm_em64t -t2 -r5
if test "$?" != 0
then
  export PATH=${PATHORIG}
  exit
fi
export PATH=${PATHORIG}
rm -f ../cvmlib.$CVMVER.lapack.em64t.mingw.tar
rm -f ../cvmlib.$CVMVER.lapack.em64t.mingw.tar.gz
tar -cf ../cvmlib.$CVMVER.lapack.em64t.mingw.tar ${FILESET} mingw64/*
gzip ../cvmlib.$CVMVER.lapack.em64t.mingw.tar
popd


make clean EM64T=1 ILP64=1
make       EM64T=1 ILP64=1
pushd ${LIBDIR}
export PATH=mingw64
./regtest_cvm_em64t_ilp64 -t2 -r5
if test "$?" != 0
then
  export PATH=${PATHORIG}
  exit
fi
export PATH=${PATHORIG}
rm -f ../cvmlib.$CVMVER.lapack.em64t.ilp64.mingw.tar
rm -f ../cvmlib.$CVMVER.lapack.em64t.ilp64.mingw.tar.gz
tar -cf ../cvmlib.$CVMVER.lapack.em64t.ilp64.mingw.tar ${FILESET_ILP64} mingw64/*
gzip ../cvmlib.$CVMVER.lapack.em64t.ilp64.mingw.tar
popd
