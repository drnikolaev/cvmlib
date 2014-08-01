#!/bin/bash
CVMVER=8.1
LIBDIR=../lib
FILESET='LIC* libcvm_ia32.* libcvm_ia32_debug.* regtest_cvm_ia32 regtest_cvm_ia32_debug'

unset LD_LIBRARY_PATH
source /opt/intel/bin/compilervars.sh ia32
LD_LIBRARY_PATH_ORIG=${LD_LIBRARY_PATH}

#FIXED: don't mix icpc and g++ until they fix this mess
make clean IFORT=1 ICC=1 MKL=1
make       IFORT=1 ICC=1 MKL=1
#make clean IFORT=1 ICC=1 ICCT=1 MKL=1
#make       IFORT=1 ICC=1 ICCT=1 MKL=1
#make clean IFORT=1 MKL=1
#make       IFORT=1 MKL=1
pushd $LIBDIR
rm -rf intel32
mkdir intel32
cp /opt/intel/lib/ia32/libintlc.so.5 intel32
cp /opt/intel/lib/ia32/libifcoremt.so.5 intel32
cp /opt/intel/lib/ia32/libifcore.so.5 intel32
cp /opt/intel/lib/ia32/libpdbx.so.5 intel32
cp /opt/intel/lib/ia32/libifport.so.5 intel32
cp /opt/intel/lib/ia32/libiompstubs5.so intel32
cp /opt/intel/lib/ia32/libiomp5.so intel32
cp /opt/intel/lib/ia32/libomp_db.so intel32
cp /opt/intel/lib/ia32/libirc.so intel32
cp /opt/intel/lib/ia32/libsvml.so intel32
cp /opt/intel/lib/ia32/libirng.so intel32
cp /opt/intel/lib/ia32/libimf.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_p4p.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_gf.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_p4.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_gnu_thread.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_sequential.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_intel.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_intel_thread.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_p4m3.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_p4m.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_core.so intel32
cp /opt/intel/mkl/lib/ia32/libmkl_rt.so intel32
export LD_LIBRARY_PATH=.:intel32
./regtest_cvm_ia32 -t2 -r3
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.mkl.ia32.tar
rm -f ../cvmlib.$CVMVER.mkl.ia32.tar.gz
tar -cf ../cvmlib.$CVMVER.mkl.ia32.tar ${FILESET} intel32/*
gzip ../cvmlib.$CVMVER.mkl.ia32.tar
popd

