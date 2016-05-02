#!/bin/bash
CVMVER=8.2.mac
LIBDIR=../lib64
FILESET='LIC* libcvm_em64t.* libcvm_em64t_debug.* regtest_cvm_em64t regtest_cvm_em64t_debug'
FILESET_ILP64='LIC* libcvm_em64t_ilp64.* libcvm_em64t_ilp64_debug.* regtest_cvm_em64t_ilp64 regtest_cvm_em64t_ilp64_debug'

source /opt/intel/bin/compilervars.sh intel64
DYLD_LIBRARY_PATH_ORIG=${DYLD_LIBRARY_PATH}

pushd $LIBDIR
rm -rf intel64
mkdir intel64

cp /opt/intel/lib/intel64/libistrconv.dylib intel64
cp /opt/intel/lib/libirng.dylib intel64
cp /opt/intel/lib/libifcoremt.dylib intel64
cp /opt/intel/lib/libifcore.dylib intel64
cp /opt/intel/lib/libifportmt.dylib intel64
cp /opt/intel/lib/libifport.dylib intel64
cp /opt/intel/lib/libimf.dylib intel64
cp /opt/intel/lib/libintlc.dylib intel64
cp /opt/intel/lib/libiomp5.dylib intel64
cp /opt/intel/lib/libiompstubs5.dylib intel64
cp /opt/intel/lib/libirc.dylib intel64
cp /opt/intel/lib/libsvml.dylib intel64
cp /opt/intel/mkl/lib/libmkl_avx.dylib intel64
cp /opt/intel/mkl/lib/libmkl_avx2.dylib intel64
cp /opt/intel/mkl/lib/libmkl_core.dylib intel64
cp /opt/intel/mkl/lib/libmkl_intel_ilp64.dylib intel64
cp /opt/intel/mkl/lib/libmkl_intel_lp64.dylib intel64
cp /opt/intel/mkl/lib/libmkl_intel_thread.dylib intel64
cp /opt/intel/mkl/lib/libmkl_mc3.dylib intel64
cp /opt/intel/mkl/lib/libmkl_mc.dylib intel64
cp /opt/intel/mkl/lib/libmkl_p4m.dylib intel64
cp /opt/intel/mkl/lib/libmkl_p4m3.dylib intel64
cp /opt/intel/mkl/lib/libmkl_rt.dylib intel64
cp /opt/intel/mkl/lib/libmkl_sequential.dylib intel64
popd

make clean MAC=1 IFORT=1 MKL=1 EM64T=1
make       MAC=1 IFORT=1 MKL=1 EM64T=1
pushd $LIBDIR
export DYLD_LIBRARY_PATH=.:intel64
./regtest_cvm_em64t -t2 -r3
if test "$?" != 0
then
  echo "ERROR!"
  exit
fi
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.mkl.em64t.tar
rm -f ../cvmlib.$CVMVER.mkl.em64t.tar.gz
tar -cf ../cvmlib.$CVMVER.mkl.em64t.tar ${FILESET} intel64/*
gzip ../cvmlib.$CVMVER.mkl.em64t.tar
popd


make clean MAC=1 IFORT=1 MKL=1 EM64T=1 ILP64=1
make       MAC=1 IFORT=1 MKL=1 EM64T=1 ILP64=1
pushd $LIBDIR
export DYLD_LIBRARY_PATH=.:intel64
./regtest_cvm_em64t_ilp64 -t2 -r3
if test "$?" != 0
then
  exit
fi
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar
rm -f ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar.gz
tar -cf ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar ${FILESET_ILP64} intel64/*
gzip ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar
popd
