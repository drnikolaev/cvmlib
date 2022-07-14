#!/bin/bash
CVMVER=9.0.mac
LIBDIR=../lib
FILESET='LIC* libcvm_em64t.* libcvm_em64t_debug.* regtest_cvm_em64t regtest_cvm_em64t_debug'
FILESET_ILP64='LIC* libcvm_em64t_ilp64.* libcvm_em64t_ilp64_debug.* regtest_cvm_em64t_ilp64 regtest_cvm_em64t_ilp64_debug'

source /opt/intel/oneapi/setvars.sh intel64
DYLD_LIBRARY_PATH_ORIG=${DYLD_LIBRARY_PATH}

pushd $LIBDIR || exit
rm -rf intel64
mkdir intel64
cp /opt/intel/oneapi/mkl/latest/lib/*.dylib intel64/
cp /opt/intel/oneapi/lib/*.dylib intel64/
cp /opt/intel/oneapi/compiler/latest/mac/compiler/lib/*.dylib intel64/
popd

make clean MAC=1 ICC=1 IFORT=1 MKL=1 EM64T=1
make       MAC=1 ICC=1 IFORT=1 MKL=1 EM64T=1
pushd $LIBDIR
export DYLD_LIBRARY_PATH=.:intel64
./regtest_cvm_em64t -t2 -r3
#if test "$?" != 0
#then
#  echo "ERROR!"
#  exit
#fi
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH_ORIG}
#rm -f ../cvmlib.$CVMVER.mkl.em64t.tar
#rm -f ../cvmlib.$CVMVER.mkl.em64t.tar.gz
#tar -cf ../cvmlib.$CVMVER.mkl.em64t.tar ${FILESET} intel64/*
#gzip ../cvmlib.$CVMVER.mkl.em64t.tar
popd


make clean MAC=1 ICC=1 IFORT=1 MKL=1 EM64T=1 ILP64=1
make       MAC=1 ICC=1 IFORT=1 MKL=1 EM64T=1 ILP64=1
pushd $LIBDIR || exit
ls -al
export DYLD_LIBRARY_PATH=.:intel64
./regtest_cvm_em64t_ilp64 -t2 -r3
#if test "$?" != 0
#then
#  exit
#fi
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH_ORIG}
#rm -f ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar
#rm -f ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar.gz
#tar -cf ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar ${FILESET_ILP64} intel64/*
#gzip ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar
popd
