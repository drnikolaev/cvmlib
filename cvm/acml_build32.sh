#!/bin/bash
CVMVER=8.1
LIBDIR=../lib
export ACML_PATH=/opt/acml4.4.0
FILESET='LIC* libcvm_ia32.* libcvm_ia32_debug.* regtest_cvm_ia32 regtest_cvm_ia32_debug'

#unset LD_LIBRARY_PATH
#source /opt/intel/bin/compilervars.sh intel64
LD_LIBRARY_PATH_ORIG=${LD_LIBRARY_PATH}

#don't mix icpc and g++ until they fix this mess
#make clean IFORT=1 ICC=1 ACML=1
#make       IFORT=1 ICC=1 ACML=1
#make clean IFORT=1 ICC=1 ICCT=1 ACML=1
#make       IFORT=1 ICC=1 ICCT=1 ACML=1
make clean ACML=1
make       ACML=1
pushd $LIBDIR
rm -rf acml32
mkdir acml32
cp ${ACML_PATH}/gfortran32/lib/libacml.so acml32
export LD_LIBRARY_PATH=.:acml32
./regtest_cvm_ia32 -t2 -r3
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.acml.ia32.tar
rm -f ../cvmlib.$CVMVER.acml.ia32.tar.gz
tar -cf ../cvmlib.$CVMVER.acml.ia32.tar ${FILESET} acml32/*
gzip ../cvmlib.$CVMVER.acml.ia32.tar
popd


#don't mix icpc and g++ until they fix this mess
#make clean IFORT=1 ICC=1 ACML_MP=1
#make       IFORT=1 ICC=1 ACML_MP=1
#make clean IFORT=1 ICC=1 ICCT=1 ACML_MP=1
#make       IFORT=1 ICC=1 ICCT=1 ACML_MP=1
make clean ACML_MP=1
make       ACML_MP=1
pushd $LIBDIR
rm -rf acml_mp32
mkdir acml_mp32
cp ${ACML_PATH}/gfortran32_mp/lib/libacml_mp.so acml_mp32
export LD_LIBRARY_PATH=.:acml_mp32
./regtest_cvm_ia32 -t2 -r3
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.acml_mp.ia32.tar
rm -f ../cvmlib.$CVMVER.acml_mp.ia32.tar.gz
tar -cf ../cvmlib.$CVMVER.acml_mp.ia32.tar ${FILESET} acml_mp32/*
gzip ../cvmlib.$CVMVER.acml_mp.ia32.tar
popd

