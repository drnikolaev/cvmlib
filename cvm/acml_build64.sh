#!/bin/bash
CVMVER=8.1
LIBDIR=../lib64
export ACML_PATH=/opt/acml5.3.1
INTELLIB64=/opt/intel/lib/intel64
FILESET='LIC* libcvm_em64t.* libcvm_em64t_debug.* regtest_cvm_em64t regtest_cvm_em64t_debug'
FILESET_ILP64='LIC* libcvm_em64t_ilp64.* libcvm_em64t_ilp64_debug.* regtest_cvm_em64t_ilp64 regtest_cvm_em64t_ilp64_debug'

unset LD_LIBRARY_PATH
source /opt/intel/bin/compilervars.sh intel64
LD_LIBRARY_PATH_ORIG=${LD_LIBRARY_PATH}


#FIXED no ICC until they fix their compatibility issues
make clean IFORT=1 ICC=1 ACML=1 EM64T=1
make       IFORT=1 ICC=1 ACML=1 EM64T=1
#make clean IFORT=1 ACML=1 EM64T=1
#make       IFORT=1 ACML=1 EM64T=1
#make clean ACML=1 EM64T=1
#make       ACML=1 EM64T=1

pushd $LIBDIR
rm -rf acml64
mkdir acml64
#cp ${ACML_PATH}/gfortran64/lib/libacml.so acml64
cp ${ACML_PATH}/ifort64/lib/libacml.so acml64
cp ${INTELLIB64}/libifport.so.5 acml64
cp ${INTELLIB64}/libifcoremt.so.5 acml64
cp ${INTELLIB64}/libintlc.so.5 acml64
cp ${INTELLIB64}/libimf.so acml64
cp ${INTELLIB64}/libirng.so acml64

export LD_LIBRARY_PATH=.:acml64
./regtest_cvm_em64t -t2 -r3
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.acml.em64t.tar
rm -f ../cvmlib.$CVMVER.acml.em64t.tar.gz
tar -cf ../cvmlib.$CVMVER.acml.em64t.tar ${FILESET} acml64/*
gzip ../cvmlib.$CVMVER.acml.em64t.tar
popd



make clean IFORT=1 ICC=1 ACML=1 EM64T=1 ILP64=1
make       IFORT=1 ICC=1 ACML=1 EM64T=1 ILP64=1
#make clean IFORT=1 ACML=1 EM64T=1 ILP64=1
#make       IFORT=1 ACML=1 EM64T=1 ILP64=1
#make clean ACML=1 EM64T=1 ILP64=1
#make       ACML=1 EM64T=1 ILP64=1

pushd $LIBDIR
rm -rf acml64_int64
mkdir acml64_int64
#cp ${ACML_PATH}/gfortran64_int64/lib/libacml.so acml64_int64
cp ${ACML_PATH}/ifort64_int64/lib/libacml.so acml64_int64
cp ${INTELLIB64}/libifport.so.5 acml64_int64
cp ${INTELLIB64}/libifcoremt.so.5 acml64_int64
cp ${INTELLIB64}/libintlc.so.5 acml64_int64
cp ${INTELLIB64}/libimf.so acml64_int64
cp ${INTELLIB64}/libirng.so acml64_int64

export LD_LIBRARY_PATH=.:acml64_int64
./regtest_cvm_em64t_ilp64 -t2 -r3
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.acml.em64t.ilp64.tar
rm -f ../cvmlib.$CVMVER.acml.em64t.ilp64.tar.gz
tar -cf ../cvmlib.$CVMVER.acml.em64t.ilp64.tar ${FILESET_ILP64} acml64_int64/*
gzip ../cvmlib.$CVMVER.acml.em64t.ilp64.tar
popd



# no ICC until they fix their TR1 compatibility issue
make clean IFORT=1 ICC=1 ACML_MP=1 EM64T=1
make       IFORT=1 ICC=1 ACML_MP=1 EM64T=1
#make clean IFORT=1 ACML_MP=1 EM64T=1
#make       IFORT=1 ACML_MP=1 EM64T=1
#make clean ACML_MP=1 EM64T=1
#make       ACML_MP=1 EM64T=1

pushd $LIBDIR
rm -rf acml64_mp
mkdir acml64_mp
#cp ${ACML_PATH}/gfortran64_mp/lib/libacml_mp.so acml64_mp
cp ${ACML_PATH}/ifort64_mp/lib/libacml_mp.so acml64_mp
cp ${INTELLIB64}/libifport.so.5 acml64_mp
cp ${INTELLIB64}/libifcoremt.so.5 acml64_mp
cp ${INTELLIB64}/libintlc.so.5 acml64_mp
cp ${INTELLIB64}/libimf.so acml64_mp
cp ${INTELLIB64}/libirng.so acml64_mp

export LD_LIBRARY_PATH=.:acml64_mp
./regtest_cvm_em64t -t2 -r3
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.acml_mp.em64t.tar
rm -f ../cvmlib.$CVMVER.acml_mp.em64t.tar.gz
tar -cf ../cvmlib.$CVMVER.acml_mp.em64t.tar ${FILESET} acml64_mp/*
gzip ../cvmlib.$CVMVER.acml_mp.em64t.tar
popd



# no ICC until they fix their TR1 compatibility issue
make clean IFORT=1 ICC=1 ACML_MP=1 EM64T=1 ILP64=1
make       IFORT=1 ICC=1 ACML_MP=1 EM64T=1 ILP64=1
#make clean IFORT=1 ACML_MP=1 EM64T=1 ILP64=1
#make       IFORT=1 ACML_MP=1 EM64T=1 ILP64=1
#make clean ACML_MP=1 EM64T=1 ILP64=1
#make       ACML_MP=1 EM64T=1 ILP64=1
pushd $LIBDIR

export LD_LIBRARY_PATH=.:acml64_mp_int64
rm -rf acml64_mp_int64
mkdir acml64_mp_int64
#cp ${ACML_PATH}/gfortran64_mp_int64/lib/libacml_mp.so acml64_mp_int64
cp ${ACML_PATH}/ifort64_mp_int64/lib/libacml_mp.so acml64_mp_int64
cp ${INTELLIB64}/libifport.so.5 acml64_mp_int64
cp ${INTELLIB64}/libifcoremt.so.5 acml64_mp_int64
cp ${INTELLIB64}/libintlc.so.5 acml64_mp_int64
cp ${INTELLIB64}/libimf.so acml64_mp_int64
cp ${INTELLIB64}/libirng.so acml64_mp_int64

./regtest_cvm_em64t_ilp64 -t2 -r3
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.acml_mp.em64t.ilp64.tar
rm -f ../cvmlib.$CVMVER.acml_mp.em64t.ilp64.tar.gz
tar -cf ../cvmlib.$CVMVER.acml_mp.em64t.ilp64.tar ${FILESET_ILP64} acml64_mp_int64/*
gzip ../cvmlib.$CVMVER.acml_mp.em64t.ilp64.tar
popd

