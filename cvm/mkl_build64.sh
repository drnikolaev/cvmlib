#!/bin/bash
CVMVER=8.1
LIBDIR=../lib64
FILESET='LIC* libcvm_em64t.* libcvm_em64t_debug.* regtest_cvm_em64t regtest_cvm_em64t_debug'
FILESET_ILP64='LIC* libcvm_em64t_ilp64.* libcvm_em64t_ilp64_debug.* regtest_cvm_em64t_ilp64 regtest_cvm_em64t_ilp64_debug'

source /opt/intel/bin/compilervars.sh intel64
LD_LIBRARY_PATH_ORIG=${LD_LIBRARY_PATH}

pushd $LIBDIR
rm -rf intel64
mkdir intel64
cp /opt/intel/lib/intel64/libirng.so intel64
cp /opt/intel/lib/intel64/libifcoremt.so.5 intel64
cp /opt/intel/lib/intel64/libifcore.so.5 intel64
cp /opt/intel/lib/intel64/libifport.so.5 intel64
cp /opt/intel/lib/intel64/libimf.so intel64
cp /opt/intel/lib/intel64/libintlc.so.5 intel64
cp /opt/intel/lib/intel64/libiomp5.so intel64
cp /opt/intel/lib/intel64/libiompstubs5.so intel64
cp /opt/intel/lib/intel64/libirc.so intel64
cp /opt/intel/lib/intel64/libomp_db.so intel64
cp /opt/intel/lib/intel64/libpdbx.so.5 intel64
cp /opt/intel/lib/intel64/libsvml.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_avx.so intel64
#cp /opt/intel/mkl/lib/intel64/libmkl_blacs_intelmpi_ilp64.so intel64
#cp /opt/intel/mkl/lib/intel64/libmkl_cdft_core.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_core.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_def.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_gf_ilp64.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_gf_lp64.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_gnu_thread.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_intel_ilp64.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_intel_lp64.so intel64
#cp /opt/intel/mkl/lib/intel64/libmkl_intel_sp2dp.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_intel_thread.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_mc3.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_mc.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_p4n.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_rt.so intel64
cp /opt/intel/mkl/lib/intel64/libmkl_sequential.so intel64
popd

# no ICC until they make it compatible with gcc
make clean IFORT=1 ICC=1 MKL=1 EM64T=1
make       IFORT=1 ICC=1 MKL=1 EM64T=1
#make clean IFORT=1 MKL=1 EM64T=1
#make       IFORT=1 MKL=1 EM64T=1
pushd $LIBDIR
export LD_LIBRARY_PATH=.:intel64
./regtest_cvm_em64t -t2 -r3
if test "$?" != 0
then
  echo "ERROR!"
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.mkl.em64t.tar
rm -f ../cvmlib.$CVMVER.mkl.em64t.tar.gz
tar -cf ../cvmlib.$CVMVER.mkl.em64t.tar ${FILESET} intel64/*
gzip ../cvmlib.$CVMVER.mkl.em64t.tar
popd


# no ICC until they make it compatible with gcc
make clean IFORT=1 ICC=1 MKL=1 EM64T=1 ILP64=1
make       IFORT=1 ICC=1 MKL=1 EM64T=1 ILP64=1
#make clean IFORT=1 MKL=1 EM64T=1 ILP64=1
#make       IFORT=1 MKL=1 EM64T=1 ILP64=1
pushd $LIBDIR
export LD_LIBRARY_PATH=.:intel64
./regtest_cvm_em64t_ilp64 -t2 -r3
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar
rm -f ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar.gz
tar -cf ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar ${FILESET_ILP64} intel64/*
gzip ../cvmlib.$CVMVER.mkl.em64t.ilp64.tar
popd

