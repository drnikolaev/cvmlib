#!/bin/bash
CVMVER=8.2.mac
LIBDIR=../lib64
FILESET='LIC* libcvm_em64t.* libcvm_em64t_debug.* regtest_cvm_em64t regtest_cvm_em64t_debug'
FILESET_ILP64='LIC* libcvm_em64t_ilp64.* libcvm_em64t_ilp64_debug.* regtest_cvm_em64t_ilp64 regtest_cvm_em64t_ilp64_debug'
DYLD_LIBRARY_PATH_ORIG=${DYLD_LIBRARY_PATH}

pushd ${LIBDIR}/mac/gf
cp *.a ../..
popd

make clean EM64T=1 MAC=1
make       EM64T=1 MAC=1
pushd $LIBDIR
export DYLD_LIBRARY_PATH=.
./regtest_cvm_em64t -t2 -r3
if test "$?" != 0
then
  exit
fi
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.lapack.em64t.tar
rm -f ../cvmlib.$CVMVER.lapack.em64t.tar.gz
tar -cf ../cvmlib.$CVMVER.lapack.em64t.tar ${FILESET}
gzip ../cvmlib.$CVMVER.lapack.em64t.tar
popd

exit

make clean EM64T=1 ILP64=1 MAC=1
make       EM64T=1 ILP64=1 MAC=1
pushd $LIBDIR
export DYLD_LIBRARY_PATH=.
./regtest_cvm_em64t_ilp64 -t2 -r3
if test "$?" != 0
then
  exit
fi
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar
rm -f ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar.gz
tar -cf ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar ${FILESET_ILP64}
gzip ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar
popd

