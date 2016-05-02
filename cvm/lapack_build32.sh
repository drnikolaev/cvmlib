#!/bin/bash
LIBDIR=../lib
FILESET='LIC* libcvm_ia32.* libcvm_ia32_debug.* regtest_cvm_ia32 regtest_cvm_ia32_debug'
CVMVER=8.2
LD_LIBRARY_PATH_ORIG=${LD_LIBRARY_PATH}

#<<COMMENT

make clean
make
pushd $LIBDIR
export LD_LIBRARY_PATH=.
./regtest_cvm_ia32
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.lapack.ia32.tar
rm -f ../cvmlib.$CVMVER.lapack.ia32.tar.gz
tar -cf ../cvmlib.$CVMVER.lapack.ia32.tar ${FILESET}
gzip ../cvmlib.$CVMVER.lapack.ia32.tar
popd

#COMMENT

# 0-based smoke test
make clean
make release CVM_ZERO_BASED=1 VERBOSE=1
pushd $LIBDIR
export LD_LIBRARY_PATH=.
./regtest_cvm_ia32
if test "$?" != 0
then
  exit
fi
popd

make clean
