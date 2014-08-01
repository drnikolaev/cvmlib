#!/bin/sh
CVMVER=8.1
LIBDIR=../lib64
FILESET='LIC* libcvm_em64t.* libcvm_em64t_debug.* regtest_cvm_em64t regtest_cvm_em64t_debug'
FILESET_ILP64='LIC* libcvm_em64t_ilp64.* libcvm_em64t_ilp64_debug.* regtest_cvm_em64t_ilp64 regtest_cvm_em64t_ilp64_debug'
LD_LIBRARY_PATH_ORIG=${LD_LIBRARY_PATH}

if test "$OSTYPE" = FreeBSD
then
  MAKEUTIL=gmake
else
  MAKEUTIL=make
fi

#RUNDIR="$(pwd)"
RUNDIR=`pwd`
echo ${RUNDIR}


cd ${LIBDIR}/linux/gf
cp *.a ../..
cd ${RUNDIR}
cd ${LIBDIR}/linux/ifort
cp *.a ../..
cd ${RUNDIR}

#<<COMMENT


${MAKEUTIL} clean EM64T=1
${MAKEUTIL}       EM64T=1
cd $LIBDIR
export LD_LIBRARY_PATH=.
./regtest_cvm_em64t -t2 -r3
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.lapack.em64t.tar
rm -f ../cvmlib.$CVMVER.lapack.em64t.tar.gz
tar -cf ../cvmlib.$CVMVER.lapack.em64t.tar ${FILESET}
gzip ../cvmlib.$CVMVER.lapack.em64t.tar
cd ${RUNDIR}


#COMMENT


${MAKEUTIL} clean EM64T=1 ILP64=1
${MAKEUTIL}       EM64T=1 ILP64=1
cd $LIBDIR
export LD_LIBRARY_PATH=.
./regtest_cvm_em64t_ilp64 -t2 -r3
if test "$?" != 0
then
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar
rm -f ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar.gz
tar -cf ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar ${FILESET_ILP64}
gzip ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar
cd ${RUNDIR}


# CVM_FLOAT smoke test
${MAKEUTIL} clean EM64T=1 CVM_FLOAT=1
${MAKEUTIL} release EM64T=1 CVM_FLOAT=1
cd $LIBDIR
export LD_LIBRARY_PATH=.
./regtest_cvm_em64t -t2 -r3
if test "$?" != 0
then
  exit
fi
cd ${RUNDIR}



# CVM_FLOAT & CVM0 smoke test
${MAKEUTIL} clean CVM_ZERO_BASED=1 EM64T=1 CVM_FLOAT=1
${MAKEUTIL} release CVM_ZERO_BASED=1 EM64T=1 CVM_FLOAT=1
cd $LIBDIR
export LD_LIBRARY_PATH=.
./regtest_cvm_em64t -t2 -r3
if test "$?" != 0
then
  exit
fi
cd ${RUNDIR}


# CVM0 smoke test
${MAKEUTIL} clean CVM_ZERO_BASED=1 EM64T=1
${MAKEUTIL} release CVM_ZERO_BASED=1 EM64T=1
cd $LIBDIR
export LD_LIBRARY_PATH=.
./regtest_cvm_em64t -t2 -r3
if test "$?" != 0
then
  exit
fi
cd ${RUNDIR}


