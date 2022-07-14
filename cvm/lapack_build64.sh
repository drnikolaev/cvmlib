#!/bin/sh
CVMVER=9.0
LIBDIR=../lib
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

#<<COMMENT

${MAKEUTIL} clean EM64T=1
${MAKEUTIL}       EM64T=1
cd $LIBDIR
export LD_LIBRARY_PATH=.
./regtest_cvm_em64t
if test "$?" != 0
then
  export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
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
./regtest_cvm_em64t_ilp64
if test "$?" != 0
then
  export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
  exit
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH_ORIG}
rm -f ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar
rm -f ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar.gz
tar -cf ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar ${FILESET_ILP64}
gzip ../cvmlib.$CVMVER.lapack.em64t.ilp64.tar
cd ${RUNDIR}
