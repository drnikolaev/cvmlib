c BLAS routine missing in ACML and some other implementations

      integer function izamin(n,zx,incx)
c
c     finds the index of element having max. absolute value.
c
      double complex zx(*)
      double precision dmin
      integer i,incx,ix,n
      double precision cdabs
c
      izamin = 0
      if( n.lt.1 .or. incx.le.0 )return
      izamin = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      dmin = cdabs(zx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(cdabs(zx(ix)).ge.dmin) go to 5
         izamin = i
         dmin = cdabs(zx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 dmin = cdabs(zx(1))
      do 30 i = 2,n
         if(cdabs(zx(i)).ge.dmin) go to 30
         izamin = i
         dmin = cdabs(zx(i))
   30 continue
      return
      end
