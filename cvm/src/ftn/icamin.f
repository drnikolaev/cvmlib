c BLAS routine missing in ACML and some other implementations

      integer function icamin(n,cx,incx)
c
c     finds the index of element having min. absolute value.
c
      complex cx(*)
      real smin
      integer i,incx,ix,n
      real cabs
c
      icamin = 0
      if( n.lt.1 .or. incx.le.0 ) return
      icamin = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      smin = cabs(cx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(cabs(cx(ix)).ge.smin) go to 5
         icamin = i
         smin = cabs(cx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 smin = cabs(cx(1))
      do 30 i = 2,n
         if(cabs(cx(i)).ge.smin) go to 30
         icamin = i
         smin = cabs(cx(i))
   30 continue
      return
      end

