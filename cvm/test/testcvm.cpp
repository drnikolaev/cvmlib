#if 0
        // 8.0 move
        {
            string_array sa;
            sa.push_back ("{x,z} sign(x+2)");
            sa.push_back ("{x,z} z+3");

            rfvector fa(sa);
            rfvector fb(fa+fa);

            treal x[2];
            treal y[2] = {};

            x[0] = -2.1;
            x[1] = 8.8;
            fb.value(x,y);

            CheckReal    (y[0], (treal)-2., "rfvector - value", os, __LINE__);
            CheckReal    (y[1], (treal)23.6, "rfvector - value", os, __LINE__);

            rvector yv = fb(x);
            CheckReal    (yv[CVM0], (treal)-2., "rfvector - value", os, __LINE__);
            CheckReal    (yv[CVM0+1], (treal)23.6, "rfvector - value", os, __LINE__);

            rfvector fc = fb + fa;
            CheckString  (fc.simp()[0].format(), "sign(x+2)*3", "a + b - format()", os, __LINE__);
            CheckString  (fc.simp()[1].format(), "(z+3)*3", "a + b - format()", os, __LINE__);
        }

        // 8.0 move
        {
            string_array sa;
            sa.push_back ("{x,z} sign(x+2)");
            sa.push_back ("{x,z} z+3");
            sa.push_back ("{x,z} sign(x-2)");
            sa.push_back ("{x,z} z-3");

            rfmatrix fa(2,2,sa);
            rfmatrix fb(fa+fa);

            treal x[2];
            x[0] = -2.1;
            x[1] = 8.8;

            rmatrix ym = fb(x);
            CheckReal (ym(CVM0,CVM0), (treal)-2., "rfmatrix - value", os, __LINE__);
            CheckReal (ym(CVM0+1,CVM0), (treal)23.6, "rfmatrix - value", os, __LINE__);

            rfmatrix fc = fb - fa;
            CheckString  (fc.simp().at(0,0).format(), "sign(x+2)", "a - b - format()", os, __LINE__);
            CheckString  (fc.simp().at(1,1).format(), "z-3", "a - b - format()", os, __LINE__);
        }



        // exception extender test
        try {
            rfunction f ("{q,z} x-z");
            Fail("No exception about parsing error", os, __LINE__);
        } catch (cvmexception& ex) {
            CheckInt (ex.cause(), CFUN_PARSEERROR, "CFUN_PARSEERROR exception cause", os, __LINE__);
        }

        rfunction rf0;
        CheckString  (rf0.format(), "0", "rfunction default ctr", os, __LINE__);
        CheckString  (rf0.simp().format(), "0", "rfunction default ctr - simp()", os, __LINE__);
        CheckReal    (rf0(),     (treal)0., "rfunction default ctr - value", os, __LINE__);
        CheckReal    (rf0.value(NULL), (treal)0., "rfunction default ctr - value", os, __LINE__);
        CheckReal    (rf0(), (treal)0., "rfunction default ctr - value", os, __LINE__);
        CheckReal    (rf0(1.1),  (treal)0., "rfunction default ctr - value", os, __LINE__);
        CheckReal    (rf0.drv()(), (treal)0., "rfunction default ctr - drv - value", os, __LINE__);
        cfunction cf0;
        CheckString  (cf0.format(), "(0,0)", "cfunction default ctr", os, __LINE__);
        CheckString  (cf0.simp().format(), "(0,0)", "cfunction default ctr - simp()", os, __LINE__);
        CheckComplex (cf0(), tcomplex ((treal)0., (treal)0.), "cfunction default ctr - value", os, __LINE__);
        CheckComplex (cf0.value(NULL), tcomplex ((treal)0., (treal)0.), "cfunction default ctr - value", os, __LINE__);
        CheckComplex (cf0(), tcomplex ((treal)0., (treal)0.), "cfunction default ctr - value", os, __LINE__);
        CheckComplex (cf0(tcomplex ((treal)1.2, (treal)3.4)), tcomplex ((treal)0., (treal)0.), "cfunction default ctr - value", os, __LINE__);
        CheckComplex (cf0.drv()(), tcomplex ((treal)0., (treal)0.), "cfunction default ctr - drv - value", os, __LINE__);

        // Fconst
        rfunction rfc ("7.77 ");
        CheckString  (rfc.format().substr(0, 4), "7.77", "rfunction const - format()", os, __LINE__);
        CheckString  (rfc.simp().format().substr(0, 4), "7.77", "rfunction const - simp() - format()", os, __LINE__);
        CheckReal    (rfc(),     (treal)7.77, "rfunction const - value", os, __LINE__);
        CheckReal    (rfc.value(NULL), (treal)7.77, "rfunction const - value", os, __LINE__);
        CheckReal    (rfc(), (treal)7.77, "rfunction const - value", os, __LINE__);
        CheckReal    (rfc(1.1),  (treal)7.77, "rfunction const - value", os, __LINE__);
        CheckReal    (rfc.drv()(), (treal)0., "rfunction const - drv - value", os, __LINE__);

        cfunction cfc ("(7.77,8.88)");
        cfunction cfc2 ("5.55");
        CheckString  (cfc.format().substr(1, 4), "7.77", "cfunction const - format()", os, __LINE__);
        CheckString  (cfc.format().substr(cfc.format().find(",")+1, 4), "8.88", "cfunction const - format()", os, __LINE__);
        CheckString  (cfc.simp().format().substr(1, 4), "7.77", "cfunction const - simp() - format()", os, __LINE__);
        CheckString  (cfc.simp().format().substr(cfc.format().find(",")+1, 4), "8.88", "cfunction const - simp() - format()", os, __LINE__);
        CheckComplex (cfc(), tcomplex ((treal)7.77, (treal)8.88), "cfunction const - value", os, __LINE__);
        CheckComplex (cfc.value(NULL), tcomplex ((treal)7.77, (treal)8.88), "cfunction const - value", os, __LINE__);
        CheckComplex (cfc(), tcomplex ((treal)7.77, (treal)8.88), "cfunction const - value", os, __LINE__);
        CheckComplex (cfc(1.23), tcomplex ((treal)7.77, (treal)8.88), "cfunction const - value", os, __LINE__);
        CheckComplex (cfc.drv()(), tcomplex ((treal)0., (treal)0.), "cfunction const - drv - value", os, __LINE__);
        CheckString  (cfc2.format().substr(1, 4), "5.55", "cfunction const - format()", os, __LINE__);
        CheckString  (cfc2.format(2).substr(cfc2.format(2).find(",")+1, 4), "0.00", "cfunction const - format()", os, __LINE__);
        CheckString  (cfc2.simp().format().substr(1, 4), "5.55", "cfunction const - simp() - format()", os, __LINE__);
        CheckString  (cfc2.simp().format(2).substr(cfc2.format(2).find(",")+1, 4), "0.00", "cfunction const - simp() - format()", os, __LINE__);
        CheckComplex (cfc2(), tcomplex ((treal)5.55, (treal)0.), "cfunction const - value", os, __LINE__);
        CheckComplex (cfc2.value(NULL), tcomplex ((treal)5.55, (treal)0.), "cfunction const - value", os, __LINE__);
        CheckComplex (cfc2(), tcomplex ((treal)5.55, (treal)0.), "cfunction const - value", os, __LINE__);
        CheckComplex (cfc2(1.23), tcomplex ((treal)5.55, (treal)0.), "cfunction const - value", os, __LINE__);
        CheckComplex (cfc2.drv()(), tcomplex ((treal)0., (treal)0.), "cfunction const - drv - value", os, __LINE__);

        // Finfinity
        rfunction rfi("INF");
        CheckString  (rfi.format(), "(INF)", "rfunction inf - format()", os, __LINE__);
        CheckString  (rfi.simp().format(), "(INF)", "rfunction inf - simp() - format()", os, __LINE__);
        CheckBool    (rfi() > (treal)1.e38, true, "rfunction inf - value", os, __LINE__);
        CheckBool    (rfi.value(NULL) > (treal)1.e38, true, "rfunction inf - value", os, __LINE__);
        CheckBool    (rfi() > (treal)1.e38, true, "rfunction inf - value", os, __LINE__);
        CheckBool    (rfi(1.1) > (treal)1.e38, true, "rfunction inf - value", os, __LINE__);
        CheckString  (rfi.drv().format(), "(INF)", "rfunction inf - drv() - format()", os, __LINE__);

        rfunction rfmi("-INF");
        CheckString  (rfmi.format(), "-(INF)", "rfunction minus inf - format()", os, __LINE__);
        CheckString  (rfmi.simp().format(), "(-INF)", "rfunction minus inf - format()", os, __LINE__);
        CheckBool    (rfmi() < (treal)-1.e38, true, "rfunction minus inf - value", os, __LINE__);
        CheckBool    (rfmi.value(NULL) < (treal)-1.e38, true, "rfunction minus inf - value", os, __LINE__);
        CheckBool    (rfmi() < (treal)-1.e38, true, "rfunction minus inf - value", os, __LINE__);
        CheckBool    (rfmi(1.1) < (treal)-1.e38, true, "rfunction minus inf - value", os, __LINE__);

        // Fplus
        rfunction rfplus("{x,y} x+y");
        CheckString  (rfplus.format(), "x+y", "rfunction plus - format()", os, __LINE__);
        rfunction rfplus2("{x} x+x");
        CheckString  (rfplus2.simp().format(), "2*x", "rfunction plus - simp() - format()", os, __LINE__);
        CheckReal    (rfplus2((treal)7.36), (treal)14.72, "rfunction plus - value", os, __LINE__);
        {
            treal a[2];
            a[0] = (treal)1.35;
            a[1] = (treal)2.45;
            CheckReal    (rfplus.value(a), (treal)3.8, "rfunction plus - value", os, __LINE__);
            CheckReal    (rfplus(a), (treal)3.8, "rfunction plus - value", os, __LINE__);
        }
        CheckString  (rfplus2.drv().format(), "2", "rfunction plus - drv() - format()", os, __LINE__);
        CheckString  (rfplus.drv(0).format(), "1", "rfunction plus - drv() - format()", os, __LINE__);

        cfunction cfplus("{x,y} x+y");
        CheckString  (cfplus.format(), "x+y", "cfunction plus - format()", os, __LINE__);
        cfunction cfplus2("{x} x+x");
        CheckString  (cfplus2.simp().format(), "(2,0)*x", "cfunction plus - simp() - format()", os, __LINE__);
        CheckComplex (cfplus2(tcomplex ((treal)7.36, (treal)3.17)), tcomplex ((treal)14.72, (treal)6.34), "cfunction plus - value", os, __LINE__);
        {
            tcomplex a[2];
            a[0] = tcomplex ((treal)7.31, (treal)3.18);
            a[1] = tcomplex ((treal)2.44, (treal)3.12);
            CheckComplex (cfplus.value(a), tcomplex ((treal)9.75, (treal)6.3), "cfunction plus - value", os, __LINE__);
            CheckComplex (cfplus(a), tcomplex ((treal)9.75, (treal)6.3), "cfunction plus - value", os, __LINE__);
        }
        CheckString  (cfplus2.drv().format(), "(2,0)", "cfunction plus - drv() - format()", os, __LINE__);
        CheckString  (cfplus.drv(0).format(), "(1,0)", "cfunction plus - drv() - format()", os, __LINE__);

        // Fminus
        rfunction rfminus("{x,y} x-y");
        CheckString  (rfminus.format(), "x-y", "rfunction minus - format()", os, __LINE__);
        rfunction rfminus2("{x} x-x");
        CheckString  (rfminus2.simp().format(), "0", "rfunction minus - simp() - format()", os, __LINE__);
        CheckReal    (rfminus2((treal)7.36), (treal)0., "rfunction minus - value", os, __LINE__);
        {
            treal a[2];
            a[0] = (treal)1.45;
            a[1] = (treal)2.35;
            CheckReal    (rfminus.value(a), (treal)-0.9, "rfunction minus - value", os, __LINE__);
        }
        CheckString  (rfminus2.drv().format(), "0", "rfunction minus - drv() - format()", os, __LINE__);
        CheckString  (rfminus.drv(0).format(), "1", "rfunction minus - drv() - format()", os, __LINE__);

        cfunction cfminus("{x,y} x-y");
        CheckString  (cfminus.format(), "x-y", "cfunction minus - format()", os, __LINE__);
        cfunction cfminus2("{x} x-x");
        CheckString  (cfminus2.simp().format(), "(0,0)", "cfunction minus - simp() - format()", os, __LINE__);
        CheckComplex (cfminus2(tcomplex ((treal)7.36, (treal)3.17)), tcomplex ((treal)0., (treal)0.), "cfunction minus - value", os, __LINE__);
        {
            tcomplex a[2];
            a[0] = tcomplex ((treal)7.31, (treal)3.18);
            a[1] = tcomplex ((treal)2.44, (treal)3.12);
            CheckComplex (cfminus.value(a), tcomplex ((treal)4.87, (treal)0.06), "cfunction minus - value", os, __LINE__);
        }
        CheckString  (cfminus2.drv().format(), "(0,0)", "cfunction minus - drv() - format()", os, __LINE__);
        CheckString  (cfminus.drv(0).format(), "(1,0)", "cfunction minus - drv() - format()", os, __LINE__);

        // Fmult
        rfunction rfmult("{x,y} x*y");
        CheckString  (rfmult.format(), "x*y", "rfunction mult - format()", os, __LINE__);
        rfunction rfmult2("{x} x*x");
        CheckString  (rfmult2.simp().format(), "x^2", "rfunction mult - simp() - format()", os, __LINE__);
        CheckReal    (rfmult2((treal)7.36), (treal)7.36*7.36, "rfunction mult - value", os, __LINE__);
        {
            treal a[2];
            a[0] = (treal)1.45;
            a[1] = (treal)2.35;
            CheckReal    (rfmult.value(a), (treal)1.45*2.35, "rfunction mult - value", os, __LINE__);
        }
        CheckString  (rfmult2.drv().format(), "2*x", "rfunction mult - drv() - format()", os, __LINE__);
        CheckString  (rfmult.drv(0).format(), "y", "rfunction mult - drv() - format()", os, __LINE__);

        cfunction cfmult("{x,y} x*y");
        CheckString  (cfmult.format(), "x*y", "cfunction mult - format()", os, __LINE__);
        cfunction cfmult2("{x} x*x");
        CheckString  (cfmult2.simp().format(), "x^(2,0)", "cfunction mult - simp() - format()", os, __LINE__);
        CheckComplex (cfmult2(tcomplex ((treal)7.36, (treal)3.17)),
                      tcomplex ((treal)7.36, (treal)3.17) * tcomplex ((treal)7.36, (treal)3.17),
                      "cfunction mult - value", os, __LINE__, dPessimisticSp);
        {
            tcomplex a[2];
            a[0] = tcomplex ((treal)7.31, (treal)3.18);
            a[1] = tcomplex ((treal)2.44, (treal)3.12);
            CheckComplex (cfmult.value(a), tcomplex ((treal)7.31, (treal)3.18) *
                          tcomplex ((treal)2.44, (treal)3.12), "cfunction mult - value", os, __LINE__, dPessimisticSp);
        }
        CheckString  (cfmult2.drv().format(), "(2,0)*x", "cfunction mult - drv() - format()", os, __LINE__);
        CheckString  (cfmult.drv(0).format(), "y", "cfunction mult - drv() - format()", os, __LINE__);

        // Fdiv
        rfunction rfdiv("{x,y} x/y");
        CheckString  (rfdiv.format(), "x/y", "rfunction div - format()", os, __LINE__);
        rfunction rfdiv2("{x} x/x");
        CheckString  (rfdiv2.simp().format(), "1", "rfunction div - simp() - format()", os, __LINE__);
        CheckReal    (rfdiv2((treal)7.36), (treal)1., "rfunction div - value", os, __LINE__);
        {
            treal a[2];
            a[0] = (treal)1.45;
            a[1] = (treal)2.35;
            CheckReal    (rfdiv.value(a), (treal)1.45/2.35, "rfunction div - value", os, __LINE__);
        }
        CheckString  (rfdiv2.drv().format(), "0", "rfunction div - drv() - format()", os, __LINE__);
        CheckString  (rfdiv.drv(0).format(), "1/y", "rfunction div - drv() - format()", os, __LINE__);

        cfunction cfdiv("{x,y} x/y");
        CheckString  (cfdiv.format(), "x/y", "cfunction div - format()", os, __LINE__);
        cfunction cfdiv2("{x} x/x");
        CheckString  (cfdiv2.simp().format(), "(1,0)", "cfunction div - simp() - format()", os, __LINE__);
        CheckComplex (cfdiv2(tcomplex ((treal)7.36, (treal)3.17)), tcomplex ((treal)1., (treal)0.), "cfunction div - value", os, __LINE__);
        {
            tcomplex a[2];
            a[0] = tcomplex ((treal)7.31, (treal)3.18);
            a[1] = tcomplex ((treal)2.44, (treal)3.12);
            CheckComplex (cfdiv.value(a), tcomplex ((treal)7.31, (treal)3.18) /
                          tcomplex ((treal)2.44, (treal)3.12), "cfunction div - value", os, __LINE__, dPessimisticSp);
        }
        CheckString  (cfdiv2.drv().format(), "(0,0)", "cfunction div - drv() - format()", os, __LINE__);
        CheckString  (cfdiv.drv(0).format(), "(1,0)/y", "cfunction div - drv() - format()", os, __LINE__);


        // Fpower
        rfunction rfpow("{x,y} x^y");
        CheckString  (rfpow.format(), "x^y", "rfunction power - format()", os, __LINE__);
        rfunction rfpow2("{x} (x^3)^2");
        CheckString  (rfpow2.simp().format(), "x^6", "rfunction power - simp() - format()", os, __LINE__);
        CheckReal    (rfpow2((treal)1.123), (treal)::pow(1.123,6.), "rfunction power - value", os, __LINE__);
        {
            treal a[2];
            a[0] = (treal)1.45;
            a[1] = (treal)2.35;
            CheckReal    (rfpow.value(a), (treal)::pow(a[0],a[1]), "rfunction power - value", os, __LINE__);
        }
        CheckString  (rfpow.drv().format(), "y*x^(y-1)", "rfunction power - drv() - format()", os, __LINE__);
        CheckString  (rfpow.drv(1).format(), "x^y*log(x)", "rfunction power - drv() - format()", os, __LINE__);
        CheckString  (rfpow2.drv().format(), "6*x^5", "rfunction power - drv() - format()", os, __LINE__);

        cfunction cfpow("{x,y} x^y");
        CheckString  (cfpow.format(), "x^y", "cfunction power - format()", os, __LINE__);
        cfunction cfpow2("{x} (x^(3,1))^(1,2)");

        CheckString  (cfpow2.simp().format(), "x^(1,7)", "cfunction power - simp() - format()", os, __LINE__);
        CheckComplex (cfpow2(tcomplex ((treal)7.36, (treal)3.17)),
                      ElementaryFunctions<tcomplex>::pow(tcomplex ((treal)7.36, (treal)3.17), tcomplex ((treal)1., (treal)7.)),
                      "cfunction power - value", os, __LINE__);
        {
            tcomplex a[2];
            a[0] = tcomplex ((treal)7.31, (treal)3.18);
            a[1] = tcomplex ((treal)2.44, (treal)3.12);
            CheckComplex (cfpow.value(a), ElementaryFunctions<tcomplex>::pow(tcomplex ((treal)7.31, (treal)3.18),
                                                                               tcomplex ((treal)2.44, (treal)3.12)), "cfunction power - value", os, __LINE__, dPessimisticSp);
        }
        CheckString  (cfpow.drv().format(), "y*x^(y-(1,-0))", "y*x^(y-(1,0))", "cfunction power - drv() - format()", os, __LINE__);
        CheckString  (cfpow.drv(1).format(), "x^y*log(x)", "cfunction power - drv() - format()", os, __LINE__);
        CheckString  (cfpow2.drv().format(), "(1,7)*x^(0,7)", "cfunction power - drv() - format()", os, __LINE__);

        // Fsat
        rfunction rfsat("{x,y} sat(x,y)");
        CheckString  (rfsat.format(), "sat(x,y)", "rfunction sat - format()", os, __LINE__);
        rfunction rfsat2("{x} sat(x,2+1)");
        CheckString  (rfsat2.simp().format(), "sat(x,3)", "rfunction sat - simp() - format()", os, __LINE__);
        CheckReal    (rfsat2((treal)-3.0001), (treal)-1., "rfunction sat - value", os, __LINE__);
        CheckReal    (rfsat2((treal)-3.), (treal)0., "rfunction sat - value", os, __LINE__);
        CheckReal    (rfsat2((treal)3.), (treal)0., "rfunction sat - value", os, __LINE__);
        CheckReal    (rfsat2((treal)3.00001), (treal)1., "rfunction sat - value", os, __LINE__);
        {
            treal a[2];
            a[0] = (treal)1.45;
            a[1] = (treal)2.35;
            CheckReal    (rfsat.value(a), (treal)0., "rfunction sat - value", os, __LINE__);
        }
        CheckString  (rfsat2.drv().format(), "delta(x,3)+delta(x,(-3))", "rfunction sat - drv() - format()", os, __LINE__);

        cfunction cfsat("{x,y} sat(x,y)");
        CheckString  (cfsat.format(), "sat(x,y)", "cfunction sat - format()", os, __LINE__);
        cfunction cfsat2("{x} sat(x,(3,1)+(1,2))");
        CheckString  (cfsat2.simp().format(), "sat(x,(4,3))", "cfunction sat - simp() - format()", os, __LINE__);
        {
            tcomplex a[2];
            a[0] = tcomplex ((treal)7.31, (treal)3.18);
            a[1] = tcomplex ((treal)2.44, (treal)3.12);
            CheckComplex (cfsat.value(a), tcomplex ((treal)1., (treal)0.), "cfunction sat - value", os, __LINE__);
        }
        CheckString  (cfsat2.drv().format(), "delta(x,(4,3))+delta(x,(-4,-3))", "cfunction sat - drv() - format()", os, __LINE__);

        // Fexp
        rfunction rfexp("{x} exp(x)");
        CheckString  (rfexp.format(), "exp(x)", "rfunction exp - format()", os, __LINE__);
        CheckReal    (rfexp((treal)1.), (treal)CFUN_M_E, "rfunction exp - value", os, __LINE__);
        CheckReal    (rfexp((treal)2.), (treal)CFUN_M_E * CFUN_M_E, "rfunction exp - value", os, __LINE__);
        CheckString  (rfexp.drv().format(), "exp(x)", "rfunction exp - drv() - format()", os, __LINE__);
        rfunction rfexp2("exp (0)");
        CheckString  (rfexp2.simp().format(), "1", "rfunction exp - format()", os, __LINE__);

        cfunction cfexp("{x} exp(x)");
        CheckString  (cfexp.format(), "exp(x)", "cfunction exp - format()", os, __LINE__);
        CheckComplex (cfexp(tcomplex ((treal)1., (treal)0.)), tcomplex ((treal)CFUN_M_E, (treal)0.), "cfunction exp - value", os, __LINE__);
        CheckString  (cfexp.drv().format(), "exp(x)", "cfunction exp - drv() - format()", os, __LINE__);

        // Fsqrt
        rfunction rfsqrt("{x} sqrt(x)");
        CheckString  (rfsqrt.format(), "sqrt(x)", "rfunction sqrt - format()", os, __LINE__);
        CheckReal    (rfsqrt((treal)2.), (treal)::sqrt((treal)2.), "rfunction sqrt - value", os, __LINE__);
        CheckString  (rfsqrt.drv().format(), "0.5/sqrt(x)", "rfunction sqrt - drv() - format()", os, __LINE__);
        rfunction rfsqrt2("sqrt(4)");
        CheckString  (rfsqrt2.simp().format(), "2", "rfunction sqrt - simp - format()", os, __LINE__);

        cfunction cfsqrt("{x} sqrt(x)");
        CheckString  (cfsqrt.format(), "sqrt(x)", "cfunction sqrt - format()", os, __LINE__);
        CheckComplex (cfsqrt(tcomplex ((treal)-1., (treal)0.)), tcomplex ((treal)0., (treal)1.), "cfunction sqrt - value", os, __LINE__);
        CheckString  (cfsqrt.drv().format(), "(0.5,0)/sqrt(x)", "cfunction sqrt - drv() - format()", os, __LINE__);
        cfunction cfsqrt2("{x} sqrt(4,0)");
        CheckString  (cfsqrt2.simp().format(), "(2,0)", "cfunction sqrt - simp() - format()", os, __LINE__);

        // Flog
        rfunction rflog("{x} log(x)");
        CheckString  (rflog.format(), "log(x)", "rfunction log - format()", os, __LINE__);
        CheckReal    (rflog((treal)2.), (treal)::log((treal)2.), "rfunction log - value", os, __LINE__);
        CheckString  (rflog.drv().format(), "1/x", "rfunction log - drv() - format()", os, __LINE__);
        rfunction rflog2("{x} log(x^2)");
        CheckString  (rflog2.simp().format(), "log(x)*2", "rfunction log - simp - format()", os, __LINE__);

        cfunction cflog("{x} log(x)");
        CheckString  (cflog.format(), "log(x)", "cfunction log - format()", os, __LINE__);
        {
            std::ostringstream oss;
            oss.precision(15);
            oss << cflog(tcomplex ((treal)-1., (treal)0.));
#ifdef CVM_FLOAT
            CheckString(oss.str().substr(0, 10), "(0,3.14159", "cfunction log - value()", os, __LINE__);
#else
            CheckString(oss.str().substr(0,17), "(0,3.141592653589", "cfunction log - value()", os, __LINE__);
#endif
        }
        cfunction cflog2("{x} log(sqrt(x))");
        CheckString (cflog2.simp().format(), "log(x)*(0.5,0)", "cfunction log - simp()", os, __LINE__);

        // Flog10
        rfunction rflog10("{x} log10(x)");
        CheckString  (rflog10.format(), "log10(x)", "rfunction log10 - format()", os, __LINE__);
        CheckReal    (rflog10((treal)2.), (treal)::log10((treal)2.), "rfunction log10 - value", os, __LINE__);
        CheckString  (rflog10.drv().format(), "0.434294/x", "rfunction log10 - drv - format()", os, __LINE__);
        rfunction rflog10_2("{x} log10(x^2)");
        CheckString  (rflog10_2.simp().format(), "log10(x)*2", "rfunction log10 - simp - format()", os, __LINE__);

        cfunction cflog10("{x} log10(x)");
        CheckString  (cflog10.format(), "log10(x)", "cfunction log10 - format()", os, __LINE__);
        {
            std::ostringstream oss;
            oss.precision(15);
            oss << cflog10(tcomplex ((treal)-1., (treal)0.));
//            CheckString(oss.str().substr(0, 17), "(0,1.364376353841", "cfunction log10 - value()", os, __LINE__);
            CheckString(oss.str().substr(0, 10), "(0,1.36437", "cfunction log10 - value()", os, __LINE__);
        }
        cfunction cflog10_2("{x} log10(sqrt(x))");
        CheckString (cflog10_2.simp().format(), "log10(x)*(0.5,0)", "cfunction log10 - simp()", os, __LINE__);

        // Fsin
        rfunction rfsin("{x} sin(x)");
        CheckString  (rfsin.format(), "sin(x)", "rfunction sin - format()", os, __LINE__);
        CheckReal    (rfsin((treal)2.), (treal)::sin((treal)2.), "rfunction sin - value", os, __LINE__);
        CheckString  (rfsin.drv().format(), "cos(x)", "rfunction sin - drv - format()", os, __LINE__);
        rfunction rfsin_2("sin(1)");
        CheckString  (rfsin_2.simp().format(), "0.841471", "rfunction sin - simp - format()", os, __LINE__);

        cfunction cfsin("{x} sin(x)");
        CheckString  (cfsin.format(), "sin(x)", "cfunction sin - format()", os, __LINE__);
        {
            std::ostringstream oss;
            oss.precision(15);
            oss << cfsin(tcomplex ((treal)0., (treal)1.));
//            CheckString(oss.str().substr(0, 16), "(0,1.17520119364", "cfunction sin - value()", os, __LINE__);
            CheckString(oss.str().substr(0, 10), "(0,1.17520", "cfunction sin - value()", os, __LINE__);
        }
        cfunction cfsin_2("{x} sin(0,0)");
        CheckString (cfsin_2.simp().format(), "(0,0)", "cfunction sin - simp()", os, __LINE__);

        // Fcos
        rfunction rfcos("{x} cos(x)");
        CheckString  (rfcos.format(), "cos(x)", "rfunction cos - format()", os, __LINE__);
        CheckReal    (rfcos((treal)2.), (treal)::cos((treal)2.), "rfunction cos - value", os, __LINE__);
        CheckString  (rfcos.drv().format(), "-sin(x)", "rfunction cos - drv - format()", os, __LINE__);
        rfunction rfcos_2("cos(1)");
        CheckString  (rfcos_2.simp().format(), "0.540302", "rfunction cos - simp - format()", os, __LINE__);

        cfunction cfcos("{x} cos(x)");
        CheckString  (cfcos.format(), "cos(x)", "cfunction cos - format()", os, __LINE__);
        {
            std::ostringstream oss;
            oss.precision(15);
            oss << cfcos(tcomplex ((treal)-1., (treal)1.));
#ifdef CVM_FLOAT
            CheckString(oss.str().substr(0,9), "(0.833729", "cfunction cos - value()", os, __LINE__);
            CheckString (oss.str().substr(18,9), ",0.988897", "cfunction cos - value()", os, __LINE__);
#else
            CheckString(oss.str().substr(0, 16), "(0.8337300251311", "cfunction cos - value()", os, __LINE__);
            CheckString(oss.str().substr(18, 16), ",0.9888977057628", "cfunction cos - value()", os, __LINE__);
#endif
        }
        cfunction cfcos_2("{x} cos(1,-1)");
        CheckString (cfcos_2.simp().format(), "(0.83373,0.988898)", "cfunction cos - simp()", os, __LINE__);


#if !defined(__MINGW32__)



        // Ftan
        rfunction rftan("{x} tan(x)");
        CheckString  (rftan.format(), "tan(x)", "rfunction tan - format()", os, __LINE__);
        CheckReal    (rftan((treal)2.), (treal)::tan((treal)2.), "rfunction tan - value", os, __LINE__);
        CheckString  (rftan.drv().format(), "1/cos(x)^2", "rfunction tan - drv - format()", os, __LINE__);
        rfunction rftan_2("tan(1)");
        CheckString  (rftan_2.simp().format(), "1.55741", "rfunction tan - simp - format()", os, __LINE__);

        cfunction cftan("{x} tan(x)");
        CheckString  (cftan.format(), "tan(x)", "cfunction tan - format()", os, __LINE__);
        {
            std::ostringstream oss;
            oss.precision(15);
            oss << cftan(tcomplex ((treal)-1., (treal)1.));
#ifdef CVM_FLOAT
            CheckString(oss.str().substr(0, 9), "(-0.27175", "cfunction tan - value()", os, __LINE__);
            CheckString (oss.str().substr(19,9), ",1.083923", "cfunction tan - value()", os, __LINE__);
#else
            CheckString(oss.str().substr(0, 16), "(-0.271752585319", "cfunction tan - value()", os, __LINE__);
            CheckString (oss.str().substr(19,16), ",1.0839233273386", "cfunction tan - value()", os, __LINE__);
#endif
        }
        cfunction cftan_2("{x} tan(0,0)");
        CheckString (cftan_2.simp().format(), "(0,0)", "cfunction tan - simp()", os, __LINE__);

        // Fasin
        rfunction rfasin("{x} asin(x)");
        CheckString  (rfasin.format(), "asin(x)", "rfunction asin - format()", os, __LINE__);
        CheckReal    (rfasin((treal)0.5), (treal)::asin((treal)0.5), "rfunction asin - value", os, __LINE__);
        CheckString  (rfasin.drv().format(), "1/sqrt(1-x^2)", "rfunction asin - drv - format()", os, __LINE__);
        rfunction rfasin_2("asin(1)");
        CheckString  (rfasin_2.simp().format(), "1.5708", "rfunction asin - simp - format()", os, __LINE__);

        cfunction cfasin("{x} asin(x)");
        CheckString  (cfasin.format(), "asin(x)", "cfunction asin - format()", os, __LINE__);
        {
            std::ostringstream oss;
            oss.precision(15);
            oss << cfasin(tcomplex ((treal)-1., (treal)1.));
#ifdef CVM_FLOAT
            CheckString(oss.str().substr(0, 9), "(-0.66623", "cfunction asin - value()", os, __LINE__);
            CheckString (oss.str().substr(19,9), ",1.061275", "cfunction asin - value()", os, __LINE__);
#else
            CheckString(oss.str().substr(0, 16), "(-0.666239432492", "cfunction asin - value()", os, __LINE__);
            CheckString (oss.str().substr(19,16), ",1.0612750619050", "cfunction asin - value()", os, __LINE__);
#endif
        }
        cfunction cfasin_2("{x} asin(0,0)");
        CheckString (cfasin_2.simp().format(), "(0,0)", "cfunction asin - simp()", os, __LINE__);

        // Facos
        rfunction rfacos("{x} acos(x)");
        CheckString  (rfacos.format(), "acos(x)", "rfunction acos - format()", os, __LINE__);
        CheckReal    (rfacos((treal)0.5), (treal)::acos((treal)0.5), "rfunction acos - value", os, __LINE__);
        CheckString  (rfacos.drv().format(), "(-1)/sqrt(1-x^2)", "rfunction acos - drv - format()", os, __LINE__);
        rfunction rfacos_2("acos(1)");
        CheckString  (rfacos_2.simp().format(), "0", "rfunction acos - simp - format()", os, __LINE__);

        cfunction cfacos("{x} acos(x)");
        CheckString  (cfacos.format(), "acos(x)", "cfunction acos - format()", os, __LINE__);
        {
            std::ostringstream oss;
            oss.precision(15);
            oss << cfacos(tcomplex ((treal)-1., (treal)1.));
#ifdef CVM_FLOAT
            CheckString(oss.str().substr(0, 9), "(2.237035", "cfunction acos - value()", os, __LINE__);
            CheckString (oss.str().substr(18,9), "-1.061275", "cfunction acos - value()", os, __LINE__);
#else
            CheckString(oss.str().substr(0, 16), "(2.2370357592874", "cfunction acos - value()", os, __LINE__);
            CheckString(oss.str().substr(18, 16), "-1.0612750619050", "cfunction acos - value()", os, __LINE__);
#endif
        }
        cfunction cfacos_2("{x} acos(0,0)");
        CheckString (cfacos_2.simp().format(), "(1.5708,0)", "cfunction acos - simp()", os, __LINE__);

        // Fatan
        rfunction rfatan("{x} atan(x)");
        CheckString  (rfatan.format(), "atan(x)", "rfunction atan - format()", os, __LINE__);
        CheckReal    (rfatan((treal)0.5), (treal)::atan((treal)0.5), "rfunction atan - value", os, __LINE__);
        CheckString  (rfatan.drv().format(), "1/(1+x^2)", "rfunction atan - drv - format()", os, __LINE__);
        rfunction rfatan_2("atan(1)");
        CheckString  (rfatan_2.simp().format(), "0.785398", "rfunction atan - simp - format()", os, __LINE__);

        cfunction cfatan("{x} atan(x)");
        CheckString  (cfatan.format(), "atan(x)", "cfunction atan - format()", os, __LINE__);
        {
            std::ostringstream oss;
            oss.precision(15);
            oss << cfatan(tcomplex ((treal)-1., (treal)1.));
#ifdef CVM_FLOAT
            CheckString(oss.str().substr(0, 9), "(-1.01722", "cfunction acos - value()", os, __LINE__);
            CheckString(oss.str().substr(18, 9), ",0.402359", "cfunction acos - value()", os, __LINE__);
#else
            CheckString(oss.str().substr(0, 16), "(-1.017221967897", "cfunction acos - value()", os, __LINE__);
            CheckString (oss.str().substr(18,16), ",0.4023594781085", "cfunction acos - value()", os, __LINE__);
#endif
        }
        cfunction cfatan_2("{x} atan(0,0)");
        CheckString (cfatan_2.simp().format(), "(0,0)", "cfunction atan - simp()", os, __LINE__);

        // Fsinh
        rfunction rfsinh("{x} sinh(x)");
        CheckString  (rfsinh.format(), "sinh(x)", "rfunction sinh - format()", os, __LINE__);
        CheckReal    (rfsinh((treal)1.), (treal)1.1752011936438014, "rfunction sinh - value", os, __LINE__);
        CheckString  (rfsinh.drv().format(), "cosh(x)", "rfunction sinh - drv - format()", os, __LINE__);
        rfunction rfsinh2("sinh(-1.)");
        CheckString  (rfsinh2.simp().format(), "(-1.1752)", "rfunction sinh - simp - format()", os, __LINE__);

        cfunction cfsinh("{x} sinh(x)");
        CheckString  (cfsinh.format(), "sinh(x)", "cfunction sinh - format()", os, __LINE__);
        CheckComplex (cfsinh(tcomplex ((treal)-1., (treal)1.)),
#ifdef CVM_FLOAT
            tcomplex((treal)-0.63496381, (treal)1.2984574), "cfunction sinh - value", os, __LINE__, dPessimisticSp);
#else
            tcomplex((treal)-0.63496391478473613, (treal)1.2984575814159773), "cfunction sinh - value", os, __LINE__);
#endif
        cfunction cfsinh2("sinh(0,0)");
        CheckString  (cfsinh2.simp().format(), "(0,0)", "cfunction sinh - simp - format()", os, __LINE__);

        // Fcosh
        rfunction rfcosh("{x} cosh(x)");
        CheckString  (rfcosh.format(), "cosh(x)", "rfunction cosh - format()", os, __LINE__);
        CheckReal    (rfcosh((treal)1.), (treal)1.5430806348152437, "rfunction cosh - value", os, __LINE__);
        CheckString  (rfcosh.drv().format(), "-sinh(x)", "rfunction cosh - drv - format()", os, __LINE__);
        rfunction rfcosh2("cosh(-1.)");
        CheckString  (rfcosh2.simp().format(), "1.54308", "rfunction sinh - simp - format()", os, __LINE__);

        cfunction cfcosh("{x} cosh(x)");
        CheckString  (cfcosh.format(), "cosh(x)", "cfunction cosh - format()", os, __LINE__);
        CheckComplex (cfcosh(tcomplex ((treal)-1., (treal)1.)),
#ifdef CVM_FLOAT
            tcomplex((treal)0.83372986, (treal)-0.98889756),"cfunction cosh - value", os, __LINE__, dPessimisticSp);
#else
            tcomplex ((treal)0.83373002513114913, (treal)-0.98889770576286506),"cfunction cosh - value", os, __LINE__);
#endif

        cfunction cfcosh2("cosh(0,0)");
        CheckString  (cfcosh2.simp().format(), "(1,0)", "cfunction cosh - simp - format()", os, __LINE__);

        // Ftanh
        rfunction rftanh("{x} tanh(x)");
        CheckString  (rftanh.format(), "tanh(x)", "rfunction tanh - format()", os, __LINE__);
        CheckReal    (rftanh((treal)-1.), ::tanh((treal)-1.), "rfunction tanh - value", os, __LINE__);
        CheckString  (rftanh.drv().format(), "1/cosh(x)^2", "rfunction tanh - drv - format()", os, __LINE__);
        rfunction rftanh2("{x} tanh(x^2)");
        CheckString  (rftanh2.drv().format(), "2*x/cosh(x^2)^2", "rfunction tanh - drv - format()", os, __LINE__);

        cfunction cftanh("{x} tanh(x)");
        CheckString  (cftanh.format(), "tanh(x)", "cfunction tanh - format()", os, __LINE__);
        CheckComplex (cftanh(tcomplex ((treal)-1., (treal)1.)),
                      std::tanh(tcomplex ((treal)-1., (treal)1.)), "cfunction tanh - value", os, __LINE__);
        cfunction cftanh2("tanh(0,0)");
        CheckString  (cftanh2.simp().format(), "(0,0)", "cfunction tanh - simp - format()", os, __LINE__);

        // Fuminus
        rfunction rfuminus("{x} -x");
        CheckString  (rfuminus.format(), "-x", "rfunction unary minus - format()", os, __LINE__);
        CheckReal    (rfuminus((treal)0.5), (treal)-0.5, "rfunction unary minus - value", os, __LINE__);
        CheckString  (rfuminus.drv().format(), "(-1)", "rfunction unary minus - drv - format()", os, __LINE__);
        rfunction rfuminus_2("{x} -(-x)");
        CheckString  (rfuminus_2.simp().format(), "x", "rfunction unary minus - simp - format()", os, __LINE__);

        cfunction cfuminus("{x} -x");
        CheckString  (cfuminus.format(), "-x", "cfunction unary minus - format()", os, __LINE__);
        {
            std::ostringstream oss;
            oss.precision(15);
            oss << cfuminus(tcomplex ((treal)-1., (treal)1.));
            CheckString (oss.str(), "(1,-1)", "cfunction unary minus - value()", os, __LINE__);
        }
        cfunction cfuminus_2("{x} -(1,2)");
        CheckString (cfuminus_2.simp().format(), "(-1,-2)", "cfunction unary minus - simp()", os, __LINE__);

        // Fsign
        rfunction rfsign("{x} sign(x)");
        CheckString  (rfsign.format(), "sign(x)", "rfunction sign - format()", os, __LINE__);
        CheckReal    (rfsign((treal)0.5), (treal)1., "rfunction sign - value", os, __LINE__);
        CheckReal    (rfsign((treal)0.), (treal)0., "rfunction sign - value", os, __LINE__);
        CheckReal    (rfsign((treal)-0.01), (treal)-1., "rfunction sign - value", os, __LINE__);
        CheckString  (rfsign.drv().format(), "delta(x,0)", "rfunction sign - drv - format()", os, __LINE__);

        cfunction cfsign("{x} sign(x)");
        CheckString  (cfsign.format(), "sign(x)", "cfunction sign - format()", os, __LINE__);
        CheckComplex (cfsign(tcomplex ((treal)-1., (treal)1.)), tcomplex ((treal)-1., (treal)0.), "cfunction sign - value", os, __LINE__);
        CheckComplex (cfsign(tcomplex ((treal)0., (treal)1.)), tcomplex ((treal)0., (treal)0.), "cfunction sign - value", os, __LINE__);
        CheckComplex (cfsign(tcomplex ((treal)0.5, (treal)-1.)), tcomplex ((treal)1., (treal)0.), "cfunction sign - value", os, __LINE__);
        CheckString  (cfsign.drv().format(), "delta(x,(0,0))", "cfunction sign - drv - format()", os, __LINE__);


        // Fabs
        rfunction rfabs("{x} abs(x)");
        CheckString  (rfabs.format(), "abs(x)", "rfunction abs - format()", os, __LINE__);
        CheckReal    (rfabs((treal)-0.5), (treal)0.5, "rfunction abs - value", os, __LINE__);
        CheckString  (rfabs.drv().format(), "sign(x)", "rfunction abs - drv - format()", os, __LINE__);

        cfunction cfabs("{x} abs(x)");
        CheckString  (cfabs.format(), "abs(x)", "cfunction abs - format()", os, __LINE__);
        CheckComplex (cfabs(tcomplex ((treal)-1., (treal)1.)),
                      tcomplex ((treal)sqrt(2.), (treal)0.), "cfunction abs - value", os, __LINE__, dPessimisticSp);
        CheckString  (cfabs.drv().format(), "sign(x)", "cfunction abs - drv - format()", os, __LINE__);


        // Fdelta
        rfunction rfdelta("{x} delta(x,1.)");
        CheckString  (rfdelta.format(), "delta(x,1)", "rfunction delta - format()", os, __LINE__);
        CheckReal    (rfdelta((treal)-0.5), (treal)0., "rfunction delta - value", os, __LINE__);
        CheckReal    (rfdelta((treal)1.), (std::numeric_limits<treal>::max)(), "rfunction delta - value", os, __LINE__);
        CheckString  (rfdelta.drv().format(), "delta(x,1)", "rfunction delta - drv - format()", os, __LINE__);

        cfunction cfdelta("{x} delta(x,1)");
        CheckString  (cfdelta.format(), "delta(x,(1,0))", "cfunction delta - format()", os, __LINE__);
        CheckComplex (cfdelta(tcomplex ((treal)-1., (treal)1.)), tcomplex ((treal)sqrt(0.), (treal)0.),
                      "cfunction delta - value", os, __LINE__, dPessimisticSp);
        CheckString  (cfdelta.drv().format(), "delta(x,(1,0))", "cfunction delta - drv - format()", os, __LINE__);

        // Fiif
        rfunction rfiif("{x} iif(x+1,1.,2.)");
        CheckString  (rfiif.format(), "iif(x+1,1,2)", "rfunction iif - format()", os, __LINE__);
        CheckReal    (rfiif((treal)-1.5), (treal)1., "rfunction iif - value", os, __LINE__);
        CheckReal    (rfiif((treal)-1.), (treal)2., "rfunction iif - value", os, __LINE__);
        CheckString  (rfiif.drv().format(), "iif(x+1,0,0)", "rfunction iif - drv - format()", os, __LINE__);

        cfunction cfiif("{x} iif(x+(1.,1.),(1.,1.),(2.,2.))");
        CheckString  (cfiif.format(), "iif(x+(1,1),(1,1),(2,2))", "cfunction iif - format()", os, __LINE__);
        CheckComplex (cfiif(tcomplex ((treal)-1., (treal)1.)), tcomplex ((treal)2., (treal)2.),
                      "cfunction iif - value", os, __LINE__, dPessimisticSp);
        CheckString  (cfiif.drv().format(), "iif(x+(1,1),(0,0),(0,0))", "cfunction iif - drv - format()", os, __LINE__);

        // Fsinint
        rfunction rfSi("{x} sinint(x)");
        CheckString  (rfSi.format(), "sinint(x)", "rfunction sinint - format()", os, __LINE__);
        CheckReal    (rfSi((treal)0.5), (treal)0.493107418043067, "rfunction sinint - value", os, __LINE__, dPessimisticSp);
        CheckString  (rfSi.drv().format(), "sin(x)/x", "rfunction sinint - drv - format()", os, __LINE__);

        cfunction cfSi("{x} sinint(x)");
        CheckString  (cfSi.format(), "sinint(x)", "cfunction sinint - format()", os, __LINE__);
        CheckComplex (cfSi(tcomplex ((treal)0., (treal)0.)),
                      tcomplex ((treal)0., (treal)0.), "cfunction sinint - value", os, __LINE__, dPessimisticSp);
        CheckComplex (cfSi(tcomplex ((treal)1., (treal)1.)),
                      tcomplex ((treal)1.10422265823558, (treal)0.882453805007918),
                      "cfunction sinint - value", os, __LINE__, dPessimisticSp);
        CheckComplex (cfSi(tcomplex ((treal)-1., (treal)0.)),
                      tcomplex ((treal)-0.946083070367183, (treal)0.), "cfunction sinint - value", os, __LINE__, dPessimisticSp);
        CheckString  (cfSi.drv().format(), "sin(x)/x", "cfunction sinint - drv - format()", os, __LINE__);

        // Fcosint
        rfunction rfCi("{x} cosint(x)");
        CheckString  (rfCi.format(), "cosint(x)", "rfunction cosint - format()", os, __LINE__);
        CheckReal    (rfCi((treal)1.e-5), (treal)-10.9357098000937, "rfunction cosint - value", os, __LINE__, dPessimisticSp);
        CheckReal    (rfCi((treal)2.), (treal)0.422980828774865, "rfunction cosint - value", os, __LINE__, dPessimisticSp);
        CheckString  (rfCi.drv().format(), "cos(x)/x", "rfunction cosint - drv - format()", os, __LINE__);

        cfunction cfCi("{x} cosint(x)");
        CheckString  (cfCi.format(), "cosint(x)", "cfunction cosint - format()", os, __LINE__);
        CheckComplex (cfCi(tcomplex ((treal)-1., (treal)0.)),
                      tcomplex ((treal)0.337403922900968, (treal)3.14159265358979),
                      "cfunction cosint - value", os, __LINE__, dPessimisticSp);
        CheckComplex (cfCi(tcomplex ((treal)1., (treal)1.)),
                      tcomplex ((treal)0.882172180555936, (treal)0.287249133519956),
                      "cfunction cosint - value", os, __LINE__, dPessimisticSp);
        CheckString  (cfCi.drv().format(), "cos(x)/x", "cfunction cosint - drv - format()", os, __LINE__);


        rfunction f1 ("{t} sin(t)^2 + cos(t)^2");
        CheckString  ((f1 * rf0).simp().format(), "0", "rfunction * rfunction", os, __LINE__);
        CheckBool    ((f1 * rf0).simp() == rfunction("{t} 0"), true, "rfunction * rfunction", os, __LINE__);

        rfunction rf_plus = rfsin + rfcos;
        CheckString  (rf_plus.format(), "sin(x)+cos(x)", "rf_plus.format", os, __LINE__);
        CheckBool    (rf_plus == rfunction("{x} sin(x)+cos(x)"), true, "rf_plus.format", os, __LINE__);

        rfunction rf_minus = rfsin - rfcos;
        CheckString  (rf_minus.format(), "sin(x)-cos(x)", "rf_minus.format", os, __LINE__);
        CheckBool    (rf_minus == rfunction("{x} sin(x)-cos(x)"), true, "rf_minus.format", os, __LINE__);

        rfunction rf_mult = rfsin * rfcos;
        CheckString  (rf_mult.format(), "sin(x)*cos(x)", "rf_mult.format", os, __LINE__);
        CheckBool    (rf_mult == rfunction("{x} sin(x)*cos(x)"), true, "rf_mult.format", os, __LINE__);

        rfunction rf_div = rfsin / rfcos;
        CheckString  (rf_div.format(), "sin(x)/cos(x)", "rf_div.format", os, __LINE__);
        CheckBool    (rf_div == rfunction("{x} sin(x)/cos(x)"), true, "rf_div.format", os, __LINE__);

        rfunction rf_power = rfsin ^ rfcos;
        CheckString  (rf_power.format(), "sin(x)^cos(x)", "rf_power.format", os, __LINE__);
        CheckBool    (rf_power == rfunction("{x} sin(x)^cos(x)"), true, "rf_power.format", os, __LINE__);

        rfunction rf_uminus = - rfsin;
        CheckString  (rf_uminus.format(), "-sin(x)", "rf_uminus.format", os, __LINE__);
        CheckBool    (rf_uminus == rfunction("{x} -sin(x)"), true, "rf_uminus.format", os, __LINE__);

        cfunction cf_plus = cfsin + cfcos;
        CheckString  (cf_plus.format(), "sin(x)+cos(x)", "cf_plus.format", os, __LINE__);
        CheckBool    (cf_plus == cfunction("{x} sin(x)+cos(x)"), true, "cf_plus.format", os, __LINE__);

        cfunction cf_minus = cfsin - cfcos;
        CheckString  (cf_minus.format(), "sin(x)-cos(x)", "cf_minus.format", os, __LINE__);
        CheckBool    (cf_minus == cfunction("{x} sin(x)-cos(x)"), true, "cf_minus.format", os, __LINE__);

        cfunction cf_mult = cfsin * cfcos;
        CheckString  (cf_mult.format(), "sin(x)*cos(x)", "cf_mult.format", os, __LINE__);
        CheckBool    (cf_mult == cfunction("{x} sin(x)*cos(x)"), true, "cf_mult.format", os, __LINE__);

        cfunction cf_div = cfsin / cfcos;
        CheckString  (cf_div.format(), "sin(x)/cos(x)", "cf_div.format", os, __LINE__);
        CheckBool    (cf_div == cfunction("{x} sin(x)/cos(x)"), true, "cf_div.format", os, __LINE__);

        cfunction cf_power = cfsin ^ cfcos;
        CheckString  (cf_power.format(), "sin(x)^cos(x)", "cf_power.format", os, __LINE__);
        CheckBool    (cf_power == cfunction("{x} sin(x)^cos(x)"), true, "cf_power.format", os, __LINE__);

        cfunction cf_uminus = - cfsin;
        CheckString  (cf_uminus.format(), "-sin(x)", "cf_uminus.format", os, __LINE__);
        CheckBool    (cf_uminus == cfunction("{x} -sin(x)"), true, "cf_uminus.format", os, __LINE__);

        rf_plus += rfcos;
        CheckString  (rf_plus.format(), "sin(x)+cos(x)+cos(x)", "rf_plus.format", os, __LINE__);
        CheckBool    (rf_plus == rfunction("{x} sin(x)+cos(x)+cos(x)"), true, "rf_plus.format", os, __LINE__);

        rf_plus.simp();
        CheckString  (rf_plus.format(), "2*cos(x)+sin(x)", "rf_plus.format", os, __LINE__);
        CheckBool    (rf_plus == rfunction("{x} 2*cos(x)+sin(x)"), true, "rf_plus.format", os, __LINE__);

        rf_plus -= rfcos;
        CheckString  (rf_plus.format(), "2*cos(x)+sin(x)-cos(x)", "rf_plus.format", os, __LINE__);
        CheckBool    (rf_plus == rfunction("{x} 2*cos(x)+sin(x)-cos(x)"), true, "rf_plus.format", os, __LINE__);

        rf_plus.simp();
        CheckString  (rf_plus.format(), "sin(x)+cos(x)", "rf_plus.format", os, __LINE__);
        CheckBool    (rf_plus == rfunction("{x} sin(x)+cos(x)"), true, "rf_plus.format", os, __LINE__);

        rf_div *= rfcos;
        CheckString  (rf_div.format(), "sin(x)/cos(x)*cos(x)", "rf_div.format", os, __LINE__);
        CheckBool    (rf_div == rfunction("{x} sin(x)/cos(x)*cos(x)"), true, "rf_div.format", os, __LINE__);

        rf_div.simp();
        CheckString  (rf_div.format(), "sin(x)", "rf_div.format", os, __LINE__);
        CheckBool    (rf_div == rfunction("{x} sin(x)"), true, "rf_div.format", os, __LINE__);

        rf_div /= rfsin;
        CheckString  (rf_div.format(), "sin(x)/sin(x)", "rf_div.format", os, __LINE__);
        CheckBool    (rf_div == rfunction("{x} sin(x)/sin(x)"), true, "rf_div.format", os, __LINE__);

        rf_div.simp();
        CheckString  (rf_div.format(), "1", "rf_div.format", os, __LINE__);
        CheckBool    (rf_div == rfunction("{x} 1"), true, "rf_div.format", os, __LINE__);

        cf_plus += cfcos;
        CheckString  (cf_plus.format(), "sin(x)+cos(x)+cos(x)", "cf_plus.format", os, __LINE__);
        CheckBool    (cf_plus == cfunction("{x} sin(x)+cos(x)+cos(x)"), true, "cf_plus.format", os, __LINE__);

        cf_plus.simp();
        CheckString  (cf_plus.format(), "(2,0)*cos(x)+sin(x)", "cf_plus.format", os, __LINE__);
        CheckBool    (cf_plus == cfunction("{x} (2,0)*cos(x)+sin(x)"), true, "cf_plus.format", os, __LINE__);

        cf_plus -= cfcos;
        CheckString  (cf_plus.format(), "(2,0)*cos(x)+sin(x)-cos(x)", "cf_plus.format", os, __LINE__);
        CheckBool    (cf_plus == cfunction("{x} (2,0)*cos(x)+sin(x)-cos(x)"), true, "cf_plus.format", os, __LINE__);

        cf_plus.simp();
        CheckString  (cf_plus.format(), "sin(x)+cos(x)", "cf_plus.format", os, __LINE__);
        CheckBool    (cf_plus == cfunction("{x} sin(x)+cos(x)"), true, "cf_plus.format", os, __LINE__);

        cf_div *= cfcos;
        CheckString  (cf_div.format(), "sin(x)/cos(x)*cos(x)", "cf_div.format", os, __LINE__);
        CheckBool    (cf_div == cfunction("{x} sin(x)/cos(x)*cos(x)"), true, "cf_div.format", os, __LINE__);

        cf_div.simp();
        CheckString  (cf_div.format(), "sin(x)", "cf_div.format", os, __LINE__);
        CheckBool    (cf_div == cfunction("{x} sin(x)"), true, "cf_div.format", os, __LINE__);

        cf_div /= cfsin;
        CheckString  (cf_div.format(), "sin(x)/sin(x)", "cf_div.format", os, __LINE__);
        CheckBool    (cf_div == cfunction("{x} sin(x)/sin(x)"), true, "cf_div.format", os, __LINE__);

        cf_div.simp();
        CheckString  (cf_div.format(), "(1,0)", "cf_div.format", os, __LINE__);
        CheckBool    (cf_div == cfunction("{x} (1,0)"), true, "cf_div.format", os, __LINE__);

        rfc = (treal)1.;
        CheckString  (rfc.format(), "1", "rfunction = treal", os, __LINE__);
        CheckString  ((-rfc).format(), "-1", "-rfunction = treal", os, __LINE__);
        rfc += 3;
        rfc.simp();
        CheckString  (rfc.format(), "4", "rfunction += treal", os, __LINE__);
        rfc -= 9;
        rfc.simp();
        CheckString  (rfc.format(), "(-5)", "rfunction -= treal", os, __LINE__);
        rfc *= 3;
        rfc.simp();
        CheckString  (rfc.format(), "(-15)", "rfunction *= treal", os, __LINE__);
        rfc /= 5;
        rfc.simp();
        CheckString  (rfc.format(), "(-3)", "rfunction /= treal", os, __LINE__);
        rfc ^= 2;
        rfc.simp();
        CheckString  (rfc.format(), "9", "rfunction ^= treal", os, __LINE__);

        cfc = tcomplex ((treal)1., (treal)1.);
        CheckString  (cfc.format(), "(1,1)", "cfc = tcomplex", os, __LINE__);
        CheckString  ((-cfc).format(), "-(1,1)", "-cfc = tcomplex", os, __LINE__);
        cfc += tcomplex ((treal)3., (treal)4.);
        cfc.simp();
        CheckString  (cfc.format(), "(4,5)", "cfunction += tcomplex", os, __LINE__);
        cfc -= tcomplex ((treal)2., (treal)1.);
        cfc.simp();
        CheckString  (cfc.format(), "(2,4)", "cfunction -= tcomplex", os, __LINE__);
        cfc *= tcomplex ((treal)1., (treal)3.);
        cfc.simp();
        CheckString  (cfc.format(), "(-10,10)", "cfunction -= tcomplex", os, __LINE__);
        cfc /= tcomplex ((treal)10., (treal)0.);
        cfc.simp();
        CheckString  (cfc.format(), "(-1,1)", "cfunction -= tcomplex", os, __LINE__);

        rfunction rfc2(2);
        CheckReal    ((rfc + rfc2)(), (treal)11., "rfunction + rfunction", os, __LINE__);
        CheckReal    ((rfc - rfc2)(), (treal)7., "rfunction - rfunction", os, __LINE__);
        CheckReal    ((rfc * rfc2)(), (treal)18., "rfunction * rfunction", os, __LINE__);
        CheckReal    ((rfc / rfc2)(), (treal)4.5, "rfunction / rfunction", os, __LINE__);
        CheckReal    ((rfc ^ rfc2)(), (treal)81., "rfunction ^ rfunction", os, __LINE__);

        CheckReal    ((2. + rfc)(), (treal)11., "treal + rfunction", os, __LINE__);
        CheckReal    ((2 + rfc)(), (treal)11., "treal + rfunction", os, __LINE__);
        CheckReal    ((2 - rfc)(), (treal)-7., "treal + rfunction", os, __LINE__);
        CheckReal    ((2 * rfc)(), (treal)18., "treal + rfunction", os, __LINE__);
        CheckReal    ((2 / rfc)(), (treal)0.2222222222222222, "treal + rfunction", os, __LINE__);
        CheckReal    ((2 ^ rfc)(), (treal)512., "treal + rfunction", os, __LINE__);



        const treal r2 = (treal) 2.;
        const tcomplex c11 = tcomplex ((treal)1., (treal)-1.);
        rfc2 = r2;
        cfc2 = c11;

        rfunction rf_self("{x} x");
        CheckReal    (rf_self.sat(rfc2)((treal)-2.000001), (treal)-1., "rfunction.sat - value", os, __LINE__);
        CheckReal    (rf_self.sat(rfc2)((treal)-2.), (treal)0., "rfunction.sat - value", os, __LINE__);
        CheckReal    (rf_self.sat(rfc2)((treal)2.), (treal)0., "rfunction.sat - value", os, __LINE__);
        CheckReal    (rf_self.sat(rfc2)((treal)2.00001), (treal)1., "rfunction.sat - value", os, __LINE__);
        cfunction cf_self("{x} x");
        CheckComplex (cf_self.sat(cfc2)(tcomplex ((treal)-1.0001, (treal)1.)),
                      tcomplex ((treal)-1., (treal)0.), "cfunction.sat - value", os, __LINE__);
        CheckComplex (cf_self.sat(cfc2)(tcomplex ((treal)-1., (treal)1.)),
                      tcomplex ((treal)0., (treal)0.), "cfunction.sat - value", os, __LINE__);
        CheckComplex (cf_self.sat(cfc2)(tcomplex ((treal)1., (treal)1.)),
                      tcomplex ((treal)0., (treal)0.), "cfunction.sat - value", os, __LINE__);
        CheckComplex (cf_self.sat(cfc2)(tcomplex ((treal)1.0001, (treal)1.)),
                      tcomplex ((treal)1., (treal)0.), "cfunction.sat - value", os, __LINE__);

        rfc = rf_self.exp().simp();
        CheckReal    (rfc(r2), CFUN_M_E * CFUN_M_E, "rfunction.exp - value", os, __LINE__);
        cfc = cf_self.exp().simp();
        CheckComplex (cfc(c11), std::exp(c11), "cfunction.exp - value", os, __LINE__);

        rfc = rf_self.sqrt().simp();
        CheckReal    (rfc(r2), ::sqrt(r2), "rfunction.sqrt - value", os, __LINE__);
        cfc = cf_self.sqrt().simp();
        CheckComplex (cfc(c11), std::sqrt(c11), "cfunction.sqrt - value", os, __LINE__);

        rfc = rf_self.log().simp();
        CheckReal    (rfc(r2), ::log(r2), "rfunction.log - value", os, __LINE__);
        cfc = cf_self.log().simp();
        CheckComplex (cfc(c11), std::log(c11), "cfunction.log - value", os, __LINE__);

        rfc = rf_self.log10().simp();
        CheckReal    (rfc(r2), ::log10(r2), "rfunction.log10 - value", os, __LINE__);
        cfc = cf_self.log10().simp();
        CheckComplex (cfc(c11), std::log10(c11), "cfunction.log - value", os, __LINE__);

        rfc = rf_self.sin().simp();
        CheckReal    (rfc(r2), ::sin(r2), "rfunction.sin - value", os, __LINE__);
        cfc = cf_self.sin().simp();
        CheckComplex (cfc(c11), std::sin(c11), "cfunction.sin - value", os, __LINE__);

        rfc = rf_self.cos().simp();
        CheckReal    (rfc(r2), ::cos(r2), "rfunction.cos - value", os, __LINE__);
        cfc = cf_self.cos().simp();
        CheckComplex (cfc(c11), std::cos(c11), "cfunction.cos - value", os, __LINE__);

        rfc = rf_self.tan().simp();
        CheckReal    (rfc(r2), ::tan(r2), "rfunction.tan - value", os, __LINE__);
        cfc = cf_self.tan().simp();
        CheckComplex (cfc(c11), std::tan(c11), "cfunction.tan - value", os, __LINE__);

        rfc = rf_self.asin().simp();
        CheckReal    (rfc(1/r2), ::asin(1/r2), "rfunction.asin - value", os, __LINE__);
        cfc = cf_self.asin().simp();
        CheckComplex (cfc(c11), ElementaryFunctions<tcomplex>::asin(c11), "cfunction.asin - value", os, __LINE__);

        rfc = rf_self.acos().simp();
        CheckReal    (rfc(1/r2), ::acos(1/r2), "rfunction.acos - value", os, __LINE__);
        cfc = cf_self.acos().simp();
        CheckComplex (cfc(c11), ElementaryFunctions<tcomplex>::acos(c11), "cfunction.acos - value", os, __LINE__);

        rfc = rf_self.atan().simp();
        CheckReal    (rfc(r2), ::atan(r2), "rfunction.atan - value", os, __LINE__);
        cfc = cf_self.atan().simp();
        CheckComplex (cfc(c11), ElementaryFunctions<tcomplex>::atan(c11), "cfunction.atan - value", os, __LINE__);

        rfc = rf_self.sinh().simp();
        CheckReal    (rfc(r2), ::sinh(r2), "rfunction.sinh - value", os, __LINE__);
        cfc = cf_self.sinh().simp();
        CheckComplex (cfc(c11), std::sinh(c11), "cfunction.sinh - value", os, __LINE__);

        rfc = rf_self.cosh().simp();
        CheckReal    (rfc(r2), ::cosh(r2), "rfunction.cosh - value", os, __LINE__);
        cfc = cf_self.cosh().simp();
        CheckComplex (cfc(c11), std::cosh(c11), "cfunction.cosh - value", os, __LINE__);

        rfc = rf_self.tanh().simp();
        CheckReal    (rfc(r2), ::tanh(r2), "rfunction.tanh - value", os, __LINE__);
        cfc = cf_self.tanh().simp();
        CheckComplex (cfc(c11), std::tanh(c11), "cfunction.tanh - value", os, __LINE__);

        rfc = rf_self.sinint().simp();
        CheckReal    (rfc(r2), ElementaryFunctions<treal>::sinint(r2, cvm::cvmMachSp()),
                      "rfunction.sinint - value", os, __LINE__);
        cfc = cf_self.sinint().simp();
        CheckComplex (cfc(c11), ElementaryFunctions<tcomplex>::sinint(c11, cvm::cvmMachSp()),
                      "cfunction.sinint - value", os, __LINE__);


        rfc = rf_self.cosint().simp();
        CheckReal    (rfc(r2), ElementaryFunctions<treal>::cosint(r2, cvm::cvmMachSp()),
                      "rfunction.cosint - value", os, __LINE__);
        cfc = cf_self.cosint().simp();
        CheckComplex (cfc(c11), ElementaryFunctions<tcomplex>::cosint(c11, cvm::cvmMachSp()),
                      "cfunction.cosint - value", os, __LINE__);

        rfc = rf_self.sign().simp();
        CheckReal    (rfc(r2), r2 > 0 ? (treal) 1. : (treal) -1., "rfunction.sign - value", os, __LINE__);
        CheckReal    (rfc((treal) 0.), (treal) 0., "rfunction.sign - value", os, __LINE__);
        cfc = cf_self.sign().simp();
        CheckComplex (cfc(c11), c11.real() > 0 ? tcomplex ((treal)1., (treal)0.) : tcomplex ((treal)-1., (treal)0.),
                      "cfunction.sign - value", os, __LINE__);
        CheckComplex (cfc(tcomplex ((treal)0., (treal)0.)), tcomplex ((treal)0., (treal)0.),
                      "cfunction.sign - value", os, __LINE__);

        rfc = rf_self.abs().simp();
        CheckReal    (rfc(-r2), ::fabs(r2), "rfunction.abs - value", os, __LINE__);
        CheckReal    (rfc(r2), ::fabs(r2), "rfunction.abs - value", os, __LINE__);
        cfc = cf_self.abs().simp();
        CheckComplex (cfc(c11), tcomplex (std::abs(c11), (treal)0.), "cfunction.abs - value", os, __LINE__);

        rfc = rf_self.delta(rfunction((treal) 1.)).simp();
        CheckReal    (rfc(r2), (treal)0., "rfunction.delta - value", os, __LINE__);
        CheckReal    (rfc((treal)1), (std::numeric_limits<treal>::max)(), "rfunction.delta - value", os, __LINE__);
        cfc = cf_self.delta(cfunction(tcomplex ((treal)1., (treal)0.))).simp();
        CheckComplex (cfc(c11), tcomplex ((std::numeric_limits<treal>::max)(), (treal)0.), "cfunction.delta - value", os, __LINE__);

        rfc = rf_self.iif(rfunction((treal) 3.), rfunction((treal) 4.)).simp();
        CheckReal    (rfc((treal)-1.), (treal)3., "rfunction.iif - value", os, __LINE__);
        CheckReal    (rfc((treal)0.), (treal)4., "rfunction.iif - value", os, __LINE__);
        CheckReal    (rfc((treal)1.), (treal)4., "rfunction.iif - value", os, __LINE__);

        cfc = cf_self.iif(cfunction((treal) 3.), cfunction((treal) 4.)).simp();
        CheckComplex (cfc(tcomplex ((treal)-1., (treal)0.)), tcomplex ((treal)3., (treal)0.),
                      "cfunction.iif - value", os, __LINE__);
        CheckComplex (cfc(tcomplex ((treal)0., (treal)0.)), tcomplex ((treal)4., (treal)0.),
                      "cfunction.iif - value", os, __LINE__);
        CheckComplex (cfc(tcomplex ((treal)1., (treal)0.)), tcomplex ((treal)4., (treal)0.),
                      "cfunction.iif - value", os, __LINE__);


        {
            std::vector<std::string> saVars;
            std::vector<std::string> saParameters;
            std::vector<std::string> saMeanings;

            saVars.push_back("t");
            saParameters.push_back("p");
            saMeanings.push_back("2");

            rfunction f2 (saVars, "sin(t)^p + cos(t)^p", saParameters, saMeanings);
            CheckReal    (f2((treal)-1.5), (treal)1., "rfunction sin(t)^p + cos(t)^p - value", os, __LINE__);

            cfunction f2c (saVars, "sin(t)^p + cos(t)^p", saParameters, saMeanings);
            CheckComplex  (f2c((treal)-1.5), tcomplex((treal)1., (treal)0.),
                           "cfunction sin(t)^p + cos(t)^p - value", os, __LINE__, dPessimisticSp);
        }

        // drv
        {
            rfunction f("{x,y} y*x+y^3");
            treal x[2];
            x[0] = (treal)1.;
            x[1] = (treal)2.;

            CheckString  (f.drv(0).format(), "y", "rfunction drv", os, __LINE__);
            CheckString  (f.drv(1).format(), "x+3*y^2", "rfunction drv", os, __LINE__);

            {
                std::ostringstream oss;
                oss << f.drv(0);
                CheckString  (oss.str(), "{x,y} y", "rfunction drv", os, __LINE__);
            }
            {
                std::ostringstream oss;
                oss << f.drv(0).drv(1);
                CheckString  (oss.str(), "{x,y} 1", "rfunction drv drv", os, __LINE__);
            }
            {
                std::ostringstream oss;
                oss << f.drv(1);
                CheckString  (oss.str(), "{x,y} x+3*y^2", "rfunction drv", os, __LINE__);
            }
            {
                std::ostringstream oss;
                oss << f.drv(1).drv(0);
                CheckString  (oss.str(), "{x,y} 1", "rfunction drv drv", os, __LINE__);
            }

            CheckReal    (f.drv(0)(x), (treal)2., "rfunction drv value", os, __LINE__);
            CheckReal    (f.drv(1)(x), (treal)13., "rfunction drv value", os, __LINE__);
        }
        {
            cfunction f("{x,y} y*x+y^3");
            tcomplex x[2];
            x[0] = tcomplex ((treal)1., (treal)-1.);
            x[1] = tcomplex ((treal)2., (treal)-3.);

            CheckString  (f.drv(0).format(), "y", "cfunction drv", os, __LINE__);
            CheckString  (f.drv(1).format(), "x+(3,0)*y^(2,0)", "cfunction drv", os, __LINE__);

            {
                std::ostringstream oss;
                oss << f.drv(0);
                CheckString  (oss.str(), "{x,y} y", "cfunction drv", os, __LINE__);
            }
            {
                std::ostringstream oss;
                oss << f.drv(0).drv(1);
                CheckString  (oss.str(), "{x,y} (1,0)", "cfunction drv drv", os, __LINE__);
            }
            {
                std::ostringstream oss;
                oss << f.drv(1);
                CheckString  (oss.str(), "{x,y} x+(3,0)*y^(2,0)", "cfunction drv", os, __LINE__);
            }
            {
                std::ostringstream oss;
                oss << f.drv(1).drv(0);
                CheckString  (oss.str(), "{x,y} (1,0)", "cfunction drv drv", os, __LINE__);
            }

            CheckComplex  (f.drv(0)(x), tcomplex((treal)2., (treal)-3.), "cfunction drv value", os, __LINE__, dPessimisticSp);
            CheckComplex  (f.drv(1)(x), x[0] + tcomplex((treal)3., (treal)0.) * x[1] * x[1],
                           "cfunction drv value", os, __LINE__, dPessimisticSp);
        }

        // assign number
        {
            rfunction f;
            f = (treal)2.2;
            CheckString  (f.format(), "2.2", "rfunction = number", os, __LINE__);

            cfunction fc;
            fc = tcomplex((treal)2., (treal)-3.);
            CheckString  (fc.format(), "(2,-3)", "cfunction = number", os, __LINE__);
        }


        // complex number
        {
            cfunction c1("(1,-2.3)");
            CheckComplex  (c1(), tcomplex((treal)1., (treal)-2.3), "cfunction (1,2) - value", os, __LINE__, dPessimisticSp);
            cfunction c2("(-.1,2.)");
            CheckComplex  (c2(), tcomplex((treal)-.1, (treal)2.), "cfunction (.1,2.) - value", os, __LINE__, dPessimisticSp);
            cfunction c3("{x,z} z+(3,-1.6)");
            tcomplex va[2];
            va[0] = tcomplex((treal).777, (treal)-1.888);
            va[1] = tcomplex((treal)1.3, (treal)-2.1);
            CheckComplex  (c3(va), tcomplex((treal)4.3, (treal)-3.7),
                           "cfunction {x,z} z+(3,-1.6) - value", os, __LINE__, dPessimisticSp);
        }

        // rfvector
        {
            string_array sa;
            sa.push_back ("{x,z} sign(x+2)");
            sa.push_back ("{x,z} z+3");

            rfvector fa(sa);
            treal x[2];
            treal y[2];

            x[0] = -2.1;
            x[1] = 8.8;
            fa.value(x,y);

            CheckReal    (y[0], (treal)-1., "rfvector - value", os, __LINE__);
            CheckReal    (y[1], (treal)11.8, "rfvector - value", os, __LINE__);

            rvector yv = fa(x);
            CheckReal    (yv[CVM0], (treal)-1., "rfvector - value", os, __LINE__);
            CheckReal    (yv[CVM0+1], (treal)11.8, "rfvector - value", os, __LINE__);

            rfvector fcp(fa);
            rfvector fcpcp;
            fcpcp << fcp;

            CheckBool(fa == fcp, true, "rfvector copy", os, __LINE__);
            CheckBool(fa == fcpcp, true, "rfvector copy", os, __LINE__);

            try {
                rfunction xf1 ("{a,b} a+3");
                fa /= xf1;
                Fail("No exception about variables mismatch, rvector", os, __LINE__);
            } catch (cvmexception& ex) {
                CheckInt (ex.cause(), CFUN_VARSDONTMATCH, "CFUN_VARSDONTMATCH exception cause", os, __LINE__);
            }

            rfunction xf1 ("{x,z} x+3");
            fa /= xf1;
            fcpcp /= xf1;

            std::stringstream s1, s2;
            s1 << fa;
            s2 << fcpcp;

            CheckBool(s1.str() == s2.str(), true, "rfvector <<", os, __LINE__);
            CheckString(s1.str(), "{x,z} sign(x+2)/(x+3) {x,z} (z+3)/(x+3) \n", "rfvector <<", os, __LINE__);
        }
        // rfvector drv
        {
            string_array sa;
            sa.push_back ("{x,z} sin(x+2)^2");
            sa.push_back ("{x,z} z+3/x");

            rfvector fv(sa);

            std::stringstream s1, s2;
            s1 << fv.drv(0);
            s2 << fv.drv(1);

            CheckString(s1.str(), "{x,z} cos(x+2)*2*sin(x+2) {x,z} (-3)/x^2 \n", "rfvector drv", os, __LINE__);
            CheckString(s2.str(), "{x,z} 0 {x,z} 1 \n", "rfvector drv", os, __LINE__);
        }
        // cfvector
        {
            string_array sa;
            sa.push_back ("{x,z} sign(x+(2,3))");
            sa.push_back ("{x,z} z+(3,-1.6)");

            cfvector fa(sa);
            tcomplex x[2];
            tcomplex y[2];

            x[0] = tcomplex ((treal)-2.1, (treal)3.7);
            x[1] = tcomplex ((treal)8.8, (treal)-1.3);
            fa.value(x,y);

            CheckComplex  (y[0], tcomplex((treal)-1., (treal)0.), "cfvector value", os, __LINE__, dPessimisticSp);
            CheckComplex  (y[1], tcomplex((treal)11.8, (treal)-2.9), "cfvector value", os, __LINE__, dPessimisticSp);

            cvector yv = fa(x);
            CheckComplex  (yv[CVM0], tcomplex((treal)-1., (treal)0.), "cfvector value", os, __LINE__, dPessimisticSp);
            CheckComplex  (yv[CVM0+1], tcomplex((treal)11.8, (treal)-2.9), "cfvector value", os, __LINE__, dPessimisticSp);

            cfvector fcp(fa);
            cfvector fcpcp;
            fcpcp << fcp;

            CheckBool(fa == fcp, true, "cfvector copy", os, __LINE__);
            CheckBool(fa == fcpcp, true, "cfvector copy", os, __LINE__);

            try {
                cfunction xf1 ("{a,b} a+3");
                fa /= xf1;
                Fail("No exception about variables mismatch, cvector", os, __LINE__);
            } catch (cvmexception& ex) {
                CheckInt (ex.cause(), CFUN_VARSDONTMATCH, "CFUN_VARSDONTMATCH exception cause", os, __LINE__);
            }

            cfunction xf1 ("{x,z} x+(3,2.3)");
            fa /= xf1;
            fcpcp /= xf1;

            std::stringstream s1, s2;
            s1 << fa;
            s2 << fcpcp;

            CheckBool(s1.str() == s2.str(), true, "cfvector <<", os, __LINE__);
            CheckString(s1.str(), "{x,z} sign(x+(2,3))/(x+(3,2.3)) {x,z} (z+(3,-1.6))/(x+(3,2.3)) \n",
                        "cfvector <<", os, __LINE__);
        }
        // cfvector drv
        {
            string_array sa;
            sa.push_back ("{x,z} sin(x+2)^(2,-2.)");
            sa.push_back ("{x,z} z+(3,-1.)/x");

            cfvector fv(sa);

            std::stringstream s1, s2;
            s1 << fv.drv(0);
            s2 << fv.drv(1);

            CheckString(s1.str(), "{x,z} cos(x+(2,0))*(2,-2)*sin(x+(2,0))^(1,-2) {x,z} (-3,1)/x^(2,0) \n",
                        "cfvector drv", os, __LINE__);
            CheckString(s2.str(), "{x,z} (0,0) {x,z} (1,0) \n", "cfvector drv", os, __LINE__);
        }



        // rfmatrix
        {
            string_array sa2;
            sa2.push_back ("1");
            sa2.push_back ("2");
            sa2.push_back ("3");
            sa2.push_back ("4");
            sa2.push_back ("5");
            sa2.push_back ("6");

            cvm::rmatrix rm(2,3);
            rfmatrix fm(2,3,sa2);

            fm.value(rm);
            CheckReal    (rm(CVM0+1,CVM0+1), (treal)4., "rfmatrix - value", os, __LINE__);

            rm = fm();
            CheckReal    (rm(CVM0+1,CVM0+1), (treal)4., "rfmatrix - value", os, __LINE__);

            std::ostringstream oss1, oss2;
            oss1 << fm;
            oss2 << rm;
            CheckString (oss1.str(), oss2.str(), "rfmatrix <<", os, __LINE__);

            string_array sa;
            sa.push_back ("{x} x^2");
            sa.push_back ("{x} sin(x)");
            rfvector fv(sa);

            fm.set_col(2, fv);
            fm.value(3, rm);

            CheckReal    (rm(CVM0,CVM0+1), (treal)3., "rfmatrix set_col", os, __LINE__);
            CheckReal    (rm(CVM0,CVM0+2), (treal)9., "rfmatrix set_col", os, __LINE__);
            CheckReal    (rm(CVM0+1,CVM0+2), (treal)sin(3.), "rfmatrix set_col", os, __LINE__, dPessimisticSp);

            rm = fm(3);
            CheckReal    (rm(CVM0,CVM0+1), (treal)3., "rfmatrix set_col", os, __LINE__);
            CheckReal    (rm(CVM0,CVM0+2), (treal)9., "rfmatrix set_col", os, __LINE__);
            CheckReal    (rm(CVM0+1,CVM0+2), (treal)sin(3.), "rfmatrix set_col", os, __LINE__, dPessimisticSp);
        }

        // cfmatrix
        {
            string_array sa2;
            sa2.push_back ("(1,-1)");
            sa2.push_back ("(2,-2)");
            sa2.push_back ("(3,-3)");
            sa2.push_back ("(4,-4)");
            sa2.push_back ("(5,-5)");
            sa2.push_back ("(6,-6)");

            cvm::cmatrix cm(2,3);
            cfmatrix fm(2,3,sa2);

            fm.value(cm);
            CheckComplex  (cm(CVM0+1,CVM0+1), tcomplex((treal)4., (treal)-4.), "cfmatrix - value", os, __LINE__);

            cm = fm();
            CheckComplex  (cm(CVM0+1,CVM0+1), tcomplex((treal)4., (treal)-4.), "cfmatrix - value", os, __LINE__);

            std::ostringstream oss1, oss2;
            oss1 << fm;
            oss2 << cm;
            CheckString (oss1.str(), oss2.str(), "cfmatrix <<", os, __LINE__);

            string_array sa;
            sa.push_back ("{x} x^(2,1)");
            sa.push_back ("{x} sin(x)");
            cfvector fv(sa);

            fm.set_col(2, fv);
            tcomplex c = tcomplex((treal)3., (treal)-1.);
            fm.value(c, cm);

            CheckComplex  (cm(CVM0,CVM0+1), tcomplex((treal)3., (treal)-3.), "cfmatrix - value", os, __LINE__);
            CheckComplex  (cm(CVM0,CVM0+2), std::pow (c, tcomplex((treal)2., (treal)1.)), "cfmatrix - value", os, __LINE__, dPessimisticSp);
            CheckComplex  (cm(CVM0+1,CVM0+2), std::sin (c), "cfmatrix - value", os, __LINE__, dPessimisticSp);

            cm = fm(c);
            CheckComplex  (cm(CVM0,CVM0+1), tcomplex((treal)3., (treal)-3.), "cfmatrix - value", os, __LINE__);
            CheckComplex  (cm(CVM0,CVM0+2), std::pow (c, tcomplex((treal)2., (treal)1.)), "cfmatrix - value", os, __LINE__, dPessimisticSp);
            CheckComplex  (cm(CVM0+1,CVM0+2), std::sin (c), "cfmatrix - value", os, __LINE__, dPessimisticSp);
        }

        // rfvector scalar product
        {
            string_array sa;
            sa.push_back ("{x,z} x+z");
            sa.push_back ("{x,z} x-z");

            rfvector fv1(sa), fv2(sa);
            rfunction f = fv1 * fv2;
            rfunction fc("{x,z} (x+z)^2+(x-z)^2");
            CheckBool    (f == fc, true, "rfvector scalar product", os, __LINE__);
        }
        // cfvector scalar product
        {
            string_array sa1;
            sa1.push_back ("{x,z} x+z");
            sa1.push_back ("{x,z} x-z");
            string_array sa2;
            sa2.push_back ("{x,z} x-z");
            sa2.push_back ("{x,z} x+z");

            cfvector fv1(sa1), fv2(sa2);
            cfunction f = fv1 * fv2;
            cfunction fc("{x,z} (x+z)*((2,0)*(x-z))");
            CheckBool    (f == fc, true, "cfvector scalar product", os, __LINE__);
        }


        // fully qualified input - rvector
        {
            string_array saVars, saBodies, saParameters, saMeanings;
            saVars.push_back ("x");
            saVars.push_back ("y");

            saBodies.push_back ("x-y");
            saBodies.push_back ("y*p");
            saBodies.push_back ("x-p");

            try {
                string_array saParameters, saMeanings;
                saParameters.push_back ("p");
                saMeanings.push_back ("p+2");
                rfvector fv (saVars, saBodies, saParameters, saMeanings);
                Fail("No exception about parameter recursion", os, __LINE__);
            } catch (cvmexception& ex) {
                CheckInt (ex.cause(), CFUN_PARAMETER_RECURSION, "CFUN_PARAMETER_RECURSION exception cause", os, __LINE__);
            }

            try {
                string_array saParameters, saMeanings;
                saParameters.push_back ("p");
                saMeanings.push_back ("x*p -2");
                rfvector fv (saVars, saBodies, saParameters, saMeanings);
                Fail("No exception about parameter recursion", os, __LINE__);
            } catch (cvmexception& ex) {
                CheckInt (ex.cause(), CFUN_PARAMETER_RECURSION, "CFUN_PARAMETER_RECURSION exception cause", os, __LINE__);
            }

            try {
                string_array saParameters, saMeanings;
                saParameters.push_back ("p");
                saMeanings.push_back ("y^ p ");
                rfvector fv (saVars, saBodies, saParameters, saMeanings);
                Fail("No exception about parameter recursion", os, __LINE__);
            } catch (cvmexception& ex) {
                CheckInt (ex.cause(), CFUN_PARAMETER_RECURSION, "CFUN_PARAMETER_RECURSION exception cause", os, __LINE__);
            }

            saParameters.push_back ("p");
            saMeanings.push_back ("x*y-3");
            rfvector fv (saVars, saBodies, saParameters, saMeanings);

            std::stringstream ss;
            ss << fv;

            CheckString(ss.str(), "{x,y} x-y {x,y} y*(x*y-3) {x,y} x-x*y-3 \n", "fully qualified input - rvector", os, __LINE__);
        }

        // fully qualified input - cvector
        {
            string_array saVars, saBodies, saParameters, saMeanings;
            saVars.push_back ("x");
            saVars.push_back ("y");

            saBodies.push_back ("x-y");
            saBodies.push_back ("y*p");
            saBodies.push_back ("x-p");

            try {
                string_array saParameters, saMeanings;
                saParameters.push_back ("p");
                saMeanings.push_back ("p+2");
                cfvector fv (saVars, saBodies, saParameters, saMeanings);
                Fail("No exception about parameter recursion", os, __LINE__);
            } catch (cvmexception& ex) {
                CheckInt (ex.cause(), CFUN_PARAMETER_RECURSION, "CFUN_PARAMETER_RECURSION exception cause", os, __LINE__);
            }

            try {
                string_array saParameters, saMeanings;
                saParameters.push_back ("p");
                saMeanings.push_back ("x*p -2");
                cfvector fv (saVars, saBodies, saParameters, saMeanings);
                Fail("No exception about parameter recursion", os, __LINE__);
            } catch (cvmexception& ex) {
                CheckInt (ex.cause(), CFUN_PARAMETER_RECURSION, "CFUN_PARAMETER_RECURSION exception cause", os, __LINE__);
            }

            try {
                string_array saParameters, saMeanings;
                saParameters.push_back ("p");
                saMeanings.push_back ("y^ p ");
                cfvector fv (saVars, saBodies, saParameters, saMeanings);
                Fail("No exception about parameter recursion", os, __LINE__);
            } catch (cvmexception& ex) {
                CheckInt (ex.cause(), CFUN_PARAMETER_RECURSION, "CFUN_PARAMETER_RECURSION exception cause", os, __LINE__);
            }

            saParameters.push_back ("p");
            saMeanings.push_back ("x*y-3");
            cfvector fv (saVars, saBodies, saParameters, saMeanings);

            std::stringstream ss;
            ss << fv;

            CheckString(ss.str(), "{x,y} x-y {x,y} y*(x*y-(3,0)) {x,y} x-x*y-(3,0) \n", "fully qualified input - cvector", os, __LINE__);
        }


        // rfvector * rfmatrix
        {
            string_array sa1;
            sa1.push_back ("{x,z} x+z");
            sa1.push_back ("{x,z} x-z");

            string_array sa2;
            sa2.push_back ("1");
            sa2.push_back ("2");
            sa2.push_back ("3");
            sa2.push_back ("4");
            sa2.push_back ("5");
            sa2.push_back ("6");
            rfmatrix fm(2,3,sa2);

            string_array sa3;
            sa3.push_back ("{x,z} (x+z)+(x-z)*2");
            sa3.push_back ("{x,z} (x+z)*3+(x-z)*4");
            sa3.push_back ("{x,z} (x+z)*5+(x-z)*6");

            rfvector fv1(sa1), fv2(3), fvresult(sa3);

            fv2 = fv1 * fm;
            CheckBool    (fvresult == fv2, true, "rfvector * rfmatrix", os, __LINE__);

            fv2.mult (fv1, fm);
            CheckBool    (fvresult == fv2, true, "mult (rfvector, rfmatrix)", os, __LINE__);
        }

        // cfvector * cfmatrix
        {
            string_array sa1;
            sa1.push_back ("{x,z} x+z");
            sa1.push_back ("{x,z} x-z");

            string_array sa2;
            sa2.push_back ("1");
            sa2.push_back ("2");
            sa2.push_back ("3");
            sa2.push_back ("4");
            sa2.push_back ("5");
            sa2.push_back ("6");
            cfmatrix fm(2,3,sa2);

            string_array sa3;
            sa3.push_back ("{x,z} (x+z)+(x-z)*2");
            sa3.push_back ("{x,z} (x+z)*3+(x-z)*4");
            sa3.push_back ("{x,z} (x+z)*5+(x-z)*6");

            cfvector fv1(sa1), fv2(3), fvresult(sa3);

            fv2 = fv1 * fm;
            CheckBool    (fvresult == fv2, true, "cfvector * cfmatrix", os, __LINE__);

            fv2.mult (fv1, fm);
            CheckBool    (fvresult == fv2, true, "mult (cfvector, cfmatrix)", os, __LINE__);
        }

        // rfmatrix * rfvector
        {
            string_array sa1;
            sa1.push_back ("{x,z} x+z");
            sa1.push_back ("{x,z} x-z");

            string_array sa2;
            sa2.push_back ("1");
            sa2.push_back ("2");
            sa2.push_back ("3");
            sa2.push_back ("4");
            sa2.push_back ("5");
            sa2.push_back ("6");
            rfmatrix fm(3,2,sa2);

            string_array sa3;
            sa3.push_back ("{x,z} x+z+4*(x-z)");
            sa3.push_back ("{x,z} 2*(x+z)+5*(x-z)");
            sa3.push_back ("{x,z} 3*(x+z)+6*(x-z)");

            rfvector fv1(sa1), fv2(3), fvresult(sa3);

            fv2 = fm * fv1;
            CheckBool    (fvresult == fv2, true, "rfmatrix * rfvector", os, __LINE__);

            fv2.mult (fm, fv1);
            CheckBool    (fvresult == fv2, true, "mult (rfmatrix, rfvector)", os, __LINE__);
        }

        // cfmatrix * cfvector
        {
            string_array sa1;
            sa1.push_back ("{x,z} x+z");
            sa1.push_back ("{x,z} x-z");

            string_array sa2;
            sa2.push_back ("1");
            sa2.push_back ("2");
            sa2.push_back ("3");
            sa2.push_back ("4");
            sa2.push_back ("5");
            sa2.push_back ("6");
            cfmatrix fm(3,2,sa2);

            string_array sa3;
            sa3.push_back ("{x,z} (x+z)+4*(x-z)");
            sa3.push_back ("{x,z} 2*(x+z)+5*(x-z)");
            sa3.push_back ("{x,z} 3*(x+z)+6*(x-z)");

            cfvector fv1(sa1), fv2(3), fvresult(sa3);

            fv2 = fm * fv1;
            CheckBool    (fvresult == fv2, true, "cfmatrix * cfvector", os, __LINE__);

            fv2.mult (fm, fv1);
            CheckBool    (fvresult == fv2, true, "mult (cfmatrix, cfvector)", os, __LINE__);
        }


        // rfmatrix * rfmatrix
        {
            string_array sa1;
            sa1.push_back ("{x,y} x");
            sa1.push_back ("{x,y} y");
            sa1.push_back ("{x,y} 1");
            sa1.push_back ("{x,y} 2");
            sa1.push_back ("{x,y} 3");
            sa1.push_back ("{x,y} 4");
            rfmatrix fm1(2,3,sa1);

            string_array sa2;
            sa2.push_back ("1");
            sa2.push_back ("2");
            sa2.push_back ("3");
            sa2.push_back ("4");
            sa2.push_back ("5");
            sa2.push_back ("6");
            rfmatrix fm2(3,2,sa2);

            string_array sa3;
            sa3.push_back ("{x,y} 11+x");
            sa3.push_back ("{x,y} 16+y");
            sa3.push_back ("{x,y} 23+x*4");
            sa3.push_back ("{x,y} 34+y*4");

            rfmatrix fmcheck(2,2,sa3);

            rfmatrix fm = fm1 * fm2;
            CheckBool    (fmcheck == fm, true, "rfmatrix * rfmatrix", os, __LINE__);

            fm.mult (fm1, fm2);
            CheckBool    (fmcheck == fm, true, "mult (rfmatrix, rfmatrix)", os, __LINE__);
        }

        // cfmatrix * cfmatrix
        {
            string_array sa1;
            sa1.push_back ("{x,y} x");
            sa1.push_back ("{x,y} y");
            sa1.push_back ("{x,y} 1");
            sa1.push_back ("{x,y} 2");
            sa1.push_back ("{x,y} 3");
            sa1.push_back ("{x,y} 4");
            cfmatrix fm1(2,3,sa1);

            string_array sa2;
            sa2.push_back ("1");
            sa2.push_back ("2");
            sa2.push_back ("3");
            sa2.push_back ("4");
            sa2.push_back ("5");
            sa2.push_back ("6");
            cfmatrix fm2(3,2,sa2);

            string_array sa3;
            sa3.push_back ("{x,y} 11+x");
            sa3.push_back ("{x,y} 16+y");
            sa3.push_back ("{x,y} 23+x*4");
            sa3.push_back ("{x,y} 34+y*4");

            cfmatrix fmcheck(2,2,sa3);

            cfmatrix fm = fm1 * fm2;
            CheckBool    (fmcheck == fm, true, "cfmatrix * cfmatrix", os, __LINE__);

            fm.mult (fm1, fm2);
            CheckBool    (fmcheck == fm, true, "mult (cfmatrix, cfmatrix)", os, __LINE__);
        }

        // rfmatrix mult
        {
            string_array sa1;
            sa1.push_back ("{x,y} x");
            sa1.push_back ("{x,y} y");
            sa1.push_back ("{x,y} 1");
            sa1.push_back ("{x,y} 2");
            sa1.push_back ("{x,y} 3");
            sa1.push_back ("{x,y} 4");
            rfmatrix fm1(2,3,sa1);

            string_array sa2;
            sa2.push_back ("1");
            sa2.push_back ("2");
            sa2.push_back ("3");
            sa2.push_back ("4");
            sa2.push_back ("5");
            sa2.push_back ("6");
            rfmatrix fm2(3,2,sa2);

            string_array sa3;
            sa3.push_back ("{x,y} 11+x");
            sa3.push_back ("{x,y} 16+y");
            sa3.push_back ("{x,y} 23+x*4");
            sa3.push_back ("{x,y} 34+y*4");

            rfmatrix fm(2,2), fmcheck(2,2,sa3);
            fm.mult(fm1, fm2);

            CheckBool    (fmcheck == fm, true, "rfmatrix mult", os, __LINE__);
        }

        // cfmatrix mult
        {
            string_array sa1;
            sa1.push_back ("{x,y} x");
            sa1.push_back ("{x,y} y");
            sa1.push_back ("{x,y} 1");
            sa1.push_back ("{x,y} 2");
            sa1.push_back ("{x,y} 3");
            sa1.push_back ("{x,y} 4");
            cfmatrix fm1(2,3,sa1);

            string_array sa2;
            sa2.push_back ("1");
            sa2.push_back ("2");
            sa2.push_back ("3");
            sa2.push_back ("4");
            sa2.push_back ("5");
            sa2.push_back ("6");
            cfmatrix fm2(3,2,sa2);

            string_array sa3;
            sa3.push_back ("{x,y} 11+x");
            sa3.push_back ("{x,y} 16+y");
            sa3.push_back ("{x,y} 23+x*4");
            sa3.push_back ("{x,y} 34+y*4");

            cfmatrix fm(2,2), fmcheck(2,2,sa3);
            fm.mult(fm1, fm2);

            CheckBool    (fmcheck == fm, true, "cfmatrix mult", os, __LINE__);
        }

        // rfmatrix jacobi
        {
            string_array sa1;
            sa1.push_back ("{x,y} 2*x^2+3*y^3");
            sa1.push_back ("{x,y} 4*x^3-5*y^2");
            sa1.push_back ("{x,y} -x^4+6*y^2");
            rfvector fv(sa1);

            string_array sa2;
            sa2.push_back ("{x,y} 4*x");
            sa2.push_back ("{x,y} 12*x^2");
            sa2.push_back ("{x,y} (-4)*x^3");
            sa2.push_back ("{x,y} 9*y^2");
            sa2.push_back ("{x,y} (-10)*y");
            sa2.push_back ("{x,y} 12*y");
            rfmatrix fmcheck(3,2,sa2);

            rfmatrix fm = fv.jacobian();
            CheckBool    (fmcheck == fm, true, "rfmatrix jacobi", os, __LINE__);

            fv.jacobian(fm);
            CheckBool    (fmcheck == fm, true, "rfmatrix jacobi", os, __LINE__);
        }

        // rfmatrix jacobi nfrom
        {
            string_array sa1;
            sa1.push_back ("{x,y} 2*x^2+3*y^3");
            sa1.push_back ("{x,y} 4*x^3-5*y^2");
            sa1.push_back ("{x,y} -x^4+6*y^2");
            rfvector fv(sa1);

            string_array sa2;
            sa2.push_back ("{x,y} 9*y^2");
            sa2.push_back ("{x,y} (-10)*y");
            sa2.push_back ("{x,y} 12*y");
            rfmatrix fmcheck(3,1,sa2);

            rfmatrix fm = fv.jacobian(1);
            CheckBool    (fmcheck == fm, true, "rfmatrix jacobi, nfrom", os, __LINE__);

            fv.jacobian(fm, 1);
            CheckBool    (fmcheck == fm, true, "rfmatrix jacobi, nfrom", os, __LINE__);
        }

        // rfmatrix jacobi nfrom, nsize
        {
            string_array sa1;
            sa1.push_back ("{x,y,z} 2*x^2+3*y^3+z/2");
            sa1.push_back ("{x,y,z} 4*x^3-5*y^2-sin(z)");
            sa1.push_back ("{x,y,z} -x^4+6*y^2+z^(.5)");
            rfvector fv(sa1);

            string_array sa2;
            sa2.push_back ("{x,y,z} 4*x");
            sa2.push_back ("{x,y,z} 12*x^2");
            sa2.push_back ("{x,y,z} (-4)*x^3");
            sa2.push_back ("{x,y,z} 9*y^2");
            sa2.push_back ("{x,y,z} (-10)*y");
            sa2.push_back ("{x,y,z} 12*y");
            rfmatrix fmcheck(3,2,sa2);

            rfmatrix fm = fv.jacobian(0, 2);
            CheckBool    (fmcheck == fm, true, "rfmatrix jacobi, nfrom, nsize", os, __LINE__);

            fv.jacobian(fm, 0, 2);
            CheckBool    (fmcheck == fm, true, "rfmatrix jacobi, nfrom, nsize", os, __LINE__);
        }


        // cfmatrix jacobi
        {
            string_array sa1;
            sa1.push_back ("{x,y} 2*x^2+3*y^3");
            sa1.push_back ("{x,y} 4*x^3-5*y^2");
            sa1.push_back ("{x,y} -x^4+6*y^2");
            cfvector fv(sa1);

            string_array sa2;
            sa2.push_back ("{x,y} 4*x");
            sa2.push_back ("{x,y} 12*x^2");
            sa2.push_back ("{x,y} (-4)*x^3");
            sa2.push_back ("{x,y} 9*y^2");
            sa2.push_back ("{x,y} (-10)*y");
            sa2.push_back ("{x,y} 12*y");
            cfmatrix fmcheck(3,2,sa2);

            cfmatrix fm = fv.jacobian();
            CheckBool    (fmcheck == fm, true, "cfmatrix jacobi", os, __LINE__);

            fv.jacobian(fm);
            CheckBool    (fmcheck == fm, true, "cfmatrix jacobi", os, __LINE__);
        }

        // cfmatrix jacobi nfrom
        {
            string_array sa1;
            sa1.push_back ("{x,y} 2*x^2+3*y^3");
            sa1.push_back ("{x,y} 4*x^3-5*y^2");
            sa1.push_back ("{x,y} -x^4+6*y^2");
            cfvector fv(sa1);

            string_array sa2;
            sa2.push_back ("{x,y} 9*y^2");
            sa2.push_back ("{x,y} (-10)*y");
            sa2.push_back ("{x,y} 12*y");
            cfmatrix fmcheck(3,1,sa2);

            cfmatrix fm = fv.jacobian(1);
            CheckBool    (fmcheck == fm, true, "cfmatrix jacobi, nfrom", os, __LINE__);

            fv.jacobian(fm, 1);
            CheckBool    (fmcheck == fm, true, "cfmatrix jacobi, nfrom", os, __LINE__);
        }

        // cfmatrix jacobi nfrom, nsize
        {
            string_array sa1;
            sa1.push_back ("{x,y,z} 2*x^2+3*y^3+z/2");
            sa1.push_back ("{x,y,z} 4*x^3-5*y^2-sin(z)");
            sa1.push_back ("{x,y,z} -x^4+6*y^2+z^(.5)");
            cfvector fv(sa1);

            string_array sa2;
            sa2.push_back ("{x,y,z} 4*x");
            sa2.push_back ("{x,y,z} 12*x^2");
            sa2.push_back ("{x,y,z} (-4)*x^3");
            sa2.push_back ("{x,y,z} 9*y^2");
            sa2.push_back ("{x,y,z} (-10)*y");
            sa2.push_back ("{x,y,z} 12*y");
            cfmatrix fmcheck(3,2,sa2);

            cfmatrix fm = fv.jacobian(0, 2);
            CheckBool    (fmcheck == fm, true, "cfmatrix jacobi, nfrom, nsize", os, __LINE__);

            fv.jacobian(fm, 0, 2);
            CheckBool    (fmcheck == fm, true, "cfmatrix jacobi, nfrom, nsize", os, __LINE__);
        }

        // rfvector indexing, 0-based
        {
            char s1[] = "{x,z} sign(x+2)";
            char s2[] = "{x,z} z+3";
            string_array sa;
            sa.push_back (s1);
            sa.push_back (s2);

            rfvector fa(sa);
            rfunction f1(s1);
            rfunction f2(s2);

            CheckBool    (fa[0] == f1, true, "rfvector indexing", os, __LINE__);
            CheckBool    (fa[1] == f2, true, "rfvector indexing", os, __LINE__);
        }

        // cfvector indexing, 0-based
        {
            char s1[] = "{x,z} sign(x+(2,3))";
            char s2[] = "{x,z} z+(3,-1.6)";
            string_array sa;
            sa.push_back (s1);
            sa.push_back (s2);

            cfvector fa(sa);
            cfunction f1(s1);
            cfunction f2(s2);

            CheckBool    (fa[0] == f1, true, "cfvector indexing", os, __LINE__);
            CheckBool    (fa[1] == f2, true, "cfvector indexing", os, __LINE__);
        }

        // rfmatrix indexing
        {
            char s1[] = "{x,y} x";
            char s2[] = "{x,y} y";
            char s3[] = "{x,y} 1";
            char s4[] = "{x,y} 2";
            char s5[] = "{x,y} 3";
            char s6[] = "{x,y} 4";
            string_array sa1;
            sa1.push_back (s1);
            sa1.push_back (s2);
            sa1.push_back (s3);
            sa1.push_back (s4);
            sa1.push_back (s5);
            sa1.push_back (s6);

            rfmatrix fm(2,3,sa1);
            rfunction f2(s2);
            rfunction f3(s3);

            CheckBool    (fm[1] == f2, true, "rfmatrix indexing", os, __LINE__);
            CheckBool    (fm[2] == f3, true, "rfmatrix indexing", os, __LINE__);

            CheckBool    (fm.at(1,0) == f2, true, "rfmatrix indexing", os, __LINE__);
            CheckBool    (fm.at(0,1) == f3, true, "rfmatrix indexing", os, __LINE__);
        }

        // cfmatrix indexing
        {
            char s1[] = "{x,y} x";
            char s2[] = "{x,y} y";
            char s3[] = "{x,y} (1,0)";
            char s4[] = "{x,y} 2";
            char s5[] = "{x,y} 3";
            char s6[] = "{x,y} 4";
            string_array sa1;
            sa1.push_back (s1);
            sa1.push_back (s2);
            sa1.push_back (s3);
            sa1.push_back (s4);
            sa1.push_back (s5);
            sa1.push_back (s6);

            cfmatrix fm(2,3,sa1);
            cfunction f2(s2);
            cfunction f3(s3);

            CheckBool    (fm[1] == f2, true, "cfmatrix indexing", os, __LINE__);
            CheckBool    (fm[2] == f3, true, "cfmatrix indexing", os, __LINE__);

            CheckBool    (fm.at(1,0) == f2, true, "cfmatrix indexing", os, __LINE__);
            CheckBool    (fm.at(0,1) == f3, true, "cfmatrix indexing", os, __LINE__);
        }

        // rfmatrix columns and rows
        {
            char s1[] = "{x,y} x";
            char s2[] = "{x,y} y";
            char s3[] = "{x,y} 1";
            char s4[] = "{x,y} 2";
            char s5[] = "{x,y} 3";
            char s6[] = "{x,y} 4";
            string_array sa1;
            sa1.push_back (s1);
            sa1.push_back (s2);
            sa1.push_back (s3);
            sa1.push_back (s4);
            sa1.push_back (s5);
            sa1.push_back (s6);

            string_array sa1r;
            sa1r.push_back (s2);
            sa1r.push_back (s4);
            sa1r.push_back (s6);

            string_array sa1c;
            sa1c.push_back (s3);
            sa1c.push_back (s4);

            rfmatrix fm(2,3,sa1);
            rfvector fr(sa1r);
            rfvector fc(sa1c);

            CheckBool    (fm.get_row(1) == fr, true, "rfmatrix get_row", os, __LINE__);
            CheckBool    (fm.get_col(1) == fc, true, "rfmatrix get_col", os, __LINE__);

            fr *= 2.;
            fr.simp();
            fm.set_row(1, fr);
            fc *= 2.;
            fc.simp();
            fm.set_col(1, fc);

            CheckString(fm.at(1,0).vformat(), "{x,y} y*2", "rfmatrix set_row", os, __LINE__);
            CheckString(fm.at(1,1).vformat(), "{x,y} 4", "rfmatrix set_col", os, __LINE__);
        }

        // cfmatrix columns and rows
        {
            char s1[] = "{x,y} x";
            char s2[] = "{x,y} y";
            char s3[] = "{x,y} 1";
            char s4[] = "{x,y} 2";
            char s5[] = "{x,y} 3";
            char s6[] = "{x,y} 4";
            string_array sa1;
            sa1.push_back (s1);
            sa1.push_back (s2);
            sa1.push_back (s3);
            sa1.push_back (s4);
            sa1.push_back (s5);
            sa1.push_back (s6);

            string_array sa1r;
            sa1r.push_back (s2);
            sa1r.push_back (s4);
            sa1r.push_back (s6);

            string_array sa1c;
            sa1c.push_back (s3);
            sa1c.push_back (s4);

            cfmatrix fm(2,3,sa1);
            cfvector fr(sa1r);
            cfvector fc(sa1c);

            CheckBool    (fm.get_row(1) == fr, true, "cfmatrix get_row", os, __LINE__);
            CheckBool    (fm.get_col(1) == fc, true, "cfmatrix get_col", os, __LINE__);

            fr *= 2.;
            fr.simp();
            fm.set_row(1, fr);
            fc *= 2.;
            fc.simp();
            fm.set_col(1, fc);

            CheckString(fm.at(1,0).vformat(), "{x,y} y*(2,0)", "cfmatrix set_row", os, __LINE__);
            CheckString(fm.at(1,1).vformat(), "{x,y} (4,0)", "cfmatrix set_col", os, __LINE__);
        }


        // rfvector * /
        {
            char s1[] = "{x} 1";
            char s2[] = "{x} 2";
            char s3[] = "{x} 3";
            char s4[] = "{x} 4";
            string_array sa1;
            sa1.push_back (s1);
            sa1.push_back (s2);
            sa1.push_back (s3);
            sa1.push_back (s4);

            rfvector fv(sa1);
            rfunction f("{x} 5");

            fv *= f;
            fv.simp();
            CheckBool    (fv[1] == (f * 2.).simp(), true, "rfvector * rfunction", os, __LINE__);

            fv /= f;
            fv.simp();
            CheckBool    (fv[0] == (f / 5).simp(), true, "rfvector / rfunction", os, __LINE__);

            fv *= 5.;
            fv.simp();
            CheckBool    (fv[1] == (f * 2.).simp(), true, "rfvector * real", os, __LINE__);

            fv /= 5.;
            fv.simp();
            CheckBool    (fv[0] == (f / 5).simp(), true, "rfvector / real", os, __LINE__);
        }

        // cfvector * /
        {
            char s1[] = "{x} 1";
            char s2[] = "{x} 2";
            char s3[] = "{x} 3";
            char s4[] = "{x} 4";
            string_array sa1;
            sa1.push_back (s1);
            sa1.push_back (s2);
            sa1.push_back (s3);
            sa1.push_back (s4);

            cfvector fv(sa1);
            cfunction f("{x} 5");

            fv *= f;
            fv.simp();
            CheckBool    (fv[1] == (f * 2.).simp(), true, "cfvector * cfunction", os, __LINE__);

            fv /= f;
            fv.simp();
            CheckBool    (fv[0] == (f / 5.).simp(), true, "cfvector / cfunction", os, __LINE__);

            fv *= std::complex<treal>(5.,0.);
            fv.simp();
            CheckBool    (fv[1] == (f * 2.).simp(), true, "cfvector * complex", os, __LINE__);

            fv /= std::complex<treal>(5.,0.);
            fv.simp();
            CheckBool    (fv[0] == (f / 5).simp(), true, "cfvector / complex", os, __LINE__);
        }


        // rfmatrix * /
        {
            char s1[] = "{x} 1";
            char s2[] = "{x} 2";
            char s3[] = "{x} 3";
            char s4[] = "{x} 4";
            string_array sa1;
            sa1.push_back (s1);
            sa1.push_back (s2);
            sa1.push_back (s3);
            sa1.push_back (s4);

            rfmatrix fm(2,2,sa1);
            rfunction f("{x} 5");

            fm *= f;
            fm.simp();

            CheckBool    (fm.at(1,0) == (f * 2.).simp(), true, "rfmatrix * rfunction", os, __LINE__);

            fm /= f;
            fm.simp();
            CheckBool    (fm.at(0,0) == (f / 5).simp(), true, "rfmatrix / rfunction", os, __LINE__);

            fm *= 5.;
            fm.simp();
            CheckBool    (fm.at(1,0) == (f * 2.).simp(), true, "rfmatrix * real", os, __LINE__);

            fm /= 5.;
            fm.simp();
            CheckBool    (fm.at(0,0) == (f / 5).simp(), true, "rfmatrix / real", os, __LINE__);
        }

        // cfmatrix * /
        {
            char s1[] = "{x} 1";
            char s2[] = "{x} 2";
            char s3[] = "{x} 3";
            char s4[] = "{x} 4";
            string_array sa1;
            sa1.push_back (s1);
            sa1.push_back (s2);
            sa1.push_back (s3);
            sa1.push_back (s4);

            cfmatrix fm(2,2,sa1);
            cfunction f("{x} 5");

            fm *= f;
            fm.simp();
            CheckBool    (fm.at(1,0) == (f * 2.).simp(), true, "cfmatrix * cfunction", os, __LINE__);

            fm /= f;
            fm.simp();
            CheckBool    (fm.at(0,0) == (f / 5).simp(), true, "cfmatrix / cfunction", os, __LINE__);

            fm *= std::complex<treal>(5.,0.);
            fm.simp();
            CheckBool    (fm.at(1,0) == (f * 2.).simp(), true, "cfmatrix * complex", os, __LINE__);

            fm /= std::complex<treal>(5.,0.);
            fm.simp();
            CheckBool    (fm.at(0,0) == (f / 5).simp(), true, "cfmatrix / complex", os, __LINE__);
        }

#endif  // !defined(__MINGW32__)

        // 8.1 CVM_SIZESMISMATCH
        {
            char s1[] = "{x} 1";
            char s2[] = "{x} 2";
            char s3[] = "{x} 3";
            char s4[] = "{x} 4";
            string_array sa1;
            sa1.push_back(s1);
            sa1.push_back(s2);
            sa1.push_back(s3);
            sa1.push_back(s4);

            rfvector fv(sa1);
            rfvector fv2(2);

            bool bThrown = false;
            try
            {
                fv2 = fv;
            }
            catch (cvmexception e)
            {
                if (e.cause() == CVM_SIZESMISMATCH) bThrown = true;
            }
            CheckBool(bThrown, true, "rfvector CVM_SIZESMISMATCH", os, __LINE__);
        }

#endif
