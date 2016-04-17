#if 0
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
