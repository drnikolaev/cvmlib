#if 0
        {
            // Gantmaher, p. 33
            rmatrix mA(3,4);
            mA(CVM0,CVM0) =  1.; mA(CVM0,CVM0+1) = -1.; mA(CVM0,CVM0+2) =  2.; mA(CVM0,CVM0+3) =  0.;
            mA(CVM0+1,CVM0) = -1.; mA(CVM0+1,CVM0+1) =  2.; mA(CVM0+1,CVM0+2) = -3.; mA(CVM0+1,CVM0+3) =  1.;
            mA(CVM0+2,CVM0) =  0.; mA(CVM0+2,CVM0+1) =  1.; mA(CVM0+2,CVM0+2) = -1.; mA(CVM0+2,CVM0+3) =  1.;

            // lower rank case
            rmatrix mX = mA.pinv(dPessimisticSp);
            CheckReal ((mA * mX * mA - mA).norm2(), (treal) 0.,  "pinv, lower rank, m < n", os, __LINE__, dPessimisticSp);

            // m > n
            mA.transpose();
            rmatrix mX2 = mA.pinv(dPessimisticSp);
            CheckReal ((mA * mX2 * mA - mA).norm2(), (treal) 0.,  "pinv, lower rank, m > n", os, __LINE__, dPessimisticSp);

            // full rank case
            mA.transpose();
            mA(CVM0,CVM0+2) = 4.;
            mX.pinv (mA);
            CheckReal ((mA * mX * mA - mA).norm2(), (treal) 0.,  "pinv, full rank, m < n", os, __LINE__, dPessimisticSp);

            // m > n
            mA.transpose();
            mX2.pinv(mA);
            CheckReal ((mA * mX2 * mA - mA).norm2(), (treal) 0.,  "pinv, full rank, m > n", os, __LINE__, dPessimisticSp);
        }

        {
            cmatrix mA(3,4), mX(4,3);
            mA.randomize_real(-2., 11.);
            mA.randomize_imag(-9., 7.);

            mX.pinv (mA);
            CheckReal ((mA * mX * mA - mA).norm2(), (treal) 0.,  "complex pinv, m < n", os, __LINE__, dVeryPessimisticSp);

            // m > n
            mA.conj();
            cmatrix mX2 = mA.pinv();
            CheckReal ((mA * mX2 * mA - mA).norm2(), (treal) 0.,  "complex pinv, m > n", os, __LINE__, dPessimisticSp);
        }

        {
            srbmatrix mA (40, 1, 2);
            mA.diag(0).randomize(-1.,1.);
            mA.diag(-1).randomize(-1.,1.);
            mA.diag(1).randomize(-1.,1.);

            rmatrix mX = mA.pinv(dPessimisticSp);
            CheckReal ((mA * mX * mA - mA).norm2(), (treal) 0.,  "srbmatrix pinv", os, __LINE__, dVeryPessimisticSp);

            mA.transpose();
            mX.pinv (mA);
            CheckReal ((mA * mX * mA - mA).norm2(), (treal) 0.,  "srbmatrix pinv", os, __LINE__, dVeryPessimisticSp);
        }

        {
            scbmatrix mA (40, 1, 2);
            mA.diag(0).randomize_real(-1.,1.);
            mA.diag(-1).randomize_real(-1.,1.);
            mA.diag(1).randomize_real(-1.,1.);
            mA.diag(0).randomize_imag(-1.,2.);
            mA.diag(-1).randomize_imag(-1.,2.);
            mA.diag(1).randomize_imag(-1.,2.);

            scmatrix mX = mA.pinv(dPessimisticSp);
            CheckReal ((mA * mX * mA - mA).norm2(), (treal) 0.,  "scbmatrix pinv", os, __LINE__, dVeryPessimisticSp);

            mA.conj();
            mX.pinv (mA);
            CheckReal ((mA * mX * mA - mA).norm2(), (treal) 0.,  "scbmatrix pinv", os, __LINE__, dPessimisticSp);
        }

        // 5.4.1
        {
            treal m[] = {1., -1., 1., 2., -2., 1.,
                          3., -2., 1., 0., -2., 1.};
            rmatrix mA(m,4,3);
            rmatrix mSigma(4,3);
            rvector v(3);
            srmatrix mU(4), mVH(3);
            v.svd(mA, mU, mVH);
            mSigma.diag(0) = v;
            CheckReal ((mA * ~mVH - mU * mSigma).norm(), 0.,  "rmatrix svd",  os, __LINE__, dPessimisticSp);
            CheckReal ((~mA * mU - ~(mSigma * mVH)).norm(), 0.,  "rmatrix svd",  os, __LINE__, dPessimisticSp);
        }

        {
            treal m[] = {1., -1., 1., 2., -2., 1.,
                          3., -2., 1., 0., -2., 1.};
            cmatrix mA((std::complex<treal>*) m, 2, 3);
            cmatrix mSigma(2,3);
            rvector v(2);
            scmatrix mU(2), mVH(3);

            v = mA.svd(mU, mVH);
            mSigma.diag(0) = cvector(v);
            CheckReal ((mA * ~mVH - mU * mSigma).norm(), 0.,  "rmatrix svd",  os, __LINE__, dPessimisticSp);
            CheckReal ((~mA * mU - ~(mSigma * mVH)).norm(), 0.,  "rmatrix svd",  os, __LINE__, dPessimisticSp);
        }


        {
            cvm::srmatrix s(9);
            cvm::srbmatrix m(3,0,1);
            for (int i = 1; i <= 9; ++i) {
                s[i - (1 - CVM0)].set((treal)i);
            }
            m.assign(s(CVM0+8));     // should be 1,2,..9
            CheckReal (m(CVM0,CVM0), (treal) 2., "rmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0,CVM0+1), (treal) 3., "rmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0+2,CVM0+2), (treal) 6., "rmatrix.assign(vector)",  os, __LINE__);
            m.assign(s[CVM0+8]);     // should be 9,9,..9
            CheckReal (m(CVM0,CVM0), (treal) 9., "rmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0,CVM0+1), (treal) 9., "rmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0+2,CVM0+2), (treal) 9., "rmatrix.assign(vector)",  os, __LINE__);
        }

        {
            cvm::scmatrix s(9);
            cvm::scbmatrix m(3,1,1);
            for (int i = 1; i <= 9; ++i) {
                s[i - (1 - CVM0)].set(tcomplex((treal)i,(treal)-i));
            }
            m.assign(s(CVM0+8));     // should be 1,2,..9
            CheckComplex (m(CVM0,CVM0), tcomplex((treal) 2., (treal) -2.), "scbmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0,CVM0+1), tcomplex((treal) 4., (treal) -4.), "scbmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex((treal) 8., (treal) -8.), "scbmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0,CVM0+2), tcomplex((treal) 0., (treal) 0.),  "scbmatrix.assign(vector)",  os, __LINE__);
            m.assign(s[CVM0+8]);     // should be 9,9,..9
            CheckComplex (m(CVM0,CVM0), tcomplex((treal) 9., (treal) -9.), "scbmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0,CVM0+1), tcomplex((treal) 9., (treal) -9.), "scbmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex((treal) 9., (treal) -9.), "scbmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0,CVM0+2), tcomplex((treal) 0., (treal) 0.),  "scbmatrix.assign(vector)",  os, __LINE__);
        }


        {
            cvm::srmatrix s(9);
            cvm::rmatrix mbig(30,30);
            cvm::rmatrix m(mbig,4,7,3,3);
            for (int i = 1; i <= 9; ++i) {
                s[i - (1 - CVM0)].set((treal)i);
            }
            m.assign(s(CVM0+8));     // should be 1,2,..9
            CheckReal (m(CVM0,CVM0), (treal) 1., "rmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0,CVM0+1), (treal) 4., "rmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0+2,CVM0+2), (treal) 9., "rmatrix.assign(vector)",  os, __LINE__);
            m.assign(s[CVM0+8]);     // should be 9,9,..9
            CheckReal (m(CVM0,CVM0), (treal) 9., "rmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0,CVM0+1), (treal) 9., "rmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0+2,CVM0+2), (treal) 9., "rmatrix.assign(vector)",  os, __LINE__);
        }

        {
            cvm::srmatrix s(9);
            cvm::srmatrix mbig(20);
            cvm::srmatrix m(mbig,4,7,3);
            for (int i = 1; i <= 9; ++i) {
                s[i - (1 - CVM0)].set((treal)i);
            }
            m.assign(s(CVM0+8));     // should be 1,2,..9
            CheckReal (m(CVM0,CVM0), (treal) 1., "srmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0,CVM0+1), (treal) 4., "srmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0+2,CVM0+2), (treal) 9., "srmatrix.assign(vector)",  os, __LINE__);
            m.assign(s[CVM0+8]);     // should be 9,9,..9
            CheckReal (m(CVM0,CVM0), (treal) 9., "srmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0,CVM0+1), (treal) 9., "srmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0+2,CVM0+2), (treal) 9., "srmatrix.assign(vector)",  os, __LINE__);
        }

        {
            cvm::scmatrix s(9);
            cvm::cmatrix mbig(30,30);
            cvm::cmatrix m(mbig,4,7,3,3);
            for (int i = 1; i <= 9; ++i) {
                s[i - (1 - CVM0)].set(tcomplex((treal)i,(treal)-i));
            }
            m.assign(s(CVM0+8));     // should be 1,2,..9
            CheckComplex (m(CVM0,CVM0), tcomplex((treal) 1., (treal) -1.), "cmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0,CVM0+1), tcomplex((treal) 4., (treal) -4.), "cmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex((treal) 9., (treal) -9.), "cmatrix.assign(vector)",  os, __LINE__);
            m.assign(s[CVM0+8]);     // should be 9,9,..9
            CheckComplex (m(CVM0,CVM0), tcomplex((treal) 9., (treal) -9.), "cmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0,CVM0+1), tcomplex((treal) 9., (treal) -9.), "cmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex((treal) 9., (treal) -9.), "cmatrix.assign(vector)",  os, __LINE__);
        }

        {
            cvm::scmatrix s(9);
            cvm::scmatrix mbig(20);
            cvm::scmatrix m(mbig,4,7,3);
            for (int i = 1; i <= 9; ++i) {
                s[i - (1 - CVM0)].set(tcomplex((treal)i,(treal)-i));
            }
            m.assign(s(CVM0+8));     // should be 1,2,..9
            CheckComplex (m(CVM0,CVM0), tcomplex((treal) 1., (treal) -1.), "scmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0,CVM0+1), tcomplex((treal) 4., (treal) -4.), "scmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex((treal) 9., (treal) -9.), "scmatrix.assign(vector)",  os, __LINE__);
            m.assign(s[CVM0+8]);     // should be 9,9,..9
            CheckComplex (m(CVM0,CVM0), tcomplex((treal) 9., (treal) -9.), "scmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0,CVM0+1), tcomplex((treal) 9., (treal) -9.), "scmatrix.assign(vector)",  os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex((treal) 9., (treal) -9.), "scmatrix.assign(vector)",  os, __LINE__);
        }

        {
            cvm::srmatrix s(9);
            cvm::srsmatrix m(3);
            s(CVM0+8,CVM0+1) = (treal) 1.;
            s(CVM0+8,CVM0+3) = (treal) 1.;
            s(CVM0+8,CVM0+8) = (treal) 5.;
            m.assign(s[CVM0+8]);
            CheckReal (m(CVM0,CVM0), (treal) 0., "srsmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0,CVM0+1), (treal) 1., "srsmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0+1,CVM0), (treal) 1., "srsmatrix.assign(vector)",  os, __LINE__);
            CheckReal (m(CVM0+2,CVM0+2), (treal) 5., "srsmatrix.assign(vector)",  os, __LINE__);
        }


        // 5.4.2
        {
            // bug fix check
            cmatrix a(3,4);
            CheckInt (a.ld(), 3, "a.ld()", os, __LINE__);
            a.resize(0,0);
            CheckInt (a.ld(), 0, "a.ld()", os, __LINE__);
        }
        {
            std::vector<cvector> vcv;
            vcv.reserve(5);
            vcv.push_back(cvector(10));
            vcv.push_back(cvector());
            vcv[0][CVM0] = tcomplex((treal) 1., (treal) -1.);
            CheckComplex (vcv[0](CVM0), tcomplex((treal) 1., (treal) -1.), "std::vector<cvector>[][]",  os, __LINE__);
        }
        {
            std::vector<cmatrix> vcm;
            vcm.reserve(5);
            vcm.push_back(cmatrix(10,20));
            vcm[0][CVM0][CVM0+1] = tcomplex((treal) 1., (treal) -1.);
            CheckComplex (vcm[0](CVM0,CVM0+1), tcomplex((treal) 1., (treal) -1.), "std::vector<cmatrix>[][][]",  os, __LINE__);
        }
        {
            std::vector<rmatrix> vcm;
            vcm.reserve(5);
            vcm.push_back(srmatrix(10));
            vcm.push_back(srmatrix());
            vcm[0][CVM0][CVM0+1] = (treal) 7.77;
            CheckReal (vcm[0](CVM0,CVM0+1), (treal) 7.77, "std::vector<srmatrix>[][][]",  os, __LINE__);
        }

        // 5.5 QR stuff
        {
            treal a[] = {1., 2., 3., 4., 5., 6.};
            const cvm::rmatrix mh(a, 2, 3);
            const cvm::rmatrix mv(a, 3, 2);
            cvm::srmatrix s2(2), s3(3);
            cvm::rmatrix h(2,3), v(3,2);

            mh.qr(h,s3);
            CheckReal ((eye_real(2) - ~rmatrix(h,CVM0,CVM0,2,2) * rmatrix(h,CVM0,CVM0,2,2)).norm(), (treal) 0., "cvm::rmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - h * s3).norm(), (treal) 0., "cvm::rmatrix QR",  os, __LINE__, dPessimisticSp);
            mh.qr(s2,h);
            CheckReal ((eye_real(2) - ~s2 * s2).norm(), (treal) 0., "cvm::rmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - s2 * h).norm(), (treal) 0., "cvm::rmatrix QR",  os, __LINE__, dPessimisticSp);
            mv.qr(v,s2);
            CheckReal ((eye_real(2) - ~v * v).norm(), (treal) 0., "cvm::rmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((mv - v * s2).norm(), (treal) 0., "cvm::rmatrix QR",  os, __LINE__, dPessimisticSp);
            mv.qr(s3,v);
            CheckReal ((eye_real(3) - ~s3 * s3).norm(), (treal) 0., "cvm::rmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((mv - s3 * v).norm(), (treal) 0., "cvm::rmatrix QR",  os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
            const cvm::srmatrix m(a, 3);
            cvm::srmatrix q(3), r(3);

            m.qr(q,r);
            CheckReal ((eye_real(3) - ~q * q).norm(), (treal) 0., "cvm::srmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((m - q * r).norm(), (treal) 0., "cvm::srmatrix QR",  os, __LINE__, dPessimisticSp);
        }

        {
            treal ar[] = {1., 2., 3., 4., 5., 6.};
            treal ai[] = {1., -1., 2., -2., 3., -3.};
            const cvm::cmatrix mh(ar, ai, 2, 3);
            const cvm::cmatrix mv(ar, ai, 3, 2);
            cvm::scmatrix s2(2), s3(3);
            cvm::cmatrix  h(2,3), v(3,2);

            mh.qr(h,s3);
            CheckReal ((eye_complex(2) - ~cmatrix(h,CVM0,CVM0,2,2) * cmatrix(h,CVM0,CVM0,2,2)).norm(), (treal) 0., "cvm::cmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - h * s3).norm(), (treal) 0., "cvm::cmatrix QR",  os, __LINE__, dPessimisticSp);

            mh.qr(s2,h);
            CheckReal ((eye_complex(2) - ~s2 * s2).norm(), (treal) 0., "cvm::cmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - s2 * h).norm(), (treal) 0., "cvm::cmatrix QR",  os, __LINE__, dPessimisticSp);

            mv.qr(v,s2);
            CheckReal ((eye_complex(2) - ~v * v).norm(), (treal) 0., "cvm::cmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((mv - v * s2).norm(), (treal) 0., "cvm::cmatrix QR",  os, __LINE__, dPessimisticSp);

            mv.qr(s3,v);
            CheckReal ((eye_complex(3) - ~s3 * s3).norm(), (treal) 0., "cvm::cmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((mv - s3 * v).norm(), (treal) 0., "cvm::cmatrix QR",  os, __LINE__, dPessimisticSp);
        }
        {
            treal ar[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
            treal ai[] = {1., -1., 2., -2., 3., -3., 4., -4., 5.};
            const cvm::scmatrix m(ar, ai, 3);
            cvm::scmatrix q(3), r(3);

            m.qr(q,r);
            CheckReal ((eye_complex(3) - ~q * q).norm(), (treal) 0., "cvm::scmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((m - q * r).norm(), (treal) 0., "cvm::scmatrix QR",  os, __LINE__, dPessimisticSp);
        }

        {
            treal a[] = {1., 4., 7., 2., 5., 8., 3., 6., 0.};
            cvm::srmatrix A(a, 3);
            cvm::srmatrix Q(3);
            cvm::srmatrix R(3);

            A.qr(Q,R);
            CheckReal ((A - Q * R).norm(), (treal) 0., "cvm::srmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(3) - ~Q * Q).norm(), (treal) 0., "cvm::srmatrix QR - Q",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            srbmatrix A (m, 2, 3);
            A.randomize(-10., 10.);
            cvm::srmatrix Q(m);
            cvm::srmatrix R(m);

            A.qr(Q,R);
            CheckReal ((Q * R - A).norm(), (treal) 0., "cvm::srbmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(m) - ~Q * Q).norm(), (treal) 0., "cvm::srbmatrix QR - Q",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            srsmatrix A (m);
            A.randomize(-10., 10.);
            cvm::srmatrix Q(m);
            cvm::srmatrix R(m);

            A.qr(Q,R);
            CheckReal ((Q * R - A).norm(), (treal) 0., "cvm::srsmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(m) - ~Q * Q).norm(), (treal) 0., "cvm::srsmatrix QR - Q",  os, __LINE__, dPessimisticSp);
        }

        {
            const int m = 10;
            scmatrix A (m);
            A.randomize_real(-10., 10.);
            A.randomize_imag(-10., 10.);
            cvm::scmatrix Q(m);
            cvm::scmatrix R(m);

            A.qr(Q,R);
            CheckReal ((Q * R - A).norm(), (treal) 0., "cvm::scmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(m) - ~Q * Q).norm(), (treal) 0., "cvm::scmatrix QR - Q",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            scbmatrix A (m,2,3);
            A.randomize_real(-10., 10.);
            A.randomize_imag(-10., 10.);
            cvm::scmatrix Q(m);
            cvm::scmatrix R(m);

            A.qr(Q,R);
            CheckReal ((Q * R - A).norm(), (treal) 0., "cvm::scbmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(m) - ~Q * Q).norm(), (treal) 0., "cvm::scbmatrix QR - Q",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            schmatrix A (m);
            A.randomize_real(-10., 10.);
            A.randomize_imag(-10., 10.);
            cvm::scmatrix Q(m);
            cvm::scmatrix R(m);

            A.qr(Q,R);
            CheckReal ((Q * R - A).norm(), (treal) 0., "cvm::schmatrix QR",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(m) - ~Q * Q).norm(), (treal) 0., "cvm::schmatrix QR - Q",  os, __LINE__, dPessimisticSp);
        }

        // Case 1: economy mode, A is (m x n) and Q is (m x n) and R is (n x n)
        {
            int m = 10;
            int n = 5;
            cvm::rmatrix A(m,n);
            A.randomize(-10.0, 10.0);

            cvm::rmatrix Q(m,n);
            cvm::srmatrix R(n);
            A.qr(Q,R);

            CheckReal ((Q * R - A).norm(), (treal) 0., "cvm::rmatrix QR economy",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(n) - ~Q * Q).norm(), (treal) 0., "cvm::rmatrix QR - Q economy",  os, __LINE__, dPessimisticSp);
        }
        // Case 2: full mode, A is (m x n) and Q is (m x m) and R is (m x n)
        {
            int m = 10;
            int n = 5;
            cvm::rmatrix A(m,n);
            A.randomize(-10.0, 10.0);

            cvm::srmatrix Q(m);
            cvm::rmatrix R(m,n);
            A.qr(Q,R);

            CheckReal ((Q * R - A).norm(), (treal) 0., "cvm::rmatrix QR full",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(m) - ~Q * Q).norm(), (treal) 0., "cvm::rmatrix QR - Q full",  os, __LINE__, dPessimisticSp);
        }

        // Case 1: economy mode, A is (m x n) and Q is (m x n) and R is (n x n)
        {
            int m = 10;
            int n = 5;
            cvm::cmatrix A(m,n);
            A.randomize_real(-10.0, 10.0);
            A.randomize_imag(-10.0, 10.0);

            cvm::cmatrix Q(m,n);
            cvm::scmatrix R(n);
            A.qr(Q,R);

            CheckReal ((Q * R - A).norm(), (treal) 0., "cvm::cmatrix QR economy",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(n) - ~Q * Q).norm(), (treal) 0., "cvm::cmatrix QR - Q economy",  os, __LINE__, dPessimisticSp);
        }
        // Case 2: full mode, A is (m x n) and Q is (m x m) and R is (m x n)
        {
            int m = 10;
            int n = 5;
            cvm::cmatrix A(m,n);
            A.randomize_real(-10.0, 10.0);
            A.randomize_imag(-10.0, 10.0);

            cvm::scmatrix Q(m);
            cvm::cmatrix R(m,n);
            A.qr(Q,R);

            CheckReal ((Q * R - A).norm(), (treal) 0., "cvm::cmatrix QR full",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(m) - ~Q * Q).norm(), (treal) 0., "cvm::cmatrix QR - Q full",  os, __LINE__, dPessimisticSp);
        }



        // 6.0 RQ stuff
        {
            cvm::rmatrix mh(2, 3);
            cvm::rmatrix mv(3, 2);
            mh.randomize(-10., 10.);
            mv.randomize(-10., 10.);
            cvm::srmatrix s2(2), s3(3);
            cvm::rmatrix  h(2,3), v(3,2);

            mh.rq(h,s3);
            CheckReal ((eye_real(2) - rmatrix(s3,CVM0+1,CVM0,2,3) * ~rmatrix(s3,CVM0+1,CVM0,2,3)).norm(), (treal) 0., "cvm::rmatrix RQ (full mode)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - h * s3).norm(), (treal) 0., "cvm::rmatrix RQ (full mode)",  os, __LINE__, dPessimisticSp);

            mh.rq(s2,h);
            CheckReal ((eye_real(2) - h * ~h).norm(), (treal) 0., "cvm::rmatrix RQ (economy mode)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - s2 * h).norm(), (treal) 0., "cvm::rmatrix RQ (economy mode)",  os, __LINE__, dPessimisticSp);
        }
        {
            cvm::srmatrix m(3);
            m.randomize(-100., 100.);
            cvm::srmatrix q(3), r(3);

            m.rq(r,q);
            CheckReal ((eye_real(3) - ~q * q).norm(), (treal) 0., "cvm::srmatrix RQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((m - r * q).norm(), (treal) 0., "cvm::srmatrix RQ",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            srbmatrix A (m, 2, 3);
            A.randomize(-10., 10.);
            cvm::srmatrix Q(m);
            cvm::srmatrix R(m);

            A.rq(R,Q);
            CheckReal ((R * Q - A).norm(), (treal) 0., "cvm::srbmatrix RQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(m) - ~Q * Q).norm(), (treal) 0., "cvm::srbmatrix RQ - Q",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            srsmatrix A (m);
            A.randomize(-10., 10.);
            cvm::srmatrix Q(m);
            cvm::srmatrix R(m);

            A.rq(R,Q);
            CheckReal ((R * Q - A).norm(), (treal) 0., "cvm::srsmatrix RQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(m) - ~Q * Q).norm(), (treal) 0., "cvm::srsmatrix RQ - Q",  os, __LINE__, dPessimisticSp);
        }


        // 6.0 RQ stuff (complex)
        {
            cvm::cmatrix mh(2, 3);
            cvm::cmatrix mv(3, 2);
            mh.randomize_real(-10.0, 10.0);
            mh.randomize_imag(-10.0, 10.0);
            mv.randomize_real(-10.0, 10.0);
            mv.randomize_imag(-10.0, 10.0);
            cvm::scmatrix s2(2), s3(3);
            cvm::cmatrix  h(2,3), v(3,2);

            mh.rq(h,s3);
            CheckReal ((eye_complex(2) - cmatrix(s3,CVM0+1,CVM0,2,3) * ~cmatrix(s3,CVM0+1,CVM0,2,3)).norm(), (treal) 0., "cvm::cmatrix RQ (full mode)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - h * s3).norm(), (treal) 0., "cvm::cmatrix RQ (full mode)",  os, __LINE__, dPessimisticSp);

            mh.rq(s2,h);
            CheckReal ((eye_complex(2) - h * ~h).norm(), (treal) 0., "cvm::cmatrix RQ (economy mode)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - s2 * h).norm(), (treal) 0., "cvm::cmatrix RQ (economy mode)",  os, __LINE__, dPessimisticSp);
        }
        {
            cvm::scmatrix m(3);
            m.randomize_real(-10.0, 10.0);
            m.randomize_imag(-10.0, 10.0);
            cvm::scmatrix q(3), r(3);

            m.rq(r,q);
            CheckReal ((eye_complex(3) - ~q * q).norm(), (treal) 0., "cvm::scmatrix RQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((m - r * q).norm(), (treal) 0., "cvm::scmatrix RQ",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            scbmatrix A (m,2,3);
            A.randomize_real(-10., 10.);
            A.randomize_imag(-10., 10.);
            cvm::scmatrix Q(m);
            cvm::scmatrix R(m);

            A.rq(R,Q);
            CheckReal ((R * Q - A).norm(), (treal) 0., "cvm::scbmatrix RQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(m) - ~Q * Q).norm(), (treal) 0., "cvm::scbmatrix RQ - Q",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            schmatrix A (m);
            A.randomize_real(-10., 10.);
            A.randomize_imag(-10., 10.);
            cvm::scmatrix Q(m);
            cvm::scmatrix R(m);

            A.rq(R,Q);
            CheckReal ((R * Q - A).norm(), (treal) 0., "cvm::schmatrix RQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(m) - ~Q * Q).norm(), (treal) 0., "cvm::schmatrix RQ - Q",  os, __LINE__, dPessimisticSp);
        }

        // 6.0 LQ stuff
        {
            treal a[] = {1., 2., 3., 4., 5., 6.};
            const cvm::rmatrix mh(a, 2, 3);
            const cvm::rmatrix mv(a, 3, 2);
            cvm::srmatrix s2(2), s3(3);
            cvm::rmatrix h(2,3), v(3,2);

            mh.lq(s2,h);
            CheckReal ((eye_real(2) - h * ~h).norm(), (treal) 0., "cvm::rmatrix LQ (economy)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - s2 * h).norm(), (treal) 0., "cvm::rmatrix LQ (economy)",  os, __LINE__, dPessimisticSp);

            mv.lq(s3,v);
            CheckReal ((eye_real(2) - ~rmatrix(v,CVM0,CVM0,2,2) * rmatrix(v,CVM0,CVM0,2,2)).norm(), (treal) 0., "cvm::rmatrix LQ (economy)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mv - s3 * v).norm(), (treal) 0., "cvm::rmatrix LQ (economy)",  os, __LINE__, dPessimisticSp);

            mh.lq(h,s3);
            CheckReal ((eye_real(3) - s3 * ~s3).norm(), (treal) 0., "cvm::rmatrix LQ (full)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - h * s3).norm(), (treal) 0., "cvm::rmatrix LQ (full)",  os, __LINE__, dPessimisticSp);

            mv.lq(v,s2);
            CheckReal ((eye_real(2) - s2 * ~s2).norm(), (treal) 0., "cvm::rmatrix LQ (full)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mv - v * s2).norm(), (treal) 0., "cvm::rmatrix LQ (full)",  os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
            const cvm::srmatrix m(a, 3);
            cvm::srmatrix l(3), q(3);

            m.lq(l,q);
            CheckReal ((eye_real(3) - ~q * q).norm(), (treal) 0., "cvm::srmatrix LQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((m - l * q).norm(), (treal) 0., "cvm::srmatrix LQ",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            srbmatrix A (m, 2, 3);
            A.randomize(-10., 10.);
            cvm::srmatrix L(m);
            cvm::srmatrix Q(m);

            A.lq(L,Q);
            CheckReal ((L * Q - A).norm(), (treal) 0., "cvm::srbmatrix LQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(m) - ~Q * Q).norm(), (treal) 0., "cvm::srbmatrix LQ - Q",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            srsmatrix A (m);
            A.randomize(-10., 10.);
            cvm::srmatrix L(m);
            cvm::srmatrix Q(m);

            A.lq(L,Q);
            CheckReal ((L * Q - A).norm(), (treal) 0., "cvm::srsmatrix LQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(m) - ~Q * Q).norm(), (treal) 0., "cvm::srsmatrix LQ - Q",  os, __LINE__, dPessimisticSp);
        }

        {
            treal ar[] = {1., 2., 3., 4., 5., 6.};
            treal ai[] = {1., -1., 2., -2., 3., -3.};
            const cvm::cmatrix mh(ar, ai, 2, 3);
            const cvm::cmatrix mv(ar, ai, 3, 2);
            cvm::scmatrix s2(2), s3(3);
            cvm::cmatrix  h(2,3), v(3,2);

            mh.lq(s2,h);
            CheckReal ((eye_complex(2) - h * ~h).norm(), (treal) 0., "cvm::cmatrix LQ (economy)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - s2 * h).norm(), (treal) 0., "cvm::cmatrix LQ (economy)",  os, __LINE__, dPessimisticSp);

            mv.lq(s3,v);
            CheckReal ((eye_complex(2) - ~cmatrix(v,CVM0,CVM0,2,2) * cmatrix(v,CVM0,CVM0,2,2)).norm(), (treal) 0., "cvm::cmatrix LQ (economy)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mv - s3 * v).norm(), (treal) 0., "cvm::cmatrix LQ (economy)",  os, __LINE__, dPessimisticSp);

            mh.lq(h,s3);
            CheckReal ((eye_complex(3) - s3 * ~s3).norm(), (treal) 0., "cvm::cmatrix LQ (full)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mh - h * s3).norm(), (treal) 0., "cvm::cmatrix LQ (full)",  os, __LINE__, dPessimisticSp);

            mv.lq(v,s2);
            CheckReal ((eye_complex(2) - s2 * ~s2).norm(), (treal) 0., "cvm::cmatrix LQ (full)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mv - v * s2).norm(), (treal) 0., "cvm::cmatrix LQ (full)",  os, __LINE__, dPessimisticSp);
        }
        {
            treal ar[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
            treal ai[] = {1., -1., 2., -2., 3., -3., 4., -4., 5.};
            const cvm::scmatrix m(ar, ai, 3);
            cvm::scmatrix l(3), q(3);

            m.lq(l,q);
            CheckReal ((eye_complex(3) - ~q * q).norm(), (treal) 0., "cvm::scmatrix LQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((m - l * q).norm(), (treal) 0., "cvm::scmatrix LQ",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            scbmatrix A (m,2,3);
            A.randomize_real(-10., 10.);
            A.randomize_imag(-10., 10.);
            cvm::scmatrix L(m);
            cvm::scmatrix Q(m);

            A.lq(L,Q);
            CheckReal ((L * Q - A).norm(), (treal) 0., "cvm::scbmatrix LQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(m) - ~Q * Q).norm(), (treal) 0., "cvm::scbmatrix LQ - Q",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            schmatrix A (m);
            A.randomize_real(-10., 10.);
            A.randomize_imag(-10., 10.);
            cvm::scmatrix L(m);
            cvm::scmatrix Q(m);

            A.lq(L,Q);
            CheckReal ((L * Q - A).norm(), (treal) 0., "cvm::schmatrix LQ",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(m) - ~Q * Q).norm(), (treal) 0., "cvm::schmatrix LQ - Q",  os, __LINE__, dPessimisticSp);
        }

        // 6.0 QL stuff
        {
            treal a[] = {1., 2., 3., 4., 5., 6.};
            const cvm::rmatrix mv(a, 3, 2);
            cvm::srmatrix s2(2), s3(3);
            cvm::rmatrix v(3,2);

            mv.ql(v,s2);
            CheckReal ((eye_real(2) - ~v * v).norm(), (treal) 0., "cvm::rmatrix QL (economy)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mv - v * s2).norm(), (treal) 0., "cvm::rmatrix QL (economy)",  os, __LINE__, dPessimisticSp);

            mv.ql(s3,v);
            CheckReal ((eye_real(2) - ~rmatrix(s3,CVM0,CVM0+1,3,2) * rmatrix(s3,CVM0,CVM0+1,3,2)).norm(), (treal) 0., "cvm::rmatrix QL (full)", os, __LINE__, dPessimisticSp);
            CheckReal ((mv - s3 * v).norm(), (treal) 0., "cvm::rmatrix QL (full)",  os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
            const cvm::srmatrix m(a, 3);
            cvm::srmatrix l(3), q(3);

            m.ql(q,l);
            CheckReal ((eye_real(3) - ~q * q).norm(), (treal) 0., "cvm::srmatrix QL",  os, __LINE__, dPessimisticSp);
            CheckReal ((m - q * l).norm(), (treal) 0., "cvm::srmatrix QL",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            srbmatrix A (m, 2, 3);
            A.randomize(-10., 10.);
            cvm::srmatrix L(m);
            cvm::srmatrix Q(m);

            A.ql(Q,L);
            CheckReal ((Q * L - A).norm(), (treal) 0., "cvm::srbmatrix QL",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(m) - ~Q * Q).norm(), (treal) 0., "cvm::srbmatrix QL - Q",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            srsmatrix A (m);
            A.randomize(-10., 10.);
            cvm::srmatrix L(m);
            cvm::srmatrix Q(m);

            A.ql(Q,L);
            CheckReal ((Q * L - A).norm(), (treal) 0., "cvm::srsmatrix QL",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_real(m) - ~Q * Q).norm(), (treal) 0., "cvm::srsmatrix QL - Q",  os, __LINE__, dPessimisticSp);
        }

        {
            treal ar[] = {1., 2., 3., 4., 5., 6.};
            treal ai[] = {1., -1., 2., -2., 3., -3.};
            const cvm::cmatrix mv(ar, ai, 3, 2);
            cvm::scmatrix s2(2), s3(3);
            cvm::cmatrix v(3,2);

            mv.ql(v,s2);
            CheckReal ((eye_complex(2) - ~v * v).norm(), (treal) 0., "cvm::cmatrix QL (economy)",  os, __LINE__, dPessimisticSp);
            CheckReal ((mv - v * s2).norm(), (treal) 0., "cvm::cmatrix QL (economy)",  os, __LINE__, dPessimisticSp);

            mv.ql(s3,v);
            CheckReal ((eye_complex(2) - ~cmatrix(s3,CVM0,CVM0+1,3,2) * cmatrix(s3,CVM0,CVM0+1,3,2)).norm(), (treal) 0., "cvm::cmatrix QL (full)", os, __LINE__, dPessimisticSp);
            CheckReal ((mv - s3 * v).norm(), (treal) 0., "cvm::cmatrix QL (full)",  os, __LINE__, dPessimisticSp);
        }

        {
            treal ar[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
            treal ai[] = {1., -1., 2., -2., 3., -3., 4., -4., 5.};
            const cvm::scmatrix m(ar, ai, 3);
            cvm::scmatrix l(3), q(3);

            m.ql(q,l);
            CheckReal ((eye_complex(3) - ~q * q).norm(), (treal) 0., "cvm::scmatrix QL",  os, __LINE__, dPessimisticSp);
            CheckReal ((m - q * l).norm(), (treal) 0., "cvm::scmatrix QL",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            scbmatrix A (m,2,3);
            A.randomize_real(-10., 10.);
            A.randomize_imag(-10., 10.);
            cvm::scmatrix L(m);
            cvm::scmatrix Q(m);

            A.ql(Q,L);
            CheckReal ((Q * L - A).norm(), (treal) 0., "cvm::scbmatrix QL",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(m) - ~Q * Q).norm(), (treal) 0., "cvm::scbmatrix QL - Q",  os, __LINE__, dPessimisticSp);
        }
        {
            const int m = 10;
            schmatrix A (m);
            A.randomize_real(-10., 10.);
            A.randomize_imag(-10., 10.);
            cvm::scmatrix L(m);
            cvm::scmatrix Q(m);

            A.ql(Q,L);
            CheckReal ((Q * L - A).norm(), (treal) 0., "cvm::schmatrix QL",  os, __LINE__, dPessimisticSp);
            CheckReal ((eye_complex(m) - ~Q * Q).norm(), (treal) 0., "cvm::schmatrix QL - Q",  os, __LINE__, dPessimisticSp);
        }

        // 6.0 LLS stuff
        {
            cvm::rmatrix a(7, 5);
            cvm::rmatrix bn(7, 2);
            cvm::rmatrix bt(5, 2);
            rvector vErr(2);
            a.randomize(-1., 1.);
            bn.randomize(-1., 1.);
            bt.randomize(-1., 1.);

            cvm::rmatrix xn = a.gels(false, bn, vErr);
            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix xt = a.gels(true, bt, vErr);
            CheckReal ((~a*xt-bt).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);
            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix xn2(5,2);
            xn2.gels(false, a, bn, vErr);
            CheckReal ((xn-xn2).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);
            cvm::rmatrix xt2(7,2);
            xt2.gels(true, a, bt, vErr);
            CheckReal ((xt-xt2).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);
        }
        {
            cvm::rmatrix a(5, 7);
            cvm::rmatrix bn(5, 2);
            cvm::rmatrix bt(7, 2);
            rvector vErr(2);
            a.randomize(-1., 1.);
            bn.randomize(-1., 1.);
            bt.randomize(-1., 1.);

            cvm::rmatrix xn = a.gels(false, bn, vErr);
            CheckReal ((a*xn-bn).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix xt = a.gels(true, bt, vErr);
            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);
        }
        {
            cvm::srbmatrix a(5,1,0);
            cvm::rmatrix bn(5, 2);
            cvm::rmatrix bt(5, 2);
            rvector vErr(2);
            a.randomize(-1., 1.);
            bn.randomize(-1., 1.);
            bt.randomize(-1., 1.);

            cvm::rmatrix xn = a.gels(false, bn, vErr);
            CheckReal ((a*xn-bn).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix xt = a.gels(true, bt, vErr);
            CheckReal ((~a*xt-bt).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);
        }

        {
            cvm::cmatrix a(7, 5);
            cvm::cmatrix bn(7, 2);
            cvm::cmatrix bt(5, 2);
            cvector vErr(2);
            a.randomize_real(-1., 1.);
            a.randomize_imag(-1., 1.);
            bn.randomize_real(-1., 1.);
            bn.randomize_imag(-1., 1.);
            bt.randomize_real(-1., 1.);
            bt.randomize_imag(-1., 1.);

            cvm::cmatrix xn = a.gels(false, bn, vErr);
            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dPessimisticSp);

            cvm::cmatrix xt = a.gels(true, bt, vErr);
            CheckReal ((~a*xt-bt).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dPessimisticSp);
//            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dPessimisticSp);

            cvm::cmatrix xn2(5,2);
            xn2.gels(false, a, bn, vErr);
            CheckReal ((xn-xn2).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dPessimisticSp);
            cvm::cmatrix xt2(7,2);
            xt2.gels(true, a, bt, vErr);
            CheckReal ((xt-xt2).norm(), (treal) 0., "gels real transp",  os, __LINE__, dPessimisticSp);
        }
        {
            cvm::cmatrix a(5, 7);
            cvm::cmatrix bn(5, 2);
            cvm::cmatrix bt(7, 2);
            cvector vErr(2);
            a.randomize_real(-1., 1.);
            a.randomize_imag(-1., 1.);
            bn.randomize_real(-1., 1.);
            bn.randomize_imag(-1., 1.);
            bt.randomize_real(-1., 1.);
            bt.randomize_imag(-1., 1.);

            cvm::cmatrix xn = a.gels(false, bn, vErr);
            CheckReal ((a*xn-bn).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dPessimisticSp);
//            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dPessimisticSp);

            cvm::cmatrix xt = a.gels(true, bt, vErr);
            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dPessimisticSp);
        }
        {
            cvm::scbmatrix a(5,1,0);
            cvm::cmatrix bn(5, 2);
            cvm::cmatrix bt(5, 2);
            cvector vErr(2);
            a.randomize_real(-1., 1.);
            a.randomize_imag(-1., 1.);
            bn.randomize_real(-1., 1.);
            bn.randomize_imag(-1., 1.);
            bt.randomize_real(-1., 1.);
            bt.randomize_imag(-1., 1.);

            cvm::cmatrix xn = a.gels(false, bn, vErr);
            CheckReal ((a*xn-bn).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dVeryPessimisticSp);

            cvm::cmatrix xt = a.gels(true, bt, vErr);
            CheckReal ((~a*xt-bt).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dVeryPessimisticSp);
        }

        // real vector gels*
        {
            cvm::rmatrix a(7, 5);
            cvm::rvector bn(7);
            cvm::rvector bt(5);
            treal dErr;
            a.randomize(-1., 1.);
            bn.randomize(-1., 1.);
            bt.randomize(-1., 1.);

            cvm::rvector xn = a.gels(false, bn, dErr);
            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dVeryPessimisticSp);

            cvm::rvector xt = a.gels(true, bt, dErr);
            CheckReal ((~a*xt-bt).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);

            cvm::rvector xn2(5);
            xn2.gels(false, a, bn, dErr);
            CheckReal ((xn-xn2).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dPessimisticSp);
            cvm::rvector xt2(7);
            xt2.gels(true, a, bt, dErr);
            CheckReal ((xt-xt2).norm(), (treal) 0., "gels real transp",  os, __LINE__, dPessimisticSp);

            cvm::rvector xy(5);
            tint rank;
            xy.gelsy (a, bn, rank);
            CheckReal ((xy-xn2).norm(), (treal) 0., "gelsy real vector",  os, __LINE__, dPessimisticSp);
            CheckInt (rank, a.rank(), "gelsy real vector rank",  os, __LINE__);

            xy = a.gelsy (bn, rank);
            CheckReal ((xy-xn2).norm(), (treal) 0., "gelsy real vector",  os, __LINE__, dPessimisticSp);
            CheckInt (rank, a.rank(), "gelsy real vector rank",  os, __LINE__);

            cvm::rvector sv(5);

            cvm::rvector xs(5);
            xs.gelss (a, bn, sv, rank);
            CheckReal ((xy-xs).norm(), (treal) 0., "gelss real vector",  os, __LINE__, dPessimisticSp);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss real vector svd",  os, __LINE__, dPessimisticSp);
            CheckInt (rank, a.rank(), "gelss real vector rank",  os, __LINE__);

            cvm::rvector xs2 = a.gelss (bn, sv, rank);
            CheckReal ((xs-xs2).norm(), (treal) 0., "gelss real vector",  os, __LINE__, dPessimisticSp);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss real vector svd",  os, __LINE__, dPessimisticSp);
            CheckInt (rank, a.rank(), "gelss real vector rank",  os, __LINE__);

            cvm::rvector xd(5);
            xd.gelsd (a, bn, sv, rank);
            CheckReal ((xy-xd).norm(), (treal) 0., "gelsd real vector",  os, __LINE__, dPessimisticSp);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelsd real vector svd",  os, __LINE__, dPessimisticSp);
            CheckInt (rank, a.rank(), "gelsd real vector rank",  os, __LINE__);

            cvm::rvector xd2 = a.gelsd (bn, sv, rank);
            CheckReal ((xd-xd2).norm(), (treal) 0., "gelsd real vector",  os, __LINE__, dPessimisticSp);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelsd real vector svd",  os, __LINE__, dPessimisticSp);
            CheckInt (rank, a.rank(), "gelsd real vector rank",  os, __LINE__);
        }
        {
            cvm::rmatrix a(5, 7);
            cvm::rvector bn(5);
            cvm::rvector bt(7);
            treal dErr;
            a.randomize(-1., 1.);
            bn.randomize(-1., 1.);
            bt.randomize(-1., 1.);

            cvm::rvector xn = a.gels(false, bn, dErr);
            CheckReal ((a*xn-bn).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dVeryPessimisticSp);

            cvm::rvector xt = a.gels(true, bt, dErr);
            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);
        }
        {
            cvm::srbmatrix a(5,1,0);
            cvm::rvector bn(5);
            cvm::rvector bt(5);
            treal dErr;
            a.randomize(-1., 1.);
            bn.randomize(-1., 1.);
            bt.randomize(-1., 1.);

            cvm::rvector xn = a.gels(false, bn, dErr);
            CheckReal ((a*xn-bn).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels real nontransp",  os, __LINE__, dVeryPessimisticSp);

            cvm::rvector xt = a.gels(true, bt, dErr);
            CheckReal ((~a*xt-bt).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels real transp",  os, __LINE__, dVeryPessimisticSp);
        }

        // complex vector gels*
        {
            cvm::cmatrix a(7, 5);
            cvm::cvector bn(7);
            cvm::cvector bt(5);
            tcomplex cErr;
            a.randomize_real(-1., 1.);
            a.randomize_imag(-1., 1.);
            bn.randomize_real(-1., 1.);
            bn.randomize_imag(-1., 1.);
            bt.randomize_real(-1., 1.);
            bt.randomize_imag(-1., 1.);

            cvm::cvector xn = a.gels(false, bn, cErr);
            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dVeryPessimisticSp);

            cvm::cvector xt = a.gels(true, bt, cErr);
            CheckReal ((~a*xt-bt).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dPessimisticSp);
//            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dVeryPessimisticSp);

            cvm::cvector xn2(5);
            xn2.gels(false, a, bn, cErr);
            CheckReal ((xn-xn2).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dPessimisticSp);
            cvm::cvector xt2(7);
            xt2.gels(true, a, bt, cErr);
            CheckReal ((xt-xt2).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dPessimisticSp);

            cvm::cvector xy(5);
            tint rank;
            xy.gelsy (a, bn, rank);
            CheckReal ((xy-xn2).norm(), (treal) 0., "gelsy complex vector",  os, __LINE__, dPessimisticSp);
            CheckInt (rank, a.rank(), "gelsy complex vector rank",  os, __LINE__);

            xy = a.gelsy (bn, rank);
            CheckReal ((xy-xn2).norm(), (treal) 0., "gelsy complex vector",  os, __LINE__, dPessimisticSp);
            CheckInt (rank, a.rank(), "gelsy complex vector rank",  os, __LINE__);

            cvm::rvector sv(5);

            cvm::cvector xs(5);
            xs.gelss (a, bn, sv, rank);
            CheckReal ((xy-xs).norm(), (treal) 0., "gelss complex vector",  os, __LINE__, dPessimisticSp);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss complex vector svd",  os, __LINE__, dPessimisticSp);
            CheckInt (rank, a.rank(), "gelss complex vector rank",  os, __LINE__);

            cvm::cvector xd(5);
            xd.gelss (a, bn, sv, rank);
            CheckReal ((xy-xd).norm(), (treal) 0., "gelsd complex vector",  os, __LINE__, dPessimisticSp);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelsd complex vector svd",  os, __LINE__, dPessimisticSp);
            CheckInt (rank, a.rank(), "gelsd complex vector rank",  os, __LINE__);

        }
        {
            cvm::cmatrix a(5, 7);
            cvm::cvector bn(5);
            cvm::cvector bt(7);
            tcomplex cErr;
            a.randomize_real(-1., 1.);
            a.randomize_imag(-1., 1.);
            bn.randomize_real(-1., 1.);
            bn.randomize_imag(-1., 1.);
            bt.randomize_real(-1., 1.);
            bt.randomize_imag(-1., 1.);

            cvm::cvector xn = a.gels(false, bn, cErr);
            CheckReal ((a*xn-bn).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dVeryPessimisticSp);

            cvm::cvector xt = a.gels(true, bt, cErr);
            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dPessimisticSp);
        }
        {
            cvm::scbmatrix a(5,1,0);
            cvm::cvector bn(5);
            cvm::cvector bt(5);
            tcomplex cErr;
            a.randomize_real(-1., 1.);
            a.randomize_imag(-1., 1.);
            bn.randomize_real(-1., 1.);
            bn.randomize_imag(-1., 1.);
            bt.randomize_real(-1., 1.);
            bt.randomize_imag(-1., 1.);

            cvm::cvector xn = a.gels(false, bn, cErr);
            CheckReal ((a*xn-bn).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((a.pinv()*bn - xn).norm(), (treal) 0., "gels complex nontransp",  os, __LINE__, dVeryPessimisticSp);

            cvm::cvector xt = a.gels(true, bt, cErr);
            CheckReal ((~a*xt-bt).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dVeryPessimisticSp);
//            CheckReal ((~a.pinv()*bt - xt).norm(), (treal) 0., "gels complex transp",  os, __LINE__, dVeryPessimisticSp);
        }


        // 6.0 gelsy stuff
        {
            treal ar[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12., 13., 14., 15.};
            cvm::rmatrix a(ar, 3, 5);
            cvm::rmatrix b(ar, 3, 2);
            tint rank;

            cvm::rmatrix x = a.gelsy(b, rank);

            CheckInt (rank, 2, "gelsy real rank", os, __LINE__);
            CheckReal ((a*x - b).norm(), (treal) 0., "gelsy real",  os, __LINE__, dVeryPessimisticSp);
            // see MATLAB's lsqr
            CheckReal (x(CVM0,CVM0), (treal) 0.6, "gelsy real (lsqr)",  os, __LINE__, dVeryPessimisticSp);
            CheckReal (x(CVM0+1,CVM0), (treal) 0.4, "gelsy real (lsqr)",  os, __LINE__, dVeryPessimisticSp);
            CheckReal (x(CVM0+2,CVM0), (treal) 0.2, "gelsy real (lsqr)",  os, __LINE__, dVeryPessimisticSp);
            CheckReal (x(CVM0+3,CVM0), (treal) 0., "gelsy real (lsqr)",  os, __LINE__, dVeryPessimisticSp);
            CheckReal (x(CVM0+4,CVM0), (treal) -0.2, "gelsy real (lsqr)",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix x2(5,2);
            x2.gelsy(a, b, rank);
            CheckReal ((x-x2).norm(), (treal) 0., "gelsy real",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelsy real rank",  os, __LINE__);
        }
        {
            cvm::cmatrix a(3, 5);
            cvm::cmatrix b(3, 2);
            a.randomize_real(-1., 1.);
            a.randomize_imag(-1., 1.);
            b.randomize_real(-1., 1.);
            b.randomize_imag(-1., 1.);
            tint rank;

            cvm::cmatrix x = a.gelsy(b, rank);
            CheckInt (rank, 3, "gelsy complex rank", os, __LINE__);
            CheckReal ((a*x - b).norm(), (treal) 0., "gelsy complex",  os, __LINE__, dVeryPessimisticSp);

            cvm::cmatrix x2(5,2);
            x2.gelsy(a, b, rank);
            CheckReal ((x-x2).norm(), (treal) 0., "gelsy complex",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelsy complex rank",  os, __LINE__);
        }


        // 6.0 gelss/gelsd stuff
        {
            treal ar[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12., 13., 14., 15.};
            cvm::rmatrix a(ar, 3, 5);
            cvm::rmatrix b(ar, 3, 2);
            cvm::rvector sv(3);
            tint rank;

            cvm::rmatrix x = a.gelss(b, sv, rank);

            CheckInt (rank, 2, "gelss real rank", os, __LINE__);
            CheckReal ((a*x - b).norm(), (treal) 0., "gelss real",  os, __LINE__, dVeryPessimisticSp);
            // see MATLAB's lsqr
            CheckReal (x(CVM0,CVM0), (treal) 0.6, "gelss real (lsqr)",  os, __LINE__, dVeryPessimisticSp);
            CheckReal (x(CVM0+1,CVM0), (treal) 0.4, "gelss real (lsqr)",  os, __LINE__, dVeryPessimisticSp);
            CheckReal (x(CVM0+2,CVM0), (treal) 0.2, "gelss real (lsqr)",  os, __LINE__, dVeryPessimisticSp);
            CheckReal (x(CVM0+3,CVM0), (treal) 0., "gelss real (lsqr)",  os, __LINE__, dVeryPessimisticSp);
            CheckReal (x(CVM0+4,CVM0), (treal) -0.2, "gelss real (lsqr)",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix x2(5,2);
            x2.gelss(a, b, sv, rank);
            CheckReal ((x - x2).norm(), (treal) 0., "gelss real",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, 2, "gelss real rank", os, __LINE__);
            CheckReal ((sv - a.svd()).norm(), (treal) 0., "gelss real svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix xd = a.gelsd(b, sv, rank);
            CheckReal ((x - xd).norm(), (treal) 0., "gelsd real",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, 2, "gelsd real rank", os, __LINE__);
            CheckReal ((sv - a.svd()).norm(), (treal) 0., "gelsd real svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix xd2(5,2);
            xd2.gelsd(a, b, sv, rank);
            CheckReal ((xd - xd2).norm(), (treal) 0., "gelsd real",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, 2, "gelsd real rank", os, __LINE__);
            CheckReal ((sv - a.svd()).norm(), (treal) 0., "gelsd real svd",  os, __LINE__, dVeryPessimisticSp);

        }
        {
            cvm::cmatrix a(3, 5);
            cvm::cmatrix b(3, 2);
            cvm::rvector sv(3);
            a.randomize_real(-1., 1.);
            a.randomize_imag(-1., 1.);
            b.randomize_real(-1., 1.);
            b.randomize_imag(-1., 1.);
            tint rank;

            cvm::cmatrix x = a.gelss(b, sv, rank);
            CheckInt (rank, 3, "gelss complex rank", os, __LINE__);
            CheckReal ((a*x - b).norm(), (treal) 0., "gelss complex",  os, __LINE__, dVeryPessimisticSp);

            cvm::cmatrix x2(5,2);
            x2.gelss(a, b, sv, rank);
            CheckInt (rank, 3, "gelss complex rank", os, __LINE__);
            CheckReal ((x - x2).norm(), (treal) 0., "gelss complex",  os, __LINE__, dVeryPessimisticSp);

            cvm::cmatrix xd = a.gelsd(b, sv, rank);
            CheckReal ((x - xd).norm(), (treal) 0., "gelsd complex",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, 3, "gelsd complex rank", os, __LINE__);
            CheckReal ((sv - a.svd()).norm(), (treal) 0., "gelsd complex svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::cmatrix xd2(5,2);
            xd2.gelsd(a, b, sv, rank);
            CheckInt (rank, 3, "gelsd complex rank", os, __LINE__);
            CheckReal ((x - x2).norm(), (treal) 0., "gelsd complex",  os, __LINE__, dVeryPessimisticSp);
        }
        {
            cvm::srbmatrix a(5,1,0);
            cvm::rvector b(5);
            cvm::rmatrix bm(5,2);
            a.randomize(-1., 1.);
            b.randomize(-1., 1.);
            bm.randomize(-1., 1.);
            cvm::rvector sv(5);
            tint rank;

            cvm::rvector xs = a.gelss(b, sv, rank);
            CheckReal ((a*xs-b).norm(), (treal) 0., "gelss real",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss real rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss real svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix xsm = a.gelss(bm, sv, rank);
            CheckReal ((a*xsm-bm).norm(), (treal) 0., "gelss real",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss real rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss real svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix xsm2(5,2);
            xsm2.gelss(a, bm, sv, rank);
            CheckReal ((a*xsm2-bm).norm(), (treal) 0., "gelss real",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss real rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss real svd",  os, __LINE__, dVeryPessimisticSp);


            cvm::rvector xd = a.gelsd(b, sv, rank);
            CheckReal ((a*xd-b).norm(), (treal) 0., "gelsd real",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss real rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss real svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix xdm = a.gelsd(bm, sv, rank);
            CheckReal ((a*xdm-bm).norm(), (treal) 0., "gelss real",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss real rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss real svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::rmatrix xdm2(5,2);
            xdm2.gelsd(a, bm, sv, rank);
            CheckReal ((a*xsm2-bm).norm(), (treal) 0., "gelss real",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss real rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss real svd",  os, __LINE__, dVeryPessimisticSp);
        }
        {
            cvm::scbmatrix a(5,1,0);
            cvm::cvector b(5);
            cvm::cmatrix bm(5,2);
            a.randomize_real(-1., 1.);
            a.randomize_imag(-1., 1.);
            b.randomize_real(-1., 1.);
            b.randomize_imag(-1., 1.);
            bm.randomize_real(-1., 1.);
            bm.randomize_imag(-1., 1.);
            cvm::rvector sv(5);
            tint rank;

            cvm::cvector xs = a.gelss(b, sv, rank);
            CheckReal ((a*xs-b).norm(), (treal) 0., "gelss complex",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss complex rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss complex svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::cmatrix xsm = a.gelss(bm, sv, rank);
            CheckReal ((a*xsm-bm).norm(), (treal) 0., "gelss complex",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss complex rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss complex svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::cmatrix xsm2(5,2);
            xsm2.gelss(a, bm, sv, rank);
            CheckReal ((a*xsm2-bm).norm(), (treal) 0., "gelss complex",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss complex rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss complex svd",  os, __LINE__, dVeryPessimisticSp);


            cvm::cvector xd = a.gelsd(b, sv, rank);
            CheckReal ((a*xd-b).norm(), (treal) 0., "gelsd complex",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss complex rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss complex svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::cmatrix xdm = a.gelsd(bm, sv, rank);
            CheckReal ((a*xdm-bm).norm(), (treal) 0., "gelss complex",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss complex rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss complex svd",  os, __LINE__, dVeryPessimisticSp);

            cvm::cmatrix xdm2(5,2);
            xdm2.gelsd(a, bm, sv, rank);
            CheckReal ((a*xsm2-bm).norm(), (treal) 0., "gelss complex",  os, __LINE__, dVeryPessimisticSp);
            CheckInt (rank, a.rank(), "gelss complex rank",  os, __LINE__);
            CheckReal ((sv-a.svd()).norm(), (treal) 0., "gelss complex svd",  os, __LINE__, dVeryPessimisticSp);
        }


        // 5.5 left eigenvalues
        {
            scmatrix m(3);
            scmatrix e(3);
            scmatrix e_(3);
            cvector cv(3), cv1(3);
            m(CVM0,CVM0)=tcomplex(0.1,0.01); m(CVM0,CVM0+1)=tcomplex(0.5,0.05); m(CVM0,CVM0+2)=tcomplex(0.9,0.09);
            m(CVM0+1,CVM0)=tcomplex(0.2,0.02); m(CVM0+1,CVM0+1)=tcomplex(0.6,0.06); m(CVM0+1,CVM0+2)=tcomplex(1.0,0.1);
            m(CVM0+2,CVM0)=tcomplex(0.3,0.03); m(CVM0+2,CVM0+1)=tcomplex(0.7,0.07); m(CVM0+2,CVM0+2)=tcomplex(1.0,-1.0);

            cv.eig (m, e);
            cv1 = m.eig (e_);
//            CheckReal    ((cv - cv1).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * e(CVM0) - e(CVM0) * cv(CVM0)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * e(CVM0+1) - e(CVM0+1) * cv(CVM0+1)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * e(CVM0+2) - e(CVM0+2) * cv(CVM0+2)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * e_(CVM0) - e_(CVM0) * cv1(CVM0)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * e_(CVM0+1) - e_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * e_(CVM0+2) - e_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);

            cv.eig (m, e, false);
            cv1 = m.eig (e_ , false);
//            CheckReal    ((cv - cv1).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
            CheckReal    ((~e(CVM0) * m - ~e(CVM0) * cv(CVM0)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dVeryPessimisticSp);
            CheckReal    ((~e(CVM0+1) * m - ~e(CVM0+1) * cv(CVM0+1)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
            CheckReal    ((~e(CVM0+2) * m - ~e(CVM0+2) * cv(CVM0+2)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
            CheckReal    ((~e_(CVM0) * m - ~e_(CVM0) * cv1(CVM0)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dVeryPessimisticSp);
            CheckReal    ((~e_(CVM0+1) * m - ~e_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
            CheckReal    ((~e_(CVM0+2) * m - ~e_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
        }
        {
            srmatrix m(3);
            scmatrix e(3);
            scmatrix e_(3);
            cvector cv(3), cv1(3);
            m(CVM0,CVM0)=0.1; m(CVM0,CVM0+1)=0.5; m(CVM0,CVM0+2)=0.9;
            m(CVM0+1,CVM0)=0.2; m(CVM0+1,CVM0+1)=0.6; m(CVM0+1,CVM0+2)=1.0;
            m(CVM0+2,CVM0)=0.3; m(CVM0+2,CVM0+1)=0.7; m(CVM0+2,CVM0+2)=1.0;

            cv.eig (m, e);
            cv1 = m.eig (e_);
//            CheckReal    ((cv - cv1).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((scmatrix(m) * e(CVM0) - e(CVM0) * cv(CVM0)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((scmatrix(m) * e(CVM0+1) - e(CVM0+1) * cv(CVM0+1)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((scmatrix(m) * e(CVM0+2) - e(CVM0+2) * cv(CVM0+2)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((scmatrix(m) * e_(CVM0) - e_(CVM0) * cv1(CVM0)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((scmatrix(m) * e_(CVM0+1) - e_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((scmatrix(m) * e_(CVM0+2) - e_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);

            cv.eig (m, e, false);
            cv1 = m.eig (e_, false);
//            CheckReal    ((cv - cv1).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
            CheckReal    ((~e(CVM0) * scmatrix(m) - ~e(CVM0) * cv(CVM0)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
            CheckReal    ((~e(CVM0+1) * scmatrix(m) - ~e(CVM0+1) * cv(CVM0+1)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
            CheckReal    ((~e(CVM0+2) * scmatrix(m) - ~e(CVM0+2) * cv(CVM0+2)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
            CheckReal    ((~e_(CVM0) * scmatrix(m) - ~e_(CVM0) * cv1(CVM0)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
            CheckReal    ((~e_(CVM0+1) * scmatrix(m) - ~e_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
            CheckReal    ((~e_(CVM0+2) * scmatrix(m) - ~e_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
        }

        // 5.5.1 coverage
        {
            cvector v(5);
            v.set_real((treal) 3.45);
            v.set_imag((treal) -4.17);
            CheckComplex (v(CVM0), tcomplex((treal) 3.45, (treal) -4.17), "cvector set_real set_image",  os, __LINE__);
            CheckComplex (v[CVM0+4], tcomplex((treal) 3.45, (treal) -4.17), "cvector set_real set_image",  os, __LINE__);

            cmatrix m(4,5);
            m.set_real((treal) 3.45);
            m.set_imag((treal) -4.17);
            CheckComplex (m(CVM0,CVM0+2), tcomplex((treal) 3.45, (treal) -4.17), "cmatrix set_real set_image",  os, __LINE__);
            CheckComplex (m[CVM0+3][CVM0+4], tcomplex((treal) 3.45, (treal) -4.17), "cmatrix set_real set_image",  os, __LINE__);

            scmatrix sm(5);
            sm.set_real((treal) 3.45);
            sm.set_imag((treal) -4.17);
            CheckComplex (sm(CVM0,CVM0+2), tcomplex((treal) 3.45, (treal) -4.17), "scmatrix set_real set_image",  os, __LINE__);
            CheckComplex (sm[CVM0+4][CVM0+4], tcomplex((treal) 3.45, (treal) -4.17), "scmatrix set_real set_image",  os, __LINE__);

            scbmatrix bm(5,1,2);
            bm.set_real((treal) 3.45);
            bm.set_imag((treal) -4.17);
            CheckComplex (bm(CVM0,CVM0+2), tcomplex((treal) 3.45, (treal) -4.17), "scbmatrix set_real set_image",  os, __LINE__);
            CheckComplex (bm[CVM0+4][CVM0+4], tcomplex((treal) 3.45, (treal) -4.17), "scbmatrix set_real set_image",  os, __LINE__);

            schmatrix hm(5);
            hm.set_real((treal) 3.45);
            CheckComplex (hm(CVM0,CVM0+2), tcomplex((treal) 3.45, (treal) 0.), "schmatrix set_real",  os, __LINE__);

            bool ex = false;
            try {
                hm.set_imag((treal) -4.17);
            }
            catch (cvm::cvmexception&)
            {
                ex = true;
            }
            CheckBool (ex, true, "schmatrix set_image not allowed",  os, __LINE__);
        }

        {
            schmatrix hm(5);
            hm.randomize_real((treal)3., (treal)7.);
            hm.randomize_imag((treal)-5., (treal)4.);
            cvector x(5), b(5);
            b.randomize_real((treal)-4., (treal)9.);
            b.randomize_imag((treal)-2., (treal)1.);

            x = hm.solve (b);
            CheckReal    ((hm * x - b).norm(),   0,  "schmatrix solve", os, __LINE__, dPessimisticSp);
            treal err;
            x = hm.solve (b, err);
            CheckReal    ((hm * x - b).norm(),   0,  "schmatrix solve", os, __LINE__, dPessimisticSp);
            CheckReal    (err,   0,  "schmatrix solve", os, __LINE__, dVeryPessimisticSp);

            cmatrix mb(5,6), mx(5,6);
            mb.randomize_real((treal)-4., (treal)9.);
            mb.randomize_imag((treal)-2., (treal)1.);

            mx = hm.solve (mb);
            CheckReal    ((hm * mx - mb).norm(),   0,  "schmatrix solve", os, __LINE__, dPessimisticSp);

            mx = hm.solve (mb, err);
            CheckReal    ((hm * mx - mb).norm(),   0,  "schmatrix solve", os, __LINE__, dPessimisticSp);
            CheckReal    (err,   0,  "schmatrix solve", os, __LINE__, dVeryPessimisticSp);

            schmatrix im(hm.inv());
            CheckReal    ((im * hm - eye_complex(5)).norm(),   0,  "srsmatrix inv", os, __LINE__, dPessimisticSp);
            im.inv(hm);
            CheckReal    ((im * hm - eye_complex(5)).norm(),   0,  "srsmatrix inv", os, __LINE__, dPessimisticSp);

            rvector ev(5), ev1(5), ev2(5);
            scmatrix evect(5);
            ev.eig (hm, evect);
            ev1 = hm.eig(evect);
            ev2 = hm.eig();

            CheckReal    ((ev - ev1).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((ev - ev2).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((hm * cvector(evect(CVM0)) - evect(CVM0) * ev1(CVM0)).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((hm * cvector(evect(CVM0+1)) - evect(CVM0+1) * ev1(CVM0+1)).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((hm * cvector(evect(CVM0+2)) - evect(CVM0+2) * ev1(CVM0+2)).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((hm * cvector(evect(CVM0+3)) - evect(CVM0+3) * ev1(CVM0+3)).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((hm * cvector(evect(CVM0+4)) - evect(CVM0+4) * ev1(CVM0+4)).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
        }

        {
            srsmatrix m(5);
            m.randomize((treal)3., (treal)7.);
            rvector x(5), b(5);
            b.randomize((treal)-4., (treal)9.);

            x = m.solve (b);
            CheckReal    ((m * x - b).norm(),   0,  "srsmatrix solve", os, __LINE__, dPessimisticSp);
            treal err;
            x = m.solve (b, err);
            CheckReal    ((m * x - b).norm(),   0,  "srsmatrix solve", os, __LINE__, dPessimisticSp);
            CheckReal    (err,   0,  "srsmatrix solve", os, __LINE__, dVeryPessimisticSp);

            rmatrix mb(5,6), mx(5,6);
            mb.randomize((treal)-4., (treal)9.);

            mx = m.solve (mb);
            CheckReal    ((m * mx - mb).norm(),   0,  "srsmatrix solve", os, __LINE__, dPessimisticSp);

            mx = m.solve (mb, err);
            CheckReal    ((m * mx - mb).norm(),   0,  "srsmatrix solve", os, __LINE__, dPessimisticSp);
            CheckReal    (err,   0,  "srsmatrix solve", os, __LINE__, dVeryPessimisticSp);

            srsmatrix im(m.inv());
            CheckReal    ((im * m - eye_real(5)).norm(),   0,  "srsmatrix inv", os, __LINE__, dPessimisticSp);
            im.inv(m);
            CheckReal    ((im * m - eye_real(5)).norm(),   0,  "srsmatrix inv", os, __LINE__, dPessimisticSp);

            rvector ev(5), ev1(5), ev2(5);
            srmatrix evect(5);
            ev.eig (m, evect);
            ev1 = m.eig(evect);
            ev2 = m.eig();

            CheckReal    ((ev - ev1).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((ev - ev2).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * evect(CVM0) - evect(CVM0) * ev1(CVM0)).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * evect(CVM0+1) - evect(CVM0+1) * ev1(CVM0+1)).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * evect(CVM0+2) - evect(CVM0+2) * ev1(CVM0+2)).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * evect(CVM0+3) - evect(CVM0+3) * ev1(CVM0+3)).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
            CheckReal    ((m * evect(CVM0+4) - evect(CVM0+4) * ev1(CVM0+4)).norm(),   0,  "srsmatrix eig", os, __LINE__, dPessimisticSp);

        }


        // positive-definite:
        {
/*
mpositive = zeros(3,3);
m(1,1) = 1;       m(1,2) = 2+i;     m(1,3) = 1-2*i;
m(2,1) = 2-i;     m(2,2) = 15;      m(2,3) = -1-3*i;
m(3,1) = 1+2*i;   m(3,2) = -1+3*i;  m(3,3) = 20;
det (m)
*/
            treal r[] = {1., 2., 1., 2., 15., -1., 1., -1., 20.};
            treal i[] = {0., -1., 2., 1., 0., 3., -2., -3., 0.};
            const schmatrix m(r, i, 3);
            scmatrix c(3);
            c.cholesky(m);
            CheckReal    ((~c * c - m).norm(),   0,  "schmatrix cholesky", os, __LINE__, dPessimisticSp);
            CheckComplex (m.det(), tcomplex((treal) 145., (treal) 0.), "schmatrix det (positive_defiite)",  os, __LINE__, dPessimisticSp);
        }
        // not positive-definite:
        {
            treal r[] = {1., 2., 1., 2., 5., -1., 1., -1., 20.};
            treal i[] = {0., -1., 2., 1., 0., 3., -2., -3., 0.};
            const schmatrix m(r, i, 3);
            CheckComplex (m.det(), tcomplex((treal) -5., (treal) 0.), "schmatrix det (not positive_defiite)",  os, __LINE__, dPessimisticSp);
        }

        // 8.1 generalized eigenvalues
        {
            srmatrix a(5);
            srbmatrix b(5, 2, 1);
            a.randomize(-10., 10.);
            b.randomize(-10., 10.);
            cvector alpha(5);
            rvector beta(5);
            scmatrix eigVectLeft(5), eigVectRight(5);

            alpha.geneig(a, b, beta);
            for (int i = 0; i < 5; ++i) {
                if (fabs(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckBool((scmatrix(a) - alpha[CVM0+i] / beta[CVM0+i] * scmatrix(b)).rank(dPessimisticSp) < 5,
                        true, "srmatrix geneig", os, __LINE__);
                }
            }

            alpha.geneig(a, b, beta, eigVectLeft, false);
            for (int i = 0; i < 5; ++i) {
                if (fabs(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckBool((scmatrix(a) - alpha[CVM0+i] / beta[CVM0+i] * scmatrix(b)).rank(dPessimisticSp) < 5,
                        true, "srmatrix geneig", os, __LINE__);
                }
            }
            for (int i = 0; i < 5; ++i) {
                if (fabs(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckReal((~(eigVectLeft(CVM0+i)) * scmatrix(a) - (alpha[CVM0+i] / beta[CVM0+i]) * ~(eigVectLeft(CVM0+i)) * scmatrix(b)).norm(),
                        0, "srmatrix left geneig", os, __LINE__, dVeryPessimisticSp);
                }
            }

            alpha.geneig(a, b, beta, eigVectRight);
            for (int i = 0; i < 5; ++i) {
                if (fabs(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckBool((scmatrix(a) - alpha[CVM0+i] / beta[CVM0+i] * scmatrix(b)).rank(dPessimisticSp) < 5,
                        true, "srmatrix geneig", os, __LINE__);
                }
            }
            for (int i = 0; i < 5; ++i) {
                if (fabs(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckReal((scmatrix(a) * eigVectRight(CVM0+i) - (alpha[CVM0+i] / beta[CVM0+i]) * scmatrix(b) * eigVectRight(CVM0+i)).norm(),
                        0, "srmatrix right geneig", os, __LINE__, dVeryPessimisticSp);
                }
            }

            alpha.geneig(a, b, beta, eigVectLeft, eigVectRight);
            for (int i = 0; i < 5; ++i) {
                if (fabs(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckBool((scmatrix(a) - alpha[CVM0+i] / beta[CVM0+i] * scmatrix(b)).rank(dPessimisticSp) < 5,
                        true, "srmatrix geneig", os, __LINE__);
                }
            }
            for (int i = 0; i < 5; ++i) {
                if (fabs(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckReal((~(eigVectLeft(CVM0+i)) * scmatrix(a) - (alpha[CVM0+i] / beta[CVM0+i]) * ~(eigVectLeft(CVM0+i)) * scmatrix(b)).norm(),
                        0, "srmatrix left geneig", os, __LINE__, dVeryPessimisticSp);
                    CheckReal((scmatrix(a) * eigVectRight(CVM0+i) - (alpha[CVM0+i] / beta[CVM0+i]) * scmatrix(b) * eigVectRight(CVM0+i)).norm(),
                        0, "srmatrix right geneig", os, __LINE__, dVeryPessimisticSp);
                }
            }
        }

        {
            scmatrix a(5);
            scbmatrix b(5, 2, 1);
            a.randomize_real(-10., 10.);
            a.randomize_imag(-10., 10.);
            b.randomize_real(-10., 10.);
            b.randomize_imag(-10., 10.);
            cvector alpha(5);
            cvector beta(5);
            scmatrix eigVectLeft(5), eigVectRight(5);

            alpha.geneig(a, b, beta);
            for (int i = 0; i < 5; ++i) {
                if (mod(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckBool((a - alpha[CVM0+i] / beta[CVM0+i] * b).rank(dPessimisticSp) < 5,
                        true, "scmatrix geneig", os, __LINE__);
                }
            }
            alpha.geneig(a, b, beta, eigVectLeft, false);
            for (int i = 0; i < 5; ++i) {
                if (mod(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckBool((a - alpha[CVM0+i] / beta[CVM0+i] * b).rank(dPessimisticSp) < 5,
                        true, "scmatrix geneig", os, __LINE__);
                }
            }
            for (int i = 0; i < 5; ++i) {
                if (mod(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckReal((~(eigVectLeft(CVM0+i)) * a - (alpha[CVM0+i] / beta[CVM0+i]) * ~(eigVectLeft(CVM0+i)) * b).norm(),
                        0, "scmatrix left geneig", os, __LINE__, dVeryPessimisticSp);
                }
            }

            alpha.geneig(a, b, beta, eigVectRight);
            for (int i = 0; i < 5; ++i) {
                if (mod(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckBool((a - alpha[CVM0+i] / beta[CVM0+i] * b).rank(dPessimisticSp) < 5,
                        true, "scmatrix geneig", os, __LINE__);
                }
            }
            for (int i = 0; i < 5; ++i) {
                if (mod(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckReal((a * eigVectRight(CVM0+i) - (alpha[CVM0+i] / beta[CVM0+i]) * b * eigVectRight(CVM0+i)).norm(),
                        0, "scmatrix right geneig", os, __LINE__, dVeryPessimisticSp);
                }
            }

            alpha.geneig(a, b, beta, eigVectLeft, eigVectRight);
            for (int i = 0; i < 5; ++i) {
                if (mod(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckBool((a - alpha[CVM0+i] / beta[CVM0+i] * b).rank(dPessimisticSp) < 5,
                        true, "scmatrix geneig", os, __LINE__);
                }
            }
            for (int i = 0; i < 5; ++i) {
                if (mod(beta[CVM0+i]) > dVeryPessimisticSp) {
                    CheckReal((~(eigVectLeft(CVM0+i)) * a - (alpha[CVM0+i] / beta[CVM0+i]) * ~(eigVectLeft(CVM0+i)) * b).norm(),
                        0, "scmatrix left geneig", os, __LINE__, dVeryPessimisticSp);
                    CheckReal((a * eigVectRight(CVM0+i) - (alpha[CVM0+i] / beta[CVM0+i]) * b * eigVectRight(CVM0+i)).norm(),
                        0, "scmatrix right geneig", os, __LINE__, dVeryPessimisticSp);
                }
            }
        }



        {
            LockIt<gCS> l;
            os << "***END***" << std::endl;
            std::cout << "CVM TESTS SUCCEEDED" << std::endl;
        }
    }
    catch (std::exception& e) {
        LockIt<gCS> l;
        std::cout << "Exception: " << e.what () << std::endl;
        exit_code = 1;
    }

//    cvmExit ();

#if defined (CVM_TEST_WIN_THREADS)
    return 0;
#else
    return NULL;
#endif
}

#endif

