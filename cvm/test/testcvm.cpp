//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2014
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "StdAfx.h"

#if 0

#include "test.h"

static time_t last_check = 0;
int exit_code = 0;

TestCriticalSection gCS;
TestCriticalSection gCS2;


#if defined (_WIN32) && !defined(__MINGW32__)
int gettimeofday(struct timeval *tv, struct timezone *tz)
{
    FILETIME ft;
    unsigned __int64 tmpres = 0;
//    static int tzflag;

    if (NULL != tv)
    {
        GetSystemTimeAsFileTime(&ft);
        tmpres |= ft.dwHighDateTime;
        tmpres <<= 32;
        tmpres |= ft.dwLowDateTime;

        /*converting file time to unix epoch*/
        tmpres -= DELTA_EPOCH_IN_MICROSECS;
        tmpres /= 10;  /*convert into microseconds*/
        tv->tv_sec = (long)(tmpres / 1000000UL);
        tv->tv_usec = (long)(tmpres % 1000000UL);
    }
/*
    if (NULL != tz)
    {
        if (!tzflag)
        {
            _tzset();
            tzflag++;
        }
        tz->tz_minuteswest = _timezone / 60;
        tz->tz_dsttime = _daylight;
    }
*/
    return 0;
}
#endif

long long get_usecs() {
    struct timeval tv;
    gettimeofday(&tv, nullptr);
    return tv.tv_sec * 1000000L + tv.tv_usec;
}




#if !defined(__INTEL_COMPILER) || !defined (__GNUC__)
tcomplex conj (tcomplex c)
{
    return tcomplex (c.real(), - c.imag());
}
#endif


treal mod (tcomplex c)
{
    return (treal) sqrt ((treal) c.real() * (treal) c.real() + (treal) c.imag() * (treal) c.imag());
}

void Report (const char* szMsg, std::ostream& os, int line) throw (test_exception)
{
    std::ostringstream oss;
    oss << szMsg << std::endl << "EXCEPTION ON LINE " << line << std::ends;
    std::string sMsg = oss.str ();
    os << sMsg << std::endl;
    throw test_exception (sMsg.c_str());
}

void CheckBool (bool b, bool bPattern, const char* szMsg, std::ostream& os, int line) throw (test_exception)
{
    LockIt<gCS> l;
    time_t curr_time = time(NULL);
    time_t duration = curr_time - last_check;
    last_check = curr_time;
    if (duration > 0) os << "*** " << duration << " ";
    os << szMsg << std::endl << b << std::endl;
    if (b != bPattern)
    {
        Report (szMsg, os, line);
    }
}

void CheckBoolNoLock (bool b, bool bPattern, const char* szMsg, std::ostream& os, int line) throw (test_exception)
{
    time_t curr_time = time(NULL);
    time_t duration = curr_time - last_check;
    last_check = curr_time;
    if (duration > 0) os << "*** " << duration << " ";
    os << szMsg << std::endl << b << std::endl;
    if (b != bPattern)
    {
        Report (szMsg, os, line);
    }
}

void CheckInt (tint v, tint vPattern, const char* szMsg, std::ostream& os, int line) throw (test_exception)
{
    LockIt<gCS> l;
    time_t curr_time = time(NULL);
    time_t duration = curr_time - last_check;
    last_check = curr_time;
    if (duration > 0) os << "*** " << duration << " ";
    os << szMsg << std::endl << v << std::endl;
    if (v != vPattern)
    {
        os << "Expected: " << vPattern << std::endl << "Returned " << v << std::endl;
        Report (szMsg, os, line);
    }
}

void CheckReal (treal v, treal vPattern, const char* szMsg, std::ostream& os, int line, treal rSp) throw (test_exception)
{
    LockIt<gCS> l;
    time_t curr_time = time(NULL);
    time_t duration = curr_time - last_check;
    last_check = curr_time;
    if (duration > 0) os << "*** " << duration << " ";
    os << szMsg << std::endl << v << std::endl;
    const treal mp = (treal) fabs ((treal) vPattern);
    treal vn = v - vPattern;
    if (mp > (treal) 1.) vn /= mp;
    if (fabs (vn) > rSp)
    {
        os << "Expected: " << vPattern << std::endl << "Returned " << v << std::endl;
        Report (szMsg, os, line);
    }
}

void CheckComplex (tcomplex v, tcomplex vPattern, const char* szMsg, std::ostream& os, int line, treal rSp) throw (test_exception)
{
    LockIt<gCS> l;
    time_t curr_time = time(NULL);
    time_t duration = curr_time - last_check;
    last_check = curr_time;
    if (duration > 0) os << "*** " << duration << " ";
    os << szMsg << std::endl << v << std::endl;
    const treal mp = mod (vPattern);
    tcomplex vn = v - vPattern;
    if (mp > (treal) 1.) vn /= mp;
    if (mod (vn) > rSp)
    {
        os << "Expected: " << vPattern << std::endl << "Returned " << v << std::endl;
        Report (szMsg, os, line);
    }
}

void CheckCVector (cvector v, cvector vPattern, const char* szMsg, std::ostream& os, int line, treal rSp) throw (test_exception)
{
    LockIt<gCS> l;
    time_t curr_time = time(NULL);
    time_t duration = curr_time - last_check;
    last_check = curr_time;
    if (duration > 0) os << "*** " << duration << " ";

    os << szMsg << std::endl << v << std::endl;// << vPattern << std::endl;
    treal vn = (v - vPattern).norm();
    if (vn > rSp)
    {
        os << "Expected: " << vPattern << std::endl << "Returned " << v << std::endl;
        Report (szMsg, os, line);
    }
}

void CheckString (const std::string& v, const std::string& vPattern, const char* szMsg, std::ostream& os, int line) throw (test_exception)
{
    LockIt<gCS> l;
    os << szMsg << std::endl << v << std::endl;
    if (v != vPattern) {
        os << "Expected: \"" << vPattern << "\"" << std::endl << "Returned: \"" << v << "\"" << std::endl;
        Report (szMsg, os, line);
    }
}

void CheckString (const std::string& v, const std::string& vPattern, const std::string& vPattern2, const char* szMsg, std::ostream& os, int line) throw (test_exception)
{
    LockIt<gCS> l;
    os << szMsg << std::endl << v << std::endl;
    if (v != vPattern && v != vPattern2) {
        os << "Expected: \"" << vPattern << "\" or \"" << vPattern2 << "\"" << std::endl << "Returned: \"" << v << "\"" << std::endl;
        Report (szMsg, os, line);
    }
}


void Fail (const char* szMsg, std::ostream& os, int line) throw (test_exception)
{
    LockIt<gCS> l;
    Report (szMsg, os, line);
}


std::ofstream os (FILE_OUT);    // share it among multiple executions

void cprint (const std::complex<treal>* p, int size)
{
    for (int i = 0; i < size; ++i)
    {
        std::cout << p[i] << " ";
    }
    std::cout << std::endl;
}

void print_solution (const srmatrix& a, const rvector& b)
{
    std::cout << a.solve(b);
}

srmatrix invert (const srmatrix& a)
{
    return a.inv();
}

treal& ret (rmatrix& m, const unsigned row, const unsigned col)
{
    return m.operator()(row+CVM0, col+CVM0).get();
}


#if defined (CVM_TEST_WIN_THREADS)
unsigned int __stdcall
#else
void *
#endif
TestBody (void*)
{
    {
        LockIt<gCS> l;
        std::cout << "CVM TESTS STARTED" << std::endl;
        if (os.bad ())
        {
            std::cout << "Error while creating file \"" FILE_OUT "\"" << std::endl;
        }
    }

    treal dPessimisticSp =
#ifdef CVM_FLOAT
        (treal) 5.e-4;
#else
        (treal) 1.e-10;
#endif

    treal dVeryPessimisticSp =
#ifdef CVM_FLOAT
        (treal) 1.e-2;
#else
        (treal) 1.e-5;
#endif

#ifdef CVM_FLOAT
        os.precision (7);
        std::cout.precision (5);
#else
        os.precision (5);
        std::cout.precision (16);
#endif
        os.setf(std::ios::scientific | std::ios::showpoint | std::ios::left);
        std::cout.setf(std::ios::scientific | std::ios::showpoint | std::ios::left);

    try
    {

        last_check = time(NULL);





#if defined(CVM_USE_USER_LITERALS)
            tcomplex c = 3.4 + 5.6_i;
            CheckComplex    (c,  tcomplex(3.4, 5.6),  "complex user literal ", os, __LINE__);

            const cvector vc = { 2_i, -2_i, 2.1_i, -2.1_i,
                1+2_i, 1.1+2_i, 1+2.1_i, 1.1+2.1_i,
                2_i + 4, 2_i + 4.1, 2.1_i + 4, 2.1_i + 4.1,
                1-2_i, 1.1-2_i, 1-2.1_i, 1.1-2.1_i,
                2_i - 4, 2_i - 4.1, 2.1_i - 4, 2.1_i - 4.1  };
            CheckReal    (vc.norm(),  (treal) 1.4972641717479251e+01,  "cvector user literal in init list", os, __LINE__, dPessimisticSp);
#endif
        }

        // move
        {
            rmatrix rm(4,3);
            rm.set(2.);
            treal ar[] = {1., 2., 3., 4., 5.};
            rvector a(ar, 3, 2), b(3), c(3), aa(3);
            b[CVM0] = 3;
            c[CVM0] = 4;
            rm[CVM0+1] = a + c;

            aa = a; // copy from sparse
            CheckReal (aa(CVM0),   (treal) 1., "rvector copy",  os, __LINE__);
            CheckReal (aa(CVM0+1), (treal) 3., "rvector copy",  os, __LINE__);
            CheckReal (aa(CVM0+2), (treal) 5., "rvector copy",  os, __LINE__);

            CheckReal (rm(CVM0,CVM0),   (treal) 2., "rvector move",  os, __LINE__);
            CheckReal (rm(CVM0+1,CVM0), (treal) 5., "rvector move",  os, __LINE__);
            CheckReal (rm(CVM0+2,CVM0), (treal) 2., "rvector move",  os, __LINE__);
            CheckReal (rm(CVM0,CVM0+1), (treal) 2., "rvector move",  os, __LINE__);
            CheckReal (rm(CVM0+1,CVM0+1), (treal) 3., "rvector move",  os, __LINE__);
            CheckReal (rm(CVM0+1,CVM0+2), (treal) 5., "rvector move",  os, __LINE__);
            CheckReal (rm(CVM0+3,CVM0+2), (treal) 2., "rvector move",  os, __LINE__);

            rvector d = a + b + c;
            CheckReal (d(CVM0),   (treal) 8., "rvector move",  os, __LINE__);
            CheckReal (d(CVM0+1), (treal) 3., "rvector move",  os, __LINE__);
            CheckReal (d(CVM0+2), (treal) 5., "rvector move",  os, __LINE__);

            rvector e = a + b;
            CheckReal (e(CVM0),   (treal) 4., "rvector move",  os, __LINE__);
            CheckReal (e(CVM0+1), (treal) 3., "rvector move",  os, __LINE__);
            CheckReal (e(CVM0+2), (treal) 5., "rvector move",  os, __LINE__);

            iarray ia(3), ib(3);
            ib[CVM0] = 4;
            ia = std::move(ib);
            CheckInt (ia(CVM0),   4, "iarray move",  os, __LINE__);
            CheckInt (ia(CVM0+1), 0, "iarray move",  os, __LINE__);
            CheckInt (ia(CVM0+2), 0, "iarray move",  os, __LINE__);
//            CheckInt (ib.size(),  0, "iarray move",  os, __LINE__);

            rvector f(4);
            {
                rmatrix rm(4,3);
                rm.set(9.);
                f = rm(CVM0);

                rvector g = std::move(rm[CVM0+1]);
                CheckReal (g(CVM0),   (treal) 9., "rvector move",  os, __LINE__);
                CheckReal (g(CVM0+1), (treal) 9., "rvector move",  os, __LINE__);
                CheckReal (g(CVM0+2), (treal) 9., "rvector move",  os, __LINE__);
            }
            CheckReal (f(CVM0),   (treal) 9., "rvector move",  os, __LINE__);
            CheckReal (f(CVM0+1), (treal) 9., "rvector move",  os, __LINE__);
            CheckReal (f(CVM0+2), (treal) 9., "rvector move",  os, __LINE__);
            CheckReal (f(CVM0+3), (treal) 9., "rvector move",  os, __LINE__);
        }


        {
            cmatrix rm(4,3);
            rm.set(2.);
            treal ar[] = {1., 2., 3., 4., 5., 1., 2., 3., 4., 5.};
            cvector a((tcomplex*)ar, 3, 2), b(3), c(3), aa(3);
            b[CVM0] = tcomplex(3,3);
            c[CVM0] = tcomplex(4,4);
            rm[CVM0+1] = a + c;


            aa = a; // copy from sparse
            CheckComplex (aa(CVM0),   tcomplex(1.,2.), "cvector copy",  os, __LINE__);
            CheckComplex (aa(CVM0+1), tcomplex(5.,1.), "cvector copy",  os, __LINE__);
            CheckComplex (aa(CVM0+2), tcomplex(4.,5.), "cvector copy",  os, __LINE__);

            CheckComplex (rm(CVM0,CVM0),     tcomplex(2.,0.), "cvector move",  os, __LINE__);
            CheckComplex (rm(CVM0+1,CVM0),   tcomplex(5.,6.), "cvector move",  os, __LINE__);
            CheckComplex (rm(CVM0+2,CVM0),   tcomplex(2.,0.), "cvector move",  os, __LINE__);
            CheckComplex (rm(CVM0,CVM0+1),   tcomplex(2.,0.), "cvector move",  os, __LINE__);
            CheckComplex (rm(CVM0+1,CVM0+1), tcomplex(5.,1.), "cvector move",  os, __LINE__);
            CheckComplex (rm(CVM0+1,CVM0+2), tcomplex(4.,5.), "cvector move",  os, __LINE__);
            CheckComplex (rm(CVM0+3,CVM0+2), tcomplex(2.,0.), "cvector move",  os, __LINE__);

            cvector e = a + b;
            CheckComplex (e(CVM0),   tcomplex(4.,5.), "cvector move",  os, __LINE__);
            CheckComplex (e(CVM0+1), tcomplex(5.,1.), "cvector move",  os, __LINE__);
            CheckComplex (e(CVM0+2), tcomplex(4.,5.), "cvector move",  os, __LINE__);

            cvector f(4);
            {
                cmatrix cm(4,3);
                cm.set(tcomplex(2.,3.));
                f = cm(CVM0);

                cvector g = std::move(cm[CVM0+1]);
                CheckComplex (g(CVM0),   tcomplex(2.,3.), "cvector move",  os, __LINE__);
                CheckComplex (g(CVM0+1), tcomplex(2.,3.), "cvector move",  os, __LINE__);
                CheckComplex (g(CVM0+2), tcomplex(2.,3.), "cvector move",  os, __LINE__);
            }
            CheckComplex (f(CVM0),   tcomplex(2.,3.), "cvector move",  os, __LINE__);
            CheckComplex (f(CVM0+1), tcomplex(2.,3.), "cvector move",  os, __LINE__);
            CheckComplex (f(CVM0+2), tcomplex(2.,3.), "cvector move",  os, __LINE__);
            CheckComplex (f(CVM0+3), tcomplex(2.,3.), "cvector move",  os, __LINE__);
        }
        {
            rmatrix m1(2,3), m2(2,3), m3(2,3);
            m2.set(2.);
            m3.set(3.);
            m1 = m2 + m3;

            CheckReal (m1(CVM0,CVM0),   (treal) 5., "rmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0), (treal) 5., "rmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0,CVM0+1), (treal) 5., "rmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0+1), (treal) 5., "rmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0+2), (treal) 5., "rmatrix move assignment",  os, __LINE__);

            rmatrix m4(m2 + m3);
            CheckReal (m4(CVM0,CVM0),   (treal) 5., "rmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0), (treal) 5., "rmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0,CVM0+1), (treal) 5., "rmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0+1), (treal) 5., "rmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0+2), (treal) 5., "rmatrix move",  os, __LINE__);

            rmatrix m7(7,5);
            m7.set(7.);
            rmatrix ms(m7, CVM0+1, CVM0+2, 2, 3); // submatrix
            rmatrix mm = std::move(ms);

            CheckReal (mm(CVM0,CVM0),   (treal) 7., "rmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0+1,CVM0), (treal) 7., "rmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0,CVM0+1), (treal) 7., "rmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0+1,CVM0+1), (treal) 7., "rmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0+1,CVM0+2), (treal) 7., "rmatrix submatrix move",  os, __LINE__);

            CheckReal (ms(CVM0,CVM0),   (treal) 7., "rmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0), (treal) 7., "rmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0,CVM0+1), (treal) 7., "rmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+1), (treal) 7., "rmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+2), (treal) 7., "rmatrix submatrix move",  os, __LINE__);

            ms = m2 + m3;
            CheckReal (ms(CVM0,CVM0),   (treal) 5., "rmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0), (treal) 5., "rmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0,CVM0+1), (treal) 5., "rmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+1), (treal) 5., "rmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+2), (treal) 5., "rmatrix submatrix move assignment",  os, __LINE__);

            CheckReal (m7(CVM0,CVM0+2),   (treal) 7., "rmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+1,CVM0+2), (treal) 5., "rmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+2,CVM0+2), (treal) 5., "rmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+3,CVM0+2), (treal) 7., "rmatrix submatrix move assignment",  os, __LINE__);
        }
        {
            cmatrix m1(2,3), m2(2,3), m3(2,3);
            m2.set(tcomplex(2.,2.));
            m3.set(tcomplex(3.,3.));
            m1 = m2 + m3;

            CheckComplex (m1(CVM0,CVM0),   tcomplex(5.,5.), "cmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0), tcomplex(5.,5.), "cmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0,CVM0+1), tcomplex(5.,5.), "cmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0+1), tcomplex(5.,5.), "cmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0+2), tcomplex(5.,5.), "cmatrix move assignment",  os, __LINE__);

            cmatrix m4(m2 + m3);
            CheckComplex (m4(CVM0,CVM0),   tcomplex(5.,5.), "cmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0), tcomplex(5.,5.), "cmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0,CVM0+1), tcomplex(5.,5.), "cmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0+1), tcomplex(5.,5.), "cmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0+2), tcomplex(5.,5.), "cmatrix move",  os, __LINE__);

            cmatrix m7(7,5);
            m7.set(tcomplex(7.,7.));
            cmatrix ms(m7, CVM0+1, CVM0+2, 2, 3); // submatrix
            cmatrix mm = std::move(ms);

            CheckComplex (mm(CVM0,CVM0),   tcomplex(7.,7.), "cmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0+1,CVM0), tcomplex(7.,7.), "cmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0,CVM0+1), tcomplex(7.,7.), "cmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0+1,CVM0+1), tcomplex(7.,7.), "cmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0+1,CVM0+2), tcomplex(7.,7.), "cmatrix submatrix move",  os, __LINE__);

            CheckComplex (ms(CVM0,CVM0),   tcomplex(7.,7.), "cmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0), tcomplex(7.,7.), "cmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0,CVM0+1), tcomplex(7.,7.), "cmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+1), tcomplex(7.,7.), "cmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+2), tcomplex(7.,7.), "cmatrix submatrix move",  os, __LINE__);

            ms = m2 + m3;
            CheckComplex (ms(CVM0,CVM0),   tcomplex(5.,5.), "cmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0), tcomplex(5.,5.), "cmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0,CVM0+1), tcomplex(5.,5.), "cmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+1), tcomplex(5.,5.), "cmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+2), tcomplex(5.,5.), "cmatrix submatrix move assignment",  os, __LINE__);

            CheckComplex (m7(CVM0,CVM0+2),   tcomplex(7.,7.), "cmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+1,CVM0+2), tcomplex(5.,5.), "cmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+2,CVM0+2), tcomplex(5.,5.), "cmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+3,CVM0+2), tcomplex(7.,7.), "cmatrix submatrix move assignment",  os, __LINE__);
        }
        {
            srmatrix m1(3), m2(3), m3(3);
            m2.set(2.);
            m3.set(3.);
            m1 = m2 + m3;

            CheckReal (m1(CVM0,CVM0),   (treal) 5., "srmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0), (treal) 5., "srmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0,CVM0+1), (treal) 5., "srmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0+1), (treal) 5., "srmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0+2), (treal) 5., "srmatrix move assignment",  os, __LINE__);

            srmatrix m4(m2 + m3);
            CheckReal (m4(CVM0,CVM0),   (treal) 5., "srmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0), (treal) 5., "srmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0,CVM0+1), (treal) 5., "srmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0+1), (treal) 5., "srmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0+2), (treal) 5., "srmatrix move",  os, __LINE__);

            srmatrix m7(7);
            m7.set(7.);
            srmatrix ms(m7, CVM0+1, CVM0+2, 3); // submatrix
            srmatrix mm = std::move(ms);

            CheckReal (mm(CVM0,CVM0),   (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0+1,CVM0), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0,CVM0+1), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0+1,CVM0+1), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0+1,CVM0+2), (treal) 7., "srmatrix submatrix move",  os, __LINE__);

            CheckReal (ms(CVM0,CVM0),   (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0,CVM0+1), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+1), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+2), (treal) 7., "srmatrix submatrix move",  os, __LINE__);

            ms = m2 + m3;
            CheckReal (ms(CVM0,CVM0),   (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0,CVM0+1), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+1), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+2), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);

            CheckReal (m7(CVM0,CVM0+2),   (treal) 7., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+1,CVM0+2), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+2,CVM0+2), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+3,CVM0+2), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+4,CVM0+2), (treal) 7., "srmatrix submatrix move assignment",  os, __LINE__);
        }
        {
            scmatrix m1(3), m2(3), m3(3);
            m2.set(tcomplex(2.,2.));
            m3.set(tcomplex(3.,3.));
            m1 = m2 + m3;

            CheckComplex (m1(CVM0,CVM0),   tcomplex(5.,5.), "scmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0), tcomplex(5.,5.), "scmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0,CVM0+1), tcomplex(5.,5.), "scmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0+1), tcomplex(5.,5.), "scmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0+2), tcomplex(5.,5.), "scmatrix move assignment",  os, __LINE__);

            scmatrix m4(m2 + m3);
            CheckComplex (m4(CVM0,CVM0),   tcomplex(5.,5.), "scmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0), tcomplex(5.,5.), "scmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0,CVM0+1), tcomplex(5.,5.), "scmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0+1), tcomplex(5.,5.), "scmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0+2), tcomplex(5.,5.), "scmatrix move",  os, __LINE__);

            scmatrix m7(7);
            m7.set(tcomplex(7.,7.));
            scmatrix ms(m7, CVM0+1, CVM0+2, 3); // submatrix
            scmatrix mm = std::move(ms);

            CheckComplex (mm(CVM0,CVM0),   tcomplex(7.,7.), "scmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0+1,CVM0), tcomplex(7.,7.), "scmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0,CVM0+1), tcomplex(7.,7.), "scmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0+1,CVM0+1), tcomplex(7.,7.), "scmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0+1,CVM0+2), tcomplex(7.,7.), "scmatrix submatrix move",  os, __LINE__);

            CheckComplex (ms(CVM0,CVM0),   tcomplex(7.,7.), "scmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0), tcomplex(7.,7.), "scmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0,CVM0+1), tcomplex(7.,7.), "scmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+1), tcomplex(7.,7.), "scmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+2), tcomplex(7.,7.), "scmatrix submatrix move",  os, __LINE__);

            ms = m2 + m3;
            CheckComplex (ms(CVM0,CVM0),   tcomplex(5.,5.), "scmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0), tcomplex(5.,5.), "scmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0,CVM0+1), tcomplex(5.,5.), "scmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+1), tcomplex(5.,5.), "scmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+2), tcomplex(5.,5.), "scmatrix submatrix move assignment",  os, __LINE__);

            CheckComplex (m7(CVM0,CVM0+2),   tcomplex(7.,7.), "scmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+1,CVM0+2), tcomplex(5.,5.), "scmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+2,CVM0+2), tcomplex(5.,5.), "scmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+3,CVM0+2), tcomplex(5.,5.), "scmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+4,CVM0+2), tcomplex(7.,7.), "scmatrix submatrix move assignment",  os, __LINE__);
        }
        {
            srbmatrix m1(5,2,1), m2(5,2,1), m3(5,2,1);
            m2.set(2.);
            m3.set(3.);
            m1 = m2 + m3;

            CheckReal (m1(CVM0,CVM0),   (treal) 5., "srbmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0), (treal) 5., "srbmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0,CVM0+1), (treal) 5., "srbmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0+1), (treal) 5., "srbmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0+2), (treal) 5., "srbmatrix move assignment",  os, __LINE__);

            srbmatrix m4(m2 + m3);
            CheckReal (m4(CVM0,CVM0),   (treal) 5., "srbmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0), (treal) 5., "srbmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0,CVM0+1), (treal) 5., "srbmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0+1), (treal) 5., "srbmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0+2), (treal) 5., "srbmatrix move",  os, __LINE__);

            srbmatrix mt(~m4);
            CheckInt (mt.lsize(), 1, "srbmatrix move",  os, __LINE__);
            CheckInt (mt.usize(), 2, "srbmatrix move",  os, __LINE__);

            CheckReal (mt(CVM0+1,CVM0),   (treal) 5., "srbmatrix move",  os, __LINE__);
            CheckReal (mt(CVM0+2,CVM0),   (treal) 0., "srbmatrix move",  os, __LINE__);
            CheckReal (mt(CVM0,CVM0+2),   (treal) 5., "srbmatrix move",  os, __LINE__);
            CheckReal (mt(CVM0,CVM0+3),   (treal) 0., "srbmatrix move",  os, __LINE__);

            srmatrix ms(m1);
            CheckReal(ms(CVM0, CVM0), (treal) 5., "srmatrix from srbmatrix", os, __LINE__);
            CheckReal(ms(CVM0 + 1, CVM0), (treal) 5., "srmatrix from srbmatrix", os, __LINE__);
            CheckReal(ms(CVM0, CVM0 + 1), (treal) 5., "srmatrix from srbmatrix", os, __LINE__);
            CheckReal(ms(CVM0 + 1, CVM0 + 1), (treal) 5., "srmatrix from srbmatrix", os, __LINE__);
            CheckReal(ms(CVM0 + 1, CVM0 + 2), (treal) 5., "srmatrix from srbmatrix", os, __LINE__);
            CheckReal(ms(CVM0 + 1, CVM0 + 3), (treal) 0., "srmatrix from srbmatrix", os, __LINE__);
        }
        {
            scbmatrix m1(5,2,1), m2(5,2,1), m3(5,2,1);
            m2.set(tcomplex(2.,2.));
            m3.set(tcomplex(3.,3.));
            m1 = m2 + m3;

            CheckComplex (m1(CVM0,CVM0),   tcomplex(5.,5.), "scbmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0), tcomplex(5.,5.), "scbmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0,CVM0+1), tcomplex(5.,5.), "scbmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0+1), tcomplex(5.,5.), "scbmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0+2), tcomplex(5.,5.), "scbmatrix move assignment",  os, __LINE__);

            scbmatrix m4(m2 + m3);
            CheckComplex (m4(CVM0,CVM0),   tcomplex(5.,5.), "scbmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0), tcomplex(5.,5.), "scbmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0,CVM0+1), tcomplex(5.,5.), "scbmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0+1), tcomplex(5.,5.), "scbmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0+2), tcomplex(5.,5.), "scbmatrix move",  os, __LINE__);

            scbmatrix mt(~m4);
            CheckInt (mt.lsize(), 1, "scbmatrix move",  os, __LINE__);
            CheckInt (mt.usize(), 2, "scbmatrix move",  os, __LINE__);

            CheckComplex (mt(CVM0+1,CVM0),   tcomplex(5.,-5.), "scbmatrix move",  os, __LINE__);
            CheckComplex (mt(CVM0+2,CVM0),   tcomplex(0.,0.), "scbmatrix move",  os, __LINE__);
            CheckComplex (mt(CVM0,CVM0+2),   tcomplex(5.,-5.), "scbmatrix move",  os, __LINE__);
            CheckComplex (mt(CVM0,CVM0+3),   tcomplex(0.,0.), "scbmatrix move",  os, __LINE__);

            scmatrix ms(m1);
            CheckComplex(ms(CVM0, CVM0), tcomplex(5., 5.), "scmatrix from scbmatrix", os, __LINE__);
            CheckComplex(ms(CVM0 + 1, CVM0), tcomplex(5., 5.), "scmatrix from scbmatrix", os, __LINE__);
            CheckComplex(ms(CVM0, CVM0 + 1), tcomplex(5., 5.), "scmatrix from scbmatrix", os, __LINE__);
            CheckComplex(ms(CVM0 + 1, CVM0 + 1), tcomplex(5., 5.), "scmatrix from scbmatrix", os, __LINE__);
            CheckComplex(ms(CVM0 + 1, CVM0 + 2), tcomplex(5., 5.), "scmatrix from scbmatrix", os, __LINE__);
            CheckComplex(ms(CVM0 + 1, CVM0 + 3), tcomplex(0., 0.), "scmatrix from scbmatrix", os, __LINE__);
        }
        {
            srsmatrix m1(3), m2(3), m3(3);
            m2.set(2.);
            m3.set(3.);
            m1 = m2 + m3;

            CheckReal (m1(CVM0,CVM0),   (treal) 5., "srsmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0), (treal) 5., "srsmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0,CVM0+1), (treal) 5., "srsmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0+1), (treal) 5., "srsmatrix move assignment",  os, __LINE__);
            CheckReal (m1(CVM0+1,CVM0+2), (treal) 5., "srsmatrix move assignment",  os, __LINE__);

            srsmatrix m4(m2 + m3);
            CheckReal (m4(CVM0,CVM0),   (treal) 5., "srmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0), (treal) 5., "srmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0,CVM0+1), (treal) 5., "srmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0+1), (treal) 5., "srmatrix move",  os, __LINE__);
            CheckReal (m4(CVM0+1,CVM0+2), (treal) 5., "srmatrix move",  os, __LINE__);

            srsmatrix m7(7);
            m7.set(7.);
            srsmatrix ms(m7, CVM0+1, 3); // submatrix
            srsmatrix mm = std::move(ms);

            CheckReal (mm(CVM0,CVM0),   (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0+1,CVM0), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0,CVM0+1), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0+1,CVM0+1), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (mm(CVM0+1,CVM0+2), (treal) 7., "srmatrix submatrix move",  os, __LINE__);

            CheckReal (ms(CVM0,CVM0),   (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0,CVM0+1), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+1), (treal) 7., "srmatrix submatrix move",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+2), (treal) 7., "srmatrix submatrix move",  os, __LINE__);

            ms = m2 + m3;
            CheckReal (ms(CVM0,CVM0),   (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0,CVM0+1), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+1), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (ms(CVM0+1,CVM0+2), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);

            CheckReal (m7(CVM0,CVM0+2),   (treal) 7., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+1,CVM0+2), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+2,CVM0+2), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+3,CVM0+2), (treal) 5., "srmatrix submatrix move assignment",  os, __LINE__);
            CheckReal (m7(CVM0+4,CVM0+2), (treal) 7., "srmatrix submatrix move assignment",  os, __LINE__);
        }
        {
            schmatrix m1(3), m2(3), m3(3);
            m2.set_real(2.);
            m3.set_real(3.);
            m1 = m2 + m3;

            CheckComplex (m1(CVM0,CVM0),   tcomplex(5.,0.), "schmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0), tcomplex(5.,0.), "schmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0,CVM0+1), tcomplex(5.,0.), "schmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0+1), tcomplex(5.,0.), "schmatrix move assignment",  os, __LINE__);
            CheckComplex (m1(CVM0+1,CVM0+2), tcomplex(5.,0.), "schmatrix move assignment",  os, __LINE__);

            schmatrix m4(m2 + m3);
            CheckComplex (m4(CVM0,CVM0),   tcomplex(5.,0.), "schmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0), tcomplex(5.,0.), "schmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0,CVM0+1), tcomplex(5.,0.), "schmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0+1), tcomplex(5.,0.), "schmatrix move",  os, __LINE__);
            CheckComplex (m4(CVM0+1,CVM0+2), tcomplex(5.,0.), "schmatrix move",  os, __LINE__);

            schmatrix m7(7);
            m7.set_real(7.);
            schmatrix ms(m7, CVM0+1, 3); // submatrix
            schmatrix mm = std::move(ms);

            CheckComplex (mm(CVM0,CVM0),   tcomplex(7.,0.), "schmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0+1,CVM0), tcomplex(7.,0.), "schmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0,CVM0+1), tcomplex(7.,0.), "schmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0+1,CVM0+1), tcomplex(7.,0.), "schmatrix submatrix move",  os, __LINE__);
            CheckComplex (mm(CVM0+1,CVM0+2), tcomplex(7.,0.), "schmatrix submatrix move",  os, __LINE__);

            CheckComplex (ms(CVM0,CVM0),   tcomplex(7.,0.), "schmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0), tcomplex(7.,0.), "schmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0,CVM0+1), tcomplex(7.,0.), "schmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+1), tcomplex(7.,0.), "schmatrix submatrix move",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+2), tcomplex(7.,0.), "schmatrix submatrix move",  os, __LINE__);

            ms = m2 + m3;
            CheckComplex (ms(CVM0,CVM0),   tcomplex(5.,0.), "schmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0), tcomplex(5.,0.), "schmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0,CVM0+1), tcomplex(5.,0.), "schmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+1), tcomplex(5.,0.), "schmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (ms(CVM0+1,CVM0+2), tcomplex(5.,0.), "schmatrix submatrix move assignment",  os, __LINE__);

            CheckComplex (m7(CVM0,CVM0+2),   tcomplex(7.,0.), "schmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+1,CVM0+2), tcomplex(5.,0.), "schmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+2,CVM0+2), tcomplex(5.,0.), "schmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+3,CVM0+2), tcomplex(5.,0.), "schmatrix submatrix move assignment",  os, __LINE__);
            CheckComplex (m7(CVM0+4,CVM0+2), tcomplex(7.,0.), "schmatrix submatrix move assignment",  os, __LINE__);
        }


        // yet another crash test
        {
            scbmatrix ma(3,1,0);
            ma.randomize_real(-1., 1.);
            ma.randomize_imag(-1., 1.);
            scbmatrix mLU(3,1,0);
            tint nPivots[3];
            mLU.low_up(ma, nPivots);
        }

        // proxy in/out
        {
            rmatrix m(1,1);
            m(CVM0,CVM0) = 1.234;
            char buf[32];
#if defined (_MSC_VER) && !defined(__INTEL_COMPILER)
            sprintf_s (buf, sizeof(buf), "%.3f", m(CVM0,CVM0).val());  // use .val to printf type_proxy!
#else
            sprintf (buf, "%.3f", m(CVM0,CVM0).val());  // use .val to printf type_proxy!
#endif
            CheckBool (true, 0 == strcmp(buf, "1.234"), "proxy in/out",  os, __LINE__);
        }

        // zero resize sanity check
        {
            treal r[16] = {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
            rmatrix m(r,4,4);
            m.resize(3,0);
            CheckInt (0, m.size(), "zero resize sanity check",  os, __LINE__);
        }


        // ILP64 sanity check
        {
            rmatrix m(2,3);
            rmatrix mc(2,3);
            m(CVM0,CVM0+1) = (treal)7.77;
            mc = m;
            CheckReal (mc(CVM0,CVM0+1), (treal) 7.77, "ILP64 sanity check",  os, __LINE__);
        }


        // syrk bug reported by Markus Jochmann
        {
            srsmatrix mat1(2), mat2(2);
            rmatrix v(2,2);
            v(CVM0,CVM0) = 1;
            v(CVM0,CVM0+1) = 2;
            v(CVM0+1,CVM0) = 3;
            v(CVM0+1,CVM0+1) = 4;

            rvector vrow(2);
            vrow = v[CVM0];
            mat1.syrk( 1.0, v[CVM0], 0.0 );
            mat2.syrk( 1.0, vrow, 0.0 );

            CheckReal ((mat1-mat2).norm(), (treal) 0., "srsmatrix::syrk with incr=2",  os, __LINE__);

            mat1.syr2k( 1.0, v[CVM0], v[CVM0], 0.0 );
            mat2.syr2k( 1.0, vrow, vrow, 0.0 );
            CheckReal ((mat1-mat2).norm(), (treal) 0., "srsmatrix::syr2k with incr=2",  os, __LINE__);
        }
        {
            schmatrix mat1(2), mat2(2);
            cmatrix v(2,2);
            v(CVM0,CVM0) = tcomplex ((treal) 1., (treal) 1.);
            v(CVM0,CVM0+1) = tcomplex ((treal) 2., (treal) 2.);
            v(CVM0+1,CVM0) = tcomplex ((treal) 3., (treal) 3.);
            v(CVM0+1,CVM0+1) = tcomplex ((treal) 4., (treal) 4.);

            cvector vrow(2);
            vrow = v[CVM0];

            tcomplex c1 ((treal) 1.43, (treal) -0.391);
            treal r1(1.17), r2(-0.632);

            mat1.herk(r1, v[CVM0], r2);
            mat2.herk(r1, vrow, r2);
            CheckReal ((mat1-mat2).norm(), (treal) 0., "schmatrix::herk with incr=2",  os, __LINE__);

            mat1.her2k(c1, v[CVM0], v[CVM0], r2);
            mat2.her2k(c1, vrow, vrow, r2);
            CheckReal ((mat1-mat2).norm(), (treal) 0., "schmatrix::her2k with incr=2",  os, __LINE__);
        }

        {
            // Intel MKL 8.1 crash test
            int i, j;
            const int n = 1000;
            const int p = 100;

            rmatrix A(n, p);
            for (j = 0 ; j < p; ++j) {
               for (i = 0 ; i < n; ++i) {
                   A(i+CVM0, j+CVM0) = (treal)(i + j * p);
               }
            }
            rvector v(_cvm_min(n,p)) ;
            srmatrix mU(n);
            srmatrix mVH(p);

            v.svd(A, mU, mVH);
            rmatrix mv(n,p);
            mv.diag(0) = v;

#ifndef CVM_FLOAT    // too naive to expect this precision from float-based algorithm for 1000x100 matrix
            CheckReal    ((A * ~mVH - mU * mv).norm(),  (treal) 0.,  "srmatrix svd", os, __LINE__, dVeryPessimisticSp);
            CheckReal    ((~A * mU - ~(mv * mVH)).norm(),  (treal) 0.,  "srmatrix svd", os, __LINE__, dVeryPessimisticSp);
#endif
        }


        {
            LockIt<gCS2> l;
            treal d = -7.62199774163029530e-001;

            std::streamsize old_size =
                std::cout.precision (17);
            std::ios_base::fmtflags old_flags =
                std::cout.setf (std::ios::scientific | std::ios::showpoint | std::ios::left);

            {
                std::ofstream os ("out.txt");
                os.precision (17);
                os.setf (std::ios::scientific | std::ios::showpoint | std::ios::left);
                os << d;
            }
            {
                treal dcopy;
                std::ifstream is ("out.txt");
                //is.precision (17);
                is >> dcopy;

                CheckReal (d, dcopy, "treal << >>", os, __LINE__);
            }

            std::cout.precision (old_size);
            std::cout.setf (old_flags);
        }


        // 6.0 const vs non-const foreign array
        {
            treal r[16] = {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
            const treal rc[16] = {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
            tcomplex c[16];
            const tcomplex cc[16];

            srsmatrix ssr(r, 4);
            srsmatrix ssrc(rc, 4);
            ssr.set(CVM0+3,CVM0+3,5.11);
            ssrc.set(CVM0+3,CVM0+3,5.11);
            CheckReal (r[15], 5.11, "srsmatrix: non-const foreign array", os, __LINE__);
            CheckReal (rc[15], 0.00, "srsmatrix: const foreign array",  os, __LINE__);

            schmatrix shc(c, 4);
            schmatrix shcc(cc, 4);
            shc.set(CVM0+1,CVM0+1,tcomplex(6.11,0.));
            shcc.set(CVM0+1,CVM0+1,tcomplex(6.11,0.));
            CheckComplex (c[5], tcomplex(6.11,0.), "schmatrix: non-const foreign array", os, __LINE__);
            CheckComplex (cc[5], tcomplex(0.0,0.0), "schmatrix: const foreign array",  os, __LINE__);

            rvector vr(r, 16);
            rvector vrc(rc, 16);
            vr[CVM0+7]=3.33;
            vrc[CVM0+7]=3.33;
            CheckReal (r[7], 3.33, "rvector: non-const foreign array", os, __LINE__);
            CheckReal (rc[7], 0.00, "rvector: const foreign array",  os, __LINE__);

            cvector vc(c, 16);
            cvector vcc(cc, 16);
            vc[CVM0+7]=tcomplex(3.33,4.44);
            vcc[CVM0+7]=tcomplex(3.33,4.44);
            CheckComplex (c[7], tcomplex(3.33,4.44), "cvector: non-const foreign array", os, __LINE__);
            CheckComplex (cc[7], tcomplex(0.0,0.0), "cvector: const foreign array", os, __LINE__);

            rmatrix mr(r, 3, 4);
            rmatrix mrc(rc, 3, 4);
            mr(CVM0+1,CVM0+1)=3.22;
            mrc(CVM0+1,CVM0+1)=3.22;
            CheckReal (r[4], 3.22, "rmatrix: non-const foreign array", os, __LINE__);
            CheckReal (rc[4], 0.00, "rmatrix: const foreign array",  os, __LINE__);

            cmatrix mc(c, 3, 4);
            cmatrix mcc(cc, 3, 4);
            mc(CVM0+1,CVM0+1)=tcomplex(3.33,4.22);
            mcc(CVM0+1,CVM0+1)=tcomplex(3.33,4.22);
            CheckComplex (c[4], tcomplex(3.33,4.22), "cmatrix: non-const foreign array", os, __LINE__);
            CheckComplex (cc[4], tcomplex(0.0,0.0), "cmatrix: const foreign array",  os, __LINE__);

            srmatrix sr(r, 4);
            srmatrix src(rc, 4);
            sr(CVM0+1,CVM0+1)=3.11;
            src(CVM0+1,CVM0+1)=3.11;
            CheckReal (r[5], 3.11, "srmatrix: non-const foreign array", os, __LINE__);
            CheckReal (rc[5], 0.00, "srmatrix: const foreign array",  os, __LINE__);

            scmatrix sc(c, 4);
            scmatrix scc(cc, 4);
            sc(CVM0+1,CVM0+1)=tcomplex(3.11,4.22);
            scc(CVM0+1,CVM0+1)=tcomplex(3.11,4.22);
            CheckComplex (c[5], tcomplex(3.11,4.22), "scmatrix: non-const foreign array", os, __LINE__);
            CheckComplex (cc[5], tcomplex(0.0,0.0), "scmatrix: const foreign array",  os, __LINE__);

            srbmatrix br(r, 8, 1, 0);
            srbmatrix brc(rc, 8, 1, 0);
            br(CVM0+1,CVM0)=3.01;
            brc(CVM0+1,CVM0)=3.01;
            CheckReal (r[1], 3.01, "srbmatrix: non-const foreign array", os, __LINE__);
            CheckReal (rc[1], 0.00, "srbmatrix: const foreign array",  os, __LINE__);

            scbmatrix bc(c, 8, 1, 0);
            scbmatrix bcc(cc, 8, 1, 0);
            bc(CVM0+1,CVM0)=tcomplex(3.11,2.11);
            bcc(CVM0+1,CVM0)=tcomplex(3.11,2.11);
            CheckComplex (c[1], tcomplex(3.11,2.11), "scbmatrix: non-const foreign array", os, __LINE__);
            CheckComplex (cc[1], tcomplex(0.0,0.0), "scbmatrix: const foreign array",  os, __LINE__);

        }


        {
            LockIt<gCS2> l;

            // matrix in/out
            rmatrix m1 (3,4);
            rmatrix m2 (3,4);
            m1.randomize((treal) -5., (treal) 5.);

            {
                std::ofstream os ("_" FILE_OUT);
                os.precision (17);
                os.setf (std::ios::scientific | std::ios::showpoint | std::ios::left);
                os << m1;
            }
            {
                std::ifstream is ("_" FILE_OUT);
                is.precision(17);
                is >> m2;
            }

            CheckBoolNoLock (m1 == m2, true, "Matrix in/out", os, __LINE__);
        }

        {
            treal pi = (treal)3.1415926535897932384626433832795;
            treal ci = (treal)1.;
            tcomplex phase = exp(2*pi*ci);

            rmatrix tmp(2,2);
            tmp(CVM0+1,CVM0+1) = 3.;
            cmatrix H(10,10);
            H(CVM0+1,CVM0+1) += phase*tmp(CVM0+1,CVM0+1);
            CheckComplex (H(CVM0+1,CVM0+1), tcomplex ((treal) 1.606474966574294e+03, treal (0.)),
                          "tcomplex * type_proxy<treal>",  os, __LINE__, dPessimisticSp);
        }

        {
            rmatrix m(2,3);
            ret(m,0,1) = (treal)7.77;
            CheckReal (m(CVM0,CVM0+1), (treal) 7.77, "type_proxy::get",  os, __LINE__);
        }

        int i, j, l;

        treal    a1[100], a2[100], a3[100], a4[100];
        tcomplex c1[100], c2[100];
        for (i = 0; i < 100; i++)
        {
            a1[i] = (treal) (i + 1);
            a2[i] = (treal) (i + 1) / (treal) 10.;
            a3[i] = (treal) (i + 1) * (treal) 10.;
            a4[i] = (treal) (i + 1) / (treal) 100.;
            c1[i] = tcomplex(a1[i], a2[i]);
            c2[i] = tcomplex(a2[i], a4[i]);
        }

        const treal cs[] = {3., 0., 2., 1., -1., 2., 2., -1., 3., 0.,
                             0., 3., -1., -2., 0., -3., 5., 0.};
        const treal as[] = {1., 2., 1., 2., 5., -1., 1., -1., 20.};

        {
            os << "entering mult crash test" << std::endl;
            cvector  cv1  (a1, a2, 10);         // note: this constructor copies memory, does not share
            cvector  cv2  (a1, a2, 10, 3);      // note: this constructor copies memory, does not share
            cmatrix  cm1  (a1, a2, 2, 3);       // note: this constructor copies memory, does not share
            cmatrix  cm2  (cm1);
            cv1.set(tcomplex (2,1));
            cv2.set(tcomplex (-1,3));
            cm2.set(tcomplex (-4,3));
            cv1.resize (2);
            cv2.resize (3);
            cv2.mult (cv1, cm2);
            CheckComplex (cv2[CVM0], cv1 * cm2(CVM0),  "mult (crash test)", os, __LINE__, dPessimisticSp);
        }


// constructors
        rvector  rv;
        rvector  rv0  (10);
        rvector  rv1  (a1, 10);             // note: this constructor shares memory
        rvector  rv2  (a1, 10, 3);          // note: this constructor shares memory
        rvector  rv3  (11, (treal) 17.77);
        rvector  rv4  (rv2);                // note: this constructor copies memory, does not share

        rmatrix BIG_MAMA(100, 100);
        cmatrix BIG_MAMAC(100, 100);
        BIG_MAMA.randomize(0., 2.);
        BIG_MAMAC.randomize_real(0., 2.);
        BIG_MAMAC.randomize_imag(0., 2.);

        rmatrix  rm;
        rmatrix  rm0  (BIG_MAMA, 21, 34, 5, 6);
        rmatrix  rm1  (a1, 2, 3);           // note: this constructor shares memory
        rmatrix  rm2  (rm1);
        rmatrix  rm3  (rv2, true);          // column
        rmatrix  rm4  (rv2, false);         // row
        srmatrix srm;
        srmatrix srm0 (BIG_MAMA, 43, 47, 4);
        srmatrix srm1 (a1, 3);              // note: this constructor shares memory
        srmatrix srm2 (srm1);
        srmatrix srm30 (srm0);

        cvector  cv;
        cvector  cv0  (10);
        cvector  cv1  (a1, a2, 10);         // note: this constructor copies memory, does not share
        cvector  cv2  (a1, a2, 10, 3);      // note: this constructor copies memory, does not share
        cvector  cv3  (c1, 10, 3);          // note: this constructor shares memory
        cvector  cv4  (11, tcomplex ((treal) 1.3, (treal) 2.4));
        cvector  cv5  (cv3);
        cvector  cv6  (rv1, rv2);           // note: this constructor copies memory, does not share
        cvector  cv7  (a4, 10, true,  2);   // note: this constructor copies memory, does not share
        cvector  cv8  (a4, 10, false, 3);   // note: this constructor copies memory, does not share
        cvector  cv9  (rv2, true);
        cvector  cv10 (rv2, false);

        cmatrix  cm;
        cmatrix  cm0  (BIG_MAMAC, 68, 17, 5, 6);
        cmatrix  cm1  (a1, a2, 2, 3);       // note: this constructor copies memory, does not share
        cmatrix  cm2  (cm1);
        scmatrix scm;
        scmatrix scm0 (4);
        scmatrix scm1 (a1, a2, 3);          // note: this constructor copies memory, does not share
        scmatrix scm2 (scm1);

        srbmatrix srbm;
        srbmatrix srbm1 (a1, 4, 1, 2);

        scbmatrix scbm;
        scbmatrix scbm1 (c1, 4, 1, 2);

        srbmatrix srbm5 (5, 1, 2);
        scbmatrix scbm5 (5, 1, 2);

        srsmatrix srs1 (3);
        schmatrix sch1 (3);


// Array<TR,TC> derived features.
        CheckInt (rv   .size()  , 0,  "rv.size()",    os, __LINE__);
        CheckInt (rv0  .size()  , 10, "rv0.size()",   os, __LINE__);
        CheckInt (rv1  .size()  , 10, "rv1.size()",   os, __LINE__);
        CheckInt (rv2  .size()  , 10, "rv2.size()",   os, __LINE__);
        CheckInt (rv3  .size()  , 11, "rv3.size()",   os, __LINE__);
        CheckInt (rv4  .size()  , 10, "rv4.size()",   os, __LINE__);
        CheckInt (rm   .size()  , 0,  "rm.size()",    os, __LINE__);
        CheckInt (rm0  .size()  , 30, "rm0.size()",   os, __LINE__);
        CheckInt (rm1  .size()  , 6,  "rm1.size()",   os, __LINE__);
        CheckInt (rm2  .size()  , 6,  "rm2.size()",   os, __LINE__);
        CheckInt (rm3  .size()  , 10, "rm3.size()",   os, __LINE__);
        CheckInt (rm4  .size()  , 10, "rm4.size()",   os, __LINE__);
        CheckInt (srm  .size()  , 0,  "srm.size()",   os, __LINE__);
        CheckInt (srm0 .size()  , 16, "srm0.size()",  os, __LINE__);
        CheckInt (srm1 .size()  , 9,  "srm1.size()",  os, __LINE__);
        CheckInt (srm2 .size()  , 9,  "srm2.size()",  os, __LINE__);
        CheckInt (cv   .size()  , 0,  "cv.size()",    os, __LINE__);
        CheckInt (cv0  .size()  , 10, "cv0.size()",   os, __LINE__);
        CheckInt (cv1  .size()  , 10, "cv1.size()",   os, __LINE__);
        CheckInt (cv2  .size()  , 10, "cv2.size()",   os, __LINE__);
        CheckInt (cv3  .size()  , 10, "cv3.size()",   os, __LINE__);
        CheckInt (cv4  .size()  , 11, "cv4.size()",   os, __LINE__);
        CheckInt (cv5  .size()  , 10, "cv5.size()",   os, __LINE__);
        CheckInt (cv6  .size()  , 10, "cv6.size()",   os, __LINE__);
        CheckInt (cv7  .size()  , 10, "cv7.size()",   os, __LINE__);
        CheckInt (cv8  .size()  , 10, "cv8.size()",   os, __LINE__);
        CheckInt (cv9  .size()  , 10, "cv9.size()",   os, __LINE__);
        CheckInt (cv10 .size()  , 10, "cv10.size()",  os, __LINE__);
        CheckInt (cm   .size()  , 0,  "cm.size()",    os, __LINE__);
        CheckInt (cm0  .size()  , 30, "cm0.size()",   os, __LINE__);
        CheckInt (cm1  .size()  , 6,  "cm1.size()",   os, __LINE__);
        CheckInt (scm  .size()  , 0,  "scm.size()",   os, __LINE__);
        CheckInt (scm0 .size()  , 16, "scm0.size()",  os, __LINE__);
        CheckInt (scm1 .size()  , 9,  "scm1.size()",  os, __LINE__);
        CheckInt (scm2 .size()  , 9,  "scm2.size()",  os, __LINE__);
        CheckInt (srbm .size()  , 0,  "srbm.size()",  os, __LINE__);
        CheckInt (srbm1.size()  , 16, "srbm1.size()", os, __LINE__);
        CheckInt (scbm .size()  , 0,  "scbm.size()",  os, __LINE__);
        CheckInt (scbm1.size()  , 16, "scbm1.size()", os, __LINE__);
        CheckInt (srbm5.size()  , 20, "srbm5.size()",  os, __LINE__);   // 5 * (1 + 1 + 2)
        CheckInt (scbm5.size()  , 20, "scbm5.size()",  os, __LINE__);
        CheckInt (srs1.size()   , 9, "srs1.size()", os, __LINE__);
        CheckInt (sch1.size()   , 9, "sch1.size()", os, __LINE__);

        CheckInt (rv   .incr()  , 0, "rv.incr()",    os, __LINE__);
        CheckInt (rv0  .incr()  , 1, "rv0.incr()",   os, __LINE__);
        CheckInt (rv1  .incr()  , 1, "rv1.incr()",   os, __LINE__);
        CheckInt (rv2  .incr()  , 3, "rv2.incr()",   os, __LINE__);
        CheckInt (rv3  .incr()  , 1, "rv3.incr()",   os, __LINE__);
        CheckInt (rv4  .incr()  , 1, "rv4.incr()",   os, __LINE__);
        CheckInt (rm   .incr()  , 0, "rm.incr()",    os, __LINE__);
        CheckInt (rm0  .incr()  , 1, "rm0.incr()",   os, __LINE__);
        CheckInt (rm1  .incr()  , 1, "rm1.incr()",   os, __LINE__);
        CheckInt (rm2  .incr()  , 1, "rm2.incr()",   os, __LINE__);
        CheckInt (rm3  .incr()  , 1, "rm3.incr()",   os, __LINE__);
        CheckInt (rm4  .incr()  , 1, "rm4.incr()",   os, __LINE__);
        CheckInt (srm  .incr()  , 0, "srm.incr()",   os, __LINE__);
        CheckInt (srm0 .incr()  , 1, "srm0.incr()",  os, __LINE__);
        CheckInt (srm1 .incr()  , 1, "srm1.incr()",  os, __LINE__);
        CheckInt (srm2 .incr()  , 1, "srm2.incr()",  os, __LINE__);
        CheckInt (cv   .incr()  , 0, "cv.incr()",    os, __LINE__);
        CheckInt (cv0  .incr()  , 1, "cv0.incr()",   os, __LINE__);
        CheckInt (cv1  .incr()  , 1, "cv1.incr()",   os, __LINE__);
        CheckInt (cv2  .incr()  , 1, "cv2.incr()",   os, __LINE__);
        CheckInt (cv3  .incr()  , 3, "cv3.incr()",   os, __LINE__);
        CheckInt (cv4  .incr()  , 1, "cv4.incr()",   os, __LINE__);
        CheckInt (cv5  .incr()  , 1, "cv5.incr()",   os, __LINE__);
        CheckInt (cv6  .incr()  , 1, "cv6.incr()",   os, __LINE__);
        CheckInt (cv7  .incr()  , 1, "cv7.incr()",   os, __LINE__);
        CheckInt (cv8  .incr()  , 1, "cv8.incr()",   os, __LINE__);
        CheckInt (cv9  .incr()  , 1, "cv9.incr()",   os, __LINE__);
        CheckInt (cv10 .incr()  , 1, "cv10.incr()",  os, __LINE__);
        CheckInt (cm   .incr()  , 0, "cm.incr()",    os, __LINE__);
        CheckInt (cm0  .incr()  , 1, "cm0.incr()",   os, __LINE__);
        CheckInt (cm1  .incr()  , 1, "cm1.incr()",   os, __LINE__);
        CheckInt (cm2  .incr()  , 1, "cm2.incr()",   os, __LINE__);
        CheckInt (scm  .incr()  , 0, "scm.incr()",   os, __LINE__);
        CheckInt (scm0 .incr()  , 1, "scm0.incr()",  os, __LINE__);
        CheckInt (scm1 .incr()  , 1, "scm1.incr()",  os, __LINE__);
        CheckInt (scm2 .incr()  , 1, "scm2.incr()",  os, __LINE__);
        CheckInt (srbm .incr()  , 0, "srbm.incr()",  os, __LINE__);
        CheckInt (srbm1.incr()  , 1, "srbm1.incr()", os, __LINE__);
        CheckInt (scbm .incr()  , 0, "scbm.incr()",  os, __LINE__);
        CheckInt (scbm1.incr()  , 1, "scbm1.incr()", os, __LINE__);
        CheckInt (srs1.incr()  , 1, "srs1.incr()", os, __LINE__);
        CheckInt (sch1.incr()  , 1, "sch1.incr()", os, __LINE__);

        cmatrix cm1r (rm1);
        cmatrix cm1i (rm1, false);
        CheckComplex (cm1r[1][2], tcomplex(rm1(1,2),(treal)0.), "cmatrix(rmatrix)",   os, __LINE__);
        CheckComplex (cm1i[1][2], tcomplex((treal)0.,rm1(1,2)), "cmatrix(rmatrix)",   os, __LINE__);

        rm2.resize(4, 4);
        cm2.resize(4, 4);
        srmatrix srm3 (rm2);
        scmatrix scm3 (cm2);

        CheckInt (rm2  .size()  , 16, "rm2.size()",   os, __LINE__);
        CheckInt (cm2  .size()  , 16, "cm2.size()",   os, __LINE__);
        CheckInt (rm2  .incr()  , 1,  "rm2.incr()",   os, __LINE__);
        CheckInt (cm2  .incr()  , 1,  "cm2.incr()",   os, __LINE__);
        CheckInt (srm3 .size()  , 16, "srm3.size()",  os, __LINE__);
        CheckInt (scm3 .size()  , 16, "scm3.size()",  os, __LINE__);
        CheckInt (srm3 .incr()  , 1,  "srm3.incr()",  os, __LINE__);
        CheckInt (scm3 .incr()  , 1,  "scm3.incr()",  os, __LINE__);

        scmatrix scm4 (srm3, true);
        scmatrix scm5 (srm3, false);

        CheckInt (scm4  .size() , 16, "scm4.size()",  os, __LINE__);
        CheckInt (scm5  .size() , 16, "scm5.size()",  os, __LINE__);
        CheckInt (scm4  .incr() , 1 , "scm4.incr()",  os, __LINE__);
        CheckInt (scm5  .incr() , 1 , "scm5.incr()",  os, __LINE__);

        scmatrix scm6 (srm3, srm0);
        CheckInt (scm6  .size() , 16, "scm6.size()",  os, __LINE__);
        CheckInt (scm6  .incr() , 1 , "scm6.incr()",  os, __LINE__);

        srmatrix srm4 (srbm1);
        CheckInt (srm4 .size()  , 16, "srm4.size()",  os, __LINE__);
        CheckInt (srm4 .incr()  , 1,  "srm4.incr()",  os, __LINE__);

        scmatrix scm8 (scbm1);
        CheckInt (scm8 .size()  , 16, "scm8.size()",  os, __LINE__);
        CheckInt (scm8 .incr()  , 1,  "scm8.incr()",  os, __LINE__);

        srmatrix srm5 (rv2);
        CheckInt (srm5 .size()  , 100,"srm5.size()",  os, __LINE__);
        CheckInt (srm5 .incr()  , 1,  "srm5.incr()",  os, __LINE__);

        scmatrix scm7 (cv2);
        CheckInt (scm7 .size()  , 100,"srm7.size()",  os, __LINE__);
        CheckInt (scm7 .incr()  , 1,  "srm7.incr()",  os, __LINE__);

// Indexing and assignments
        CheckReal (rv2[CVM0],  1.,  "rv2[1]",   os, __LINE__);
        CheckReal (rv2[CVM0+9], 28., "rv2[10]",  os, __LINE__);

        treal r1 = (treal) -1.92;
        a1[3] = r1;
        CheckReal (rv1[CVM0+3],     r1,  "rv2[4]",      os, __LINE__);     // memoty sharing check
        CheckReal (rv2[CVM0+1],     r1,  "rv2[2]",      os, __LINE__);
        CheckReal (rm1[CVM0+1][CVM0+1],  r1,  "rm1[2][2]",   os, __LINE__);
        CheckReal (rm1(CVM0+1, CVM0+1),  r1,  "rm1(2, 2)",   os, __LINE__);
        CheckReal (srm1[CVM0][CVM0+1], r1,  "srm1[1][2]",  os, __LINE__);
        CheckReal (srm1(CVM0, CVM0+1), r1,  "srm1(1, 2)",  os, __LINE__);

        CheckComplex (cm1(CVM0+1, CVM0+1), tcomplex ((treal) 4., (treal) 0.4),  "cm1(2, 2)",  os, __LINE__);

        treal* pr1 = srm30;
        srm30(CVM0+3,CVM0+3) = r1;
        CheckReal (srm30(CVM0+3,CVM0+3),  r1,  "srm30(4,4)",      os, __LINE__);     // memoty sharing check
        CheckReal (pr1[15],    r1,  "pr1[15]",        os, __LINE__);     // memoty sharing check

        treal* pr2 = srbm1;
        srbm1(CVM0,CVM0+1) = r1;
        CheckReal (srbm1(CVM0,CVM0+1), r1,  "srbm1(1,2)",     os, __LINE__);     // memoty sharing check
        CheckReal (pr2[5],     r1,  "pr2[5]",         os, __LINE__);     // memoty sharing check

        tcomplex cr1 = tcomplex ((treal) 1.07, (treal) -0.179);
        std::complex<treal>* pc2 = scbm1;
        scbm1(CVM0,CVM0+1) = cr1;
        CheckComplex (scbm1(CVM0,CVM0+1), cr1,  "scbm1(1,2)",     os, __LINE__);     // memoty sharing check
        CheckComplex (pc2[5],     cr1,  "pc2[5]",         os, __LINE__);     // memoty sharing check

        srs1.assign(as);
        sch1.assign((tcomplex*)cs);

        CheckReal (srs1[CVM0][CVM0+1], as[3], "srs1[1][2]",   os, __LINE__);
        CheckReal (srs1(CVM0+2,CVM0+1),  as[5], "srs1(3,2)",   os, __LINE__);
        CheckReal (srs1(CVM0+2)[CVM0+1], as[7], "srs1(3)[2]",  os, __LINE__);

        CheckComplex (sch1[CVM0][CVM0+1], tcomplex(cs[6],cs[7]), "sch1[1][2]",   os, __LINE__);
        CheckComplex (sch1(CVM0+2,CVM0+1),  tcomplex(cs[10],cs[11]), "sch1(3,2)",   os, __LINE__);
        CheckComplex (sch1(CVM0+2)[CVM0+1], tcomplex(cs[14],cs[15]), "sch1(3)[2]",  os, __LINE__);


// Array<TR,TC> derived features -  continued
        rv << rv1.normalize();
        CheckReal (rv(CVM0+6), rv1[CVM0+6], "rvector << rvector",    os, __LINE__);

        treal r2 = (treal) 0.;
        for (i = 0; i < 10; i++)
        {
            r2 += a1[i] * a1[i];
        }

        CheckReal (r2,         (treal) 1.,  "normalize",       os, __LINE__, dPessimisticSp);
        CheckReal (rv.norm(),  (treal) 1.,  "rv.norm()",       os, __LINE__);

        CheckInt (rv.indofmax () ,    CVM0+9,  "rv.indofmax ()",  os, __LINE__);
        CheckInt (rv.indofmin () ,    CVM0 ,  "rv.indofmin ()",  os, __LINE__);

        r1 = rv[CVM0+9];
        CheckReal (rv.norminf (), r1,  "rv.norminf ()",   os, __LINE__);

        rv1.sum (rv1, rv);
        CheckReal (rv1[CVM0+9], r1 + r1,  "sum",   os, __LINE__);
        rv1.diff (rv1, rv);
        CheckReal (rv1[CVM0+9], r1,       "diff",   os, __LINE__);

        rv1 += rv;
        CheckReal (rv1[CVM0+9], r1 + r1,  "+=, rvector",   os, __LINE__);
        rv1 -= rv;
        CheckReal (rv1[CVM0+9], r1,       "-=, rvector",   os, __LINE__);
        rv1 += rv1;
        CheckReal (rv1[CVM0+9], r1 + r1,  "+=, rvector self",   os, __LINE__);

        cv << cv1;
        CheckComplex (cv(CVM0+6), cv1[CVM0+6], "cvector << cvector",    os, __LINE__);

        cr1 = cv1[CVM0+9];
        cv1 += cv;
        CheckComplex (cv1[CVM0+9], cr1 + cr1,  "+=, cvector",   os, __LINE__);
        cv1 -= cv;
        CheckComplex (cv1[CVM0+9], cr1,        "-=, cvector",   os, __LINE__);
        cv1 += cv1;
        CheckComplex (cv1[CVM0+9], cr1 + cr1,  "+=, cvector self",   os, __LINE__);

        rm << rm1;
        CheckReal (rm(CVM0+1,CVM0), rm1[CVM0+1][CVM0], "rmatrix << rmatrix",    os, __LINE__);

        r1 = rm(CVM0+1,CVM0+1);
        rm += rm1;
        CheckReal (rm(CVM0+1,CVM0+1), r1 + r1, "+=, rmatrix",   os, __LINE__);
        rm -= rm1;
        CheckReal (rm(CVM0+1,CVM0+1), r1,      "-=, rmatrix",   os, __LINE__);
        rm += rm;
        CheckReal (rm(CVM0+1,CVM0+1), r1 + r1, "+=, rmatrix, self",   os, __LINE__);

        cm << cm1;
        CheckComplex (cm(CVM0+1,CVM0), cm1[CVM0+1][CVM0], "cmatrix << cmatrix",    os, __LINE__);

        cr1 = cm(CVM0+1,CVM0+1);
        cm += cm1;
        CheckComplex (cm(CVM0+1,CVM0+1), cr1 + cr1, "+=, cmatrix",   os, __LINE__);
        cm -= cm1;
        CheckComplex (cm(CVM0+1,CVM0+1), cr1,       "-=, cmatrix",   os, __LINE__);
        cm += cm;
        CheckComplex (cm(CVM0+1,CVM0+1), cr1 + cr1, "+=, cmatrix, self",   os, __LINE__);

        srm << srm1;
        r1 = srm(CVM0+1,CVM0+1);
        srm += srm1;
        CheckReal (srm(CVM0+1,CVM0+1), r1 + r1, "+=, srmatrix",   os, __LINE__);
        srm -= srm1;
        CheckReal (srm(CVM0+1,CVM0+1), r1,      "-=, srmatrix",   os, __LINE__);
        srm += srm;
        CheckReal (srm(CVM0+1,CVM0+1), r1 + r1, "+=, srmatrix, self",   os, __LINE__);

        scm << scm1;
        CheckComplex (scm(CVM0+1,CVM0), scm1[CVM0+1][CVM0], "scmatrix << scmatrix",    os, __LINE__);

        cr1 = scm(CVM0+1,CVM0+1);
        scm += scm1;
        CheckComplex (scm(CVM0+1,CVM0+1), cr1 + cr1, "+=, scmatrix",   os, __LINE__);
        scm -= scm1;
        CheckComplex (scm(CVM0+1,CVM0+1), cr1,       "-=, scmatrix",   os, __LINE__);
        scm += scm;
        CheckComplex (scm(CVM0+1,CVM0+1), cr1 + cr1, "+=, scmatrix, self",   os, __LINE__);


        srbm1.set((treal)1.14);
        srbm << srbm1;
        srbm.set( (treal)-.684);
        r1 = srbm(CVM0+1,CVM0);
        r2 = srbm1(CVM0+1,CVM0);
        srbm += srbm1;
        CheckReal (srbm(CVM0+1,CVM0), r1 + r2, "+=, srbmatrix",   os, __LINE__);
        srbm -= srbm1;
        CheckReal (srbm(CVM0+1,CVM0), r1, "-=, srbmatrix",   os, __LINE__);

        tcomplex cr2 = tcomplex ((treal) 1.03, (treal) -0.79);
        scbm1.set(cr2);
        scbm << scbm1;
        scbm.set(cr1);
        cr1 = scbm(CVM0+1,CVM0);
        cr2 = scbm1(CVM0+1,CVM0);
        scbm += scbm1;

        CheckComplex (scbm(CVM0+1,CVM0), cr1 + cr2, "+=, scbmatrix",   os, __LINE__);
        scbm -= scbm1;
        CheckComplex (scbm(CVM0+1,CVM0), cr1, "-=, scbmatrix",   os, __LINE__);


        srsmatrix srs2;
        schmatrix sch2;
        srs2 << srs1;
        sch2 << sch1;
        CheckReal ((srs2 - srs1).norm(),  (treal) 0. ,  "srsmatrix <<",  os, __LINE__);
        CheckReal ((sch2 - sch1).norm(),  (treal) 0. ,  "schmatrix <<",  os, __LINE__);

        srsmatrix srs2sub (srs2, CVM0+1, 2);
        CheckReal    (srs2sub(CVM0,CVM0+1), srs2(CVM0+1,CVM0+2), "srsmatrix submatrix ctr",   os, __LINE__);
        schmatrix sch2sub (sch2, CVM0+1, 2);
        CheckComplex (sch2sub(CVM0,CVM0+1), sch2(CVM0+1,CVM0+2), "schmatrix submatrix ctr",   os, __LINE__);


        r2 = (treal) 1.13;
        cr2 = tcomplex ((treal) 1.03, (treal) -0.79);

        treal rs1 = srs2(CVM0+1,CVM0+2);
        tcomplex cs1 = sch2(CVM0+1,CVM0+2);
        srs2 *= r2;
        sch2 *= r2;
        CheckReal (srs2(CVM0+1,CVM0+2),  rs1 * r2 ,  "srsmatrix *= TR",  os, __LINE__);
        CheckComplex (sch2(CVM0+1,CVM0+2),  cs1 * r2 ,  "schmatrix *= TR",  os, __LINE__);
        srs2 /= r2;
        sch2 /= r2;
        CheckReal (srs2(CVM0+1,CVM0+2),  rs1,  "srsmatrix /= TR",  os, __LINE__);
        CheckComplex (sch2(CVM0+1,CVM0+2),  cs1,  "schmatrix /= TR",  os, __LINE__, dPessimisticSp);
        CheckComplex ((sch2 * cr2)(CVM0+1,CVM0+2),  cs1 * cr2,  "schmatrix * TC",  os, __LINE__, dPessimisticSp);

        rvector vrs1(3);
        vrs1.randomize(3., 7.);
        CheckReal ((srs1 * vrs1 - srmatrix(srs1) * vrs1).norm(), (treal) 0.,  "srsmatrix * rvector",  os, __LINE__, dPessimisticSp);

        cvector vch1(3);
        vch1.randomize_real(3., 7.);
        vch1.randomize_imag(-3., 7.);
        CheckReal ((sch1 * vch1 - scmatrix(sch1) * vch1).norm(), (treal) 0.,  "schmatrix * cvector",  os, __LINE__, dPessimisticSp);

        r1 = rv1(CVM0+8);
        rv1 *= r2;
        CheckReal (rv1(CVM0+8),     r1 * r2,  "*=, rvector",     os, __LINE__);
        r1 = rm3(CVM0+6,CVM0);
        rm3 *= r2;
        CheckReal (rm3(CVM0+6,CVM0),   r1 * r2,  "*=, rmatrix",     os, __LINE__);
        r1 = srm4(CVM0,CVM0+1);
        srm4 *= r2;
        CheckReal (srm4(CVM0,CVM0+1),  r1 * r2,  "*=, srmatrix",    os, __LINE__);
        r1 = srbm1(CVM0,CVM0+1);
        srbm1 *= r2;
        CheckReal (srbm1(CVM0,CVM0+1), r1 * r2,  "*=, srbmatrix",   os, __LINE__);
        cr1 = scbm1(CVM0,CVM0+1);
        scbm1 *= cr2;
        CheckComplex (scbm1(CVM0,CVM0+1), cr1 * cr2,  "*=, scbmatrix",   os, __LINE__);
        r1 = rv1(CVM0+8);
        rv1 /= r2;
        CheckReal (rv1(CVM0+8),     r1 / r2,  "/=, rvector",     os, __LINE__);
        r1 = rm3(CVM0+6,CVM0);

        rm3 /= r2;
        CheckReal (rm3(CVM0+6,CVM0),   r1 / r2,  "/=, rmatrix",     os, __LINE__);
        r1 = srm4(CVM0,CVM0+1);
        srm4 /= r2;
        CheckReal (srm4(CVM0,CVM0+1),  r1 / r2,  "/=, srmatrix",    os, __LINE__);
        r1 = srbm1(CVM0,CVM0+1);
        srbm1 /= r2;
        CheckReal (srbm1(CVM0,CVM0+1), r1 / r2,  "/=, srbmatrix",   os, __LINE__);
        cr1 = scbm1(CVM0+1,CVM0);
        scbm1 /= cr2;
        CheckComplex (scbm1(CVM0+1,CVM0), cr1 / cr2,  "/=, scbmatrix",   os, __LINE__);


        cr1 = cv1(CVM0+8);
        cv1 *= r2;
        CheckComplex (cv1(CVM0+8),       cr1 * r2,  "cvector *= treal",    os, __LINE__);
        cr1 = cm1(CVM0+1,CVM0+1);
        cm1 *= r2;
        CheckComplex (cm1(CVM0+1,CVM0+1),    cr1 * r2,  "cmatrix *= treal",    os, __LINE__);
        cr1 = scm1(CVM0+1,CVM0+1);
        scm1 *= r2;
        CheckComplex (scm1(CVM0+1,CVM0+1),   cr1 * r2,  "scmatrix *= treal",   os, __LINE__);
        cr1 = scbm1(CVM0,CVM0+1);
        scbm1 *= r2;
        CheckComplex (scbm1(CVM0,CVM0+1),  cr1 * r2,  "scbmatrix *= treal",   os, __LINE__);
        cr1 = cv1(CVM0+8);
        cv1 /= r2;
        CheckComplex (cv1(CVM0+8),       cr1 / r2,  "cvector /= treal",    os, __LINE__);
        cr1 = cm1(CVM0+1,CVM0+1);
        cm1 /= r2;
        CheckComplex (cm1(CVM0+1,CVM0+1),    cr1 / r2,  "cmatrix /= treal",    os, __LINE__);
        cr1 = scm1(CVM0+1,CVM0+1);
        scm1 /= r2;
        CheckComplex (scm1(CVM0+1,CVM0+1),   cr1 / r2,  "scmatrix /= treal",   os, __LINE__);
        cr1 = scbm1(CVM0+1,CVM0);
        scbm1 /= r2;
        CheckComplex (scbm1(CVM0+1,CVM0),  cr1 / r2,  "scbmatrix /= treal",   os, __LINE__);


        cr2 = tcomplex ((treal) 1.03, (treal) -0.79);

        cr1 = cv1(CVM0+8);
        cv1 *= cr2;
        CheckComplex (cv1(CVM0+8),       cr1 * cr2, "cvector *= tcomplex",  os, __LINE__);
        cr1 = cm1(CVM0+1,CVM0+1);
        cm1 *= cr2;
        CheckComplex (cm1(CVM0+1,CVM0+1),    cr1 * cr2, "cmatrix *= tcomplex",  os, __LINE__);
        cr1 = scm1(CVM0+1,CVM0+1);
        scm1 *= cr2;
        CheckComplex (scm1(CVM0+1,CVM0+1),   cr1 * cr2, "scmatrix *= tcomplex", os, __LINE__);
        cr1 = scbm1(CVM0+1,CVM0);
        scbm1 *= cr2;
        CheckComplex (scbm1(CVM0+1,CVM0),  cr1 * cr2, "scbmatrix *= tcomplex", os, __LINE__);
        cr1 = cv1(CVM0+8);
        cv1 /= cr2;
        CheckComplex (cv1(CVM0+8),       cr1 / cr2, "cvector /= tcomplex",  os, __LINE__);
        cr1 = cm1(CVM0+1,CVM0+1);
        cm1 /= cr2;
        CheckComplex (cm1(CVM0+1,CVM0+1),    cr1 / cr2, "cmatrix /= tcomplex",  os, __LINE__);
        cr1 = scm1(CVM0+1,CVM0+1);
        scm1 /= cr2;
        CheckComplex (scm1(CVM0+1,CVM0+1),   cr1 / cr2, "scmatrix /= tcomplex", os, __LINE__);
        cr1 = scbm1(CVM0,CVM0+1);
        scbm1 /= cr2;
        CheckComplex (scbm1(CVM0+1,CVM0+1),  cr1 / cr2, "scbmatrix /= tcomplex", os, __LINE__);

        srbm << srbm1;
        CheckReal (srbm(CVM0+1,CVM0+2), srbm1(CVM0+1,CVM0+2), "srbmatrix << srbmatrix",    os, __LINE__);
        CheckReal (srbm(CVM0,CVM0+3), srbm1(CVM0,CVM0+3), "srbmatrix << srbmatrix",    os, __LINE__);
        scbm << scbm1;
        CheckComplex (scbm(CVM0+1,CVM0+2), scbm1(CVM0+1,CVM0+2), "scbmatrix << scbmatrix",    os, __LINE__);
        CheckComplex (scbm(CVM0,CVM0+3), scbm1(CVM0,CVM0+3), "scbmatrix << scbmatrix",    os, __LINE__);


        srs2.set ((treal) 2.3);
        CheckReal (srs2(CVM0,CVM0+2), (treal) 2.3,  "srsmatrix.set",  os, __LINE__, dPessimisticSp);
        CheckReal (srs2(CVM0+2,CVM0+1), (treal) 2.3,  "srsmatrix.set",  os, __LINE__, dPessimisticSp);

        sch2.set_real((treal) 2.3);
        CheckReal (sch2.real()(CVM0,CVM0+2), (treal) 2.3,  "schmatrix.set_real",  os, __LINE__, dPessimisticSp);
        CheckReal (sch2.real()(CVM0+2,CVM0+1), (treal) 2.3,  "schmatrix.set_real",  os, __LINE__, dPessimisticSp);



        r1 = (treal) -0.127;
        rv.set(r1);
        CheckReal (rv[CVM0],     r1,  "rvector = treal",      os, __LINE__);
        rm.set(r1);
        CheckReal (rm[CVM0][CVM0+1],  r1,  "rmatrix = treal",      os, __LINE__);
        srm.set(r1);
        CheckReal (srm(CVM0+1,CVM0+1), r1,  "srmatrix = treal",     os, __LINE__);
        srbm.set(r1);
        CheckReal (srbm(CVM0+1,CVM0+2), r1, "srbmatrix = treal",    os, __LINE__);
        CheckReal (srbm(CVM0,CVM0+3), 0,  "srbmatrix = treal, 0", os, __LINE__);

        cr2 = tcomplex ((treal) 1.3, (treal) -0.9);
        cv.set(cr1);
        CheckComplex (cv[CVM0+6],     cr1,  "cvector = tcomplex",   os, __LINE__);
        cm.set(cr1);
        CheckComplex (cm[CVM0+1][CVM0+2],  cr1,  "cmatrix = tcomplex",   os, __LINE__);
        scm.set(cr1);
        CheckComplex (scm(CVM0+2,CVM0+1), cr1,  "scmatrix = tcomplex",  os, __LINE__);
        scbm.set(cr1);
        CheckComplex (scbm(CVM0+2,CVM0+1),cr1,  "scbmatrix = tcomplex", os, __LINE__);

        rv.assign(CVM0+1, a2);
        CheckReal (rv[CVM0+2],      a2[1], "shifted rvector = treal*",      os, __LINE__);
        rv.assign(a2);
        CheckReal (rv[CVM0+2],      a2[2], "rvector = treal*",      os, __LINE__);
        rm.assign(a2);
        CheckReal (rm(CVM0,CVM0+2),   a2[4], "rmatrix = treal*",      os, __LINE__);
        srm.assign(a2);
        CheckReal (srm(CVM0+2,CVM0+2),  a2[8], "srmatrix = treal*",     os, __LINE__);
        srbm.assign(a2);
        CheckReal (srbm(CVM0,CVM0), a2[2], "srbmatrix = treal*",    os, __LINE__);
        CheckReal (srbm(CVM0+1,CVM0+2), a2[9], "srbmatrix = treal*",    os, __LINE__);

        cv.assign(CVM0+1, c1);
        CheckComplex (cv[CVM0+2],      c1[1], "shifted cvector = tcomplex*",   os, __LINE__);
        cv.assign(c1);
        CheckComplex (cv[CVM0+2],      c1[2], "cvector = tcomplex*",   os, __LINE__);
        cm.assign(c1);
        CheckComplex (cm(CVM0,CVM0+2),    c1[4], "cmatrix = tcomplex*",   os, __LINE__);
        scm.assign(c1);
        CheckComplex (scm(CVM0+2,CVM0+2),  c1[8], "scmatrix = tcomplex*",  os, __LINE__);
        scbm.assign(c1);
        CheckComplex (scbm(CVM0,CVM0), c1[2], "scbmatrix = tcomplex*",    os, __LINE__);
        CheckComplex (scbm(CVM0+1,CVM0+2), c1[9], "scbmatrix = tcomplex*",    os, __LINE__);

        {   // sub-assignment
            rvector rv2(4);
            rv2.randomize((treal) -3., (treal) 2.);
            rv.assign(CVM0+2, rv2);
            CheckReal (rv[CVM0+2], rv2[CVM0], "rvector subvector assignment",    os, __LINE__);
            CheckReal (rv[CVM0+5], rv2[CVM0+3], "rvector subvector assignment",    os, __LINE__);

            cvector cv2(4);
            cv2.randomize_real((treal) -3., (treal) 2.);
            cv2.randomize_imag((treal) -3., (treal) 2.);
            cv.assign(CVM0+2, cv2);
            CheckComplex (cv[CVM0+2], cv2[CVM0], "cvector subvector assignment",    os, __LINE__);
            CheckComplex (cv[CVM0+5], cv2[CVM0+3], "cvector subvector assignment",    os, __LINE__);

            rm.randomize((treal) -3., (treal) 2.);
            rm2.assign(CVM0+1, CVM0+1, rm);
            CheckReal (rm2(CVM0+1,CVM0+1), rm(CVM0,CVM0), "rmatrix submatrix assignment",    os, __LINE__);
            CheckReal (rm2(CVM0+2,CVM0+3), rm(CVM0+1,CVM0+2), "rmatrix submatrix assignment",    os, __LINE__);

            srmatrix srm (5);
            srm.randomize((treal) -3., (treal) 2.);
            srm.assign(CVM0+1, CVM0+1, rm);
            CheckReal (srm(CVM0+1,CVM0+1), rm(CVM0,CVM0), "srmatrix submatrix assignment",    os, __LINE__);
            CheckReal (srm(CVM0+2,CVM0+3), rm(CVM0+1,CVM0+2), "srmatrix submatrix assignment",    os, __LINE__);

            cm.randomize_real((treal) -3., (treal) 2.);
            cm.randomize_imag((treal) -3., (treal) 2.);
            cm2.assign(CVM0+1, CVM0+1, cm);
            CheckComplex (cm2(CVM0+1,CVM0+1), cm(CVM0,CVM0), "cmatrix submatrix assignment",    os, __LINE__);
            CheckComplex (cm2(CVM0+2,CVM0+3), cm(CVM0+1,CVM0+2), "cmatrix submatrix assignment",    os, __LINE__);

            scmatrix scm (5);
            scm.randomize_real((treal) -3., (treal) 2.);
            scm.randomize_imag((treal) -3., (treal) 2.);
            scm.assign(CVM0+1, CVM0+1, cm);
            CheckComplex (scm(CVM0+1,CVM0+1), cm(CVM0,CVM0), "scmatrix submatrix assignment",    os, __LINE__);
            CheckComplex (scm(CVM0+2,CVM0+3), cm(CVM0+1,CVM0+2), "scmatrix submatrix assignment",    os, __LINE__);

            tint ns = srs1.msize();
            srs1.resize(5);
            srs2.randomize((treal) -3., (treal) 2.);
            srs1.assign(CVM0+2,srs2);
            CheckReal (srs1(CVM0+2,CVM0+2), srs2(CVM0,CVM0), "srsmatrix submatrix assignment",    os, __LINE__);
            CheckReal (srs1(CVM0+3,CVM0+4), srs2(CVM0+1,CVM0+2), "srsmatrix submatrix assignment",    os, __LINE__);
            srs1.resize(ns);

            ns = sch1.msize();
            sch1.resize(5);
            sch2.randomize_real((treal) -3., (treal) 2.);
            sch2.randomize_imag((treal) -3., (treal) 2.);
            sch1.assign(CVM0+2,sch2);
            CheckComplex (sch1(CVM0+2,CVM0+2), sch2(CVM0,CVM0), "schmatrix submatrix assignment",    os, __LINE__);
            CheckComplex (sch1(CVM0+3,CVM0+4), sch2(CVM0+1,CVM0+2), "schmatrix submatrix assignment",    os, __LINE__);
            sch1.resize(ns);
        }

        srs2 = srs1;
        CheckBool (srs1 == srs2, true,            "srsmatrix ==",   os, __LINE__);
        srs2.set(CVM0+1,CVM0+2,srs2(CVM0+1,CVM0+2) + (treal)0.000001);
        CheckBool (srs1 == srs2, false,            "srsmatrix ==",   os, __LINE__);
        CheckBool (srs1 != srs2, true,            "srsmatrix !=",   os, __LINE__);

        sch2 = sch1;
        CheckBool (sch1 == sch2, true,            "schmatrix ==",   os, __LINE__);
        sch2.set(CVM0+1,CVM0+2,sch2(CVM0+1,CVM0+2) + tcomplex ((treal)0.000001, (treal)0.00001));
        CheckBool (sch1 == sch2, false,            "schmatrix ==",   os, __LINE__);
        CheckBool (sch1 != sch2, true,            "schmatrix !=",   os, __LINE__);



        rv1 = rv;
        CheckReal (rv1[CVM0+2],       rv(CVM0+2),        "rvector = rvector",                             os, __LINE__);
        CheckBool (rv1 == rv, true,            "rvector ==",                                    os, __LINE__);
        CheckBool (rv1 != rv, false,           "rvector !=",                                    os, __LINE__);
        rm1 = rm;
        CheckReal (rm1[CVM0+1][CVM0+2],    rm(CVM0+1, CVM0+2),     "rmatrix = rmatrix",                             os, __LINE__);
        CheckBool (rm1 == rm, true,            "rmatrix ==",                                    os, __LINE__);
        CheckBool (rm1 != rm, false,           "rmatrix !=",                                    os, __LINE__);
        CheckBool (rm1[CVM0] == rm[CVM0], true,      "rmatrix = rmatrix, rm1[1] == rm[1]",            os, __LINE__);
        CheckBool (rm1(CVM0+1) != rm(CVM0+1), false,     "rmatrix = rmatrix, rm1(2) != rm(2)",            os, __LINE__);
        srm1 = srm;
        CheckReal (srm1[CVM0+1][CVM0+2],   srm(CVM0+1,CVM0+2),    "srmatrix = srmatrix",                           os, __LINE__);
        CheckBool (srm1 == srm, true,          "srmatrix ==",                                   os, __LINE__);
        CheckBool (srm1 != srm, false,         "srmatrix !=",                                   os, __LINE__);
        CheckBool (srm1[CVM0] == srm[CVM0], true,    "srmatrix = srmatrix, srm1[1] == srm[1]",        os, __LINE__);
        CheckBool (srm1(CVM0+1) != srm(CVM0+1), false,   "srmatrix = srmatrix, srm1(2) != srm(2)",        os, __LINE__);
        srbm1 = srbm;
        CheckReal (srbm1[CVM0+1][CVM0],  srbm(CVM0+1,CVM0),   "srbmatrix = srbmatrix",                         os, __LINE__);
        CheckBool (srbm1 == srbm, true,        "srbmatrix ==",                                  os, __LINE__);
        CheckBool (srbm1 != srbm, false,       "srbmatrix !=",                                  os, __LINE__);
        CheckBool (srbm1[CVM0] == srbm[CVM0], true,  "srbmatrix = srbmatrix, srbm1[1] == srbm[1]",    os, __LINE__);
        CheckBool (srbm1(CVM0+1) != srbm(CVM0+1), false, "srbmatrix = srbmatrix, srbm1(2) != srbm(2)",    os, __LINE__);

        cv1 = cv;
        CheckComplex  (cv1[CVM0+3],   cv(CVM0+3),        "cvector = cvector",                             os, __LINE__);
        CheckBool (cv1 == cv, true,            "cvector ==",                                    os, __LINE__);
        CheckBool (cv1 != cv, false,           "cvector !=",                                    os, __LINE__);
        cm1 = cm;
        CheckComplex  (cm1[CVM0+1][CVM0],   cm(CVM0+1,CVM0),   "cmatrix = cmatrix",                             os, __LINE__);
        CheckBool (cm1 == cm, true,            "cmatrix ==",                                    os, __LINE__);
        CheckBool (cm1 != cm, false,           "cmatrix !=",                                    os, __LINE__);
        CheckBool (cm1[CVM0] == cm[CVM0], true,      "cmatrix = cmatrix, cm1[1] == cm[1]",            os, __LINE__);
        CheckBool (cm1(CVM0+1) != cm(CVM0+1), false,     "cmatrix = cmatrix, cm1(2) != cm(2)",            os, __LINE__);
        scm1 = scm;
        CheckComplex  (scm1[CVM0+1][CVM0],   scm(CVM0+1,CVM0), "scmatrix = scmatrix",                           os, __LINE__);
        CheckBool (scm1 == scm, true,          "scmatrix ==",                                   os, __LINE__);
        CheckBool (scm1 != scm, false,         "scmatrix !=",                                   os, __LINE__);
        CheckBool (scm1[CVM0] == scm[CVM0], true,    "scmatrix = scmatrix, scm1[1] == scm[1]",        os, __LINE__);
        CheckBool (scm1(CVM0+1) != scm(CVM0+1), false,   "scmatrix = scmatrix, scm1(2) != scm(2)",        os, __LINE__);
        scbm1 = scbm;
        CheckComplex (scbm1[CVM0+1][CVM0],  scbm(CVM0+1, CVM0),   "scbmatrix = scbmatrix",                      os, __LINE__);
        CheckBool (scbm1 == scbm, true,        "scbmatrix ==",                                  os, __LINE__);
        CheckBool (scbm1 != scbm, false,       "scbmatrix !=",                                  os, __LINE__);
        CheckBool (scbm1[CVM0] == scbm[CVM0], true,  "scbmatrix = scbmatrix, scbm1[1] == scbm[1]",    os, __LINE__);
        CheckBool (scbm1(CVM0+1) != scbm(CVM0+1), false, "scbmatrix = scbmatrix, scbm1(2) != scbm(2)",    os, __LINE__);

//        rv2 = rv + rv1;   // wouldn't work because rv and rv1 share the same array!
        rv3.resize(10);
        rv3 = rv + rv1;
        CheckReal (rv3[CVM0],   rv(CVM0) + rv1[CVM0],                     "rvector + rvector",           os, __LINE__);
        CheckReal (rv3[CVM0+9],  rv(CVM0+9) + rv1[CVM0+9],                   "rvector + rvector",           os, __LINE__);
        rv3 = rv - rv1;
        CheckReal (rv3[CVM0],   rv(CVM0) - rv1[CVM0],                     "rvector - rvector",           os, __LINE__);
        CheckReal (rv3[CVM0+9],  rv(CVM0+9) - rv1[CVM0+9],                   "rvector - rvector",           os, __LINE__);
        cv3 = cv + cv1;
        CheckComplex (cv3[CVM0],   cv(CVM0) + cv1[CVM0],                  "cvector + cvector",           os, __LINE__);
        CheckComplex (cv3[CVM0+9],  cv(CVM0+9) + cv1[CVM0+9],                "cvector + cvector",           os, __LINE__);
        cv3 = cv - cv1;
        CheckComplex (cv3[CVM0],   cv(CVM0) - cv1[CVM0],                  "cvector - cvector",           os, __LINE__);
        CheckComplex (cv3[CVM0+9],  cv(CVM0+9) - cv1[CVM0+9],                "cvector - cvector",           os, __LINE__);
        rm2.resize(2,3);
        rm = rm1 + rm2;
        CheckReal (rm[CVM0][CVM0],   rm1(CVM0,CVM0) + rm2(CVM0,CVM0),              "rmatrix + rmatrix",           os, __LINE__);
        CheckReal (rm[CVM0+1].norm(), (rm1[CVM0+1] + rm2[CVM0+1]).norm(),       "rmatrix + rmatrix",           os, __LINE__);
        rm = rm1 - rm2;
        CheckReal (rm[CVM0][CVM0],   rm1(CVM0,CVM0) - rm2(CVM0,CVM0),              "rmatrix - rmatrix",           os, __LINE__);
        CheckReal (rm(CVM0+2).norm(), (rm1(CVM0+2) - rm2(CVM0+2)).norm(),       "rmatrix - rmatrix",           os, __LINE__);
        cm2.resize(2,3);
        cm = cm1 + cm2;
        CheckComplex (cm[CVM0][CVM0], cm1(CVM0,CVM0) + cm2(CVM0,CVM0),             "cmatrix + cmatrix",           os, __LINE__);
        CheckComplex (cm[CVM0+1].norm(), (cm1[CVM0+1] + cm2[CVM0+1]).norm(),    "cmatrix + cmatrix",           os, __LINE__);
        cm = cm1 - cm2;
        CheckComplex (cm[CVM0][CVM0],   cm1(CVM0,CVM0) - cm2(CVM0,CVM0),           "cmatrix - cmatrix",           os, __LINE__);
        CheckComplex (cm(CVM0+2).norm(), (cm1(CVM0+2) - cm2(CVM0+2)).norm(),    "cmatrix - cmatrix",           os, __LINE__);
        srm = srm1 + srm2;
        CheckReal (srm[CVM0][CVM0],   srm1(CVM0,CVM0) + srm2(CVM0,CVM0),           "srmatrix + srmatrix",         os, __LINE__);
        CheckReal (srm[CVM0+2].norm(), (srm1[CVM0+2] + srm2[CVM0+2]).norm(),    "srmatrix + srmatrix",         os, __LINE__);
        srm = srm1 - srm2;
        CheckReal (srm[CVM0][CVM0],   srm1(CVM0,CVM0) - srm2(CVM0,CVM0),           "srmatrix - srmatrix",         os, __LINE__);
        CheckReal (srm(CVM0+2).norm(), (srm1(CVM0+2) - srm2(CVM0+2)).norm(),    "srmatrix - srmatrix",         os, __LINE__);
        scm = scm1 + scm2;
        CheckComplex (scm[CVM0][CVM0],   scm1(CVM0,CVM0) + scm2(CVM0,CVM0),        "scmatrix + scmatrix",         os, __LINE__);
        CheckComplex (scm(CVM0+2).norm(), (scm1(CVM0+2) + scm2(CVM0+2)).norm(), "scmatrix + scmatrix",         os, __LINE__);
        scm = scm1 - scm2;
        CheckComplex (scm[CVM0][CVM0],   scm1(CVM0,CVM0) - scm2(CVM0,CVM0),        "scmatrix - scmatrix",         os, __LINE__);
        CheckComplex (scm(CVM0+2).norm(), (scm1(CVM0+2) - scm2(CVM0+2)).norm(), "scmatrix - scmatrix",         os, __LINE__);

        srbmatrix srbm2 (a2, 4, 1, 2);
        srbm = srbm1 + srbm2;
        CheckReal (srbm[CVM0][CVM0],   srbm1(CVM0,CVM0) + srbm2(CVM0,CVM0),        "srbmatrix + srbmatrix",       os, __LINE__);
        CheckReal (srbm(CVM0+2).norm(), (srbm1(CVM0+2) + srbm2(CVM0+2)).norm(), "srbmatrix + srbmatrix",       os, __LINE__);
        srbm = srbm1 - srbm2;
        CheckReal (srbm[CVM0][CVM0],   srbm1(CVM0,CVM0) - srbm2(CVM0,CVM0),        "srbmatrix - srbmatrix",       os, __LINE__);
        CheckReal (srbm(CVM0+2).norm(), (srbm1(CVM0+2) - srbm2(CVM0+2)).norm(), "srbmatrix - srbmatrix",       os, __LINE__);

        scbmatrix scbm2 (c1, 4, 1, 2);
        scbm = scbm1 + scbm2;
        CheckComplex (scbm[CVM0][CVM0],   scbm1(CVM0,CVM0) + scbm2(CVM0,CVM0),        "scbmatrix + scbmatrix",       os, __LINE__);
        CheckReal    (scbm(CVM0+2).norm(), (scbm1(CVM0+2) + scbm2(CVM0+2)).norm(), "scbmatrix + scbmatrix",       os, __LINE__);
        scbm = scbm1 - scbm2;
        CheckComplex (scbm[CVM0][CVM0],   scbm1(CVM0,CVM0) - scbm2(CVM0,CVM0),        "scbmatrix - scbmatrix",       os, __LINE__);
        CheckReal    (scbm(CVM0+2).norm(), (scbm1(CVM0+2) - scbm2(CVM0+2)).norm(), "scbmatrix - scbmatrix",       os, __LINE__);

        srs2 = srs1;
        rs1 = srs1(CVM0,CVM0+1);
        CheckReal    ((srs1 + srs2)(CVM0,CVM0+1), rs1 + rs1, "srsmatrix + srsmatrix",       os, __LINE__);
        CheckReal    ((srs1 - srs2).norm(), (treal)0., "srsmatrix - srsmatrix",       os, __LINE__);

        sch2 = sch1;
        cs1 = sch1(CVM0,CVM0+1);
        CheckComplex ((sch1 + sch2)(CVM0,CVM0+1), cs1 + cs1, "schmatrix + schmatrix",       os, __LINE__);
        CheckReal    ((sch1 - sch2).norm(), (treal)0., "schmatrix - schmatrix",       os, __LINE__);


        int n1 = -2;
        r1     = -2.;
        rv1 = rv * r1;
        rv3 = n1 * rv;
        CheckReal    (rv3[CVM0+2],   rv1[CVM0+2],         "rvector * number",                             os, __LINE__);
        rv3 = r1 * rv;
        CheckReal    (rv3[CVM0+2],   rv1[CVM0+2],         "rvector * number",                             os, __LINE__);
        cv1 = cv * r1;
        cv3 = n1 * cv;
        CheckComplex (cv3[CVM0+2],   cv1[CVM0+2],         "cvector * number",                             os, __LINE__);
        cv3 = r1 * cv;
        CheckComplex (cv3[CVM0+2],   cv1[CVM0+2],         "cvector * number",                             os, __LINE__);
        rm1 = rm * r1;
        rm2 = n1 * rm;
        CheckReal    (rm1(CVM0+1,CVM0+2), rm2(CVM0+1,CVM0+2),       "rmatrix * number",                             os, __LINE__);
        rm2 = r1 * rm;
        CheckReal    (rm1(CVM0+1,CVM0+2), rm2(CVM0+1,CVM0+2),       "rmatrix * number",                             os, __LINE__);
        cm1 = cm * r1;
        cm2 = n1 * cm;
        CheckComplex (cm2(CVM0+1,CVM0+2), cm1(CVM0+1,CVM0+2),       "cmatrix * number",                             os, __LINE__);
        cm2 = r1 * cm;
        CheckComplex (cm2(CVM0+1,CVM0+2), cm1(CVM0+1,CVM0+2),       "cmatrix * number",                             os, __LINE__);
        srm1 = srm * r1;
        srm2 = n1 * srm;
        CheckReal    (srm1(CVM0+1,CVM0+2), srm2(CVM0+1,CVM0+2),     "srmatrix * number",                            os, __LINE__);
        srm2 = r1 * srm;
        CheckReal    (srm1(CVM0+1,CVM0+2), srm2(CVM0+1,CVM0+2),     "srmatrix * number",                            os, __LINE__);
        scm.assign(c1);
        scm1 = scm * r1;
        scm2 = n1 * scm;
        CheckComplex (scm2(CVM0+1,CVM0+2), scm1(CVM0+1,CVM0+2),     "scmatrix * number",                            os, __LINE__);
        scm2 = r1 * scm;
        CheckComplex (scm2(CVM0+1,CVM0+2), scm1(CVM0+1,CVM0+2),     "scmatrix * number",                            os, __LINE__);
        srbm1 = srbm * r1;
        srbm2 = n1 * srbm;
        CheckReal    (srbm1(CVM0+1,CVM0+2), srbm2(CVM0+1,CVM0+2),   "srbmatrix * number",                           os, __LINE__);
        srbm2 = r1 * srbm;
        CheckReal    (srbm1(CVM0+1,CVM0+2), srbm2(CVM0+1,CVM0+2),   "srbmatrix * number",                           os, __LINE__);
        scbm1 = scbm * r1;
        scbm2 = n1 * scbm;
        CheckComplex (scbm1(CVM0+1,CVM0+2), scbm2(CVM0+1,CVM0+2),   "scbmatrix * number",                           os, __LINE__);
        scbm2 = r1 * scbm;
        CheckComplex (scbm1(CVM0+1,CVM0+2), scbm2(CVM0+1,CVM0+2),   "scbmatrix * number",                           os, __LINE__);
        cr1 = r1;
        scbm2 = cr1 * scbm;
        CheckComplex (scbm1(CVM0+1,CVM0+2), scbm2(CVM0+1,CVM0+2),   "scbmatrix * number",                           os, __LINE__);

        rv1 = rv / r1;
        CheckReal    (rv1[CVM0+9],   rv[CVM0+9] / r1,       "rvector / number",                         os, __LINE__);
        cv1 = cv / r1;
        CheckComplex (cv1[CVM0+9],   cv[CVM0+9] / r1,       "cvector / number",                         os, __LINE__);
        rm1 = rm / r1;
        CheckReal    (rm1(CVM0+1,CVM0+2), rm(CVM0+1,CVM0+2) / r1,       "rmatrix / number",                         os, __LINE__);
        cm1 = cm / r1;
        CheckComplex (cm1(CVM0+1,CVM0+2), cm(CVM0+1,CVM0+2) / r1,       "cmatrix / number",                         os, __LINE__);

        srm1 = srm / r1;
        CheckReal    (srm1(CVM0+1,CVM0+2), srm(CVM0+1,CVM0+2) / r1,     "srmatrix / number",                        os, __LINE__);
        scm1 = scm / r1;
        CheckComplex (scm1(CVM0+1,CVM0+2), scm(CVM0+1,CVM0+2) / r1,     "scmatrix / number",                        os, __LINE__);
        srbm1 = srbm / r1;
        CheckReal    (srbm1(CVM0+1,CVM0+2), srbm(CVM0+1,CVM0+2) / r1,   "srbmatrix / number",                       os, __LINE__);
        scbm1 = scbm / r1;
        CheckComplex (scbm1(CVM0+1,CVM0+2), scbm(CVM0+1,CVM0+2) / r1,   "scbmatrix / number",                       os, __LINE__);
        scbm1 = scbm / cr1;
        CheckComplex (scbm1(CVM0+1,CVM0+2), scbm(CVM0+1,CVM0+2) / cr1,  "scbmatrix / number",                       os, __LINE__);

        cv1 = cv  * cr2;
        cv3 = cr2 * cv;
        CheckComplex (cv3[CVM0+2],   cv1[CVM0+2],         "cvector * cmplx number",                       os, __LINE__);
        cm1 = cm * cr2;
        cm2 = cr2 * cm;
        CheckComplex (cm2(CVM0+1,CVM0+2), cm1(CVM0+1,CVM0+2),       "cmatrix * cmplx number",                       os, __LINE__);
        scm1 = scm * cr2;
        scm2 = cr2 * scm;
        CheckComplex (scm2(CVM0+1,CVM0+2), scm1(CVM0+1,CVM0+2),     "scmatrix * cmplx number",                      os, __LINE__);
        scbm1 = scbm * cr2;
        scbm2 = cr2 * scbm;
        CheckComplex (scbm2(CVM0+1,CVM0+2), scbm1(CVM0+1,CVM0+2),   "scbmatrix * cmplx number",                     os, __LINE__);

        rv1 = - rv;
        CheckReal    (rv1[CVM0+9],   - rv[CVM0+9],      "- rvector",                                    os, __LINE__);
        cv1 = - cv;
        CheckComplex (cv1[CVM0+9],   - cv[CVM0+9],      "- cvector",                                    os, __LINE__);
        rm1 = - rm;
        CheckReal    (rm1(CVM0+1,CVM0+2), - rm(CVM0+1,CVM0+2),      "- rmatrix",                                    os, __LINE__);
        cm1 = - cm;
        CheckComplex (cm1(CVM0+1,CVM0+2), - cm(CVM0+1,CVM0+2),      "- cmatrix",                                    os, __LINE__);
        srm1 = - srm;
        CheckReal    (srm1(CVM0+1,CVM0+2), - srm(CVM0+1,CVM0+2),    "- srmatrix",                                   os, __LINE__);
        scm1 = - scm;
        CheckComplex (scm1(CVM0+1,CVM0+2), - scm(CVM0+1,CVM0+2),    "- scmatrix",                                   os, __LINE__);
        srbm.assign(a2);
        srbm1 = - srbm;
        CheckReal    (srbm1(CVM0+1,CVM0+2), - srbm(CVM0+1,CVM0+2),  "- srbmatrix",                                  os, __LINE__);
        scbm.assign(c1);
        scbm1 = - scbm;
        CheckComplex (scbm1(CVM0+1,CVM0+2), - scbm(CVM0+1,CVM0+2),  "- scbmatrix",                                  os, __LINE__);


        rv1.set((treal)1.17);
        rv2.set((treal)-0.31);
        rm2.set((treal)9.01);
        srbm1.set((treal)13.1);
        srbm2.set((treal)5.51);
        cv1.set(tcomplex (2,1));
        cv2.set(tcomplex (-1,3));
        cm2.set(tcomplex (-4,3));
        rv1.resize (2);
        rv2.resize (3);
        rv2.mult (rv1, rm2);

        CheckReal    (rv2[CVM0], rv1 * rm2(CVM0),  "mult",                                            os, __LINE__, dPessimisticSp);
        rv1.mult (rm2, rv2);
        CheckReal    (rv1[CVM0], rv2 * rm2[CVM0],  "mult",                                            os, __LINE__, dPessimisticSp);

        cv1.resize (2);
        cv2.resize (3);
        cv2.mult (cv1, cm2);

        CheckComplex (cv2[CVM0], cv1 * cm2(CVM0),  "mult",                                            os, __LINE__, dPessimisticSp);
        cv1.mult (cm2, cv2);
        CheckComplex (cv1[CVM0], cv2 * cm2[CVM0],  "mult",                                            os, __LINE__, dPessimisticSp);

        rv1.resize (3);
        rv1.mult (srm2, rv2);
        CheckReal    (rv1[CVM0], rv2 * srm2[CVM0],  "mult",                                           os, __LINE__, dPessimisticSp);
        rv2.mult (rv1, srm2);

        CheckReal    (rv2[CVM0], rv1 * srm2(CVM0),  "mult",                                           os, __LINE__, dPessimisticSp);

        cv1.resize (3);
        cv1.mult (scm2, cv2);
        CheckComplex (cv1[CVM0], cv2 * scm2[CVM0],  "mult",                                           os, __LINE__, dPessimisticSp);
        cv2.mult (cv1, scm2);
        CheckComplex (cv2[CVM0], cv1 * scm2(CVM0),  "mult",                                           os, __LINE__, dPessimisticSp);

        rv1.resize (4);
        rv2.resize (4);
        rv2.mult (rv1, srbm2);
        CheckReal    (rv2[CVM0], rv1 * srbm2(CVM0),  "mult",                                          os, __LINE__, dPessimisticSp);
        rv1.mult (srbm2, rv2);
        CheckReal    (rv1[CVM0], rv2 * srbm2[CVM0],  "mult",                                          os, __LINE__, dPessimisticSp);

        cv1.resize (4);
        cv2.resize (4);
        cv2.mult (cv1, scbm2);
        CheckComplex (cv2[CVM0], cv1 * scbm2(CVM0),  "mult",                                          os, __LINE__, dPessimisticSp);
        cv1.mult (scbm2, cv2);
        CheckComplex (cv1[CVM0], cv2 * scbm2[CVM0],  "mult",                                          os, __LINE__, dPessimisticSp);

        rm1.resize (3, 2);
        rm1[CVM0+2].assign(a1);
        rm3.resize (2, 2);
        rm4.resize (3, 3);
        rm3.mult (rm2, rm1);
        CheckReal    (rm3(CVM0+1,CVM0+1), rm2[CVM0+1] * rm1(CVM0+1),  "mult",                                       os, __LINE__, dPessimisticSp);
        rm4.mult (rm1, rm2);
        CheckReal    (rm4(CVM0+2,CVM0+2), rm1[CVM0+2] * rm2(CVM0+2),  "mult",                                       os, __LINE__, dPessimisticSp);
        srm4.resize(3);
        srm4.mult (rm1, rm2);

        CheckReal    (srm4(CVM0+2,CVM0+2), rm1[CVM0+2] * rm2(CVM0+2), "mult",                                       os, __LINE__, dPessimisticSp);
        rm4.resize (3, 2);
        rm1.mult (srm4, rm4);
        CheckReal    (rm1(CVM0+2,CVM0+1),  srm4[CVM0+2] * rm4(CVM0+1), "mult",                                      os, __LINE__, dPessimisticSp);
        srbm1.resize(3);
        rm1.mult (srbm1, rm4);
        CheckReal    (rm1(CVM0+2,CVM0+1),  srbm1[CVM0+2] * rm4(CVM0+1), "mult",                                     os, __LINE__, dPessimisticSp);
        rm1.mult (~srbm1, rm4);
        CheckReal    (rm1(CVM0+2,CVM0+1),  srbm1(CVM0+2) * rm4(CVM0+1), "mult",                                     os, __LINE__, dPessimisticSp);
        srbm1.mult (rm1, rm2);
        CheckReal    (srbm1(CVM0+1,CVM0+1), rm1[CVM0+1] * rm2(CVM0+1),  "mult",                                     os, __LINE__, dPessimisticSp);

        r1 = (treal) -0.031;
        r2 = (treal) 0.319;
        rm1.randomize(1., 2.);
        rm2.randomize(0., 1.);
        rm3.randomize(0., 1.);
        rmatrix rm3_dub = rm3;

        rm3.gemm (rm2, false, rm1, false, r1, r2);
        CheckReal    ((rm3 - (rm2 * rm1 * r1 + rm3_dub * r2)).norm2(), (treal) 0.,  "gemm",      os, __LINE__, dVeryPessimisticSp);
        rm3_dub = rm3;
        rm3 << ~rm3;
        rm3.gemm (rm1, true, rm2, true, r1, r2);
        CheckReal    ((~rm3 - (rm2 * rm1 * r1 + rm3_dub * r2)).norm2(), (treal) 0.,  "gemm",      os, __LINE__, dVeryPessimisticSp);

        srbm1.randomize((treal) -1., (treal) 3.);
        rmatrix rm1_dub = rm1;
        rm1.gemm (srbm1, false, rm4, false, r1, r2);
        CheckReal    ((rm1 - (srbm1 * rm4 * r1 + rm1_dub * r2)).norm2(), (treal) 0.,  "gemm",      os, __LINE__, dVeryPessimisticSp);




        cm1.resize (3, 2);
        cm1[CVM0+2].assign(c1);
        cmatrix cm3 (2, 2), cm4 (3,3);
        cm3.assign(c2);
        cm3.mult (cm2, cm1);
        CheckComplex (cm3(CVM0+1,CVM0+1), cm2[CVM0+1] * cm1(CVM0+1),  "mult",                                       os, __LINE__, dPessimisticSp);
        cm4.mult (cm1, cm2);
        CheckComplex (cm4(CVM0+2,CVM0+2), cm1[CVM0+2] * cm2(CVM0+2),  "mult",                                       os, __LINE__, dPessimisticSp);
        scm4.resize(3);
        scm4.mult (cm1, cm2);
        CheckComplex (scm4(CVM0+2,CVM0+2), cm1[CVM0+2] * cm2(CVM0+2), "mult",                                       os, __LINE__, dPessimisticSp);
        cm4.resize (3, 2);
        cm1.mult (scm4, cm4);
        CheckComplex (cm1(CVM0+2,CVM0+1),  scm4[CVM0+2] * cm4(CVM0+1), "mult",                                      os, __LINE__, dPessimisticSp);
        scbm.resize(3);
        scbm.set(tcomplex ((treal) 1.23, (treal) -0.912));
        cm1.mult (scbm, cm4);
        CheckComplex (cm1(CVM0+2,CVM0+1),  scbm[CVM0+2] * cm4(CVM0+1), "mult",                                     os, __LINE__, dPessimisticSp);
        cm1.mult (~scbm, cm4);
        CheckComplex (cm1(CVM0+2,CVM0+1),  ~(scbm(CVM0+2)) * cm4(CVM0+1), "mult",                                     os, __LINE__, dPessimisticSp);
        scbm1.resize(3);
        scbm1.mult (cm1, cm2);
        CheckComplex (scbm1(CVM0+1,CVM0+1), cm1[CVM0+1] * cm2(CVM0+1),  "mult",                                     os, __LINE__, dPessimisticSp);


        cm1.randomize_real(0., 1.);
        cm2.randomize_real(0., 1.);
        cm3.randomize_real(0., 1.);
        scbm.randomize_real(0., 1.);
        cmatrix cm3_dub = cm3;
        cm3.gemm (cm2, false, cm1, false, cr1, cr2);
        CheckReal    ((cm3 - (cm2 * cm1 * cr1 + cm3_dub * cr2)).norm(), (treal) 0.,  "gemm",      os, __LINE__, dPessimisticSp);
        cmatrix cm1_dub = cm1;
        cm1.gemm (scbm, false, cm4, false, cr1, cr2);
        CheckReal    ((cm1 - (scbm * cm4 * cr1 + cm1_dub * cr2)).norm(), (treal) 0.,  "gemm",      os, __LINE__, dPessimisticSp);

        cr1 = tcomplex ((treal)-1.14,(treal)3.22);
        cr2 = tcomplex ((treal)2.04,(treal)-4.2);
        cm1_dub << cm1;
        cm1.conj();
        cm1.gemm (cm4, true, scbm, true, cr1, cr2);
        CheckReal    ((~cm1 - (scbm * cm4 * conj(cr1) + cm1_dub * conj(cr2))).norm2(), (treal) 0.,  "gemm", os, __LINE__, dVeryPessimisticSp);
        cm1.conj();

        rv1.randomize((treal)0., (treal)1.);
        rv2.randomize((treal)0., (treal)1.);
        CheckReal    (rv1 * rv2, rv1[CVM0]*rv2[CVM0]+rv1[CVM0+1]*rv2[CVM0+1]+rv1[CVM0+2]*rv2[CVM0+2]+rv1[CVM0+3]*rv2[CVM0+3],  "scalar product", os, __LINE__, dPessimisticSp);
        cv1.randomize_real((treal)0., (treal)1.);

        cv1.randomize_imag((treal)0., (treal)1.);
        cv2.randomize_real((treal)0., (treal)1.);
        cv2.randomize_imag((treal)0., (treal)1.);
        CheckComplex (cv1 * cv2, cv1[CVM0]*cv2[CVM0]+cv1[CVM0+1]*cv2[CVM0+1]+cv1[CVM0+2]*cv2[CVM0+2]+cv1[CVM0+3]*cv2[CVM0+3],  "scalar product",  os, __LINE__, dPessimisticSp);
        CheckComplex (cv1 % cv2, conj(cv1[CVM0])*cv2[CVM0]+conj(cv1[CVM0+1])*cv2[CVM0+1]+conj(cv1[CVM0+2])*cv2[CVM0+2]+conj(cv1[CVM0+3])*cv2[CVM0+3],  "scalar product, conj",  os, __LINE__, dPessimisticSp);

        CheckReal    ((rm1[CVM0+1] - (~rm1)(CVM0+1)).norm(), (treal) 0.,  "~",                            os, __LINE__);

        cvector cm1_2_conj (cm1[CVM0+1].size());
        cm1_2_conj = cm1[CVM0+1];
        cm1_2_conj.conj();
        CheckReal    ((cm1_2_conj - (~cm1)(CVM0+1)).norm(), (treal) 0.,  "~",                     os, __LINE__);
        CheckReal    ((srbm1[CVM0+1] - (~srbm1)(CVM0+1)).norm(), (treal) 0.,  "~",                        os, __LINE__);
        CheckReal    ((~(scbm1[CVM0+1]) - (~scbm1)(CVM0+1)).norm(), (treal) 0.,  "~",                     os, __LINE__);

        rv1.resize (3);
        rv2.resize (2);
        rv1 = rm1 * rv2;
        CheckReal    (rv1[CVM0+2], rv2 * rm1[CVM0+2],  "rmatrix * rvector",                               os, __LINE__, dPessimisticSp);
        rv2 = rv1 * rm1;
        CheckReal    (rv2[CVM0+1], rv1 * rm1(CVM0+1),  "rvector * rmatrix",                               os, __LINE__, dPessimisticSp);
        cv1.resize (3);
        cv2.resize (2);
        cv1 = cm1 * cv2;
        CheckComplex (cv1[CVM0+2], cv2 * cm1[CVM0+2],  "cmatrix * cvector",                               os, __LINE__, dPessimisticSp);
        cv2 = cv1 * cm1;
        CheckComplex (cv2[CVM0+1], cv1 * cm1(CVM0+1),  "cvector * cmatrix",                               os, __LINE__, dPessimisticSp);

        rv2.resize (3);
        rv2 = srm4 * rv1;
        CheckReal    (rv2[CVM0+2], rv1 * srm4[CVM0+2],  "srmatrix * rvector",                             os, __LINE__, dPessimisticSp);
        rv2 = rv1 * srm4;
        CheckReal    (rv2[CVM0+2], rv1 * srm4(CVM0+2),  "rvector * srmatrix",                             os, __LINE__, dPessimisticSp);
        cv2.resize (3);
        cv2 = scm4 * cv1;
        CheckComplex (cv2[CVM0+2], cv1 * scm4[CVM0+2],  "scmatrix * cvector",                             os, __LINE__, dPessimisticSp);
        cv2 = cv1 * scm4;
        CheckComplex (cv2[CVM0+2], cv1 * scm4(CVM0+2),  "cvector * scmatrix",                             os, __LINE__, dPessimisticSp);

        srbm1.normalize();
        rv1.normalize();
        rv2 = srbm1 * rv1;
        CheckReal    (rv2[CVM0+2], rv1 * srbm1[CVM0+2],  "srbmatrix * rvector",                           os, __LINE__);
        rv2 = rv1 * srbm1;
        CheckReal    (rv2[CVM0+2], rv1 * srbm1(CVM0+2),  "rvector * srbmatrix",                           os, __LINE__);

        scbm1.normalize();
        cv1.normalize();
        cv2 = scbm1 * cv1;
        CheckComplex (cv2[CVM0+2], cv1 * scbm1[CVM0+2],  "scbmatrix * cvector",                           os, __LINE__);
        cv2 = cv1 * scbm1;
        CheckComplex (cv2[CVM0+2], cv1 * scbm1(CVM0+2),  "cvector * scbmatrix",                           os, __LINE__);

        rv2.resize (2);
        rm1 = rv1.rank1update (rv2);
        CheckReal    (rm1(CVM0+2,CVM0), rv1[CVM0+2] * rv2[CVM0],  "rank1update",                                os, __LINE__);
        rm1.rank1update (rv1, rv2);
        CheckReal    (rm1(CVM0+2,CVM0+1), rv1[CVM0+2] * rv2[CVM0+1],  "rank1update",                                os, __LINE__);

        cv2.resize (2);
        cv1.normalize();
        cv2.normalize();
        cm1 = cv1.rank1update_u (cv2);
        CheckComplex (cm1(CVM0+2,CVM0), cv1[CVM0+2] * cv2[CVM0],  "rank1update_u",                              os, __LINE__);
        cm1.rank1update_u (cv1, cv2);
        CheckComplex (cm1(CVM0+2,CVM0+1), cv1[CVM0+2] * cv2[CVM0+1],  "rank1update_u",                              os, __LINE__);
        cm1 = cv1.rank1update_c (cv2);
        CheckComplex (cm1(CVM0+2,CVM0), cv1[CVM0+2] * conj (cv2[CVM0]),  "rank1update_c",                       os, __LINE__);
        cm1.rank1update_c (cv1, cv2);
        CheckComplex (cm1(CVM0+2,CVM0+1), cv1[CVM0+2] * conj (cv2[CVM0+1]),  "rank1update_c",                       os, __LINE__);

        srm4.assign(a3);
        srm4(CVM0+2,CVM0+2) = -(treal) 1.;
        srm4.normalize();
        CheckReal    (srm4.cond(), (treal) 1. / (srm4.norminf() * srm4.inv().norminf()),  "cond", os, __LINE__, dVeryPessimisticSp);
        treal dt = srm4.det();
        CheckReal    (dt, srm4(CVM0,CVM0) * srm4(CVM0+1,CVM0+1) * srm4(CVM0+2,CVM0+2) -
                                  srm4(CVM0,CVM0) * srm4(CVM0+1,CVM0+2) * srm4(CVM0+2,CVM0+1) -
                                  srm4(CVM0,CVM0+1) * srm4(CVM0+1,CVM0) * srm4(CVM0+2,CVM0+2) +
                                  srm4(CVM0,CVM0+1) * srm4(CVM0+1,CVM0+2) * srm4(CVM0+2,CVM0) +
                                  srm4(CVM0,CVM0+2) * srm4(CVM0+1,CVM0) * srm4(CVM0+2,CVM0+1) -
                                  srm4(CVM0,CVM0+2) * srm4(CVM0+1,CVM0+1) * srm4(CVM0+2,CVM0),  "det", os, __LINE__);

        scm4.assign(c2);
        scm4.normalize();
        CheckReal    (scm4.cond(), (treal) 1. / (scm4.norminf() * scm4.inv().norminf()),  "cond", os, __LINE__, dVeryPessimisticSp);
        tcomplex dtc = scm4.det();
        CheckComplex (dtc, scm4(CVM0,CVM0) * scm4(CVM0+1,CVM0+1) * scm4(CVM0+2,CVM0+2) -
                                  scm4(CVM0,CVM0) * scm4(CVM0+1,CVM0+2) * scm4(CVM0+2,CVM0+1) -
                                  scm4(CVM0,CVM0+1) * scm4(CVM0+1,CVM0) * scm4(CVM0+2,CVM0+2) +
                                  scm4(CVM0,CVM0+1) * scm4(CVM0+1,CVM0+2) * scm4(CVM0+2,CVM0) +
                                  scm4(CVM0,CVM0+2) * scm4(CVM0+1,CVM0) * scm4(CVM0+2,CVM0+1) -
                                  scm4(CVM0,CVM0+2) * scm4(CVM0+1,CVM0+1) * scm4(CVM0+2,CVM0),  "det", os, __LINE__);


        r1 = (treal) 2.;
        rv1.resize (4);
        rm1.resize (4, 4);
        srbm1.resize (4);
        rv1.set(1.);
        rm1 << eye_real(4);
        srm4 << eye_real(4);
        srbm1 << srbmatrix (eye_real(4), 0, 0);

        CheckReal    (rv1.norm(), r1,  "rvector norm", os, __LINE__);
        CheckReal    (rm1.norm(), r1,  "rmatrix norm", os, __LINE__);
        CheckReal    (srm4.norm(), r1,  "srmatrix norm", os, __LINE__);
        CheckReal    (srbm1.norm(), r1,  "srbmatrix norm", os, __LINE__);

        r1 = (treal) 2. * (treal) sqrt ((treal) 2.);
        cv1.resize (4);


        cm1.resize (4, 4);
        scm1.resize (4);
        cv1.set(tcomplex (1, 1));
        cm1 << scmatrix (cv1);
        scm1 = cm1;
        scbm2 = scbmatrix(cm1, scbm2.lsize(), scbm2.usize());

        CheckReal    (cv1.norm(), r1,  "cvector norm", os, __LINE__);
        CheckReal    (cm1.norm(), r1,  "cmatrix norm", os, __LINE__);
        CheckReal    (scm1.norm(), r1,  "scmatrix norm", os, __LINE__);
        CheckReal    (scbm2.norm(), r1,  "scbmatrix norm", os, __LINE__);

        // mix
        scbm2.set(tcomplex((treal)1.23, (treal)-0.977));

        cm1 = scbm2;
        CheckComplex (cm1(CVM0+1,CVM0+2), scbm2(CVM0+1,CVM0+2),  "mix cmatrix  scbm", os, __LINE__);
        CheckComplex (cm1(CVM0+3,CVM0), scbm2(CVM0+3,CVM0),  "mix cmatrix  scbm", os, __LINE__);

        cm1 = cm1 + scbm2;
        cm1 += scbm2;
        CheckComplex (cm1(CVM0+1,CVM0+2), scbm2(CVM0+1,CVM0+2) * 3,  "mix cmatrix  scbm", os, __LINE__);
        CheckComplex (cm1(CVM0+3,CVM0), scbm2(CVM0+3,CVM0) * (treal)3.,  "mix cmatrix  scbm", os, __LINE__);
        CheckComplex (cm1(CVM0,CVM0+1), 3 * scbm2(CVM0+1,CVM0+2),  "mix cmatrix  scbm", os, __LINE__);
        CheckComplex (cm1(CVM0+1,CVM0), 3. * scbm2(CVM0+1,CVM0),  "mix cmatrix  scbm", os, __LINE__);

        rm1 = srbm2;
        CheckReal    (rm1(CVM0+1,CVM0+2), srbm2(CVM0+1,CVM0+2),  "mix rmatrix  srbm", os, __LINE__);
        CheckReal    (rm1(CVM0+3,CVM0), srbm2(CVM0+3,CVM0),  "mix rmatrix  srbm", os, __LINE__);

        rm1 = rm1 + srbm2;
        rm1 += srbm2;
        CheckReal    (rm1(CVM0+1,CVM0+2), srbm2(CVM0+1,CVM0+2) * 3.,  "mix rmatrix  srbm", os, __LINE__);
        CheckReal    (rm1(CVM0+3,CVM0), 3. * srbm2(CVM0+3,CVM0),  "mix matrix  srbm", os, __LINE__);
        CheckReal    (rm1(CVM0+3,CVM0), 3 * srbm2(CVM0+3,CVM0),  "mix rmatrix  srbm", os, __LINE__);
        CheckReal    (rm1(CVM0+1,CVM0+2), srbm2(CVM0+1,CVM0+2) * 3,  "mix rmatrix  srbm", os, __LINE__);

        scbm1.resize(4);
        for (j = CVM0; j <= CVM0+3; j++)
        {
            for (i = CVM0; i <= CVM0+3; i++)
            {
                rm1(i,j)  = - (treal) ((j - CVM0) * 4 + i + (1 - CVM0));
                srm4(i,j) = - (treal) ((j - CVM0) * 4 + i + (1 - CVM0));
                cm1(i,j)  = - (treal) ((j - CVM0) * 4 + i + (1 - CVM0));
                scm1(i,j) = - (treal) ((j - CVM0) * 4 + i + (1 - CVM0));
            }
            srbm1(j,j) = treal (j + (1 - CVM0));
            scbm1(j,j) = tcomplex (treal (j + (1 - CVM0)));
        }

        CheckReal    (rm1.norm1(),   (treal) (13 + 14 + 15 + 16),  "rmatrix norm1", os, __LINE__);
        CheckReal    (srm4.norm1(),  (treal) (13 + 14 + 15 + 16),  "srmatrix norm1", os, __LINE__);
        CheckReal    (srbm1.norm1(), (treal) 4,                    "srbmatrix norm1", os, __LINE__);
        CheckReal    (cm1.norm1(),   (treal) (13 + 14 + 15 + 16),  "cmatrix norm1", os, __LINE__);
        CheckReal    (scm1.norm1(),  (treal) (13 + 14 + 15 + 16),  "scmatrix norm1", os, __LINE__);
        CheckReal    (scbm1.norm1(), (treal) 4,                    "scbmatrix norm1", os, __LINE__);

        CheckReal    (rm1.norminf(),   (treal) (4 + 8 + 12 + 16),  "rmatrix norminf", os, __LINE__);
        CheckReal    (srm4.norminf(),  (treal) (4 + 8 + 12 + 16),  "srmatrix norminf", os, __LINE__);
        CheckReal    (srbm1.norminf(), (treal) 4,                  "srbmatrix norminf", os, __LINE__);
        CheckReal    (cm1.norminf(),   (treal) (4 + 8 + 12 + 16),  "cmatrix norminf", os, __LINE__);
        CheckReal    (scm1.norminf(),  (treal) (4 + 8 + 12 + 16),  "scmatrix norminf", os, __LINE__);
        CheckReal    (scbm1.norminf(), (treal) 4,                  "scbmatrix norminf", os, __LINE__);

        CheckReal    (eye_real(6)(CVM0+5,CVM0+5),   (treal) 1.,  "eye_real", os, __LINE__);
        CheckComplex (eye_complex(6)(CVM0+5,CVM0+5),   tcomplex (1, 0),  "eye_complex", os, __LINE__);

        rv2.resize (4);
        srmatrix rmU(4), rmVH(4);
        rv1 = srm4.svd (rmU, rmVH);
        rv2.svd (srm4, rmU, rmVH);
//        CheckBool    (rv1 == rv2,  true,  "srmatrix svd", os, __LINE__);
        CheckReal    ((rv1 - rv2).norm(),  (treal) 0.,  "srmatrix svd", os, __LINE__, dPessimisticSp);    // amd acml_mp wants it sometimes in multitreading mode
        srm1 << srmatrix (rv1);
        CheckReal    ((srm4 * ~rmVH - rmU * srm1).norm(),  (treal) 0.,  "srmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal    ((~srm4 * rmU - ~(srm1 * rmVH)).norm(),  (treal) 0.,  "srmatrix svd", os, __LINE__, dPessimisticSp);

        rv1 = srbm2.svd (rmU, rmVH);
        rv2.svd (srbm2);

        CheckReal    ((rv1 - rv2).norm(),  (treal) 0.,  "srbmatrix svd", os, __LINE__, dVeryPessimisticSp);
        rv2.svd (srbm2, rmU, rmVH);
        srm1 << srmatrix (rv1);
        CheckReal    ((srbm2 * ~rmVH - rmU * srm1).norm(),  (treal) 0.,  "srbmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal    ((~srbm2 * rmU - ~(srm1 * rmVH)).norm(),  (treal) 0.,  "srbmatrix svd", os, __LINE__, dPessimisticSp);


        // test case from Martin
        // http://www.vni.com/products/jmsl/v25/api/com/imsl/math/SVDEx1.html
        rmatrix A(6,4);
        A(CVM0,CVM0) = 1;
        A(CVM0,CVM0+1) = 2;
        A(CVM0,CVM0+2) = 1;
        A(CVM0,CVM0+3) = 4;
        A(CVM0+1,CVM0) = 3;
        A(CVM0+1,CVM0+1) = 2;
        A(CVM0+1,CVM0+2) = 1;
        A(CVM0+1,CVM0+3) = 3;
        A(CVM0+2,CVM0) = 4;
        A(CVM0+2,CVM0+1) = 3;
        A(CVM0+2,CVM0+2) = 1;
        A(CVM0+2,CVM0+3) = 4;
        A(CVM0+3,CVM0) = 2;
        A(CVM0+3,CVM0+1) = 1;
        A(CVM0+3,CVM0+2) = 3;
        A(CVM0+3,CVM0+3) = 1;
        A(CVM0+4,CVM0) = 1;
        A(CVM0+4,CVM0+1) = 5;
        A(CVM0+4,CVM0+2) = 2;
        A(CVM0+4,CVM0+3) = 2;
        A(CVM0+5,CVM0) = 1;
        A(CVM0+5,CVM0+1) = 2;
        A(CVM0+5,CVM0+2) = 2;
        A(CVM0+5,CVM0+3) = 3;
        srmatrix U(6), V(4);
        const rvector singVal = A.svd(U,V);

        rmatrix singValM (A);
        singValM.set(0.);
        singValM(CVM0,CVM0) = singVal(CVM0);
        singValM(CVM0+1,CVM0+1) = singVal(CVM0+1);
        singValM(CVM0+2,CVM0+2) = singVal(CVM0+2);
        singValM(CVM0+3,CVM0+3) = singVal(CVM0+3);

        CheckReal    ((A * ~V - U * singValM).norm(),  (treal) 0.,  "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal    ((~A * U - ~(singValM * V)).norm(),  (treal) 0.,  "rmatrix svd", os, __LINE__, dPessimisticSp);

        CheckReal (singVal[CVM0], (treal) 1.148501791155974e+001, "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (singVal[CVM0+1], (treal) 3.269751214412497e+000, "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (singVal[CVM0+2], (treal) 2.653356162007834e+000, "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (singVal[CVM0+3], (treal) 2.088729672440923e+000, "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0, CVM0)), cvm::_abs((treal)-0.38047558632), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0, CVM0+1)), cvm::_abs((treal)-0.11967099264), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0, CVM0+2)), cvm::_abs((treal)-0.43908282438), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0, CVM0+3)), cvm::_abs((treal)0.56539958591), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0, CVM0+4)), cvm::_abs((treal)0.024311516146),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0, CVM0+5)), cvm::_abs((treal)-0.5725868611), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+1, CVM0)), cvm::_abs((treal)-0.40375371317), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+1, CVM0+1)), cvm::_abs((treal)-0.34511083711), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+1, CVM0+2)), cvm::_abs((treal)0.05657618529),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+1, CVM0+3)), cvm::_abs((treal)-0.21477557652), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+1, CVM0+4)), cvm::_abs((treal)0.80890058873),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+1, CVM0+5)), cvm::_abs((treal)0.11929741721),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+2, CVM0)), cvm::_abs((treal)-0.54512048625), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+2, CVM0+1)), cvm::_abs((treal)-0.42926489349), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+2, CVM0+2)), cvm::_abs((treal)-0.051392692809), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+2, CVM0+3)), cvm::_abs((treal)-0.43214416281), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+2, CVM0+4)), cvm::_abs((treal)-0.57232764817), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+2, CVM0+5)), cvm::_abs((treal)0.040330924871),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+3, CVM0)), cvm::_abs((treal)-0.264784294), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+3, CVM0+1)), cvm::_abs((treal)0.068319525327),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+3, CVM0+2)), cvm::_abs((treal)0.88386086743),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+3, CVM0+3)), cvm::_abs((treal)0.21525369818),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+3, CVM0+4)), cvm::_abs((treal)-0.06252092259), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+3, CVM0+5)), cvm::_abs((treal)-0.30621669907), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+4, CVM0)), cvm::_abs((treal)-0.4463101123), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+4, CVM0+1)), cvm::_abs((treal)0.81682762328),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+4, CVM0+2)), cvm::_abs((treal)-0.14189967506), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+4, CVM0+3)), cvm::_abs((treal)-0.32126958427), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+4, CVM0+4)), cvm::_abs((treal)0.062133782096),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+4, CVM0+5)), cvm::_abs((treal)-0.079935268), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+5, CVM0)), cvm::_abs((treal)-0.35462865661), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+5, CVM0+1)), cvm::_abs((treal)0.10214739916),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+5, CVM0+2)), cvm::_abs((treal)0.0043184439799),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+5, CVM0+3)), cvm::_abs((treal)0.54580022185),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+5, CVM0+4)), cvm::_abs((treal)-0.098794626562), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs(U(CVM0+5, CVM0+5)), cvm::_abs((treal)0.74573957611),"rmatrix svd", os, __LINE__, dVeryPessimisticSp);

        CheckReal (cvm::_abs((~V)(CVM0, CVM0)), cvm::_abs((treal)-4.442941288423535e-001), "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+1, CVM0)), cvm::_abs((treal)-5.580672381903871e-001), "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+2, CVM0)), cvm::_abs((treal)-3.243861032062802e-001), "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+3, CVM0)), cvm::_abs((treal)-6.212385538433783e-001), "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0, CVM0+1)), cvm::_abs((treal)5.555312577999473e-001), "rmatrix svd", os, __LINE__, dVeryPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+1, CVM0+1)), cvm::_abs((treal)-6.542987401123238e-001), "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+2, CVM0+1)), cvm::_abs((treal)-3.513606455925113e-001), "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+3, CVM0+1)), cvm::_abs((treal)3.739303103834293e-001),"rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0, CVM0+2)), cvm::_abs((treal)-4.353789666739416e-001), "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+1, CVM0+2)), cvm::_abs((treal)2.774569004588126e-001),"rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+2, CVM0+2)), cvm::_abs((treal)-7.320995334295977e-001), "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+3, CVM0+2)), cvm::_abs((treal)4.444019542237462e-001),"rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0, CVM0+3)), cvm::_abs((treal)-5.517543874418699e-001), "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+1, CVM0+3)), cvm::_abs((treal)-4.283360651798634e-001), "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+2, CVM0+3)), cvm::_abs((treal)4.851284633245337e-001),"rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal (cvm::_abs((~V)(CVM0+3, CVM0+3)), cvm::_abs((treal)5.260662365874236e-001),"rmatrix svd", os, __LINE__, dPessimisticSp);

        rmatrix rm6(3,4);
        for (j = CVM0; j <= CVM0+3; j++)
        {
            for (i = CVM0; i <= CVM0+2; i++)
            {
                rm6(i,j)  = - (treal) ((j - CVM0) * 4 + i);
            }
        }

        rv1.resize (3);
        rv2.resize (3);
        rmU.resize(3);
        rmVH.resize(4);
        rv1 = rm6.svd (rmU, rmVH);
        rv2.svd (rm6, rmU, rmVH);
        CheckBool    (rv1 == rv2,  true,  "srmatrix svd", os, __LINE__);

        singValM << rm6;
        singValM.set(0.);
        singValM(CVM0,CVM0) = rv1(CVM0);
        singValM(CVM0+1,CVM0+1) = rv1(CVM0+1);
        singValM(CVM0+2,CVM0+2) = rv1(CVM0+2);
        CheckReal    ((rm6 * ~rmVH - rmU * singValM).norm(),  (treal) 0.,  "rmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal    ((~rm6 * rmU - ~(singValM * rmVH)).norm(),  (treal) 0.,  "rmatrix svd", os, __LINE__, dPessimisticSp);


        rv1.resize (4);
        rv2.resize (4);
        scmatrix cmU(4), cmVH(4);
        rv1 = scm1.svd (cmU, cmVH);
        rv2.svd (scm1, cmU, cmVH);
        CheckBool    (rv1 == rv2,  true,  "scmatrix svd", os, __LINE__);
        cv1 << cvector (rv1);
        scm << scmatrix (cv1);
        CheckReal    ((scm1 * ~cmVH - cmU * scm).norm(),  (treal) 0.,  "scmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm1 * cmU - ~(scm * cmVH)).norm(),   (treal) 0.,  "scmatrix svd", os, __LINE__, dPessimisticSp);

        scbm1(CVM0+3,CVM0+2)=-cr1;
        rv1 = scbm1.svd (cmU, cmVH);
        rv2.svd (scbm1);
        CheckReal    ((rv1 - rv2).norm(),  (treal) 0.,  "scbmatrix svd", os, __LINE__, dVeryPessimisticSp);
        rv2.svd (scbm1, cmU, cmVH);
        scm1 << scmatrix (srmatrix(rv1));
        CheckReal    ((scbm1 * ~cmVH - cmU * scm1).norm(),  (treal) 0.,  "scbmatrix svd", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scbm1 * cmU - ~(scm1 * cmVH)).norm(),  (treal) 0.,  "scbmatrix svd", os, __LINE__, dPessimisticSp);

        // 6.1: real transpositions
        {
            rmatrix rm(7,6), rm2(6,7);
            rm.randomize(-3., 5.);
            rm2.transpose(rm);
            CheckReal((rm - ~rm2).norm(),  (treal) 0.,  "rmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckReal(rm(1,1), rm2(1,1), "rmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckReal(rm(1,2), rm2(2,1), "rmatrix transposed", os, __LINE__, dPessimisticSp);
            rm2.transpose();
            CheckReal((rm - rm2).norm(),  (treal) 0.,  "rmatrix transposed", os, __LINE__, dPessimisticSp);

            srmatrix srm(7), srm2(7);
            srm.randomize(-3., 5.);
            srm2.transpose(srm);
            CheckReal((srm - ~srm2).norm(),  (treal) 0.,  "srmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckReal(srm(1,1), srm2(1,1), "srmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckReal(srm(1,2), srm2(2,1), "srmatrix transposed", os, __LINE__, dPessimisticSp);
            srm2.transpose();
            CheckReal((srm - srm2).norm(),  (treal) 0.,  "srmatrix transposed", os, __LINE__, dPessimisticSp);

            srbmatrix srbm(7,1,2), srbm2(7,2,1);
            srbm.randomize(-3., 5.);
            srbm2.transpose(srbm);
            CheckReal((srbm - ~srbm2).norm(),  (treal) 0.,  "srbmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckReal(srbm(1,1), srbm2(1,1), "srbmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckReal(srbm(1,2), srbm2(2,1), "srbmatrix transposed", os, __LINE__, dPessimisticSp);
            srbm2.transpose();
            CheckReal((srbm - srbm2).norm(),  (treal) 0.,  "srbmatrix transposed", os, __LINE__, dPessimisticSp);

            srsmatrix srsm(7), srsm2(7);
            srsm.randomize(-3., 5.);
            srsm2.transpose(srsm);
            CheckReal((srsm - ~srsm2).norm(),  (treal) 0.,  "srsmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckReal(srsm(1,1), srsm2(1,1), "srsmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckReal(srsm(1,2), srsm2(2,1), "srsmatrix transposed", os, __LINE__, dPessimisticSp);
            srsm2.transpose();
            CheckReal((srsm - srsm2).norm(),  (treal) 0.,  "srsmatrix transposed", os, __LINE__, dPessimisticSp);
        }

        // 6.1: complex transpositions and conjugations
        {
            cmatrix cm(7,6), cm2(6,7);
            cm.randomize_real(-3., 5.);
            cm.randomize_imag(-5., 4.);
            cm2.transpose(cm);
            CheckReal((cm - !cm2).norm(),  (treal) 0.,  "cmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckComplex(cm(1,1), cm2(1,1), "cmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckComplex(cm(1,2), cm2(2,1), "cmatrix transposed", os, __LINE__, dPessimisticSp);
            cm2.transpose();
            CheckReal((cm - cm2).norm(),  (treal) 0.,  "cmatrix transposed", os, __LINE__, dPessimisticSp);
            cm2.transpose();
            cm2.conj(cm);
            CheckReal((cm - ~cm2).norm(),  (treal) 0.,  "cmatrix conjugated", os, __LINE__, dPessimisticSp);
            CheckComplex(cm(1,1), conj(cm2(1,1)), "cmatrix conjugated", os, __LINE__, dPessimisticSp);
            CheckComplex(cm(1,2), conj(cm2(2,1)), "cmatrix conjugated", os, __LINE__, dPessimisticSp);
            cm2.conj();
            CheckReal((cm - cm2).norm(),  (treal) 0.,  "cmatrix conjugated", os, __LINE__, dPessimisticSp);

            scmatrix scm(7), scm2(7);
            scm.randomize_real(-3., 5.);
            scm.randomize_imag(-5., 3.);
            scm2.transpose(scm);
            CheckReal((scm - !scm2).norm(),  (treal) 0.,  "scmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckComplex(scm(1,1), scm2(1,1), "scmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckComplex(scm(1,2), scm2(2,1), "scmatrix transposed", os, __LINE__, dPessimisticSp);
            scm2.transpose();
            CheckReal((scm - scm2).norm(),  (treal) 0.,  "scmatrix transposed", os, __LINE__, dPessimisticSp);
            scm2.conj(scm);
            CheckReal((scm - ~scm2).norm(),  (treal) 0.,  "scmatrix conjugated", os, __LINE__, dPessimisticSp);
            CheckComplex(scm(1,1), conj(scm2(1,1)), "scmatrix conjugated", os, __LINE__, dPessimisticSp);
            CheckComplex(scm(1,2), conj(scm2(2,1)), "scmatrix conjugated", os, __LINE__, dPessimisticSp);
            scm2.conj();
            CheckReal((scm - scm2).norm(),  (treal) 0.,  "scmatrix conjugated", os, __LINE__, dPessimisticSp);

            scbmatrix scbm(7,1,2), scbm2(7,2,1);
            scbm.randomize_real(-3., 5.);
            scbm.randomize_imag(-4., 3.);
            scbm2.transpose(scbm);
            CheckReal((scbm - !scbm2).norm(),  (treal) 0.,  "scbmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckComplex(scbm(1,1), scbm2(1,1), "scbmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckComplex(scbm(1,2), scbm2(2,1), "scbmatrix transposed", os, __LINE__, dPessimisticSp);
            scbm2.transpose();
            CheckReal((scbm - scbm2).norm(),  (treal) 0.,  "scbmatrix transposed", os, __LINE__, dPessimisticSp);
            scbm2.transpose();
            scbm2.conj(scbm);
            CheckReal((scbm - ~scbm2).norm(),  (treal) 0.,  "scbmatrix conjugated", os, __LINE__, dPessimisticSp);
            CheckComplex(scbm(1,1), conj(scbm2(1,1)), "scbmatrix conjugated", os, __LINE__, dPessimisticSp);
            CheckComplex(scbm(1,2), conj(scbm2(2,1)), "scbmatrix conjugated", os, __LINE__, dPessimisticSp);
            scbm2.conj();
            CheckReal((scbm - scbm2).norm(),  (treal) 0.,  "scbmatrix conjugated", os, __LINE__, dPessimisticSp);

            schmatrix schm(5);
            schm.randomize_real(-2., 3.);
            schm.randomize_imag(-3., 2.);
            schmatrix schmc = ~schm;
            CheckReal((schmc - schm).norm(),  (treal) 0.,  "schmatrix conjugated", os, __LINE__, dPessimisticSp);
            ((cmatrix)schmc).conj();
            CheckReal((schmc - schm).norm(),  (treal) 0.,  "schmatrix conjugated", os, __LINE__, dPessimisticSp);
            ((scmatrix)schmc).conj();
            CheckReal((schmc - schm).norm(),  (treal) 0.,  "schmatrix conjugated", os, __LINE__, dPessimisticSp);
            scmatrix scmc = ~((scmatrix)schm);
            CheckReal((scmc - schm).norm(),  (treal) 0.,  "schmatrix conjugated", os, __LINE__, dPessimisticSp);
            schmc = !schm;
            schmc.transpose(schm);
            CheckReal((schmc - !schm).norm(),  (treal) 0.,  "schmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckComplex(schmc(1,1), schm(1,1), "schmatrix transposed", os, __LINE__, dPessimisticSp);
            CheckComplex(schmc(1,2), schm(2,1), "schmatrix transposed", os, __LINE__, dPessimisticSp);
            schmc.transpose();
            CheckReal((schmc - schm).norm(),  (treal) 0.,  "schmatrix transposed", os, __LINE__, dPessimisticSp);
        }

        // solvers
        {
            srm4 *=     (treal) -1.;
            srm4(CVM0+2,CVM0+2) = (treal) 1.;
            srm4(CVM0+3,CVM0+3) = (treal) 1.;
            rv.resize (4);
            srm4.normalize();

            rv.solve (srm4, rv1);
            CheckReal    ((srm4 * rv - rv1).norm(),   0,  "srmatrix solve", os, __LINE__, dPessimisticSp);
            rv.solve_tran (srm4, rv1);
            CheckReal    ((rv * srm4 - rv1).norm(),   0,  "srmatrix solve transposed", os, __LINE__, dPessimisticSp);

            rv = srm4.solve (rv1);
            CheckReal    ((srm4 * rv - rv1).norm(),   0,  "srmatrix solve", os, __LINE__, dPessimisticSp);
            rv = srm4.solve_tran (rv1);
            CheckReal    ((rv * srm4 - rv1).norm(),   0,  "srmatrix solve transposed", os, __LINE__, dPessimisticSp);

            rv.solve (srbm2, rv1);
            CheckReal    ((srbm2 * rv - rv1).norm(),   0,  "srbmatrix solve", os, __LINE__, dPessimisticSp);
            rv.solve_tran (srbm2, rv1);
            CheckReal    ((rv * srbm2 - rv1).norm(),   0,  "srbmatrix solve transposed", os, __LINE__, dPessimisticSp);

            rv = srbm2.solve (rv1);
            CheckReal    ((srbm2 * rv - rv1).norm(),   0,  "srbmatrix solve", os, __LINE__, dPessimisticSp);
            rv = srbm2.solve_tran (rv1);
            CheckReal    ((rv * srbm2 - rv1).norm(),   0,  "srbmatrix solve transposed", os, __LINE__, dPessimisticSp);

            rmatrix rmB(4,5), rmX(4,5);
            rmB.randomize(-3., 4.);

            rmX.solve (srm4, rmB);
            CheckReal    ((srm4 * rmX - rmB).norm(),   0,  "srmatrix solve for matrix B", os, __LINE__, dPessimisticSp);
            rmX.solve_tran (srm4, rmB);
            CheckReal    ((~srm4 * rmX - rmB).norm(),   0,  "srmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            CheckReal    ((~rmX * srm4 - ~rmB).norm(),   0,  "srmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);

            rmX = srm4.solve (rmB);
            CheckReal    ((srm4 * rmX - rmB).norm(),   0,  "srmatrix solve for matrix B", os, __LINE__, dPessimisticSp);
            rmX = srm4.solve_tran (rmB);
            CheckReal    ((~srm4 * rmX - rmB).norm(),   0,  "srmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            CheckReal    ((~rmX * srm4 - ~rmB).norm(),   0,  "srmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);

            rmX.solve (srbm2, rmB);
            CheckReal    ((srbm2 * rmX - rmB).norm(),   0,  "srbmatrix solve for matrix B", os, __LINE__, dPessimisticSp);
            rmX.solve_tran (srbm2, rmB);
            CheckReal    ((~srbm2 * rmX - rmB).norm(),   0,  "srbmatrix solve  for matrix B transposed", os, __LINE__, dPessimisticSp);
            CheckReal    ((~rmX * srbm2 - ~rmB).norm(),   0,  "srbmatrix solve  for matrix B transposed", os, __LINE__, dPessimisticSp);

            rmX = srbm2.solve (rmB);
            CheckReal    ((srbm2 * rmX - rmB).norm(),   0,  "srbmatrix solve for matrix B", os, __LINE__, dPessimisticSp);
            rmX = srbm2.solve_tran (rmB);
            CheckReal    ((~srbm2 * rmX - rmB).norm(),   0,  "srbmatrix solve  for matrix B transposed", os, __LINE__, dPessimisticSp);
            CheckReal    ((~rmX * srbm2 - ~rmB).norm(),   0,  "srbmatrix solve  for matrix B transposed", os, __LINE__, dPessimisticSp);

            scm1.assign(c2);
            scm1(CVM0+2,CVM0+2) = tcomplex (1, -1);
            scm1(CVM0+3,CVM0+3) = tcomplex (1, -1);
            cv.resize (4);
            cv1.resize (4);

            cv.solve (scm1, cv1);
            CheckReal    ((scm1 * cv - cv1).norm(),   0,  "scmatrix solve", os, __LINE__, dPessimisticSp);
            cv.solve_tran (scm1, cv1);
            CheckReal    ((cv * scm1 - cv1).norm(),   0,  "scmatrix solve transposed", os, __LINE__, dPessimisticSp);
            cv.solve_conj (scm1, cv1);
            CheckReal    ((~scm1 * cv - cv1).norm(),   0,  "scmatrix solve conjugated", os, __LINE__, dPessimisticSp);

            cv = scm1.solve (cv1);
            CheckReal    ((scm1 * cv - cv1).norm(),   0,  "scmatrix solve", os, __LINE__, dPessimisticSp);
            cv = scm1.solve_tran (cv1);
            CheckReal    ((cv * scm1 - cv1).norm(),   0,  "scmatrix solve transposed", os, __LINE__, dPessimisticSp);
            cv = scm1.solve_conj (cv1);
            CheckReal    ((~scm1 * cv - cv1).norm(),   0,  "scmatrix solve conjugated", os, __LINE__, dPessimisticSp);

            cv.solve (scbm1, cv1);
            CheckReal    ((scbm1 * cv - cv1).norm(),   0,  "scbmatrix solve", os, __LINE__, dPessimisticSp);
            cv.solve_tran (scbm1, cv1);
            CheckReal    ((cv * scbm1 - cv1).norm(),   0,  "scbmatrix solve transposed", os, __LINE__, dPessimisticSp);
            cv.solve_conj (scbm1, cv1);
            CheckReal    ((~scbm1 * cv - cv1).norm(),   0,  "scbmatrix solve conjugated", os, __LINE__, dPessimisticSp);

            cv = scbm1.solve (cv1);
            CheckReal    ((scbm1 * cv - cv1).norm(),   0,  "scbmatrix solve", os, __LINE__, dPessimisticSp);
            cv = scbm1.solve_tran (cv1);
            CheckReal    ((cv * scbm1 - cv1).norm(),   0,  "scbmatrix solve transposed", os, __LINE__, dPessimisticSp);
            cv = scbm1.solve_conj (cv1);
            CheckReal    ((~scbm1 * cv - cv1).norm(),   0,  "scbmatrix solve conjugated", os, __LINE__, dPessimisticSp);

            cmatrix cmB(4,5), cmX(4,5);
            cmB.randomize_real(-3., 4.);
            cmB.randomize_imag(-5., 2.);
            scm1.randomize_real(-3., 4.);
            scm1.randomize_imag(-2., 4.);
            cmX.solve (scm1, cmB);
            CheckReal    ((scm1 * cmX - cmB).norm(),   0,  "scmatrix solve for matrix B", os, __LINE__, dPessimisticSp);
            cmX = scm1.solve (cmB);
            CheckReal    ((scm1 * cmX - cmB).norm(),   0,  "scmatrix solve for matrix B", os, __LINE__, dPessimisticSp);
            cmX.solve_tran (scm1, cmB);
            CheckReal    ((!cmX * scm1 - !cmB).norm(),   0,  "scmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            CheckReal    ((!scm1 * cmX - cmB).norm(),   0,  "scmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            cmX = scm1.solve_tran (cmB);
            CheckReal    ((!cmX * scm1 - !cmB).norm(),   0,  "scmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            CheckReal    ((!scm1 * cmX - cmB).norm(),   0,  "scmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            cmX = scm1.solve_conj (cmB);
            CheckReal    ((~cmX * scm1 - ~cmB).norm(),   0,  "scmatrix solve for matrix B conjugated", os, __LINE__, dPessimisticSp);
            CheckReal    ((~scm1 * cmX - cmB).norm(),   0,  "scmatrix solve for matrix B conjugated", os, __LINE__, dPessimisticSp);

            scbmatrix scbm(4,1,2);
            scbm.randomize_real(-3., 4.);
            scbm.randomize_imag(-2., 4.);
            cmX.solve (scbm, cmB);
            CheckReal    ((scbm * cmX - cmB).norm(),   0,  "scbmatrix solve for matrix B", os, __LINE__, dPessimisticSp);
            cmX = scbm.solve (cmB);
            CheckReal    ((scbm * cmX - cmB).norm(),   0,  "scbmatrix solve for matrix B", os, __LINE__, dPessimisticSp);
            cmX.solve_tran (scbm, cmB);
            CheckReal    ((!cmX * scbm - !cmB).norm(),   0,  "scbmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            CheckReal    ((!scbm * cmX - cmB).norm(),   0,  "scbmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            cmX = scbm.solve_tran (cmB);
            CheckReal    ((!cmX * scbm - !cmB).norm(),   0,  "scbmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            CheckReal    ((!scbm * cmX - cmB).norm(),   0,  "scbmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            cmX.solve_conj (scbm, cmB);
            CheckReal    ((~cmX * scbm - ~cmB).norm(),   0,  "scbmatrix solve for matrix B conjugated", os, __LINE__, dPessimisticSp);
            CheckReal    ((~scbm * cmX - cmB).norm(),   0,  "scbmatrix solve for matrix B conjugated", os, __LINE__, dPessimisticSp);
            cmX = scbm.solve_conj (cmB);
            CheckReal    ((~cmX * scbm - ~cmB).norm(),   0,  "scbmatrix solve for matrix B conjugated", os, __LINE__, dPessimisticSp);
            CheckReal    ((~scbm * cmX - cmB).norm(),   0,  "scbmatrix solve for matrix B conjugated", os, __LINE__, dPessimisticSp);


            schmatrix schm(4);
            schm.randomize_real(-5., 1.);   // 6.1: fixed non-positive definite bug
            schm.randomize_imag(-2., 4.);
            cmX.solve (schm, cmB);
            CheckReal    ((schm * cmX - cmB).norm(),   0,  "schmatrix solve for matrix B", os, __LINE__, dPessimisticSp);
            cmX = schm.solve (cmB);
            CheckReal    ((schm * cmX - cmB).norm(),   0,  "schmatrix solve for matrix B", os, __LINE__, dPessimisticSp);
            cmX.solve_tran (schm, cmB);
            CheckReal    ((!cmX * schm - !cmB).norm(),   0,  "schmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            CheckReal    ((!schm * cmX - cmB).norm(),   0,  "schmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            cmX = schm.solve_tran (cmB);
            CheckReal    ((!cmX * schm - !cmB).norm(),   0,  "schmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            CheckReal    ((!schm * cmX - cmB).norm(),   0,  "schmatrix solve for matrix B transposed", os, __LINE__, dPessimisticSp);
            cmX.solve_conj (schm, cmB);
            CheckReal    ((~cmX * schm - ~cmB).norm(),   0,  "schmatrix solve for matrix B conjugated", os, __LINE__, dPessimisticSp);
            CheckReal    ((~schm * cmX - cmB).norm(),   0,  "schmatrix solve for matrix B conjugated", os, __LINE__, dPessimisticSp);
            cmX = schm.solve_conj (cmB);
            CheckReal    ((~cmX * schm - ~cmB).norm(),   0,  "schmatrix solve for matrix B conjugated", os, __LINE__, dPessimisticSp);
            CheckReal    ((~schm * cmX - cmB).norm(),   0,  "schmatrix solve for matrix B conjugated", os, __LINE__, dPessimisticSp);

        }


        // MATLAB-style operator B/A returning solution of X*A=B equation which is actually A'*X'=B'
        // 6.1
        {
            srmatrix srm(7);
            srbmatrix srbm(7, 1, 3);
            srsmatrix srsm(7);
            rvector vB(7), vX(7);
            srm.randomize(-10., 20.);
            srbm.randomize(-10., 20.);
            srsm.randomize(-10., 5.);
            vB.randomize(-10., 20.);

            vX = vB / srm;
            CheckReal    ((vX * srm - vB).norm(),   0,  "rvector / srmatrix", os, __LINE__, dPessimisticSp);
            vX = srm % vB;
            CheckReal    ((vX * srm - vB).norm(),   0,  "srmatrix % rvector", os, __LINE__, dPessimisticSp);

            vX = vB / srbm;
            CheckReal    ((vX * srbm - vB).norm(),   0,  "rvector / srbmatrix", os, __LINE__, dVeryPessimisticSp);
            vX = srbm % vB;
            CheckReal    ((vX * srbm - vB).norm(),   0,  "srbmatrix % rvector", os, __LINE__, dPessimisticSp);

            vX = vB / srsm;
            CheckReal    ((vX * srsm - vB).norm(),   0,  "rvector / srsmatrix", os, __LINE__, dPessimisticSp);
            vX = srsm % vB;
            CheckReal    ((vX * srsm - vB).norm(),   0,  "srsmatrix % rvector", os, __LINE__, dPessimisticSp);

            vX = vB % srm;
            CheckReal    ((srm * vX - vB).norm(),   0,  "rvector % srmatrix", os, __LINE__, dPessimisticSp);
            vX = srm / vB;
            CheckReal    ((srm * vX - vB).norm(),   0,  "srmatrix / rvector", os, __LINE__, dPessimisticSp);

            vX = vB % srbm;
            CheckReal    ((srbm * vX - vB).norm(),   0,  "rvector % srbmatrix", os, __LINE__, dPessimisticSp);
            vX = srbm / vB;
            CheckReal    ((srbm * vX - vB).norm(),   0,  "srbmatrix / rvector", os, __LINE__, dPessimisticSp);

            vX = vB % srsm;
            CheckReal    ((srsm * vX - vB).norm(),   0,  "rvector % srsmatrix", os, __LINE__, dPessimisticSp);
            vX = srsm / vB;
            CheckReal    ((srsm * vX - vB).norm(),   0,  "srsmatrix / rvector", os, __LINE__, dPessimisticSp);
        }

        // 6.1
        {
            scmatrix scm(7);
            scbmatrix scbm(7, 1, 3);
            schmatrix schm(7);
            cvector vB(7), vX(7);
            scm.randomize_real(-10., 20.);
            scm.randomize_imag(-10., 20.);
            scbm.randomize_real(-10., 20.);
            scbm.randomize_imag(-10., 20.);
            schm.randomize_real(-10., 20.);
            schm.randomize_imag(-10., 20.);
            vB.randomize_real(-10., 20.);
            vB.randomize_imag(-10., 20.);

            vX = vB / scm;
            CheckReal    ((vX * scm - vB).norm(),   0,  "cvector / scmatrix", os, __LINE__, dPessimisticSp);
            vX = scm % vB;
            CheckReal    ((vX * scm - vB).norm(),   0,  "scmatrix % cvector", os, __LINE__, dPessimisticSp);

            vX = vB / scbm;
            CheckReal    ((vX * scbm - vB).norm(),   0,  "cvector / scbmatrix", os, __LINE__, dPessimisticSp);
            vX = scbm % vB;
            CheckReal    ((vX * scbm - vB).norm(),   0,  "scbmatrix % cvector", os, __LINE__, dPessimisticSp);

            vX = vB / schm;
            CheckReal    ((vX * schm - vB).norm(),   0,  "cvector / schmatrix", os, __LINE__, dPessimisticSp);
            vX = schm % vB;
            CheckReal    ((vX * schm - vB).norm(),   0,  "schmatrix % cvector", os, __LINE__, dPessimisticSp);

            vX = vB % scm;
            CheckReal    ((scm * vX - vB).norm(),   0,  "scmatrix % cvector", os, __LINE__, dPessimisticSp);
            vX = scm / vB;
            CheckReal    ((scm * vX - vB).norm(),   0,  "scmatrix / cvector", os, __LINE__, dPessimisticSp);

            vX = vB % scbm;
            CheckReal    ((scbm * vX - vB).norm(),   0,  "cvector % scbmatrix", os, __LINE__, dPessimisticSp);
            vX = scbm / vB;
            CheckReal    ((scbm * vX - vB).norm(),   0,  "scbmatrix / cvector", os, __LINE__, dPessimisticSp);

            vX = vB % schm;
            CheckReal    ((schm * vX - vB).norm(),   0,  "cvector % schmatrix", os, __LINE__, dPessimisticSp);
            vX = schm / vB;
            CheckReal    ((schm * vX - vB).norm(),   0,  "schmatrix / cvector", os, __LINE__, dPessimisticSp);
        }

        srm.resize  (3);
        scm.resize  (3);
        cv .resize  (3);
        cv1.resize  (3);
        scmatrix scm_(scm.msize());

        srm(CVM0,CVM0) = (treal) 0.1;  srm(CVM0,CVM0+1) = (treal) 0.2;  srm(CVM0,CVM0+2) = (treal) 0.1;
        srm(CVM0+1,CVM0) = (treal) 0.11; srm(CVM0+1,CVM0+1) = (treal) -2.9; srm(CVM0+1,CVM0+2) = (treal) -8.4;
        srm(CVM0+2,CVM0) = (treal) 0.;   srm(CVM0+2,CVM0+1) = (treal) 2.91; srm(CVM0+2,CVM0+2) = (treal) 8.2;

        cv.eig (srm, scm);
        cv1 = srm.eig (scm_);
//        CheckReal    ((cv - cv1).norm(),  0.,  "srmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm(CVM0) - scm(CVM0) * cv(CVM0)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm(CVM0+1) - scm(CVM0+1) * cv(CVM0+1)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm(CVM0+2) - scm(CVM0+2) * cv(CVM0+2)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm_(CVM0) - scm_(CVM0) * cv1(CVM0)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm_(CVM0+1) - scm_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm_(CVM0+2) - scm_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);

        cv.eig (srm, scm, false);
        cv1 = srm.eig (scm_, false);
//        CheckReal    ((cv - cv1).norm(),  0.,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm(CVM0) * scmatrix (srm) - ~scm(CVM0) * cv(CVM0)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dVeryPessimisticSp);
        CheckReal    ((~scm(CVM0+1) * scmatrix (srm) - ~scm(CVM0+1) * cv(CVM0+1)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm(CVM0+2) * scmatrix (srm) - ~scm(CVM0+2) * cv(CVM0+2)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm_(CVM0) * scmatrix (srm) - ~scm_(CVM0) * cv1(CVM0)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm_(CVM0+1) * scmatrix (srm) - ~scm_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm_(CVM0+2) * scmatrix (srm) - ~scm_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);

        srm(CVM0+1,CVM0+1) = (treal) 2.9;
        cv.eig (srm, scm);
        cv1 = srm.eig (scm_);
//        CheckReal    ((cv - cv1).norm(),  0.,  "scmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm(CVM0) - scm(CVM0) * cv(CVM0)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm(CVM0+1) - scm(CVM0+1) * cv(CVM0+1)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm(CVM0+2) - scm(CVM0+2) * cv(CVM0+2)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm_(CVM0) - scm_(CVM0) * cv1(CVM0)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm_(CVM0+1) - scm_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "srmatrix eig", os, __LINE__, dVeryPessimisticSp);
        CheckReal    ((scmatrix (srm) * scm_(CVM0+2) - scm_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "srmatrix eig", os, __LINE__, dPessimisticSp);

        cv.eig (srm, scm, false);
        cv1 = srm.eig (scm_, false);
//        CheckReal    ((cv - cv1).norm(),  0.,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm(CVM0) * scmatrix (srm) - ~scm(CVM0) * cv(CVM0)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dVeryPessimisticSp);
        CheckReal    ((~scm(CVM0+1) * scmatrix (srm) - ~scm(CVM0+1) * cv(CVM0+1)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm(CVM0+2) * scmatrix (srm) - ~scm(CVM0+2) * cv(CVM0+2)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm_(CVM0) * scmatrix (srm) - ~scm_(CVM0) * cv1(CVM0)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm_(CVM0+1) * scmatrix (srm) - ~scm_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm_(CVM0+2) * scmatrix (srm) - ~scm_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "srmatrix eig, left", os, __LINE__, dPessimisticSp);

        scm1.resize  (3);
        cv.eig (scm1, scm);
        cv1 = scm1.eig (scm_);
//        CheckReal    ((cv - cv1).norm(),  0.,  "scmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scm1 * scm(CVM0) - scm(CVM0) * cv(CVM0)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scm1 * scm(CVM0+1) - scm(CVM0+1) * cv(CVM0+1)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scm1 * scm(CVM0+2) - scm(CVM0+2) * cv(CVM0+2)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scm1 * scm_(CVM0) - scm_(CVM0) * cv1(CVM0)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scm1 * scm_(CVM0+1) - scm_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scm1 * scm_(CVM0+2) - scm_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "scmatrix eig", os, __LINE__, dPessimisticSp);

        cv.eig (scm1, scm, false);
        cv1 = scm1.eig (scm_ , false);
//        CheckReal    ((cv - cv1).norm(),  0.,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm(CVM0) * scm1 - ~scm(CVM0) * cv(CVM0)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm(CVM0+1) * scm1 - ~scm(CVM0+1) * cv(CVM0+1)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm(CVM0+2) * scm1 - ~scm(CVM0+2) * cv(CVM0+2)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm_(CVM0) * scm1 - ~scm_(CVM0) * cv1(CVM0)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dVeryPessimisticSp);
        CheckReal    ((~scm_(CVM0+1) * scm1 - ~scm_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);
        CheckReal    ((~scm_(CVM0+2) * scm1 - ~scm_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "scmatrix eig, left", os, __LINE__, dPessimisticSp);

        scm.resize  (4);
        scm_.resize  (4);
        cv .resize  (4);
        cv1.resize  (4);


        cv.eig (srbm2, scm);
        cv1 = srbm2.eig (scm_);
//        CheckCVector (cv, cv1,  "srbmatrix eig vectors", os, __LINE__, dPessimisticSp);
//        CheckReal    ((cv - cv1).norm(),  0.,  "srbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srbm2) * scm(CVM0) - scm(CVM0) * cv(CVM0)).norm(),   0,  "srbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srbm2) * scm(CVM0+1) - scm(CVM0+1) * cv(CVM0+1)).norm(),   0,  "srbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srbm2) * scm(CVM0+2) - scm(CVM0+2) * cv(CVM0+2)).norm(),   0,  "srbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srbm2) * scm(CVM0+3) - scm(CVM0+3) * cv(CVM0+3)).norm(),   0,  "srbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srbm2) * scm_(CVM0) - scm_(CVM0) * cv1(CVM0)).norm(),   0,  "srbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srbm2) * scm_(CVM0+1) - scm_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "srbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srbm2) * scm_(CVM0+2) - scm_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "srbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scmatrix (srbm2) * scm_(CVM0+3) - scm_(CVM0+3) * cv1(CVM0+3)).norm(),   0,  "srbmatrix eig", os, __LINE__, dPessimisticSp);

        cv.eig (scbm2, scm);
        cv1 = scbm2.eig (scm_);
//        CheckCVector (cv, cv1,  "scbmatrix eig vectors", os, __LINE__, dPessimisticSp);
//        CheckReal    ((cv - cv1).norm(),  0.,  "scbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scbm2 * scm(CVM0) - scm(CVM0) * cv(CVM0)).norm(),   0,  "scbmatrix eig", os, __LINE__, dVeryPessimisticSp);
        CheckReal    ((scbm2 * scm(CVM0+1) - scm(CVM0+1) * cv(CVM0+1)).norm(),   0,  "scbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scbm2 * scm(CVM0+2) - scm(CVM0+2) * cv(CVM0+2)).norm(),   0,  "scbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scbm2 * scm(CVM0+3) - scm(CVM0+3) * cv(CVM0+3)).norm(),   0,  "scbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scbm2 * scm_(CVM0) - scm_(CVM0) * cv1(CVM0)).norm(),   0,  "scbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scbm2 * scm_(CVM0+1) - scm_(CVM0+1) * cv1(CVM0+1)).norm(),   0,  "scbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scbm2 * scm_(CVM0+2) - scm_(CVM0+2) * cv1(CVM0+2)).norm(),   0,  "scbmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((scbm2 * scm_(CVM0+3) - scm_(CVM0+3) * cv1(CVM0+3)).norm(),   0,  "scbmatrix eig", os, __LINE__, dPessimisticSp);


        rvector b(4), x(4);
        srsmatrix B(4);
        srmatrix EV(4);

        B.set(CVM0,CVM0,(treal)1.00000000000000e+000);
        B.set(CVM0+1,CVM0,(treal)5.55244534996568e-001); B.set(CVM0+1,CVM0+1,(treal)2.00000000000000e+000);
        B.set(CVM0+2,CVM0,(treal)1.00000000000000e+003); B.set(CVM0+2,CVM0+1,(treal)1.38811133749142e+000); B.set(CVM0+2,CVM0+2,(treal)3.00000000000000e+000);
        B.set(CVM0+3,CVM0,(treal)1.94335587248799e+000); B.set(CVM0+3,CVM0+1,(treal)2.22097813998627e+000); B.set(CVM0+3,CVM0+2,(treal)2.49860040748456e+000); B.set(CVM0+3,CVM0+3,(treal)4.00000000000000e+000);

        b.eig (B, EV);
        x = B.eig ();

        CheckReal    ((x - b).norm(),   0.,  "srsmatrix eig", os, __LINE__, dPessimisticSp);
        CheckReal    ((B * EV(CVM0) - EV(CVM0) * x(CVM0)).norm(),   0,  "srsmatrix eig", os, __LINE__, dVeryPessimisticSp);
        CheckReal    ((B * EV(CVM0+1) - EV(CVM0+1) * x(CVM0+1)).norm(),   0,  "srsmatrix eig", os, __LINE__, dVeryPessimisticSp);
        CheckReal    ((B * EV(CVM0+2) - EV(CVM0+2) * x(CVM0+2)).norm(),   0,  "srsmatrix eig", os, __LINE__, dVeryPessimisticSp);
        // put schmatrix here

        rv1 = cv1.real();
        rv2 = cv1.imag();
        CheckReal    (rv1[CVM0+3] - cv1(CVM0+3).real(),   0,  "cvector::real", os, __LINE__);
        CheckReal    (rv2[CVM0+2] - cv1(CVM0+2).imag(),   0,  "cvector::imag", os, __LINE__);

        rm1 = cm1.real();
        rm2 << cm1.imag();
        CheckReal    (rm1[CVM0+2][CVM0+1] - cm1(CVM0+2,CVM0+1).real(),   0,  "cmatrix::real", os, __LINE__);
        CheckReal    (rm2(CVM0+1, CVM0+2) - cm1(CVM0+1,CVM0+2).imag(),   0,  "cmatrix::imag", os, __LINE__);

        srm = scm1.real();
        srm1 << scm1.imag();
        CheckReal    (srm[CVM0+2][CVM0+1]  - scm1(CVM0+2,CVM0+1).real(),   0,  "scmatrix::real", os, __LINE__);
        CheckReal    (srm1(CVM0+1, CVM0+2) - scm1(CVM0+1,CVM0+2).imag(),   0,  "scmatrix::imag", os, __LINE__);

        cv1.set(tcomplex ((treal)-13.45, (treal)1.778));
        cr1 = cv1[CVM0+1];
        cv << cv1.conj();
        CheckComplex (cv1(CVM0+1),   conj(cr1),  "cvector::conj", os, __LINE__);
        cv.conj (cv1);
        CheckComplex (cv(CVM0+1),   cr1,  "cvector::conj", os, __LINE__);
        cv1 = ~cv;
        CheckComplex (cv1(CVM0+1),   conj(cr1),  "cvector ~", os, __LINE__);

        cr1 = cm1(CVM0+1,CVM0+2);
        cm << cm1.conj();
        CheckComplex (cm1(CVM0+2,CVM0+1),   conj(cr1),  "cmatrix::conj", os, __LINE__);

        cm.conj (cm1);
        CheckComplex (cm(CVM0+1,CVM0+2),   cr1,  "cmatrix::conj", os, __LINE__);
        cm.resize(2,3);
        cm1.resize(3,2);
        cm1 = ~cm;
        CheckComplex (cm1(CVM0+2,CVM0+1),   conj(cr1),  "cmatrix ~", os, __LINE__);

        cr1 = scm1(CVM0+1,CVM0+2);
        scm << scm1.conj();
        CheckComplex (scm1(CVM0+2,CVM0+1),   conj(cr1),  "scmatrix::conj", os, __LINE__);
        scm.conj (scm1);
        CheckComplex (scm(CVM0+1,CVM0+2),   cr1,  "scmatrix::conj", os, __LINE__);
        scm1 = ~scm;
        CheckComplex (scm1(CVM0+2,CVM0+1),   conj(cr1),  "scmatrix ~", os, __LINE__);

        cr1 = scbm1(CVM0+1,CVM0+2);
        scbm << scbm1.conj();
        CheckComplex (scbm1(CVM0+2,CVM0+1),   conj(cr1),  "scbmatrix::conj", os, __LINE__);
        scbm.conj (scbm1);
        CheckComplex (scbm(CVM0+1,CVM0+2),   cr1,  "scbmatrix::conj", os, __LINE__);
        scbm1 = ~scbm;
        CheckComplex (scbm1(CVM0+2,CVM0+1),   conj(cr1),  "scbmatrix ~", os, __LINE__);


        r1 = (treal) 1.389;
        rv1.set(r1);
        cr1 = cv1[CVM0+2];
        cv1.assign_real(rv1);
        CheckReal    (cv1(CVM0+2).real(),   r1,  "cvector::assign_real", os, __LINE__);
        CheckReal    (cv1(CVM0+2).imag(),   cr1.imag(),  "cvector::assign_real", os, __LINE__);
        cv1.assign_imag(rv1);
        CheckReal    (cv1(CVM0+1).real(),   r1,  "cvector::assign_imag", os, __LINE__);
        CheckReal    (cv1(CVM0+1).imag(),   r1,  "cvector::assign_imag", os, __LINE__);

        rm1.resize(3,2);
        rm1.set(r1);
        cr1 = cm1(CVM0+2,CVM0+1);
        cm1.assign_real(rm1);
        CheckReal    (cm1(CVM0+2,CVM0+1).real(),   r1,  "cmatrix::assign_real", os, __LINE__);
        CheckReal    (cm1(CVM0+2,CVM0+1).imag(),   cr1.imag(),  "cmatrix::assign_real", os, __LINE__);
        cm1.assign_imag(rm1);
        CheckReal    (cm1(CVM0+1,CVM0+1).real(),   r1,  "cmatrix::assign_imag", os, __LINE__);
        CheckReal    (cm1(CVM0+1,CVM0+1).imag(),   r1,  "cmatrix::assign_imag", os, __LINE__);


        srm.set(r1);
        cr1 = scm1(CVM0+2,CVM0+1);
        scm1.assign_real(srm);
        CheckReal    (scm1(CVM0+2,CVM0+1).real(),   r1,  "scmatrix::assign_real", os, __LINE__);
        CheckReal    (scm1(CVM0+2,CVM0+1).imag(),   cr1.imag(),  "scmatrix::assign_real", os, __LINE__);
        scm1.assign_imag(srm);
        CheckReal    (scm1(CVM0+1,CVM0+1).real(),   r1,  "scmatrix::assign_imag", os, __LINE__);
        CheckReal    (scm1(CVM0+1,CVM0+1).imag(),   r1,  "scmatrix::assign_imag", os, __LINE__);

        srbm.resize(4);
        srbm.set(r1);
        scbm1.resize_lu (1, 2);
        cr1 = scbm1(CVM0, CVM0+1);
        scbm1.assign_real(srbm);
        CheckReal    (scbm1(CVM0,CVM0+1).real(),   r1,  "scbmatrix::assign_real", os, __LINE__);
        CheckReal    (scbm1(CVM0,CVM0+1).imag(),   cr1.imag(),  "scbmatrix::assign_real", os, __LINE__);
        scbm1.assign_imag(srbm);
        CheckReal    (scbm1(CVM0,CVM0+1).real(),   r1,  "scbmatrix::assign_imag", os, __LINE__);
        CheckReal    (scbm1(CVM0,CVM0+1).imag(),   r1,  "scbmatrix::assign_imag", os, __LINE__);

        rm.set(1.);
        rm.normalize();
        rm(CVM0+1,CVM0+2) = (treal) -1.1;
        rm(CVM0,CVM0+1) = (treal) 1.e-9;
        CheckInt     (rm.rowofmax (),   CVM0+1,  "rmatrix::rowofmax", os, __LINE__);

        CheckInt     (rm.colofmax (),   CVM0+2,  "rmatrix::colofmax", os, __LINE__);
        CheckInt     (rm.rowofmin (),   CVM0,  "rmatrix::rowofmin", os, __LINE__);
        CheckInt     (rm.colofmin (),   CVM0+1,  "rmatrix::colofmin", os, __LINE__);
        CheckInt     (rm.msize (),      2,  "rmatrix::msize", os, __LINE__);
        CheckInt     (rm.nsize (),      3,  "rmatrix::nsize", os, __LINE__);
        rm1 << rm.swap_rows (CVM0,CVM0+1);
        CheckReal    (rm1(CVM0,CVM0+2),   (treal) -1.1,  "rmatrix::swap_rows", os, __LINE__);
        rm1.swap_cols (CVM0,CVM0+1);
        CheckReal    (rm1(CVM0+1,CVM0),   (treal) 1.e-9,  "rmatrix::swap_cols", os, __LINE__);

        srm.set(1.);
        srm.normalize();
        srm(CVM0+1,CVM0+2) = (treal) -1.1;
        srm(CVM0,CVM0+1) = (treal) 1.e-9;
        CheckInt     (srm.rowofmax (),   CVM0+1,  "srmatrix::rowofmax", os, __LINE__);
        CheckInt     (srm.colofmax (),   CVM0+2,  "srmatrix::colofmax", os, __LINE__);
        CheckInt     (srm.rowofmin (),   CVM0,  "srmatrix::rowofmin", os, __LINE__);
        CheckInt     (srm.colofmin (),   CVM0+1,  "srmatrix::colofmin", os, __LINE__);
        CheckInt     (srm.msize (),      3,  "srmatrix::msize", os, __LINE__);
        CheckInt     (srm.nsize (),      3,  "srmatrix::nsize", os, __LINE__);
        srm1 << srm.swap_rows (CVM0,CVM0+1);
        CheckReal    (srm1(CVM0,CVM0+2),   (treal) -1.1,  "srmatrix::swap_rows", os, __LINE__);
        srm1.swap_cols (CVM0,CVM0+1);
        CheckReal    (srm1(CVM0+1,CVM0),   (treal) 1.e-9,  "srmatrix::swap_cols", os, __LINE__);

        cm.set(tcomplex ((treal) 1., treal (1.)));
        cm.normalize();
        cm(CVM0+1,CVM0+2) = tcomplex ((treal) 1.1, treal (1.1));
        cm(CVM0,CVM0+1) = tcomplex ((treal) 1.e-9, (treal) 0.);
        CheckInt     (cm.rowofmax (),   CVM0+1,  "cmatrix::rowofmax", os, __LINE__);
        CheckInt     (cm.colofmax (),   CVM0+2,  "cmatrix::colofmax", os, __LINE__);
        CheckInt     (cm.rowofmin (),   CVM0,  "cmatrix::rowofmin", os, __LINE__);
        CheckInt     (cm.colofmin (),   CVM0+1,  "cmatrix::colofmin", os, __LINE__);
        CheckInt     (cm.msize (),      2,  "cmatrix::msize", os, __LINE__);
        CheckInt     (cm.nsize (),      3,  "cmatrix::nsize", os, __LINE__);
        cm1 << cm.swap_rows (CVM0,CVM0+1);
        CheckComplex (cm1(CVM0,CVM0+2),   tcomplex ((treal) 1.1, treal (1.1)),  "cmatrix::swap_rows", os, __LINE__);
        cm1.swap_cols (CVM0,CVM0+1);
        CheckComplex (cm1(CVM0+1,CVM0),   tcomplex ((treal) 1.e-9, (treal) 0.),  "cmatrix::swap_cols", os, __LINE__);

        scm.set(tcomplex ((treal) 1., treal (1.)));
        scm.normalize();
        scm(CVM0+1,CVM0+2) = tcomplex ((treal) 1.1, treal (1.1));
        scm(CVM0,CVM0+1) = tcomplex ((treal) 1.e-9, (treal) 0.);
        CheckInt     (scm.rowofmax (),   CVM0+1,  "scmatrix::rowofmax", os, __LINE__);
        CheckInt     (scm.colofmax (),   CVM0+2,  "scmatrix::colofmax", os, __LINE__);
        CheckInt     (scm.rowofmin (),   CVM0,  "scmatrix::rowofmin", os, __LINE__);
        CheckInt     (scm.colofmin (),   CVM0+1,  "scmatrix::colofmin", os, __LINE__);
        CheckInt     (scm.msize (),      3,  "scmatrix::msize", os, __LINE__);
        CheckInt     (scm.nsize (),      3,  "scmatrix::nsize", os, __LINE__);
        scm1 << scm.swap_rows (CVM0,CVM0+1);
        CheckComplex (scm1(CVM0,CVM0+2),   tcomplex ((treal) 1.1, treal (1.1)),  "scmatrix::swap_rows", os, __LINE__);
        scm1.swap_cols (CVM0,CVM0+1);
        CheckComplex (scm1(CVM0+1,CVM0),   tcomplex ((treal) 1.e-9, (treal) 0.),  "scmatrix::swap_cols", os, __LINE__);

        srbm.diag(0) = rv;
        srbm.normalize();
        srbm(CVM0+1,CVM0+2) = (treal) -1.1;
        srbm(CVM0,CVM0+1) = (treal) 1.e-7;

        CheckInt     (srbm.rowofmax (),   CVM0+1,  "srbmatrix::rowofmax", os, __LINE__);
        CheckInt     (srbm.colofmax (),   CVM0+2,  "srbmatrix::colofmax", os, __LINE__);
        CheckInt     (srbm.rowofmin (),   CVM0+2,  "srbmatrix::rowofmin", os, __LINE__);
        CheckInt     (srbm.colofmin (),   CVM0,  "srbmatrix::colofmin", os, __LINE__);
        CheckInt     (srbm.msize (),      4,  "srbmatrix::msize", os, __LINE__);
        CheckInt     (srbm.nsize (),      4,  "srbmatrix::nsize", os, __LINE__);

        scbm.diag(0).set(tcomplex ((treal) 1., treal (1.)));
        scbm.normalize();
        scbm(CVM0+1,CVM0+2) = tcomplex ((treal) 2., treal (1.));
        scbm(CVM0,CVM0+1) = tcomplex ((treal) -1.e-10, treal (-1.e-10));
        CheckInt     (scbm.rowofmax (),   CVM0+1,  "scbmatrix::rowofmax", os, __LINE__);
        CheckInt     (scbm.colofmax (),   CVM0+2,  "scbmatrix::colofmax", os, __LINE__);
        CheckInt     (scbm.rowofmin (),   CVM0+3,  "scbmatrix::rowofmin", os, __LINE__);
        CheckInt     (scbm.colofmin (),   CVM0,  "scbmatrix::colofmin", os, __LINE__);
        CheckInt     (scbm.msize (),      4,  "scbmatrix::msize", os, __LINE__);
        CheckInt     (scbm.nsize (),      4,  "scbmatrix::nsize", os, __LINE__);


        for (i = 0; i < 100; i++)
        {
            a1[i] = (treal) sqrt ((treal) (i + 1));
            a2[i] = (treal) (i + 1) / (treal) 10.;
            c1[i] = tcomplex(a1[i], a2[i]);
        }

        rm2.set((treal)-0.34);
        rm2(CVM0+1,CVM0+2) = (treal) 0.;

        CheckInt     (rm2.rank (),      2,  "rmatrix::rank", os, __LINE__);
        rm2.assign(a1);
        CheckInt     (rm2.rank (),      4,  "rmatrix::rank", os, __LINE__);

        srm2.assign(a1);
        srm2[CVM0+1].set((treal)0.);
        CheckInt     (srm2.rank (),     2,  "srmatrix::rank", os, __LINE__);
        srm2.diag(0).set((treal)0.);
        CheckInt     (srm2.rank (),     2,  "srmatrix::rank", os, __LINE__);

        cm2.resize (3, 4);
        cm2.assign(c1);
        CheckInt     (cm2.rank (),      3,  "cmatrix::rank", os, __LINE__);

        scm2.assign(c1);
        CheckInt     (scm2.rank (),     3,  "scmatrix::rank", os, __LINE__);
        scm2.diag(0).set((treal)0.);
        CheckInt     (scm2.rank (),     3,  "scmatrix::rank", os, __LINE__);

        srbm.assign(a1);
        CheckInt     (srbm.rank (),     4,  "srbmatrix::rank", os, __LINE__);

        scbm.assign(c1);
        CheckInt     (scbm.rank (),     4,  "scbmatrix::rank", os, __LINE__);


        r1 = (treal) -8.76;
        srm2.set(r1);
        srm2.diag(1).set(0.);
        CheckReal    (srm2(CVM0,CVM0),   r1,  "srmatrix::diag", os, __LINE__);
        CheckReal    (srm2(CVM0,CVM0+1),   (treal) 0.,  "srmatrix::diag", os, __LINE__);

        cr1 = tcomplex ((treal) -8.76, (treal) -3.6);
        scm2.set(cr1);

        scm2.diag(1).set(0L);
        CheckComplex (scm2(CVM0,CVM0),   cr1,  "scmatrix::diag", os, __LINE__);
        CheckComplex (scm2(CVM0,CVM0+1),   0,    "scmatrix::diag", os, __LINE__);

        srbm.set(r1);
        srbm.diag(1).set(0.);
        CheckReal    (srbm(CVM0,CVM0),   r1,  "srbmatrix::diag", os, __LINE__);
        CheckReal    (srbm(CVM0,CVM0+1),   (treal) 0.,  "srbmatrix::diag", os, __LINE__);

        scbm.set(cr1);
        scbm.diag(1).set(0.);
        CheckComplex (scbm(CVM0,CVM0),   cr1,  "scbmatrix::diag", os, __LINE__);
        CheckComplex (scbm(CVM0,CVM0+1),   tcomplex ((treal) 0., (treal) 0.),  "srbmatrix::diag", os, __LINE__);


        srm2.set(r1);
        srm2++;
        CheckReal    (srm2(CVM0,CVM0),   r1 + 1,  "srmatrix++", os, __LINE__);
        ++srm2;
        CheckReal    (srm2(CVM0+1,CVM0+1),   r1 + 2,  "++srmatrix", os, __LINE__);
        srm2--;
        CheckReal    (srm2(CVM0,CVM0),   r1 + 1,  "srmatrix--", os, __LINE__);
        --srm2;
        CheckReal    (srm2(CVM0+1,CVM0+1),   r1,      "--srmatrix", os, __LINE__);

        scm2.set(cr1);
        scm2++;
        CheckComplex (scm2(CVM0,CVM0),   cr1 + tcomplex (1), "scmatrix++", os, __LINE__);
        ++scm2;
        CheckComplex (scm2(CVM0+1,CVM0+1),   cr1 + tcomplex (2), "++scmatrix", os, __LINE__);
        scm2--;
        CheckComplex (scm2(CVM0,CVM0),   cr1 + tcomplex (1), "scmatrix--", os, __LINE__);
        --scm2;
        CheckComplex (scm2(CVM0+1,CVM0+1),   cr1,                "--scmatrix", os, __LINE__);

        srbm.set(r1);
        srbm++;
        CheckReal    (srbm(CVM0,CVM0),   r1 + 1,  "srbmatrix++", os, __LINE__);
        ++srbm;
        CheckReal    (srbm(CVM0+1,CVM0+1),   r1 + 2,  "++srbmatrix", os, __LINE__);
        srbm--;
        CheckReal    (srbm(CVM0,CVM0),   r1 + 1,  "srbmatrix--", os, __LINE__);
        --srbm;
        CheckReal    (srbm(CVM0+1,CVM0+1),   r1,      "--srbmatrix", os, __LINE__);

        scbm.set(cr1);
        scbm++;
        CheckComplex (scbm(CVM0,CVM0),   cr1 + tcomplex (1), "scbmatrix++", os, __LINE__);
        ++scbm;
        CheckComplex (scbm(CVM0+1,CVM0+1),   cr1 + tcomplex (2), "++scbmatrix", os, __LINE__);
        scbm--;
        CheckComplex (scbm(CVM0,CVM0),   cr1 + tcomplex (1), "scbmatrix--", os, __LINE__);
        --scbm;
        CheckComplex (scbm(CVM0+1,CVM0+1),   cr1,                "--scbmatrix", os, __LINE__);


        srm << srm2.identity();
        CheckReal    (srm(CVM0,CVM0),    1,  "srmatrix::identity", os, __LINE__);
        CheckReal    (srm2(CVM0,CVM0+1),   0,  "srmatrix::identity", os, __LINE__);

        scm << scm2.identity();
        CheckComplex (scm(CVM0,CVM0),    1,  "scmatrix::identity", os, __LINE__);
        CheckComplex (scm2(CVM0,CVM0+1),   0,  "scmatrix::identity", os, __LINE__);


        srbm << srbm1.identity();
        CheckReal    (srbm(CVM0,CVM0),    1,  "srbmatrix::identity", os, __LINE__);
        CheckReal    (srbm1(CVM0,CVM0+1),   0,  "srbmatrix::identity", os, __LINE__);

        scbm << scbm1.identity();
        CheckComplex (scbm(CVM0,CVM0),    1,  "scbmatrix::identity", os, __LINE__);
        CheckComplex (scbm1(CVM0,CVM0+1),   0,  "scbmatrix::identity", os, __LINE__);


        rm.assign(a2);
        rm1 << rm.transpose();
        CheckReal    (rm(CVM0,CVM0+1),    a2[1],  "rmatrix::transpose", os, __LINE__);
        rm1.resize (rm.nsize(), rm.msize());
        rm1.transpose (rm);
        CheckReal    (rm1(CVM0,CVM0+1),   a2[2],  "rmatrix::transpose", os, __LINE__);

        srm.assign(a2);
        srm1 << srm.transpose();
        CheckReal    (srm(CVM0,CVM0+1),   a2[1],  "srmatrix::transpose", os, __LINE__);
        srm1.transpose (srm);
        CheckReal    (srm1(CVM0,CVM0+1),  a2[3],  "srmatrix::transpose", os, __LINE__);

        srbm.resize_lu (2, 1);
        srbm.assign(a2);
        srbm1 << srbm.transpose();
        CheckReal    (srbm(CVM0,CVM0+1),   a2[2],  "srbmatrix::transpose", os, __LINE__);
        srbm.transpose();
        srbm.assign(a2);
        srbm1.transpose (srbm);
        CheckReal    (srbm1(CVM0,CVM0+1),  a2[2],  "srbmatrix::transpose", os, __LINE__);

        scbm1 = scbm.assign(c1);
        scbm1.conj();
        CheckComplex (scbm1(CVM0,CVM0+1),  conj(scbm(CVM0+1,CVM0)),  "scbmatrix::conj", os, __LINE__);
        scbm1.assign(c1);
        scbm.conj(scbm1);
        CheckComplex (scbm1(CVM0,CVM0+2),  conj(scbm(CVM0+2,CVM0)),  "scbmatrix::conj", os, __LINE__);
        CheckComplex (scbm1(CVM0,CVM0+3),  conj(scbm(CVM0+3,CVM0)),  "scbmatrix::conj", os, __LINE__);


        srm.resize(3);
        srm1.resize(3);
        srm(CVM0,CVM0) = (treal) 2;    srm(CVM0,CVM0+1) = (treal) -0.8; srm(CVM0,CVM0+2) = (treal) -0.7;
        srm(CVM0+1,CVM0) = (treal) -0.4; srm(CVM0+1,CVM0+1) = (treal) -1;   srm(CVM0+1,CVM0+2) = (treal) -0.8;
        srm(CVM0+2,CVM0) = (treal) -0.6; srm(CVM0+2,CVM0+1) = (treal) -1.2; srm(CVM0+2,CVM0+2) = (treal) -0.9;

        srm1 = srm.exp();
        CheckReal    (srm1(CVM0,CVM0), (treal)  8.484495096274699e+000, "srmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0,CVM0+1), (treal) -1.555963610758445e+000, "srmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0,CVM0+2), (treal) -1.484978300761370e+000, "srmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0), (treal) -7.330690267073194e-001, "srmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0+1), (treal)  6.959256837027834e-001, "srmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0+2), (treal) -2.385221030493092e-001, "srmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+2,CVM0), (treal) -1.324167433420492e+000, "srmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+2,CVM0+1), (treal) -3.128703759020610e-001, "srmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+2,CVM0+2), (treal)  8.267946985957282e-001, "srmatrix::exp", os, __LINE__, dPessimisticSp);

        scm.resize(3);
        scm1.resize(3);
        scm(CVM0,CVM0) = tcomplex ((treal) 1.e-01, (treal) 2.e-01);
        scm(CVM0,CVM0+1) = tcomplex ((treal) 3.e-01, (treal) 4.e-01);
        scm(CVM0,CVM0+2) = tcomplex ((treal) 5.e-01, (treal) 6.e-01);
        scm(CVM0+1,CVM0) = tcomplex ((treal) 1.e+00, (treal) 1.e+00);
        scm(CVM0+1,CVM0+1) = tcomplex ((treal) 0.e+00, (treal) 0.e+00);
        scm(CVM0+1,CVM0+2) = tcomplex ((treal) 0.e+00, (treal) 0.e+00);
        scm(CVM0+2,CVM0) = tcomplex ((treal) -1.e-01,(treal) -1.e-01);
        scm(CVM0+2,CVM0+1) = tcomplex ((treal) -3.e-01,(treal) -3.e-01);
        scm(CVM0+2,CVM0+2) = tcomplex ((treal) -5.e-01,(treal) -5.e-01);

        scm(CVM0+1,CVM0+1)=-cr1;
        scm1 = scm.exp();
        CheckComplex (scm1(CVM0,CVM0),  tcomplex ((treal) -4.816680814596321e+000, (treal) -4.855745768190474e+001), "scmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+1,CVM0),  tcomplex((treal) -5.878045515841980e+002, (treal) -7.809398663068483e+002), "scmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+2,CVM0),  tcomplex((treal) 1.111774160999558e+001, (treal)  3.979363145886382e+001), "scmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0,CVM0+1),  tcomplex((treal) -1.623884970745376e+002, (treal)  -2.805917519984524e+002), "scmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+1,CVM0+1),  tcomplex((treal) -5.604582009475869e+003, (treal)   -3.219074690441815e+003), "scmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+2,CVM0+1),  tcomplex((treal) 1.715815440786858e+002, (treal)  2.129974004265882e+002), "scmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0,CVM0+2),  tcomplex((treal) 1.710263520348249e+000, (treal)  -3.149555947204208e+000), "scmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+1,CVM0+2),  tcomplex((treal) -1.432034221529735e+001, (treal)  -7.375809596051487e+001), "scmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+2,CVM0+2),  tcomplex((treal) -4.639004479804901e-002, (treal)  2.814422951041492e+000), "scmatrix::exp", os, __LINE__, dPessimisticSp);


        srbm1.resize(2);
        srbm1.resize_lu(0, 1);
        srbm1(CVM0,CVM0) = (treal) 1.3;
        srbm1(CVM0,CVM0+1) = (treal) -11.2;
        srbm1(CVM0+1,CVM0+1) = (treal) 4.1;
        os << srbm1;


        srm1 << srbm1.exp();
        CheckReal    (srm1(CVM0,CVM0), (treal) 3.669296667619233e+000, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0,CVM0+1), (treal) -2.266839637189685e+002, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0), (treal) 0, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0+1), (treal) 6.034028759736115e+001, "srbmatrix::exp", os, __LINE__, dPessimisticSp);

        iarray aPivots(3);
        srmatrix mLU (3), mLU2 (3), mLo(3), mUp(3);

        mLU = srm.low_up (aPivots);
        mLU2.low_up (srm, aPivots);
        CheckBool    (mLU == mLU2, true, "srmatrix::low_up", os, __LINE__);

        mLo.identity ();
        mLo(CVM0+1,CVM0) = mLU(CVM0+1,CVM0);
        mLo(CVM0+2,CVM0) = mLU(CVM0+2,CVM0);
        mLo(CVM0+2,CVM0+1) = mLU(CVM0+2,CVM0+1);

        mUp(CVM0,CVM0) = mLU(CVM0,CVM0);
        mUp(CVM0,CVM0+1) = mLU(CVM0,CVM0+1);
        mUp(CVM0,CVM0+2) = mLU(CVM0,CVM0+2);
        mUp(CVM0+1,CVM0+1) = mLU(CVM0+1,CVM0+1);
        mUp(CVM0+1,CVM0+2) = mLU(CVM0+1,CVM0+2);
        mUp(CVM0+2,CVM0+2) = mLU(CVM0+2,CVM0+2);

        mLU = mLo * mUp;
        for (l = CVM0+2; l >= CVM0; l--) {
            mLU.swap_rows (l, aPivots[l] - (1 - CVM0));
        }
        CheckReal    ((srm - mLU).norminf(), (treal) 0, "srmatrix::low_up", os, __LINE__, dPessimisticSp);


        scmatrix cmLU (3), cmLU2 (3), cmLo(3), cmUp(3);
        cmLU = scm.low_up (aPivots);
        cmLU2.low_up (scm, aPivots);
        CheckBool    (cmLU == cmLU2, true, "scmatrix::low_up", os, __LINE__);

        cmLo.identity ();
        cmLo(CVM0+1,CVM0) = cmLU(CVM0+1,CVM0);
        cmLo(CVM0+2,CVM0) = cmLU(CVM0+2,CVM0);
        cmLo(CVM0+2,CVM0+1) = cmLU(CVM0+2,CVM0+1);

        cmUp(CVM0,CVM0) = cmLU(CVM0,CVM0);
        cmUp(CVM0,CVM0+1) = cmLU(CVM0,CVM0+1);
        cmUp(CVM0,CVM0+2) = cmLU(CVM0,CVM0+2);
        cmUp(CVM0+1,CVM0+1) = cmLU(CVM0+1,CVM0+1);

        cmUp(CVM0+1,CVM0+2) = cmLU(CVM0+1,CVM0+2);
        cmUp(CVM0+2,CVM0+2) = cmLU(CVM0+2,CVM0+2);

        cmLU = cmLo * cmUp;
        for (l = CVM0+2; l >= CVM0; l--) {
                cmLU.swap_rows (l, aPivots[l] - (1 - CVM0));
        }
        CheckReal    ((scm - cmLU).norminf(), (treal) 0, "scmatrix::low_up", os, __LINE__, dPessimisticSp);


        srm1 << srm.inv();
        CheckReal    (((srm1 * srm)--).norm(), (treal) 0, "srmatrix::inv", os, __LINE__, dPessimisticSp);
        scm1 << scm.inv();
        CheckReal    (((scm1 * scm)--).norm(), (treal) 0, "scmatrix::inv", os, __LINE__, dPessimisticSp);


        rv.resize(11);
        rv(CVM0)  = (treal) 2.2;
        rv(CVM0+1)  = (treal) 1.3;
        rv(CVM0+2)  = (treal) 1.1;
        rv(CVM0+3)  = (treal) - 0.9;
        rv(CVM0+4)  = (treal) 0.2;
        rv(CVM0+5)  = (treal) - 0.45;
        rv(CVM0+6)  = (treal) 45;
        rv(CVM0+7)  = (treal) - 30;
        rv(CVM0+8)  = (treal) 10;
        rv(CVM0+9) = (treal) 3;
        rv(CVM0+10) = (treal) 3.2;

        srm1.polynom (srm, rv);
        srm2 << srm.polynom (rv);
        CheckBool    (srm1 == srm2, true, "srmatrix::polynom", os, __LINE__);
        CheckReal    (srm1(CVM0,CVM0), (treal)  1.415106245372072e+004, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0,CVM0+1), (treal)  8.018578436580816e+002, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0,CVM0+2), (treal)  1.516628273102821e+002, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0), (treal)  6.009153894255998e+002, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0+1), (treal)  8.458618026988163e+003, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0+2), (treal)  6.668127559823842e+003, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+2,CVM0), (treal) -9.855925384439991e+001, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+2,CVM0+1), (treal)  1.020217780733232e+004, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+2,CVM0+2), (treal)  8.075071634102441e+003, "srmatrix::polynom", os, __LINE__, dPessimisticSp);

        rv.resize(3);
        srm1.polynom (srm, rv);
        CheckReal    (srm1(CVM0,CVM0), (treal)  1.001400000000000e+001, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0,CVM0+1), (treal) -9.960000000000001e-001, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0,CVM0+2), (treal) -1.053000000000000e+000, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0), (treal) -4.320000000000001e-001, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0+1), (treal)  3.408000000000000e+000, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+1,CVM0+2), (treal)  9.400000000000004e-001, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+2,CVM0), (treal) -9.780000000000000e-001, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+2,CVM0+1), (treal)  1.476000000000000e+000, "srmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckReal    (srm1(CVM0+2,CVM0+2), (treal)  3.439000000000000e+000, "srmatrix::polynom", os, __LINE__, dPessimisticSp);


        cv.resize(11);
        cv(CVM0)  = tcomplex ((treal) 2.2, (treal) -1);
        cv(CVM0+1)  = tcomplex ((treal) 1.3, (treal) -0.6);
        cv(CVM0+2)  = tcomplex ((treal) 1.1, (treal) 2.3);
        cv(CVM0+3)  = tcomplex ((treal) -0.9);
        cv(CVM0+4)  = tcomplex ((treal) 0.2, (treal) 1);
        cv(CVM0+5)  = tcomplex ((treal) -0.45, (treal) 2);
        cv(CVM0+6)  = tcomplex ((treal) 45, (treal) -17.3);
        cv(CVM0+7)  = tcomplex ((treal) -30);
        cv(CVM0+8)  = tcomplex ((treal) 10, (treal) 1.5);
        cv(CVM0+9) = tcomplex ((treal) 3);
        cv(CVM0+10) = tcomplex ((treal) 3.2, (treal) -18.9);


        scm(CVM0,CVM0) = tcomplex ((treal) 2., (treal) 0.1);
        scm(CVM0,CVM0+1) = tcomplex ((treal) -8.e-001, (treal) -1);
        scm(CVM0,CVM0+2) = tcomplex ((treal) -7.e-001, (treal) -2.1);
        scm(CVM0+1,CVM0) = tcomplex ((treal) -4.e-001, (treal) -0.1);
        scm(CVM0+1,CVM0+1) = tcomplex ((treal) -1.e+000);
        scm(CVM0+1,CVM0+2) = tcomplex ((treal) -8.e-001, (treal) 3.1);
        scm(CVM0+2,CVM0) = tcomplex ((treal) -6.e-001,(treal) 1);
        scm(CVM0+2,CVM0+1) = tcomplex ((treal) -1.2e+000);
        scm(CVM0+2,CVM0+2) = tcomplex ((treal) -9.e-001,(treal) -5.4);

        scm1.polynom (scm, cv);

        CheckComplex (scm1(CVM0,CVM0),  tcomplex ((treal)  5.264016832618990e+006, (treal) -1.051212804982833e+007), "scmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0,CVM0+1),  tcomplex ((treal)  9.386518437203571e+006, (treal) -9.534002545240149e+006), "scmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0,CVM0+2),  tcomplex ((treal)  2.313187132312614e+007, (treal)  4.742508767071142e+007), "scmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+1,CVM0),  tcomplex ((treal) -1.143556158726668e+007, (treal)  2.626370923270145e+007), "scmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+1,CVM0+1),  tcomplex ((treal) -2.183671220461629e+007, (treal)  2.471364343201455e+007), "scmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+1,CVM0+2),  tcomplex ((treal) -6.325599106881835e+007, (treal) -1.133746860502928e+008), "scmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+2,CVM0),  tcomplex ((treal)  1.143469364494270e+007, (treal) -4.448575764049879e+007), "scmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+2,CVM0+1),  tcomplex ((treal)  2.832544852276585e+007, (treal) -4.473797233313387e+007), "scmatrix::polynom", os, __LINE__, dPessimisticSp);
        CheckComplex (scm1(CVM0+2,CVM0+2),  tcomplex ((treal)  1.291773725514465e+008, (treal)  1.634454865648127e+008), "scmatrix::polynom", os, __LINE__, dPessimisticSp);

        scm2 << scm.polynom (cv);
        CheckReal    ((scm1.normalize() - scm2.normalize()).norm(), (treal) 0., "scmatrix::polynom", os, __LINE__, dVeryPessimisticSp);

        srbm.resize_lu(1,0);
        bool bThrew = false;
        try
        {
            srbm(CVM0,CVM0+1) = (treal) 1.;
        }
        catch (cvmexception e)
        {
            if (e.cause() == CVM_READ_ONLY_ACCESS) bThrew = true;
        }
        CheckBool (bThrew, true, "srbmatrix read only exception", os, __LINE__);

        srbm.diag(0).set(1.);
        srbm.diag(-1).set(1.);

        srm << srbm.exp();

        CheckReal    (srm(CVM0,CVM0), (treal)  2.718281828459041e+000, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0,CVM0+1), (treal)  0., "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0,CVM0+2), (treal)  0., "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0,CVM0+3), (treal)  0., "srbmatrix::exp", os, __LINE__, dPessimisticSp);

        CheckReal    (srm(CVM0+1,CVM0), (treal)  2.718281828459041e+000, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0+1,CVM0+1), (treal)  2.718281828459041e+000, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0+1,CVM0+2), (treal)  0., "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0+1,CVM0+3), (treal)  0., "srbmatrix::exp", os, __LINE__, dPessimisticSp);

        CheckReal    (srm(CVM0+2,CVM0), (treal)  1.359140914229521e+000, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0+2,CVM0+1), (treal)  2.718281828459041e+000, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0+2,CVM0+2), (treal)  2.718281828459041e+000, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0+2,CVM0+3), (treal)  0., "srbmatrix::exp", os, __LINE__, dPessimisticSp);

        CheckReal    (srm(CVM0+3,CVM0), (treal)  4.530469714098402e-001, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0+3,CVM0+1), (treal)  1.359140914229521e+000, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0+3,CVM0+2), (treal)  2.718281828459041e+000, "srbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckReal    (srm(CVM0+3,CVM0+3), (treal)  2.718281828459041e+000, "srbmatrix::exp", os, __LINE__, dPessimisticSp);

        srm -= (srmatrix) srbm;

        scbm.resize_lu(1,0);
        bThrew = false;
        try
        {
            scbm(CVM0,CVM0+1) = (treal) 1.;
        }
        catch (cvmexception e)
        {
            if (e.cause() == CVM_READ_ONLY_ACCESS) bThrew = true;
        }
        CheckBool (bThrew, true, "scbmatrix read only exception", os, __LINE__);

        scbm.diag(0).set(tcomplex ((treal) 1., (treal) 1.));
        scbm.diag(-1).set(tcomplex ((treal) 1., (treal) 1.));

        scm << scbm.exp();

        CheckComplex (scm(CVM0,CVM0),  tcomplex ((treal)  1.468693939915887e+000, (treal) 2.287355287178844e+000), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+1,CVM0),  tcomplex ((treal)  -8.186613472629570e-001, (treal) 3.756049227094730e+000), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+2,CVM0),  tcomplex ((treal)  -2.287355287178843e+000, (treal) 1.468693939915886e+000), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+3,CVM0),  tcomplex ((treal)  -1.252016409031576e+000, (treal) -2.728871157543187e-001), "scbmatrix::exp", os, __LINE__, dPessimisticSp);

        CheckComplex (scm(CVM0,CVM0+1),  tcomplex ((treal)  0., (treal) 0.), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+1,CVM0+1),  tcomplex ((treal)  1.468693939915887e+000, (treal) 2.287355287178844e+000), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+2,CVM0+1),  tcomplex ((treal)  -8.186613472629570e-001, (treal) 3.756049227094730e+000), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+3,CVM0+1),  tcomplex ((treal)  -2.287355287178843e+000, (treal) 1.468693939915886e+000), "scbmatrix::exp", os, __LINE__, dPessimisticSp);

        CheckComplex (scm(CVM0,CVM0+2),  tcomplex ((treal)  0., (treal) 0.), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+1,CVM0+2),  tcomplex ((treal)  0., (treal) 0.), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+2,CVM0+2),  tcomplex ((treal)  1.468693939915887e+000, (treal) 2.287355287178844e+000), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+3,CVM0+2),  tcomplex ((treal)  -8.186613472629570e-001, (treal) 3.756049227094730e+000), "scbmatrix::exp", os, __LINE__, dPessimisticSp);

        CheckComplex (scm(CVM0,CVM0+3),  tcomplex ((treal)  0., (treal) 0.), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+1,CVM0+3),  tcomplex ((treal)  0., (treal) 0.), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+2,CVM0+3),  tcomplex ((treal)  0., (treal) 0.), "scbmatrix::exp", os, __LINE__, dPessimisticSp);
        CheckComplex (scm(CVM0+3,CVM0+3),  tcomplex ((treal)  1.468693939915887e+000, (treal) 2.287355287178844e+000), "scbmatrix::exp", os, __LINE__, dPessimisticSp);

        r1 = (treal) 1.217;
        r2 = (treal) -.179;
        rv << rv2 << rv1;
        rv2.normalize();

        rv1.gemv (true, srm, r1, rv2, r2);
        rv = rv2 * srm * r1 + r2 * rv;
        CheckReal    ((rv - rv1).norm(), (treal) 0., "rvector::gemv", os, __LINE__, dPessimisticSp);

        rv = rv1;
        rv1.gemv (false, srm, r1, rv2, r2);
        rv = srm * rv2 * r1 + r2 * rv;
        CheckReal    ((rv - rv1).norm(), (treal) 0., "rvector::gemv", os, __LINE__, dPessimisticSp);

        rv = rv1;
        rv1.gbmv (true, srbm, r1, rv2, r2);
        rv = rv2 * srbm * r1 + r2 * rv;
        CheckReal    ((rv - rv1).norm(), (treal) 0., "rvector::gbmv", os, __LINE__, dPessimisticSp);

        rv = rv1;
        rv1.gbmv (false, srbm, r1, rv2, r2);
        rv = srbm * rv2 * r1 + r2 * rv;
        CheckReal    ((rv - rv1).norm(), (treal) 0., "rvector::gbmv", os, __LINE__, dPessimisticSp);

        cv2 << cv1;
        cv << cv2;
        cv2.normalize();

        cv1.gemv (true, scm, cr1, cv2, cr2);
        cv = cv2 * scm * cr1 + cr2 * cv;
        CheckReal    ((cv - cv1).norm(), (treal) 0., "cvector::gemv", os, __LINE__, dPessimisticSp);

        cv = cv1;
        cv1.gemv (false, scm, cr1, cv2, cr2);
        cv = scm * cv2 * cr1 + cr2 * cv;
        CheckReal    ((cv - cv1).norm(), (treal) 0., "cvector::gemv", os, __LINE__, dPessimisticSp);

        cv = cv1;
        cv1.gbmv (true, scbm, cr1, cv2, cr2);
        cv = cv2 * scbm * cr1 + cr2 * cv;
        CheckReal    ((cv - cv1).norm(), (treal) 0., "cvector::gemv", os, __LINE__, dPessimisticSp);

        cv = cv1;
        cv1.gbmv (false, scbm, cr1, cv2, cr2);

        cv = scbm * cv2 * cr1 + cr2 * cv;
        CheckReal    ((cv.normalize() - cv1.normalize()).norm(), (treal) 0., "cvector::gemv", os, __LINE__, dPessimisticSp);

        {
            treal a[] = {1., 2., 3., -4., 5., -6.};
            const rvector vr(a, 6);
            const cvector vc((std::complex<treal>*) a, 3);
            CheckReal    (vr.norm1(), (treal) 21., "rvector::norm1", os, __LINE__);
            CheckReal    (vc.norm1(), (treal) 15.04631765340644, "cvector::norm1", os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6.};
            const rmatrix m1(a, 2, 3);
            rmatrix m2(2, 3);
            rmatrix m(2, 3);
            m2.set(1.);

            CheckReal    (m.sum(m1, m2)(CVM0+1,CVM0+1), (treal) 5., "rmatrix::sum", os, __LINE__);
            CheckReal    (m.sum(m, m2)(CVM0,CVM0+2), (treal) 7., "rmatrix::sum", os, __LINE__);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6.};
            const rmatrix m1(a, 2, 3);
            rmatrix m2(2, 3);
            rmatrix m(2, 3);
            m2.set(1.);

            CheckReal    (m.diff(m1, m2)(CVM0+1,CVM0+1), (treal) 3., "rmatrix::sum", os, __LINE__);
            CheckReal    (m.diff(m, m2)(CVM0,CVM0+2), (treal) 3., "rmatrix::sum", os, __LINE__);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6.,
                        7., 8., 9., 10., 11., 12.};
            const cmatrix ma ((std::complex<treal>*) a, 2, 3);
            cmatrix mb (2, 3);
            cmatrix m (2, 3);
            mb.set (std::complex<treal>(1.,1.));

            CheckComplex (m.sum(ma, mb)(CVM0+1,CVM0+1), tcomplex (8., 9.), "cmatrix::sum" , os, __LINE__);
            CheckComplex (m.sum(m, mb)(CVM0,CVM0+2), tcomplex (11., 12.), "cmatrix::sum" , os, __LINE__);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6.,
                        7., 8., 9., 10., 11., 12.};
            const cmatrix ma ((std::complex<treal>*) a, 2, 3);
            cmatrix mb (2, 3);
            cmatrix m (2, 3);
            mb.set (std::complex<treal>(1.,1.));

            CheckComplex (m.diff(ma, mb)(CVM0+1,CVM0+1), tcomplex (6., 7.), "cmatrix::diff" , os, __LINE__);
            CheckComplex (m.diff(m, mb)(CVM0,CVM0+2), tcomplex (7., 8.), "cmatrix::diff" , os, __LINE__);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6.};
            const srbmatrix m1(a,3,1,0);
            srbmatrix m2(3,1,0);
            srbmatrix m(3,1,0);
            m2.set(1.);
            CheckReal    (m.sum(m1, m2)(CVM0+1,CVM0+1), (treal) 4., "srbmatrix::sum", os, __LINE__);
            CheckReal    (m(CVM0+1,CVM0+2), (treal) 0., "srbmatrix::sum", os, __LINE__);
            CheckReal    (m(CVM0+1,CVM0), (treal) 3., "srbmatrix::sum", os, __LINE__);
            CheckReal    (m.sum(m, m2)(CVM0+1,CVM0), (treal) 4., "srbmatrix::sum", os, __LINE__);
            CheckReal    (m(CVM0+2,CVM0+2), (treal) 7., "srbmatrix::sum", os, __LINE__);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8.,
                        9., 10., 11., 12.};
            const scbmatrix m1((std::complex<treal>*)a,3,1,0);
            scbmatrix m2(3,1,0);
            scbmatrix m(3,1,0);
            m2.set(std::complex<treal>(1.,1.));
            CheckComplex (m.sum(m1, m2)(CVM0+1,CVM0), tcomplex (4., 5.), "scbmatrix::sum" , os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex (10., 11.), "scbmatrix::sum" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0+2), tcomplex (0., 0.), "scbmatrix::sum" , os, __LINE__);

            CheckComplex (m.sum(m, m2)(CVM0+1,CVM0), tcomplex (5., 6.), "scbmatrix::sum" , os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex (11., 12.), "scbmatrix::sum" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0+2), tcomplex (0., 0.), "scbmatrix::sum" , os, __LINE__);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8.,
                        9., 10., 11., 12.};
            const scbmatrix m1((std::complex<treal>*)a,3,1,0);
            scbmatrix m2(3,1,0);
            scbmatrix m(3,1,0);
            m2.set(std::complex<treal>(1.,1.));
            CheckComplex (m.diff(m1, m2)(CVM0+1,CVM0), tcomplex (2., 3.), "scbmatrix::diff" , os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex (8., 9.), "scbmatrix::diff" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0+2), tcomplex (0., 0.), "scbmatrix::diff" , os, __LINE__);
            CheckComplex (m.diff(m, m2)(CVM0+1,CVM0), tcomplex (1., 2.), "scbmatrix::diff" , os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex (7., 8.), "scbmatrix::diff" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0+2), tcomplex (0., 0.), "scbmatrix::diff" , os, __LINE__);
        }
        {
            treal a[] = {1., 2., 3., 2., 5., 6., 3., 6., 9.};
            const srsmatrix m1(a, 3);
            srsmatrix m2(3);
            srsmatrix m(3);
            m2.set(1.);

            CheckReal    (m.sum(m1, m2)(CVM0+1,CVM0+1), (treal) 6., "srsmatrix::sum", os, __LINE__);
            CheckReal    (m(CVM0,CVM0+2), (treal) 4., "srsmatrix::sum", os, __LINE__);
            CheckReal    (m(CVM0+2,CVM0), (treal) 4., "srsmatrix::sum", os, __LINE__);
            CheckReal    (m.sum(m, m2)(CVM0+1,CVM0+1), (treal) 7., "srsmatrix::sum", os, __LINE__);
            CheckReal    (m(CVM0,CVM0+2), (treal) 5., "srsmatrix::sum", os, __LINE__);
            CheckReal    (m(CVM0+2,CVM0), (treal) 5., "srsmatrix::sum", os, __LINE__);
        }
        {
            treal a[] = {1., 2., 3., 2., 5., 6., 3., 6., 9.};
            const srsmatrix m1(a, 3);
            srsmatrix m2(3);
            srsmatrix m(3);
            m2.set(1.);

            CheckReal    (m.diff(m1, m2)(CVM0+1,CVM0+1), (treal) 4., "srsmatrix::diff", os, __LINE__);
            CheckReal    (m(CVM0,CVM0+2), (treal) 2., "srsmatrix::diff", os, __LINE__);
            CheckReal    (m(CVM0+2,CVM0), (treal) 2., "srsmatrix::diff", os, __LINE__);
            CheckReal    (m.diff(m, m2)(CVM0+1,CVM0+1), (treal) 3., "srsmatrix::diff", os, __LINE__);
            CheckReal    (m(CVM0,CVM0+2), (treal) 1., "srsmatrix::diff", os, __LINE__);
            CheckReal    (m(CVM0+2,CVM0), (treal) 1., "srsmatrix::diff", os, __LINE__);
        }
        {
            treal a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
                        0., 3., -1., -2., 0., -3., 3., 0.};
            treal b[] = {1., 0., 1., 1., 1., 1., 1., -1., 1., 0.,
                        1., 1., 1., -1., 1., -1., 1., 0.};
            schmatrix m1((std::complex<treal>*)a,3);
            schmatrix m2((std::complex<treal>*)b,3);
            schmatrix m(3);
            CheckComplex (m.sum(m1, m2)(CVM0+1,CVM0), tcomplex (3., 2.), "schmatrix::sum" , os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex (4., 0.), "schmatrix::sum" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0+2), tcomplex (1., -4.), "schmatrix::sum" , os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+1), tcomplex (1., 4.), "schmatrix::sum" , os, __LINE__);
        }
        {
            treal a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
                        0., 3., -1., -2., 0., -3., 3., 0.};
            treal b[] = {1., 0., 1., 1., 1., 1., 1., -1., 1., 0.,
                        1., 1., 1., -1., 1., -1., 1., 0.};
            schmatrix m1((std::complex<treal>*)a,3);
            schmatrix m2((std::complex<treal>*)b,3);
            schmatrix m(3);
            CheckComplex (m.diff(m1, m2)(CVM0+1,CVM0), tcomplex (1., 0.), "schmatrix::diff" , os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex (2., 0.), "schmatrix::diff" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0+2), tcomplex (-1., -2.), "schmatrix::diff" , os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+1), tcomplex (-1., 2.), "schmatrix::diff" , os, __LINE__);
        }

        {
            treal a[] = {3., 0., 2., 1., -1., 2., 2., -1., 3., 0.,
                        0., 3., -1., -2., 0., -3., 5., 0.};
            const schmatrix m((std::complex<treal>*)a,3);
            scmatrix h = m.cholesky();
            CheckReal ((~h * h - m).norm(), (treal) 0., "schmatrix::cholesky" , os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
                        0., 3., -1., -2., 0., -3., 3., 0.};
            schmatrix m((std::complex<treal>*)a,3);
            scmatrix me(3);
            rvector v(3);

            v = m.eig(me);
            cvector vc(v);

            CheckReal ((m * me(CVM0) - me(CVM0) * vc(CVM0)).norm(), (treal) 0., "schmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckReal ((m * me(CVM0+1) - me(CVM0+1) * vc(CVM0+1)).norm(), (treal) 0., "schmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckReal ((m * me(CVM0+2) - me(CVM0+2) * vc(CVM0+2)).norm(), (treal) 0., "schmatrix::eig" , os, __LINE__, dPessimisticSp);

            CheckComplex (me(CVM0) % me(CVM0+1), tcomplex (0., 0.), "schmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckComplex (me(CVM0+1) % me(CVM0+2), tcomplex (0., 0.), "schmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckComplex (me(CVM0) % me(CVM0+2), tcomplex (0., 0.), "schmatrix::eig" , os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
                        0., 3., -1., -2., 0., -3., 3., 0.};
            schmatrix m((std::complex<treal>*)a,3);
            treal re[]={2.2,1.3,1.1,-0.9,0.2,-0.45,45.,-30.,10.,3.,1.13};
            const rvector vr(re, 11);
            schmatrix mp(3);

            mp.polynom (m, vr);

            CheckComplex (mp(CVM0,CVM0), tcomplex (1.231954875800000e+008, 0.), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+1,CVM0), tcomplex (1.417932391600000e+008, 7.089661958000000e+007), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+2,CVM0), tcomplex (-8.080273845999999e+007, 1.616054769200000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0,CVM0+1), tcomplex (1.417932391600000e+008, -7.089661958000000e+007), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+1,CVM0+1), tcomplex (2.039982260400000e+008, 0.), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+2,CVM0+1), tcomplex (0., 2.325020965000000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0,CVM0+2), tcomplex (-8.080273845999999e+007, -1.616054769200000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+1,CVM0+2), tcomplex (0., -2.325020965000000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+2,CVM0+2), tcomplex (2.649887267400000e+008, 0.), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
/*
 Column 1

     1.231954875800000e+008
     1.417932391600000e+008 +7.089661958000000e+007i
    -8.080273845999999e+007 +1.616054769200000e+008i

  Column 2

     1.417932391600000e+008 -7.089661958000000e+007i
     2.039982260400000e+008
                          0 +2.325020965000000e+008i

  Column 3

    -8.080273845999999e+007 -1.616054769200000e+008i
                          0 -2.325020965000000e+008i
     2.649887267400000e+008
*/
        }

        {
            treal a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
                        0., 3., -1., -2., 0., -3., 3., 0.};
            schmatrix m((std::complex<treal>*)a,3);
            treal re[]={2.2,1.3,1.1,-0.9,0.2,-0.45,45.,-30.,10.,3.,1.13};
            treal im[]={0.5,-2,0,1,3,-3.,30.,0.,-9.,0.,1.};

            const cvector vc(re, im, 11);
            scmatrix mp(3);

            mp.polynom (m, vc);

            CheckComplex (mp(CVM0,CVM0), tcomplex (1.231954875800000e+008, 6.128500650000000e+007), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+1,CVM0), tcomplex (1.065249031600000e+008, 1.414332915800000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+2,CVM0), tcomplex (-1.611952344600000e+008, 1.214092289200000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0,CVM0+1), tcomplex (1.770615751600000e+008, -3.599475799999982e+005), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+1,CVM0+1), tcomplex (2.039982260400000e+008, 1.014812545000000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+2,CVM0+1), tcomplex (-1.156608320000000e+008, 2.325020965000000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0,CVM0+2), tcomplex (-4.102424600000009e+005, -2.018017249200000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+1,CVM0+2), tcomplex (1.156608320000000e+008, -2.325020965000000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckComplex (mp(CVM0+2,CVM0+2), tcomplex (2.649887267400000e+008, 1.318216785000000e+008), "schmatrix::polynom" , os, __LINE__, dPessimisticSp);
/*
  Column 1

     1.231954875800000e+008 +6.128500650000000e+007i
     1.065249031600000e+008 +1.414332915800000e+008i
    -1.611952344600000e+008 +1.214092289200000e+008i

  Column 2

     1.770615751600000e+008 -3.599475799999982e+005i
     2.039982260400000e+008 +1.014812545000000e+008i
    -1.156608320000000e+008 +2.325020965000000e+008i

  Column 3

    -4.102424600000009e+005 -2.018017249200000e+008i
     1.156608320000000e+008 -2.325020965000000e+008i
     2.649887267400000e+008 +1.318216785000000e+008i
*/

        }
        {
            treal a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
                        0., 3., -1., -2., 0., -3., 3., 0.};
            schmatrix m((std::complex<treal>*)a,3);
            schmatrix me(3);
            me.exp(m);

            CheckComplex (me(CVM0,CVM0), tcomplex (2.673228708371998e+002, 0.), "schmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckComplex (me(CVM0+1,CVM0), tcomplex (3.071187567026802e+002, 1.535593783513401e+002), "schmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckComplex (me(CVM0+2,CVM0), tcomplex (-1.749365628720764e+002, 3.498731257441527e+002), "schmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckComplex (me(CVM0,CVM0+1), tcomplex (3.071187567026802e+002, -1.535593783513401e+002), "schmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckComplex (me(CVM0+1,CVM0+1), tcomplex (4.422594337092769e+002, 0.), "schmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckComplex (me(CVM0+2,CVM0+1), tcomplex (3.549798266275454e-015, 5.034325040954932e+002), "schmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckComplex (me(CVM0,CVM0+2), tcomplex (-1.749365628720763e+002, -3.498731257441526e+002), "schmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckComplex (me(CVM0+1,CVM0+2), tcomplex (-1.776065298147746e-014, -5.034325040954931e+002), "schmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckComplex (me(CVM0+2,CVM0+2), tcomplex (5.744416275398801e+002, 0.), "schmatrix::exp" , os, __LINE__, dPessimisticSp);

/*
            Column 1

                2.673228708371998e+002 -7.105427357601002e-015i
                3.071187567026802e+002 +1.535593783513401e+002i
                -1.749365628720764e+002 +3.498731257441527e+002i

            Column 2

                3.071187567026802e+002 -1.535593783513401e+002i
                4.422594337092769e+002 -5.489286670342458e-016i
                3.549798266275454e-015 +5.034325040954932e+002i

            Column 3

                -1.749365628720763e+002 -3.498731257441526e+002i
                -1.776065298147746e-014 -5.034325040954931e+002i
                5.744416275398801e+002 -2.096383162906490e-014i
*/
        }
        {
            treal a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
                        0., 3., -1., -2., 0., -3., 3., 0.};
            schmatrix m((std::complex<treal>*)a,3);
            const schmatrix mi = m.inv();
            CheckReal ((mi * m - eye_complex(3)).norm(), (treal) 0., "schmatrix::inv" , os, __LINE__, dPessimisticSp);
        }
        {
            const treal a[] = {1., -1., 2., 2., 3., -3.};
            const cvector v((std::complex<treal>*)a, 3);
            const treal alpha = 2.12;
            const treal beta = -3.07;
            schmatrix mh(3), mh2(3);
            mh.randomize_real(-1.,2.);
            mh.randomize_imag(-2.,3.);
            mh2 = mh;
            mh.herk (alpha, v, beta);
            mh2 = alpha * schmatrix(v.rank1update_c(v)) + beta * mh2;
            CheckReal ((mh - mh2).norm(), (treal) 0., "schmatrix::herk" , os, __LINE__, dPessimisticSp);
        }
        {
            cmatrix m(3,3);
            const treal alpha = 2.12;
            const treal beta = -3.07;
            schmatrix mh(3), mh2(3);
            m.randomize_real(-1.,2.);
            m.randomize_imag(-2.,3.);
            mh.randomize_real(-1.,2.);
            mh.randomize_imag(-2.,3.);
            mh2 = mh;
            mh.herk (false, alpha, m, beta);
            mh2 = alpha * schmatrix (m * ~m, 1.e-14) + beta * mh2;
            CheckReal ((mh - mh2).norm(), (treal) 0., "schmatrix::herk" , os, __LINE__, dPessimisticSp);
        }
        {
            cmatrix m(3,3);
            const treal alpha = 2.12;
            const treal beta = -3.07;
            schmatrix mh(3), mh2(3);
            m.randomize_real(-1.,2.);
            m.randomize_imag(-2.,3.);
            mh.randomize_real(-1.,2.);
            mh.randomize_imag(-2.,3.);
            mh2 = mh;
            mh.herk (true, alpha, m, beta);
            mh2 = alpha * schmatrix (~m * m, 1.e-14) + beta * mh2;
            CheckReal ((mh - mh2).norm(), (treal) 0., "schmatrix::herk" , os, __LINE__, dPessimisticSp);
        }


        {
            const tcomplex alpha(2.12,-0.14);
            const tcomplex alphac(2.12,0.14);
            const treal beta = -3.07;
            cvector v1(3), v2(3);
            schmatrix mh(3), mh2(3);
            v1.randomize_real(-1.,2.);
            v1.randomize_imag(-2.,3.);
            v2.randomize_real(-1.,2.);
            v2.randomize_imag(-2.,3.);
            mh.randomize_real(-1.,2.);
            mh.randomize_imag(-2.,3.);
            mh2 = mh;
            mh.her2k (alpha, v1, v2, beta);
#ifdef CVM_FLOAT
            mh2 = schmatrix(alpha * v1.rank1update_c(v2) + alphac * v2.rank1update_c(v1), 1.5e-5) + beta * mh2;
#else
            mh2 = schmatrix(alpha * v1.rank1update_c(v2) + alphac * v2.rank1update_c(v1), 1.5e-14) + beta * mh2;
#endif
            CheckReal ((mh - mh2).norm(), (treal) 0., "schmatrix::her2k" , os, __LINE__, dPessimisticSp);
        }
        {
            const tcomplex alpha(2.12,-0.14);
            const tcomplex alphac(2.12,0.14);
            const treal beta = -3.07;
            cmatrix m1(3,3), m2(3,3);
            schmatrix mh(3), mh2(3);
            m1.randomize_real(-1.,2.);
            m1.randomize_imag(-2.,3.);
            m2.randomize_real(-1.,2.);
            m2.randomize_imag(-2.,3.);
            mh.randomize_real(-1.,2.);
            mh.randomize_imag(-2.,3.);
            mh2 = mh;
            mh.her2k (false, alpha, m1, m2, beta);
#ifdef CVM_FLOAT
            mh2 = schmatrix(alpha * m1 * ~m2 + alphac * m2 * ~m1, 1.5e-5) + beta * mh2;
#else
            mh2 = schmatrix(alpha * m1 * ~m2 + alphac * m2 * ~m1, 1.5e-14) + beta * mh2;
#endif
            CheckReal ((mh - mh2).norm(), (treal) 0., "schmatrix::her2k" , os, __LINE__, dPessimisticSp);
        }
        {
            const tcomplex alpha(2.12,-0.14);
            const tcomplex alphac(2.12,0.14);
            const treal beta = -3.07;
            cmatrix m1(3,3), m2(3,3);
            schmatrix mh(3), mh2(3);
            m1.randomize_real(-1.,2.);
            m1.randomize_imag(-2.,3.);
            m2.randomize_real(-1.,2.);
            m2.randomize_imag(-2.,3.);
            mh.randomize_real(-1.,2.);
            mh.randomize_imag(-2.,3.);
            mh2 = mh;
            mh.her2k (true, alpha, m1, m2, beta);
#ifdef CVM_FLOAT
            mh2 = schmatrix(alpha * ~m1 * m2 + alphac * ~m2 * m1, 1.5e-5) + beta * mh2;
#else
            mh2 = schmatrix(alpha * ~m1 * m2 + alphac * ~m2 * m1, 1.5e-14) + beta * mh2;
#endif
            CheckReal((mh - mh2).norm(), (treal) 0., "schmatrix::her2k", os, __LINE__, dPessimisticSp);
        }




        {
            treal a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
                        0., 3., -1., -2., 0., -3., 3., 0.};
            schmatrix m((std::complex<treal>*)a,3);
            rvector v(3);
            v.set(7.7);
            m.set_main_diag(v);
            CheckComplex (m(CVM0,CVM0), tcomplex ((treal) 7.7, (treal) 0.), "schmatrix::set_main_diag" , os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex ((treal) 7.7, (treal) 0.), "schmatrix::set_main_diag" , os, __LINE__);
            CheckComplex (m(CVM0,CVM0+1), tcomplex ((treal) 2., (treal) -1.), "schmatrix::set_main_diag" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0), tcomplex ((treal) 2., (treal) 1.), "schmatrix::set_main_diag" , os, __LINE__);
        }
        {
            treal a[] = {1., 0., 2., 1., -1., 2., 2., -1., 2., 0.,
                        0., 3., -1., -2., 0., -3., 3., 0.};
            schmatrix m((std::complex<treal>*)a,3);
            cvector v(2);
            v.set(std::complex<treal>(7.,7.));
            m.set_diag(1, v);
            CheckComplex (m(CVM0,CVM0+1), tcomplex ((treal) 7., (treal) 7.), "schmatrix::set_diag" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0), tcomplex ((treal) 7., (treal) -7.), "schmatrix::set_diag" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0+1), tcomplex ((treal) 2., (treal) 0.), "schmatrix::set_diag" , os, __LINE__);
        }
        {
            treal a[] = {1., 2., 1., 2., 5., -1., 1., -1., 20.};
            const srsmatrix m(a, 3);
            srmatrix h = m.cholesky();
            CheckReal ((~h * h - m).norm(), (treal) 0., "srsmatrix::cholesky" , os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 2., 1., 2., 0., -1., 1., -1., 2.};
            const srsmatrix m(a, 3);
            srmatrix me(3);
            rvector v(3);

            v = m.eig(me);

            CheckReal ((m * me(CVM0) - me(CVM0) * v(CVM0)).norm(), (treal) 0., "srsmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckReal ((m * me(CVM0+1) - me(CVM0+1) * v(CVM0+1)).norm(), (treal) 0., "srsmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckReal ((m * me(CVM0+2) - me(CVM0+2) * v(CVM0+2)).norm(), (treal) 0., "srsmatrix::eig" , os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 2., 1., 2., 0., -1., 1., -1., 2.};
            treal av[] = {2.2, 1.3, 1.1, -0.9, 0.2,
                        -0.45, 45, -30, 10, 3, 3.2};
            const rvector v(av, 11);
            const srsmatrix m(a, 3);

            const srsmatrix mp = m.polynom (v);
            CheckReal (mp(CVM0,CVM0), (treal) 6.212740000000001e+004, "srsmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckReal (mp(CVM0+1,CVM0), (treal) 2.399800000000000e+004, "srsmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckReal (mp(CVM0+2,CVM0), (treal) 3.410055000000000e+004, "srsmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckReal (mp(CVM0,CVM0+1), (treal) 2.399800000000000e+004, "srsmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckReal (mp(CVM0+1,CVM0+1), (treal) 2.802685000000000e+004, "srsmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckReal (mp(CVM0+2,CVM0+1), (treal) 1.010255000000000e+004, "srsmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckReal (mp(CVM0,CVM0+2), (treal) 3.410055000000000e+004, "srsmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckReal (mp(CVM0+1,CVM0+2), (treal) 1.010255000000000e+004, "srsmatrix::polynom" , os, __LINE__, dPessimisticSp);
            CheckReal (mp(CVM0+2,CVM0+2), (treal) 5.202485000000000e+004, "srsmatrix::polynom" , os, __LINE__, dPessimisticSp);

/*
            Columns 1 through 2

                6.212740000000001e+004    2.399800000000000e+004
                2.399800000000000e+004    2.802685000000000e+004
                3.410055000000000e+004    1.010255000000000e+004

            Column 3

                3.410055000000000e+004
                1.010255000000000e+004
                5.202485000000000e+004
*/
        }
        {
            treal a[] = {1., 2., 1., 2., 0., -1., 1., -1., 2.};
            const srsmatrix m(a, 3);
            const srsmatrix me = m.exp();

            CheckReal (me(CVM0,CVM0), (treal) 9.198262499129212e+000, "srsmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckReal (me(CVM0+1,CVM0), (treal) 5.558586002658865e+000, "srsmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckReal (me(CVM0+2,CVM0), (treal) 3.852443363622600e+000, "srsmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckReal (me(CVM0,CVM0+1), (treal) 5.558586002658862e+000, "srsmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckReal (me(CVM0+1,CVM0+1), (treal) 5.345819135506588e+000, "srsmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckReal (me(CVM0+2,CVM0+1), (treal) -1.706142639036258e+000, "srsmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckReal (me(CVM0,CVM0+2), (treal) 3.852443363622601e+000, "srsmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckReal (me(CVM0+1,CVM0+2), (treal) -1.706142639036260e+000, "srsmatrix::exp" , os, __LINE__, dPessimisticSp);
            CheckReal (me(CVM0+2,CVM0+2), (treal) 1.090440513816545e+001, "srsmatrix::exp" , os, __LINE__, dPessimisticSp);
/*
            Columns 1 through 2

                9.198262499129212e+000    5.558586002658862e+000
                5.558586002658865e+000    5.345819135506588e+000
                3.852443363622600e+000   -1.706142639036258e+000

            Column 3

                3.852443363622601e+000
               -1.706142639036260e+000
                1.090440513816545e+001
*/
        }

        {
            treal a[] = {1., 2., 3., 4.};
            rvector v(a,4);
            srsmatrix ms(4);
            ms.set(1.);
            ms.syrk (2., v, 1.);
            CheckReal (ms(CVM0+3,CVM0+3), (treal) 33., "srsmatrix::syrk" , os, __LINE__, dPessimisticSp);
            CheckReal (ms(CVM0,CVM0+3), (treal) 9., "srsmatrix::syrk" , os, __LINE__, dPessimisticSp);

            rmatrix m(4,2);
            m(CVM0) = v;
            m(CVM0+1).set(1.);
            ms.syrk (false, 2., m, 0.);
            CheckReal (ms(CVM0+3,CVM0+3), (treal) 34., "srsmatrix::syrk" , os, __LINE__, dPessimisticSp);
            CheckReal (ms(CVM0,CVM0+3), (treal) 10., "srsmatrix::syrk" , os, __LINE__, dPessimisticSp);

            srsmatrix ms2(2);
            ms2.syrk (true, 1., m, 0.);
            CheckReal (ms2(CVM0,CVM0), (treal) 30., "srsmatrix::syrk" , os, __LINE__, dPessimisticSp);
            CheckReal (ms2(CVM0,CVM0+1), (treal) 10., "srsmatrix::syrk" , os, __LINE__, dPessimisticSp);
            CheckReal (ms2(CVM0+1,CVM0+1), (treal) 4., "srsmatrix::syrk" , os, __LINE__, dPessimisticSp);
        }
        {
            const treal alpha = 2.12;
            const treal beta = -3.07;
            rvector v(3);
            srsmatrix ms(3), ms2(3);
            v.randomize(-3.,2.);
            ms.randomize(-1.,2.);
            ms2 = ms;
            ms.syrk (alpha, v, beta);
            ms2 = alpha * srsmatrix(v.rank1update(v)) + beta * ms2;
            CheckReal ((ms - ms2).norm(), (treal) 0., "srsmatrix::syrk" , os, __LINE__, dPessimisticSp);
        }
        {
            const treal alpha = 2.12;
            const treal beta = -3.07;
            rmatrix m(3,3);
            srsmatrix ms(3), ms2(3);
            m.randomize(-1.,2.);
            ms.randomize(-1.,2.);
            ms2 = ms;
            ms.syrk (false, alpha, m, beta);
            ms2 = alpha * srsmatrix (m * ~m) + beta * ms2;
            CheckReal ((ms - ms2).norm(), (treal) 0., "srsmatrix::syrk" , os, __LINE__, dPessimisticSp);
        }
        {
            const treal alpha = 2.12;
            const treal beta = -3.07;
            rmatrix m(3,3);
            srsmatrix ms(3), ms2(3);
            m.randomize(-1.,2.);
            ms.randomize(-1.,2.);
            ms2 = ms;
            ms.syrk (true, alpha, m, beta);
            ms2 = alpha * srsmatrix (~m * m) + beta * ms2;
            CheckReal ((ms - ms2).norm(), (treal) 0., "srsmatrix::syrk" , os, __LINE__, dPessimisticSp);
        }

        {
            treal a1[] = {1., 2., 3., 4.};
            treal a2[] = {1., 2., 3., 4.};
            rvector v1(a1,4);
            rvector v2(a2,4);
            srsmatrix ms(4);
            ms.set(1.);
            ms.syr2k (2., v1, v2, 1.);
            CheckReal (ms(CVM0+3,CVM0+3), (treal) 65., "srsmatrix::syr2k" , os, __LINE__, dPessimisticSp);
            CheckReal (ms(CVM0,CVM0+3), (treal) 17., "srsmatrix::syr2k" , os, __LINE__, dPessimisticSp);

            rmatrix m1(4,2);
            rmatrix m2(4,2);
            m1.set(1.);
            m2.set(2.);
            ms.syr2k (false, 2., m1, m2, 0.);
            CheckReal (ms(CVM0+3,CVM0+3), (treal) 16., "srsmatrix::syr2k" , os, __LINE__, dPessimisticSp);
            CheckReal (ms(CVM0,CVM0+3), (treal) 16., "srsmatrix::syr2k" , os, __LINE__, dPessimisticSp);

            srsmatrix ms2(2);
            ms2.syr2k (true, 1., m1, m2, 0.);
            CheckReal (ms2(CVM0+1,CVM0+1), (treal) 16., "srsmatrix::syr2k" , os, __LINE__, dPessimisticSp);
            CheckReal (ms2(CVM0,CVM0+1), (treal) 16., "srsmatrix::syr2k" , os, __LINE__, dPessimisticSp);
        }

        {
            const treal alpha = 2.12;
            const treal beta = -3.07;
            rvector v1(4);
            rvector v2(4);
            srsmatrix ms(4), ms2(4);
            v1.randomize(-1.,3.);
            v2.randomize(-1.,3.);
            ms.randomize(-1.,3.);
            ms2 = ms;
            ms.syr2k (alpha, v1, v2, beta);
            ms2 = alpha * srsmatrix(v1.rank1update(v2) + v2.rank1update(v1)) + beta * ms2;
            CheckReal ((ms - ms2).norm(), (treal) 0., "srsmatrix::syr2k" , os, __LINE__, dPessimisticSp);
        }
        {
            const treal alpha = 2.12;
            const treal beta = -3.07;
            rmatrix m1(3,3), m2(3,3);
            srsmatrix ms(3), ms2(3);
            m1.randomize(-2.,2.);
            m2.randomize(-2.,2.);
            ms.randomize(-1.,2.);
            ms2 = ms;
            ms.syr2k (false, alpha, m1, m2, beta);
            ms2 = alpha * srsmatrix (m1 * ~m2 + m2 * ~m1) + beta * ms2;
            CheckReal ((ms - ms2).norm(), (treal) 0., "srsmatrix::syr2k" , os, __LINE__, dPessimisticSp);
        }
        {
            const treal alpha = 2.12;
            const treal beta = -3.07;
            rmatrix m1(3,3), m2(3,3);
            srsmatrix ms(3), ms2(3);
            m1.randomize(-2.,2.);
            m2.randomize(-2.,2.);
            ms.randomize(-1.,2.);
            ms2 = ms;
            ms.syr2k (true, alpha, m1, m2, beta);
            ms2 = alpha * srsmatrix (~m1 * m2 + ~m2 * m1) + beta * ms2;
            CheckReal ((ms - ms2).norm(), (treal) 0., "srsmatrix::syr2k" , os, __LINE__, dPessimisticSp);
        }


        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.,
                        10., 11., 12.};
            scbmatrix ma((std::complex<treal>*)a,3,1,0);
            scbmatrix mLU(3,1,0);
            cmatrix  mb1(3,2); cvector vb1(3);
            cmatrix  mb2(3,2); cvector vb2(3);
            cmatrix  mx1(3,2); cvector vx1(3);
            cmatrix  mx2(3,2); cvector vx2(3);
            iarray   nPivots(3);
            treal   dErr = 0.;
            mb1.randomize_real(-1.,3.); mb1.randomize_imag(1.,5.);
            mb2.randomize_real(-2.,5.); mb2.randomize_imag(-3.,0.);
            vb1.randomize_real(-2.,4.); vb1.randomize_imag(-4.,1.);
            vb2.randomize_real(-3.,1.); vb2.randomize_imag(4.,5.);

            mLU.low_up(ma, nPivots);
            mx1 = ma.solve_lu (mLU, nPivots, mb1, dErr);
            CheckReal (dErr, (treal) 0., "scbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            mx2 = ma.solve_lu (mLU, nPivots, mb2);
            CheckReal ((ma * mx1 - mb1).norm(), (treal) 0., "scbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            CheckReal ((ma * mx2 - mb2).norm(), (treal) 0., "scbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);

            vx1 = ma.solve_lu (mLU, nPivots, vb1, dErr);
            vx2 = ma.solve_lu (mLU, nPivots, vb2);
            CheckReal ((ma * vx1 - vb1).norm(), (treal) 0., "scbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            CheckReal ((ma * vx2 - vb2).norm(), (treal) 0., "scbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12.};
            scbmatrix m((std::complex<treal>*)a,3,1,0);
            m.resize_lu (0,1);
            m.diag(1).set(std::complex<treal>(9.,9.));
            CheckComplex (m(CVM0,CVM0+1), tcomplex ((treal) 9., (treal) 9.), "scbmatrix::resize_lu" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0), tcomplex ((treal) 0., (treal) 0.), "scbmatrix::resize_lu" , os, __LINE__);
            CheckComplex (m(CVM0+2,CVM0+2), tcomplex ((treal) 9., (treal) 10.), "scbmatrix::resize_lu" , os, __LINE__);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12.};
            scbmatrix m ((std::complex<treal>*)a,3,1,0);
            CheckReal (m.real()(CVM0+1,CVM0+1), (treal) 5., "scbmatrix::real" , os, __LINE__, dPessimisticSp);
            CheckReal (m.imag()(CVM0+1,CVM0+1), (treal) 6., "scbmatrix::imag" , os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8.};
            const srbmatrix m(a,4,1,0);
            scbmatrix mr(m), mi(m, false);
            CheckComplex (mr(CVM0+3,CVM0+2), tcomplex ((treal) 6., (treal) 0.), "scbmatrix(srbmatrix,bool)" , os, __LINE__);
            CheckComplex (mr(CVM0,CVM0+3), tcomplex ((treal) 0., (treal) 0.), "scbmatrix(srbmatrix,bool)" , os, __LINE__);
            CheckComplex (mi(CVM0+3,CVM0+2), tcomplex ((treal) 0., (treal) 6.), "scbmatrix(srbmatrix,bool)" , os, __LINE__);
            CheckComplex (mi(CVM0,CVM0+3), tcomplex ((treal) 0., (treal) 0.), "scbmatrix(srbmatrix,bool)" , os, __LINE__);
        }
        {
            srbmatrix mr(4,1,0), mi(4,1,0);
            mr.set(1.);
            mi.set(2.);
            const scbmatrix m(mr,mi);
            CheckComplex (m(CVM0+1,CVM0), tcomplex ((treal) 1., (treal) 2.), "scbmatrix(srbmatrix,srbmatrix)" , os, __LINE__);
            CheckComplex (m(CVM0,CVM0+1), tcomplex ((treal) 0., (treal) 0.), "scbmatrix(srbmatrix,srbmatrix)" , os, __LINE__);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8.};
            srbmatrix ma(a,4,1,0);
            srbmatrix mLU(4,1,0);
            rmatrix  mb1(4,2); rvector vb1(4);
            rmatrix  mb2(4,2); rvector vb2(4);
            rmatrix  mx1(4,2); rvector vx1(4);
            rmatrix  mx2(4,2); rvector vx2(4);
            iarray   nPivots(4);
            treal   dErr = 0.;
            mb1.randomize(-1.,3.); vb1.randomize(-2.,4.);
            mb2.randomize(-2.,5.); vb2.randomize(-3.,1.);

            mLU.low_up(ma, nPivots);
            mx1 = ma.solve_lu (mLU, nPivots, mb1, dErr);
            CheckReal (dErr, (treal) 0., "srbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            mx2 = ma.solve_lu (mLU, nPivots, mb2);
            CheckReal ((ma * mx1 - mb1).norm(), (treal) 0., "srbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            CheckReal ((ma * mx2 - mb2).norm(), (treal) 0., "srbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);

            vx1 = ma.solve_lu (mLU, nPivots, vb1, dErr);
            CheckReal (dErr, (treal) 0., "srbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            vx2 = ma.solve_lu (mLU, nPivots, vb2);
            CheckReal ((ma * vx1 - vb1).norm(), (treal) 0., "srbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            CheckReal ((ma * vx2 - vb2).norm(), (treal) 0., "srbmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., -1., 1., 2., -2., 1., 3., -2., 1.};
            srmatrix ma(a,3);
            srmatrix mLU(3);
            rmatrix  mb1(3,2); rvector vb1(3);
            rmatrix  mb2(3,2); rvector vb2(3);
            rmatrix  mx1(3,2); rvector vx1(3);
            rmatrix  mx2(3,2); rvector vx2(3);
            iarray   nPivots(3);
            treal   dErr = 0.;
            mb1.randomize(-1.,3.); vb1.randomize(-2.,4.);
            mb2.randomize(-2.,5.); vb2.randomize(-3.,1.);

            mLU.low_up(ma, nPivots);
            mx1 = ma.solve_lu (mLU, nPivots, mb1, dErr);
            CheckReal (dErr, (treal) 0., "rmatrix::solve_lu" , os, __LINE__, dPessimisticSp);

            mx2 = ma.solve_lu (mLU, nPivots, mb2);
            CheckReal ((ma * mx1 - mb1).norm(), (treal) 0., "rmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            CheckReal ((ma * mx2 - mb2).norm(), (treal) 0., "rmatrix::solve_lu" , os, __LINE__, dPessimisticSp);

            vx1 = ma.solve_lu (mLU, nPivots, vb1, dErr);
            CheckReal (dErr, (treal) 0., "rmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            vx2 = ma.solve_lu (mLU, nPivots, vb2);
            CheckReal ((ma * vx1 - vb1).norm(), (treal) 0., "rmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            CheckReal ((ma * vx2 - vb2).norm(), (treal) 0., "rmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
        }
        {
            std::complex<treal> alpha = std::complex<treal>(1.3,0.21);
            std::complex<treal> beta = std::complex<treal>(0.5,-0.1);
            cmatrix m1(2,3);
            cmatrix m2(3,2);
            schmatrix ms(2);
            cmatrix m(2,3);
            m.randomize_real(-1., 2.); m.randomize_imag(1., 3.);
            m1.randomize_real(-1., 3.); m1.randomize_imag(1., 2.);
            m2.randomize_real(0., 2.); m2.randomize_imag(-3., -1.);
            ms.randomize_real(-3., 1.); ms.randomize_imag(-1.3, 4.);

            cmatrix mr = ms * m1 * alpha + m * beta;
            CheckReal ((mr - m.hemm (true, ms, m1, alpha, beta)).norm(), (treal) 0., "cmatrix::hemm" , os, __LINE__, dPessimisticSp);

            m.resize(3,2);
            m.randomize_real(-1.4, 1.3); m.randomize_imag(1.1, 3.);
            cmatrix mr2 = m2 * ms * alpha + m * beta;
            CheckReal ((mr2 - m.hemm (false, ms, m2, alpha, beta)).norm(), (treal) 0., "cmatrix::hemm" , os, __LINE__, dPessimisticSp);
        }
        {
            std::complex<treal> alpha = std::complex<treal>(1.1,2.1);
            std::complex<treal> beta = std::complex<treal>(0.71,0.12);
            cmatrix m1(4,3); cmatrix m2(4,3);
            cmatrix m(3,3);
            m.randomize_real(-1., 2.); m1.randomize_real(-1., 3.); m2.randomize_real(0., 2.);
            m.randomize_imag(1., 3.); m1.randomize_imag(-2., 4.); m2.randomize_imag(-3., 2.);
            cmatrix mr = ~m1 * m2 * alpha + m * beta;
            CheckReal ((mr - m.gemm(m1, true, m2, false, alpha, beta)).norm(), (treal) 0., "cmatrix::gemm" , os, __LINE__, dPessimisticSp);
        }
        {
            std::complex<treal> alpha = std::complex<treal>(1.2,4.11);
            cmatrix m(3,2);
            cvector vc(3);
            cvector vr(2);
            m.randomize_real(-1., 2.); vc.randomize_real(-1., 3.); vr.randomize_real(0., 2.);
            m.randomize_imag(-3., 2.); vc.randomize_imag(1., 3.); vr.randomize_imag(-1., 2.);
            cmatrix mr = m + vc.rank1update_u (vr) * alpha;
            CheckReal ((mr - m.geru(alpha, vc, vr)).norm(), (treal) 0., "cmatrix::geru" , os, __LINE__, dPessimisticSp);
        }
        {
            std::complex<treal> alpha = std::complex<treal>(1.2,4.11);
            cmatrix m(3,2);
            cvector vc(3);
            cvector vr(2);
            m.randomize_real(-1., 2.); vc.randomize_real(-1., 3.); vr.randomize_real(0., 2.);
            m.randomize_imag(-3., 2.); vc.randomize_imag(1., 3.); vr.randomize_imag(-1., 2.);

            cmatrix mr = m + vc.rank1update_c (vr) * alpha;
            CheckReal ((mr - m.gerc(alpha, vc, vr)).norm(), (treal) 0., "cmatrix::gerc" , os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6.,
                        7., 8., 9., 10., 11., 12.,
                        13., 14., 15., 16., 17., 18.};
            cmatrix m (2, 3);
            const scmatrix ms((std::complex<treal>*)a, 3);

            m.diag(-1).set(std::complex<treal>(1.,1.));
            m.diag(0).set(std::complex<treal>(2.,2.));
            m.diag(1).set(std::complex<treal>(3.,3.));
            m.diag(2).set(std::complex<treal>(4.,4.));
            CheckComplex (m(CVM0,CVM0+1), tcomplex ((treal) 3., (treal) 3.), "cmatrix::diag" , os, __LINE__);
            CheckComplex (m(CVM0+1,CVM0+2), tcomplex ((treal) 3., (treal) 3.), "cmatrix::diag" , os, __LINE__);
            CheckComplex (ms.diag(0)[CVM0+1], tcomplex ((treal) 9., (treal) 10.), "cmatrix::diag" , os, __LINE__);
        }
        {
            treal alpha = 1.3;
            treal beta = -0.7;
            rmatrix m1(3,4);
            rmatrix m2(4,3);
            srsmatrix ms(3);
            rmatrix m(3,4);
            m.randomize(-1., 2.); m1.randomize(-1., 3.); m2.randomize(0., 2.);
            ms.randomize(-3., 1.);

            rmatrix mr1 = ms * m1 * alpha + m * beta;
            CheckReal ((mr1 - m.symm (true, ms, m1, alpha, beta)).norm(), (treal) 0., "rmatrix::symm" , os, __LINE__, dPessimisticSp);

            m.resize(4,3);
            rmatrix mr2 = m2 * ms * alpha + m * beta;
            CheckReal ((mr2 - m.symm (false, ms, m2, alpha, beta)).norm(), (treal) 0., "rmatrix::symm" , os, __LINE__, dPessimisticSp);
        }
        {
            treal alpha = 1.3;
            rmatrix m(3,4);
            rvector vc(3);
            rvector vr(4);
            m.randomize(-1., 2.); vc.randomize(-1., 3.); vr.randomize(0., 2.);

            rmatrix mr = m + vc.rank1update (vr) * alpha;
            CheckReal ((mr - m.ger(alpha, vc, vr)).norm(), (treal) 0., "rmatrix::ger" , os, __LINE__, dPessimisticSp);
        }
        {
            treal a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9.};
            rmatrix  m(2,3);
            const srmatrix ms(a,3);

            m.diag(-1).set(1.);
            m.diag(0).set(2.);
            m.diag(1).set(3.);
            m.diag(2).set(4.);
            CheckReal (m(CVM0,CVM0+1), (treal) 3., "rmatrix::diag" , os, __LINE__, dPessimisticSp);
            CheckReal (m(CVM0,CVM0+2), (treal) 4., "rmatrix::diag" , os, __LINE__, dPessimisticSp);
            CheckReal (m(CVM0+1,CVM0), (treal) 1., "rmatrix::diag" , os, __LINE__, dPessimisticSp);
            CheckReal (ms.diag(0)(CVM0+1), (treal) 5., "rmatrix::diag" , os, __LINE__, dPessimisticSp);
        }
        {
            std::complex<treal> alpha = std::complex<treal>(1.3,-0.7);
            std::complex<treal> beta  = std::complex<treal>(0.15,-1.09);
            scbmatrix m(3,1,0);
            cvector c(3);
            cvector v(3);
            m.randomize_real(-1., 2.);
            m.randomize_imag(0., 1.);
            v.randomize_real(-1., 3.);
            v.randomize_imag(2., 4.);
            c.randomize_real(0., 2.);
            c.randomize_imag(3., 7.);

            cvector vr1 = m * v * alpha + c * beta;
            CheckReal ((vr1 - c.gbmv(false, m, alpha, v, beta)).norm(), (treal) 0., "cvector::gbmv" , os, __LINE__, dPessimisticSp);
            cvector vr2 = c * m * alpha + v * beta;
            CheckReal ((vr2 - v.gbmv(true, m, alpha, c, beta)).norm(), (treal) 0., "cvector::gbmv" , os, __LINE__, dPessimisticSp);
        }
        {
            std::complex<treal> alpha = std::complex<treal>(1.3,-0.7);
            std::complex<treal> beta  = std::complex<treal>(0.15,-1.09);
            cmatrix m(3,2);
            cvector c(3);
            cvector v(2);
            m.randomize_real(-1., 2.);
            m.randomize_imag(0., 1.);
            v.randomize_real(-1., 3.);
            v.randomize_imag(2., 4.);
            c.randomize_real(0., 2.);
            c.randomize_imag(3., 7.);

            cvector vr1 = m * v * alpha + c * beta;
            CheckReal ((vr1 - c.gemv(false, m, alpha, v, beta)).norm(), (treal) 0., "cvector::gemv" , os, __LINE__, dPessimisticSp);
            cvector vr2 = c * m * alpha + v * beta;
            CheckReal ((vr2 - v.gemv(true, m, alpha, c, beta)).norm(), (treal) 0., "cvector::gemv" , os, __LINE__, dPessimisticSp);
        }
        {
            cvector vc(3);
            vc.set(std::complex<treal>(1.,1.));
            vc.imag()(CVM0) = 7.77;
            CheckComplex (vc[CVM0], tcomplex ((treal) 1., (treal) 7.77), "cvector::imag" , os, __LINE__, dPessimisticSp);
            CheckComplex (vc[CVM0+1], tcomplex ((treal) 1., (treal) 1.), "cvector::imag" , os, __LINE__, dPessimisticSp);
        }
        {
            cvector vc(3);
            vc.set(std::complex<treal>(1.,1.));
            vc.real()(CVM0) = 7.77;
            CheckComplex (vc[CVM0], tcomplex ((treal) 7.77, (treal) 1.), "cvector::real" , os, __LINE__, dPessimisticSp);
            CheckComplex (vc[CVM0+1], tcomplex ((treal) 1., (treal) 1.), "cvector::real" , os, __LINE__, dPessimisticSp);
        }
        {
            cvector v(3);
            v.set_real(1.);
            CheckComplex (v[CVM0+1], tcomplex ((treal) 1., (treal) 0.), "cvector::set_real" , os, __LINE__, dPessimisticSp);
        }
        {
            rvector v(3);
            cvector vc(3);
            v(CVM0) = 1.; v(CVM0+1) = 2.; v(CVM0+2) = 3.;
            vc.assign_imag(v);
            CheckComplex (vc[CVM0+1], tcomplex ((treal) 0., (treal) 2.), "cvector::assign_imag" , os, __LINE__, dPessimisticSp);
        }
        {
            treal alpha = 1.3;
            treal beta = -0.7;
            srbmatrix m(3, 1, 0);
            rvector c(3);
            rvector v(3);
            m.randomize(-1., 2.); v.randomize(-1., 3.); c.randomize(0., 2.);

            rvector vr1 = m * v * alpha + c * beta;
            CheckReal ((vr1 - c.gbmv(false, m, alpha, v, beta)).norm(), (treal) 0., "rvector::gbmv" , os, __LINE__, dPessimisticSp);
            rvector vr2 = c * m * alpha + v * beta;
            CheckReal ((vr2 - v.gbmv(true, m, alpha, c, beta)).norm(), (treal) 0., "rvector::gbmv" , os, __LINE__, dPessimisticSp);
        }
        {
            treal alpha = 1.3;
            treal beta = -0.7;
            rmatrix m(4,3);
            rvector c(4);
            rvector v(3);
            m.randomize(-1., 2.); v.randomize(-1., 3.); c.randomize(0., 2.);

            rvector vr1 = m * v * alpha + c * beta;
            CheckReal ((vr1 - c.gemv(false, m, alpha, v, beta)).norm(), (treal) 0., "rvector::gemv" , os, __LINE__, dPessimisticSp);
            rvector vr2 = c * m * alpha + v * beta;
            CheckReal ((vr2 - v.gemv(true, m, alpha, c, beta)).norm(), (treal) 0., "rvector::gemv" , os, __LINE__, dPessimisticSp);
        }
        {
            srsmatrix m(3);
            srmatrix me(3);
            rvector v(3);
            m.randomize(1., 3.);

            v.eig (m, me);
            CheckReal ((m * me(CVM0) - me(CVM0) * v(CVM0)).norm(), (treal) 0., "srsmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckReal ((m * me(CVM0+1) - me(CVM0+1) * v(CVM0+1)).norm(), (treal) 0., "srsmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckReal ((m * me(CVM0+2) - me(CVM0+2) * v(CVM0+2)).norm(), (treal) 0., "srsmatrix::eig" , os, __LINE__, dPessimisticSp);

            CheckReal (me(CVM0) * me(CVM0+1), (treal) 0., "srsmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckReal (me(CVM0) * me(CVM0+2), (treal) 0., "srsmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckReal (me(CVM0+2) * me(CVM0+1), (treal) 0., "srsmatrix::eig" , os, __LINE__, dPessimisticSp);

            schmatrix mc(3);
            scmatrix mce(3);
            mc.randomize_real(1., 3.);
            mc.randomize_imag(1., 3.);

            v.eig (mc, mce);

            CheckReal ((mc * mce(CVM0) - mce(CVM0) * v(CVM0)).norm(), (treal) 0., "schmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckReal ((mc * mce(CVM0+1) - mce(CVM0+1) * v(CVM0+1)).norm(), (treal) 0., "schmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckReal ((mc * mce(CVM0+2) - mce(CVM0+2) * v(CVM0+2)).norm(), (treal) 0., "schmatrix::eig" , os, __LINE__, dPessimisticSp);

            CheckComplex (mce(CVM0) % mce(CVM0+1), tcomplex ((treal) 0., (treal) 0.), "srsmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckComplex (mce(CVM0) % mce(CVM0+2), tcomplex ((treal) 0., (treal) 0.), "srsmatrix::eig" , os, __LINE__, dPessimisticSp);
            CheckComplex (mce(CVM0+2) % mce(CVM0+1), tcomplex ((treal) 0., (treal) 0.), "srsmatrix::eig" , os, __LINE__, dPessimisticSp);
        }
        {
            treal m[] = {1., -1., 1., 2., -2., 1., 3., -2., 1.};
            treal b1[] = {1., 2., 3.};
            treal b2[] = {0., -1., -2.};
            srmatrix ma(m, 3);
            srmatrix mLU(3);
            rvector  vb1(b1, 3);
            rvector  vb2(b2, 3);
            rvector  vx1(3);
            rvector  vx2(3);
            iarray   nPivots(3);
            treal   dErr = 0.;

            mLU.low_up(ma, nPivots);
            vx1.solve_lu (ma, mLU, nPivots, vb1, dErr);
            CheckReal (dErr, (treal) 0., "rmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            vx2.solve_lu (ma, mLU, nPivots, vb2);
            CheckReal ((ma * vx1 - vb1).norm(), (treal) 0., "rmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            CheckReal ((ma * vx2 - vb2).norm(), (treal) 0., "rmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
        }
        {
            scmatrix ma(3);
            scmatrix mLU(3);
            cmatrix  mb1(3,2);
            cmatrix  mb2(3,2);
            cmatrix  mx1(3,2);
            cmatrix  mx2(3,2);
            iarray   nPivots(3);
            treal   dErr = 0.;
            ma.randomize_real(0.,10.); ma.randomize_imag(0.,10.);
            mb1.randomize_real(0.,10.); mb1.randomize_imag(0.,10.);
            mb2.randomize_real(0.,10.); mb2.randomize_imag(0.,10.);

            mLU.low_up(ma, nPivots);
            mx1.solve_lu (ma, mLU, nPivots, mb1, dErr);
            CheckReal (dErr, (treal) 0., "cmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            mx2.solve_lu (ma, mLU, nPivots, mb2);
            CheckReal ((ma * mx1 - mb1).norm(), (treal) 0., "cmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
            CheckReal ((ma * mx2 - mb2).norm(), (treal) 0., "cmatrix::solve_lu" , os, __LINE__, dPessimisticSp);
        }
        {
            rvector v(5);
            v.set(3.);
            CheckReal (v[CVM0+2], (treal) 3., "rvector::set" , os, __LINE__);
        }
        {
            const treal a[] = {1., 2., 3., 4., 5., 6., 7.,};
            rvector v (5);
            rvector v2 (4);

            v.assign(a);
            CheckReal (v[CVM0+2], (treal) 3., "rvector::assign" , os, __LINE__);
            v2.assign(a, 2);
            CheckReal (v2[CVM0+2], (treal) 5., "rvector::assign" , os, __LINE__);
        }
        {
            rvector v (5, 1.5);
            CheckReal (v[CVM0+2], (treal) 1.5, "rvector (int, treal)" , os, __LINE__);
        }
        {
            rmatrix m (100, 200);
            srmatrix ms (m, 30, 40, 5); // 5x5 submatrix
            CheckInt (ms.ld(), 100, "srmatrix::ld" , os, __LINE__);
        }
        {
            iarray a(5);
            iarray::iterator pos = a.begin() + 2;
            a.insert(pos, 88);
            CheckInt (a[CVM0+2], 88, "iarray::begin, iarray::insert" , os, __LINE__);
            pos = a.begin() + 1;
            a.erase(pos);
            CheckInt (a[CVM0+1], 88, "iarray::begin, iarray::erase" , os, __LINE__);
            CheckInt (a[CVM0+2], 0, "iarray::begin, iarray::erase" , os, __LINE__);
        }
        {
            iarray a(5);
            a.push_back(88);
            CheckInt (a[CVM0+5], 88, "iarray::push_back" , os, __LINE__);
            a.pop_back();
            CheckInt (a[CVM0+4], 0, "iarray::pop_back" , os, __LINE__);
            CheckInt (a.size(), 5, "iarray::pop_back" , os, __LINE__);
        }
        {
            iarray a(5);
            a[CVM0] = 1; a[CVM0+1] = 2; a[CVM0+2] = 3; a[CVM0+3] = 4; a[CVM0+4] = 5;
            CheckInt (a.at(0), 1, "iarray::at" , os, __LINE__);
            CheckInt (a.at(4), 5, "iarray::at" , os, __LINE__);
        }
        {
            iarray a(5);
            a[CVM0] = 1; a[CVM0+1] = 2; a[CVM0+2] = 3; a[CVM0+3] = 4; a[CVM0+4] = 5;

            int val = 5;
            for (iarray::reverse_iterator it = a.rbegin(); it != a.rend(); ++it)
            {
                CheckInt (*it, val, "iarray::reverse_iterator" , os, __LINE__);
                --val;
            }

            CheckInt (a.front(), 1, "iarray::front" , os, __LINE__);
            CheckInt (a.back(), 5, "iarray::back" , os, __LINE__);
        }

        {
            const tint a[] = {1, 2, 3, 4};
            iarray v (a, 3);
            v.resize(2);
            CheckInt (v[CVM0+1], 2, "iarray.resize" , os, __LINE__);
            v.resize(4);
            CheckInt (v[CVM0+3], 0, "iarray.resize" , os, __LINE__);
        }
        {
            iarray a(5);
            a.set(3);
            iarray b(a);
            CheckInt (b[CVM0+3], 3, "iarray copy ctr" , os, __LINE__);
        }
        {
            rvector a(5);
            a.set(3.);
            rvector b(a);
            CheckReal (b[CVM0+3], 3., "rvector copy ctr" , os, __LINE__);
        }

        {
            iarray a(5), b(5);
            a.set(3);
            b = a;
            CheckInt (b[CVM0+3], 3, "iarray assignment" , os, __LINE__);
        }
        {
            iarray a(5);
            a.set(3);
            CheckInt (a[CVM0+3], 3, "iarray.set" , os, __LINE__);
        }
        {
            iarray a;
            a.resize(10);
            CheckInt (a.size(), 10, "iarray.resize" , os, __LINE__);
        }

        {
            tint a[] = {1, 2, 3, 4};
            iarray v (a, 3);
            CheckInt (v[CVM0+1], 2, "iarray (*,size)" , os, __LINE__);
            a[1] = 77;
            CheckInt (v[CVM0+1], 77, "iarray (*,size)" , os, __LINE__);
        }

        {
            const tint a[] = {1, 2, 3, 4};
            const iarray v (a+1, a+3);
            CheckInt (v.size(), 2, "iarray.size()" , os, __LINE__);
            CheckInt (v[CVM0+1], 3, "iarray (*,*)" , os, __LINE__);
        }

        {
            iarray a(10);
            a[CVM0+1] = 1;
            CheckInt (a.get()[1], 1, "iarray.get", os, __LINE__);
        }

        {
            srsmatrix ssm1(4);
            srsmatrix ssm2(4);

            rmatrix m1(4,4);
            rmatrix m2(4,4);
            rmatrix m3(4,4);

            ssm1.set(1.);
            m1.set(1.);

            m2 = ssm1 * m1;
            CheckReal (m2.norminf(),  (treal) 16. ,  "srsmatrix * rmatrix",  os, __LINE__);

            m2 = m1 + ssm1;
            CheckReal (m2(CVM0+2,CVM0+3),  (treal) 2. ,  "srsmatrix + rmatrix",  os, __LINE__);

            m2 = m1 * ssm1;
            CheckReal (m2.norminf(),  (treal) 16. ,  "rmatrix * srsmatrix",  os, __LINE__);
        }




        rvector vs1(5);
        vs1[CVM0] = 1.; vs1[CVM0+1] = 2.; vs1[CVM0+2] = 3.; vs1[CVM0+3] = 4.; vs1[CVM0+4] = 5.;

        rvector::iterator it = vs1.begin() + 1;
        rvector::iterator ite = vs1.erase(it);

        CheckReal (vs1[CVM0],  (treal) 1. ,  "rvector.insert",  os, __LINE__);
        CheckReal (vs1[CVM0+1],  (treal) 3. ,  "rvector.insert",  os, __LINE__);
        CheckReal (vs1[CVM0+2],  (treal) 4. ,  "rvector.insert",  os, __LINE__);

        vs1.insert(ite, 10.);

        CheckReal (vs1[CVM0],  (treal) 1. ,  "rvector.insert",  os, __LINE__);
        CheckReal (vs1[CVM0+1],  (treal) 10. ,  "rvector.insert",  os, __LINE__);
        CheckReal (vs1[CVM0+2],  (treal) 3. ,  "rvector.insert",  os, __LINE__);

        vs1.push_back(9.);
        CheckReal (vs1[CVM0+4],  (treal) 5. ,  "rvector.push_back",  os, __LINE__);
        CheckReal (vs1[CVM0+5],  (treal) 9. ,  "rvector.push_back",  os, __LINE__);
        CheckReal (*std::max_element(vs1.begin(), vs1.end()),  (treal) 10. ,  "rvector.max_element",  os, __LINE__);

        std::sort(vs1.begin(), vs1.end());
        CheckReal (vs1[CVM0+5],  (treal) 10. ,  "std::sort",  os, __LINE__);

        std::reverse(vs1.begin(), vs1.end());
        CheckReal (vs1[CVM0],  (treal) 10. ,  "std::reverse",  os, __LINE__);
        CheckReal (vs1[CVM0+1],  (treal) 9. ,  "std::reverse",  os, __LINE__);
        CheckReal (vs1[CVM0+5],  (treal) 1. ,  "std::reverse",  os, __LINE__);

        {
            // N > 64*M bug fixed
            rmatrix A(600,4);
            srmatrix U(600), V(4);
            const rvector singVal = A.svd(U,V);

            rmatrix singValM (A);
            singValM.set(0.);
            singValM.diag(0) = singVal;

            CheckReal    ((A * ~V - U * singValM).norm(),  (treal) 0.,  "rmatrix svd", os, __LINE__, dPessimisticSp);
            CheckReal    ((~A * U - ~(singValM * V)).norm(),  (treal) 0.,  "rmatrix svd", os, __LINE__, dPessimisticSp);
        }


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