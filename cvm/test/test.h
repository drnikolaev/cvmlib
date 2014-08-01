//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2014
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "StdAfx.h"
#include "../src/cvm.h"
#include "../src/cfun.h"

#if defined (_MSC_VER)
#   pragma warning(disable:4305)
#   if _MSC_VER < 1300
#       pragma warning(disable:4018)
#       pragma warning(disable:4018)
#       pragma warning(disable:4786)
#   endif
#endif

#if defined (_MSC_VER)
    #pragma warning(disable:4700)
#endif


#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>

#include <array>

#if (defined (WIN32) || defined (_WIN32)) && !defined (__CYGWIN__)
#   define CVM_TEST_WIN_THREADS 1
#endif

#ifndef CVM_NO_NAMESPACE
using namespace cvm;
#endif


// multithreading synchronizer
class TestCriticalSection {
private:
    bool mbOK;

#if !defined (CVM_NO_MT)
    #if defined (CVM_TEST_WIN_THREADS)
        ::CRITICAL_SECTION mCriticalSection;
    #else                                                                       // POSIX Threads library assumed
pthread_mutex_t mMutex;
pthread_mutexattr_t mMutexAttr;
    #endif
#endif

public:
    TestCriticalSection ()
    : mbOK(false)
#if !defined (CVM_NO_MT)
    #if defined (CVM_TEST_WIN_THREADS)
    , mCriticalSection()
    #else
    , mMutex(), mMutexAttr()
    #endif
#endif
    {
#if !defined (CVM_NO_MT)
    #if defined (CVM_TEST_WIN_THREADS)
        if (::InitializeCriticalSectionAndSpinCount (&mCriticalSection, 0x80000400))
        {
            mbOK = true;
        }
        else
        {
            ::InitializeCriticalSection (&mCriticalSection);
            mbOK = true;
        }
    #else

        if (pthread_mutexattr_init (&mMutexAttr) != 0)
        {
            std::cout << "FAILED TO pthread_mutexattr_init" << std::endl;
        }

        if (pthread_mutexattr_setpshared (&mMutexAttr, PTHREAD_PROCESS_PRIVATE) != 0)
        {
            std::cout << "FAILED TO pthread_mutexattr_setpshared" << std::endl;
        }

        if (pthread_mutex_init (&mMutex, &mMutexAttr) != 0)
        {
            std::cout << "FAILED TO pthread_mutex_init" << std::endl;
        }
    #endif
#endif
    }

    ~TestCriticalSection ()
    {
#if !defined (CVM_NO_MT)
    #if defined (CVM_TEST_WIN_THREADS)
        if (mbOK)
        {
            ::DeleteCriticalSection (&mCriticalSection);
        }
    #else
        if (pthread_mutexattr_destroy (&mMutexAttr) != 0)
        {
            std::cout << "FAILED TO pthread_mutexattr_destroy" << std::endl;
        }
        if (pthread_mutex_destroy (&mMutex) != 0)
        {
            std::cout << "FAILED TO pthread_mutex_destroy" << std::endl;
        }

    #endif
#endif
        mbOK = false;
    }

    void enter ()
    {
#if !defined (CVM_NO_MT)
    #if defined (CVM_TEST_WIN_THREADS)
        if (mbOK)
        {
            ::EnterCriticalSection (&mCriticalSection);
        }
    #else

        if (pthread_mutex_lock (&mMutex) != 0)
        {
            std::cout << "FAILED TO pthread_mutex_lock" << std::endl;
        }
    #endif
#endif
    }

    void leave ()
    {
#if !defined (CVM_NO_MT)
    #if defined (CVM_TEST_WIN_THREADS)
        if (mbOK)
        {
            ::LeaveCriticalSection (&mCriticalSection);
        }
    #else
        if (pthread_mutex_unlock (&mMutex) != 0)
        {
            std::cout << "FAILED TO pthread_mutex_unlock" << std::endl;
        }
    #endif
#endif
    }
};


template <TestCriticalSection& CS>
class LockIt
{
public:
    LockIt ()
    {
        CS.enter();
    }

    ~LockIt ()
    {
        CS.leave();
    }
};


#if defined (_WIN32) && !defined(__MINGW32__)
    #include <time.h>
    #include <windows.h>
    #if defined(_MSC_VER) || defined(_MSC_EXTENSIONS)
        #define DELTA_EPOCH_IN_MICROSECS  11644473600000000Ui64
    #else
        #define DELTA_EPOCH_IN_MICROSECS  11644473600000000ULL
    #endif

struct timeval {
    time_t      tv_sec;     /* seconds */
    long        tv_usec;    /* microseconds */
};

struct timezone
{
    int tz_minuteswest; /* minutes W of Greenwich */
    int tz_dsttime;     /* type of dst correction */
};

int gettimeofday(struct timeval *tv, struct timezone *tz);

#else
    #include <sys/time.h>
#endif

long long get_usecs();

class test_exception : public std::exception
{
    std::string m_s;
public:
    explicit test_exception (const char* szWhat) : m_s ("TEST FAILED: ") {m_s += szWhat;}
    virtual ~test_exception () throw () {}
    virtual const char* what () const throw ()
    {
        return m_s.c_str();
    }
};


#if !defined(__INTEL_COMPILER) || !defined (__GNUC__)
tcomplex conj (tcomplex c);
#endif

treal mod (tcomplex c);

void Report (const char* szMsg, std::ostream& os, int line) throw (test_exception);
void CheckBool (bool b, bool bPattern, const char* szMsg, std::ostream& os, int line) throw (test_exception);
void CheckBoolNoLock (bool b, bool bPattern, const char* szMsg, std::ostream& os, int line) throw (test_exception);
void CheckInt (tint v, tint vPattern, const char* szMsg, std::ostream& os, int line) throw (test_exception);
void CheckReal (treal v, treal vPattern, const char* szMsg, std::ostream& os, int line, treal rSp = cvmMachSp ()) throw (test_exception);
void CheckComplex (tcomplex v, tcomplex vPattern, const char* szMsg, std::ostream& os, int line, treal rSp = cvmMachSp ()) throw (test_exception);
void CheckCVector (cvector v, cvector vPattern, const char* szMsg, std::ostream& os, int line, treal rSp = cvmMachSp ()) throw (test_exception);
void CheckString (const std::string& v, const std::string& vPattern, const char* szMsg, std::ostream& os, int line) throw (test_exception);
void CheckString (const std::string& v, const std::string& vPattern, const std::string& vPattern2, const char* szMsg, std::ostream& os, int line) throw (test_exception);
void Fail (const char* szMsg, std::ostream& os, int line) throw (test_exception);

void cprint (const std::complex<treal>* p, int size);
void print_solution (const srmatrix& a, const rvector& b);
srmatrix invert (const srmatrix& a);
treal& ret (rmatrix& m, const unsigned row, const unsigned col);

extern int exit_code;
extern std::ofstream os;    // share it among multiple executions
extern TestCriticalSection gCS;
extern TestCriticalSection gCS2;

#ifdef CVM_FLOAT
    #if defined (_MSC_VER)
        #define FILE_OUT "testout_win32_float.txt"
    #else
        #if defined (__BORLANDC__)
            #define FILE_OUT "testout_win32_borland_float.txt"
        #else
            #define FILE_OUT "testout_linux_float.txt"
        #endif
    #endif
#else
    #if defined (_MSC_VER)
        #define FILE_OUT "testout_win32.txt"
    #else
        #if defined (__BORLANDC__)
            #define FILE_OUT "testout_win32_borland.txt"
        #else
            #define FILE_OUT "testout_linux.txt"
        #endif
    #endif
#endif

#if defined (CVM_TEST_WIN_THREADS)
unsigned int __stdcall
#else
void *
#endif
TestBody (void*);

#if defined (CVM_TEST_WIN_THREADS)
unsigned int __stdcall
#else
void *
#endif
TestFunBody (void*);
