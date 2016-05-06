//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2016
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef _CFUN_H
#define _CFUN_H

#include "cvm.h"        // http://cvmlib.com

#include <ctype.h>
#include <vector>
#include <unordered_map>

#define CFUN_O_BRACE                '{'
#define CFUN_C_BRACE                '}'
#define CFUN_O_BRACKET              '['
#define CFUN_C_BRACKET              ']'
#define CFUN_O_PARENTH              '('
#define CFUN_C_PARENTH              ')'
#define CFUN_O_SPARENTH             "("
#define CFUN_C_SPARENTH             ")"
#define CFUN_COMMA                  ','
#define CFUN_PLUS                   '+'
#define CFUN_MINUS                  '-'
#define CFUN_POWER                  '^'
#define CFUN_MULT                   '*'
#define CFUN_DIV                    '/'
#define CFUN_POINT                  '.'
#define CFUN_SIGNS                  "()+-^*/"
#define CFUN_INF                    "INF"

#define CFUN_EXP                    "exp"
#define CFUN_SQRT                   "sqrt"
#define CFUN_LOG                    "log"
#define CFUN_LOG10                  "log10"
#define CFUN_SIN                    "sin"
#define CFUN_COS                    "cos"
#define CFUN_TAN                    "tan"
#define CFUN_ASIN                   "asin"
#define CFUN_ACOS                   "acos"
#define CFUN_ATAN                   "atan"
#define CFUN_SINH                   "sinh"
#define CFUN_COSH                   "cosh"
#define CFUN_TANH                   "tanh"
#define CFUN_SI                     "sinint"
#define CFUN_CI                     "cosint"
#define CFUN_SMINUS                 "-"
#define CFUN_SPLUS                  "+"
#define CFUN_SMULT                  "*"
#define CFUN_SDIV                   "/"
#define CFUN_SPOWER                 "^"
#define CFUN_SCOMMA                 ","
#define CFUN_SSPACE                 " "
#define CFUN_SIGN                   "sign"
#define CFUN_ABS                    "abs"
#define CFUN_IIF                    "iif"
#define CFUN_DELTA                  "delta"
#define CFUN_POWERS                 "power"
#define CFUN_SAT                    "sat"
#define CFUN_I                      "i"
#define CFUN_SPACES                 " \t\r\n"
#define CFUN_SINF                   CFUN_O_SPARENTH CFUN_INF CFUN_C_SPARENTH
#define CFUN_SMINF                  CFUN_O_SPARENTH CFUN_SMINUS CFUN_INF CFUN_C_SPARENTH

// find* functions failure code
#define CFUN_NOT_FOUND              size_t(~0)

#define                CFUN_OK                            CVM_OK
#define                CFUN_PARSEERROR                    CVM_THE_LAST_ERROR_CODE + 1  //!< Error code for "Error while parsing \'%s\' for variables %s"
#define                CFUN_DOMAINERROR                   CVM_THE_LAST_ERROR_CODE + 2  //!< Error code for "Domain error while calculating %s of %g"
#define                CFUN_DOMAINERROR_C                 CVM_THE_LAST_ERROR_CODE + 3  //!< Error code for "Domain error while calculating %s of (%g,%g)"
#define                CFUN_CONVERGENCEERROR              CVM_THE_LAST_ERROR_CODE + 4  //!< Error code for "Convergence error while calculating %s of %g"
#define                CFUN_CONVERGENCEERROR_C            CVM_THE_LAST_ERROR_CODE + 5  //!< Error code for "Convergence error while calculating %s of (%g,%g)"
#define                CFUN_SUBSTPARAMETERERROR           CVM_THE_LAST_ERROR_CODE + 6  //!< Error code for "Error while substituting parameter \'%s\'"
#define                CFUN_VARSDONTMATCH                 CVM_THE_LAST_ERROR_CODE + 7  //!< Error code for "Variables don\'t match: \'%s\' vs. \'%s\'"
#define                CFUN_NULLPOINTERERROR              CVM_THE_LAST_ERROR_CODE + 8  //!< Error code for "Null pointer passed to \'%s\'"
#define                CFUN_PARAMETER_RECURSION           CVM_THE_LAST_ERROR_CODE + 9  //!< Error code for "Parameter \'%s\' can\'t be a part of its own meaning \'%s\'"

#define                CFUN_SIMPS_STACK_DEPTH             32
#define                CFUN_MAX_EI_ITERATIONS             1000


CVM_NAMESPACE_BEG

enum class Kind {
    cfun_var,
    cfun_const,
    cfun_plus,
    cfun_minus,
    cfun_mult,
    cfun_div,
    cfun_power,
    cfun_exp,
    cfun_sqrt,
    cfun_log,
    cfun_log10,
    cfun_sin,
    cfun_cos,
    cfun_tan,
    cfun_arcsin,
    cfun_arccos,
    cfun_arctan,
    cfun_sinh,
    cfun_cosh,
    cfun_tanh,
    cfun_uminus,
    cfun_sign,
    cfun_abs,
    cfun_delta,
    cfun_infinity,
    cfun_mninfinity,
    cfun_intsin,
    cfun_intcos,
    cfun_sat,
    cfun_iif
};

//! @cond INTERNAL

template <typename T>
constexpr T cfun_zero() {
    return sizeof(T) <= 4 ? 0.F : 0.;
}

template <typename T>
constexpr T cfun_one() {
    return sizeof(T) <= 4 ? 1.F : 1.;
}

template <typename T>
constexpr T cfun_mone() {
    return sizeof(T) <= 4 ? -1.F : -1.;
}

template <typename T>
constexpr T cfun_two() {
    return sizeof(T) <= 4 ? 2.F : 2.;
}

template <typename T>
constexpr T cfun_half() {
    return sizeof(T) <= 4 ? 0.5F : 0.5;
}

template <typename T>
constexpr T cfun_e() {
    return sizeof(T) <= 4 ? 2.7182818284590452353602874713527F : 2.7182818284590452353602874713527;
}

template <typename T>
constexpr T cfun_ln2() {
    return sizeof(T) <= 4 ? 0.69314718055994530941723212145818F : 0.69314718055994530941723212145818;
}

template <typename T>
constexpr T cfun_ln10() {
    return sizeof(T) <= 4 ? 2.3025850929940456840179914546844F : 2.3025850929940456840179914546844;
}

template <typename T>
constexpr T cfun_pi() {
    return sizeof(T) <= 4 ? 3.1415926535897932384626433832795F : 3.1415926535897932384626433832795;
}

template <typename T>
constexpr T cfun_pi_2() {
    return sizeof(T) <= 4 ? 1.5707963267948966192313216916398F : 1.5707963267948966192313216916398;
}

template <typename T>
constexpr T cfun_gamma() {
    return sizeof(T) <= 4 ? 0.57721566490153286060651209008240243F : 0.57721566490153286060651209008240243;
}

// specializations for some elementary functions
template <typename T>
class ElementaryFunctions 
{
    using TC = std::complex<T>;

public:
    static T asin(const T& v) throw(cvmexception)
    {
        if (v < cfun_mone<T>() || v > cfun_one<T>())
            throw cvmexception(CFUN_DOMAINERROR, "asin", v);
        return ::asin(v);
    }
    static T acos(const T& v) throw(cvmexception)
    {
        if (v < cfun_mone<T>() || v > cfun_one<T>())
            throw cvmexception(CFUN_DOMAINERROR, "acos", v);
        return ::acos(v);
    }
    static T tan(const T& v)
    {
        return ::tan(v);
    }
    static T atan(const T& v)
    {
        return ::atan(v);
    }
    static T sinh(const T& v)
    {
        return ::sinh(v);
    }
    static T cosh(const T& v)
    {
        return ::cosh(v);
    }
    static T tanh(const T& v)
    {
        return ::tanh(v);
    }
    static T sqrt(const T& v) throw(cvmexception)
    {
        if (v < cfun_zero<T>())
            throw cvmexception(CFUN_DOMAINERROR, "sqrt", v);
        return ::sqrt(v);
    }
    static T log(const T& v) throw(cvmexception)
    {
        if (v < cvm::basic_cvmMachMin<T>())
            throw cvmexception(CFUN_DOMAINERROR, "log", v);
        return ::log(v);
    }
    static T log10(const T& v) throw(cvmexception)
    {
        if (v < cvm::basic_cvmMachMin<T>())
            throw cvmexception(CFUN_DOMAINERROR, "log", v);
        return ::log10(v);
    }
    static T pow(const T& vBase, const T& vPower)
    {
        return ::pow(vBase, vPower);
    }

    // Sine integral
    static T sinint(const T& x, const T& eps) throw(cvmexception)
    {
        T si = cfun_zero<T>();
        T xa = cvm::_abs(x);

        if (xa > cfun_two<T>()) {
            si = T(cfun_pi_2<T>()) + e1(xa, eps).imag();
        }
        else {
            si = xa;
            if (xa > eps) {
                T t;
                T f = xa;
                int sign = -1;
                for (int i = 3; i < CFUN_MAX_EI_ITERATIONS; i += 2) {
                    f *= xa * xa / T((i - 1) * i);
                    t = f / T(i);
                    si += T(sign) * t;
                    sign = -sign;
                    if (t / cvm::_abs(si) < eps)
                        break;
                    if (i >= CFUN_MAX_EI_ITERATIONS - 2) {
                        throw cvmexception(CFUN_CONVERGENCEERROR, "sinint", x);
                    }
                }
            }
        }
        if (x < cfun_zero<T>()) {
            si = -si;
        }
        return si;
    }

    // Cosine integral
    static T cosint(const T& x, const T& eps) throw(cvmexception)
    {
        if (x <= cfun_zero<T>()) {
            throw cvmexception(CFUN_DOMAINERROR, "cosint", x);
        }

        T ci = cfun_zero<T>();
        if (x > cfun_two<T>()) {
            ci = -e1(x, eps).real();
        }
        else {
            T t;
            T f = cfun_one<T>();
            int sign = -1;
            for (int i = 2; i < CFUN_MAX_EI_ITERATIONS; i += 2) {
                f *= x * x / T((i - 1) * i);
                t = f / T(i);
                ci += T(sign) * t;
                sign = -sign;
                if (t / cvm::_abs(ci) < eps)
                    break;
                if (i >= CFUN_MAX_EI_ITERATIONS - 2) {
                    throw cvmexception(CFUN_CONVERGENCEERROR, "cosint", x);
                }
            }
            ci += log(x) + cfun_gamma<T>();
        }
        return ci;
    }

    // Exponential integral routine
    static TC e1(const T& x, const T& eps) throw(cvmexception)
    {
        static const TC c1(cfun_one<T>(), cfun_zero<T>());
        static const TC c2(cfun_two<T>(), cfun_zero<T>());
        TC a;
        TC b(cfun_one<T>(), x);
        TC c(cfun_one<T>() / (eps * eps), cfun_zero<T>());
        TC d(c1 / b);
        TC e(d);

        for (int i = 1; i < CFUN_MAX_EI_ITERATIONS; ++i) {
            a = TC(T(-i * i), cfun_zero<T>());
            b += c2;
            d = c1 / (a * d + b);
            c = b + a / c;
            a = c * d;
            e *= a;
            if (cvm::_abs(cfun_one<T>() - a.real()) + cvm::_abs(a.imag()) < eps)
                break;
            if (i >= CFUN_MAX_EI_ITERATIONS - 1) {
                throw cvmexception(CFUN_CONVERGENCEERROR, "ElementaryFunctions<T>::e1", x);
            }
        }
        return TC(cos(x), -sin(x)) * e;
    }
};

template <typename T>
class ElementaryFunctions<std::complex<T> >
{
    using TC = std::complex<T>;
public:
    static TC asin(const TC& v)
    {
        static const TC c1(cfun_one<T>(), cfun_zero<T>());
        static const TC cmi(cfun_zero<T>(), cfun_mone<T>());
        return cmi * std::log(std::sqrt(c1 - v * v) - cmi * v);
    }
    static TC acos(const TC& v)
    {
        static const TC c1(cfun_one<T>(), cfun_zero<T>());
        static const TC cmi(cfun_zero<T>(), cfun_mone<T>());
        return cmi * std::log(v - cmi * std::sqrt(c1 - v * v));
    }
    static TC tan(const TC& v)
    {
        const T rm = v.real() * cfun_two<T>();
        const T im = v.imag() * cfun_two<T>();
        if (cvm::_abs(im) >= basic_cvmLogMachMax<T>()) {
            return TC(cfun_zero<T>(), im > cfun_zero<T>() ? cfun_one<T>() : cfun_mone<T>());
        }
        else {
            const T d = ::cos(rm) + ::cosh(im);
            return TC(::sin(rm) / d, ::sinh(im) / d);
        }
    }
    static TC atan(const TC& v)
    {
        static const TC ci(cfun_zero<T>(), cfun_one<T>());
        static const TC ci2(cfun_zero<T>(), cfun_half<T>());
        return ci2 * std::log((ci + v) / (ci - v));
    }
    static TC sinh(const TC& v)
    {
        return std::sinh(v);
    }
    static TC cosh(const TC& v)
    {
        return std::cosh(v);
    }
    static TC tanh(const TC& v)
    {
        return std::tanh(v);
    }
    static TC sqrt(const TC& v)
    {
        return std::sqrt(v);
    }
    static TC log(const TC& v)
    {
        return std::log(v);
    }
    static TC log10(const TC& v)
    {
        return std::log10(v);
    }
    static TC pow(const TC& vBase, const TC& vPower)
    {
        return std::pow(vBase, vPower);
    }

    // Sine integral
    static TC sinint(const TC& x, const T& eps) throw(cvmexception)
    {
        TC si(x);
        if (cvm::_abs(x) > eps) {
            TC t;
            TC f(x);
            int sign = -1;
            for (int i = 3; i < CFUN_MAX_EI_ITERATIONS; i += 2) {
                f *= x * x / T((i - 1) * i);
                t = f / T(i);
                si += t * T(sign);
                sign = -sign;
                if (cvm::_abs(t) / cvm::_abs(si) < eps)
                    break;
                if (i >= CFUN_MAX_EI_ITERATIONS - 2) {
                    throw cvmexception(CFUN_CONVERGENCEERROR_C, "sinint", x.real(), x.imag());
                }
            }
        }
        return si;
    }

    // Cosine integral
    static TC cosint(const TC& x, const T& eps) throw(cvmexception)
    {
        static const TC gamma(cfun_gamma<T>(), cfun_zero<T>());
        if (cvm::_abs(x) < eps) {
            throw cvmexception(CFUN_DOMAINERROR_C, "cosint", x.real(), x.imag());
        }

        TC ci(cfun_zero<T>());
        TC t;
        TC f(cfun_one<T>());
        int sign = -1;
        for (int i = 2; i < CFUN_MAX_EI_ITERATIONS; i += 2) {
            f *= x * x / T((i - 1) * i);
            t = f / T(i);
            ci += t * T(sign);
            sign = -sign;
            if (cvm::_abs(t) / cvm::_abs(ci) < eps)
                break;
            if (i >= CFUN_MAX_EI_ITERATIONS - 2) {
                throw cvmexception(CFUN_CONVERGENCEERROR_C, "cosint", x.real(), x.imag());
            }
        }
        return ci + log(x) + gamma;
    }
};


// cross-type stuff
template <typename T1, typename T2>
class Comparator
{
public:
    static bool lt(const T1& l, const T2& r);
    static bool le(const T1& l, const T2& r);
    static bool gt(const T1& l, const T2& r);
    static bool ge(const T1& l, const T2& r);
    static bool eq(const T1& l, const T2& r);
};


/**
@brief Pair of pointers

Similar to shared_ptr but for pair of pointers.
*/
template <typename T>
class shared_ptr_pair
{
    std::shared_ptr<T> mp1;  //! First pointer in pair
    std::shared_ptr<T> mp2;  //! Second pointer in pair
public:

/**
@brief Constructor

Creates pair of shared_ptr instances.
*/
    shared_ptr_pair(T* p1, T* p2)
      : mp1(p1),
        mp2(p2)
    {
    }

/**
@brief Constructor

Creates pair of shared_ptr instances.
*/
    shared_ptr_pair(const std::shared_ptr<T>& p1, const std::shared_ptr<T>& p2)
      : mp1(p1),
        mp2(p2)
    {
    }

/**
@brief Move constructor

Creates pair of shared_ptr instances.
*/
    shared_ptr_pair(std::shared_ptr<T>&& p1, std::shared_ptr<T>&& p2)
      : mp1(std::move(p1)),
        mp2(std::move(p2))
    {
    }

/**
@brief Indexing operator

Returns one of two shared_ptr instances.
*/
    std::shared_ptr<T> operator[](size_t i) throw(cvmexception)
    {
        if (i == 0) {
            return mp1;
        }
        else if (i == 1) {
            return mp2;
        }
        throw cvmexception(CVM_INDEX_GT, i, 1);
    }
};
//! @endcond


using string_array = std::vector<std::string>; //!< Array of strings


//! @cond INTERNAL
inline size_t __not(size_t i)
{
    return i ? 0 : 1;
}

inline void __clean_array(string_array& a)
{
    if (a.size() > 0) {
        a.erase(a.begin(), a.end());
    }
}

template <typename T>
inline bool __parse_num(const std::string& s, T& result)
{
    bool ret = true;
    char* stop;
    result = T(::strtod(s.c_str(), &stop));
    if ((result == cfun_zero<T>() && s[0] != '0') ||
        result == std::numeric_limits<T>::infinity() ||
        result == - std::numeric_limits<T>::infinity() ||
        result == std::numeric_limits<T>::lowest() ||
        *stop != '\0') {
        ret = false;
    }
    return ret;
}

// 7.0 FIX: these big things must be inlined too!
// MS doesn't like to do memory allocation and deallocation in different versions of RTL.
// Therefore, to make it possible to mix MSVS 2012 and 2010 generated code we have to inline this stuff.

inline std::string::const_iterator __trim_beg(const std::string& s, size_t nEdge = 0)
{
    std::string::const_iterator it = s.end();
    size_t nLen = s.length();
    if (nLen > nEdge * 2) {       // both sides
        nLen = s.find_first_not_of(CFUN_SPACES, nEdge);
        if (nLen != CFUN_NOT_FOUND) {
            it = s.begin() + nLen;
        }
    }
    return it;
}

inline std::string::const_iterator __trim_end(const std::string& s, size_t nEdge = 0)
{
    std::string::const_iterator it = s.end();
    size_t nLen = s.length();
    if (nLen > nEdge * 2) {       // both sides
        nLen = s.find_last_not_of(CFUN_SPACES, nLen - nEdge - 1);
        if (nLen != CFUN_NOT_FOUND) {
            it = s.begin() + (nLen + 1);
        }
    }
    return it;
}

// returns false if empty
inline bool __strip_parenth(const std::string& s, size_t& nFirst, size_t& nLast)
{
    static const char szSpaces[] = CFUN_SPACES;
    static const std::string sSigns(CFUN_SIGNS);
    bool bRet = true;
    int nLevel;
    char c;
    const size_t nLastOld = nLast;
    if (s.length() <= nLast || (s[nLast] && sSigns.find(s[nLast]) != CFUN_NOT_FOUND)) {
        --nLast;
    }

    for (;;) {
        nFirst = s.find_first_not_of(szSpaces, nFirst);
        nLast = s.find_last_not_of(szSpaces, nLast);
        if (nFirst == CFUN_NOT_FOUND || nLast == CFUN_NOT_FOUND) {
            bRet = false;
            break;
        }
        if (s[nFirst] == CFUN_O_PARENTH && s[nLast] == CFUN_C_PARENTH) {    // if (xxxxx)
            nLevel = 1;
            for (size_t i = nFirst + 1; i < nLast; i++) {                   // but like (x) + (y) or (x*(2+y)-3)
                c = s[i];
                if (c == CFUN_C_PARENTH) {
                    nLevel--;
                    if (!nLevel)
                        break;
                }
                if (c == CFUN_O_PARENTH) {
                    nLevel++;
                }
            }
            if (nLevel) {
                nFirst++;
                nLast--;
                continue;
            }
            break;
        }
        else {
            break;
        }
    }
    if (nLastOld > nLast) {
        ++nLast;             // -> '\0'
    }
    return bRet;
}

inline size_t __tassign(std::string& sDest, const std::string& sSrc, size_t nFirst, size_t nLast)
{
    size_t nRet = 0;
    if (__strip_parenth(sSrc, nFirst, nLast)) {
        if (&sDest == &sSrc) {
            if (nLast < sDest.length()) {
                sDest.erase(sDest.begin() + nLast, sDest.end());
            }
            if (nFirst) {
                sDest.erase(sDest.begin(), sDest.begin() + nFirst);
            }
        }
        else {
            sDest.assign(sSrc.begin() + nFirst, sSrc.begin() + nLast);
        }
        nRet = nLast - nFirst;
    }
    else {
        sDest.erase();
    }
    return nRet;
}

// "(x, 2)" -> "x" and "2"
// 0 - empty string
// 1 - there was a comma
// 2 - there wasn't a comma
inline int __separate(const std::string& sBody, size_t nFirst, size_t nLast,
                      std::string& sLeft, std::string& sRight)
{
    if (__strip_parenth(sBody, nFirst, nLast)) {
        size_t nOPos = sBody.find(CFUN_O_PARENTH, nFirst);
        size_t nCPos = sBody.find(CFUN_C_PARENTH, nFirst);

        size_t i = nFirst;
        for (;;) {
            i = sBody.find(CFUN_COMMA, i);
            if (i == CFUN_NOT_FOUND) {
                __tassign(sLeft, sBody, nFirst, nLast);
                sRight.erase();
                return 2;
            }
            if (nOPos == CFUN_NOT_FOUND ||
                nCPos == CFUN_NOT_FOUND ||
                (nCPos > nOPos && (i < nOPos || i > nCPos))) {
                __tassign(sLeft, sBody, nFirst, i);
                __tassign(sRight, sBody, i + 1, nLast);
                return 1;
            }
            i++;
        }
    }
    sLeft.erase();
    sRight.erase();
    return 0;
}

inline bool __arrays_equal(const string_array& a1, const string_array& a2)
{
    bool ret = a1.size() == a2.size();
    if (ret) {
        string_array::const_iterator i1 = a1.begin();
        string_array::const_iterator e1 = a1.end();
        string_array::const_iterator i2 = a2.begin();
        while (i1 != e1) {
            if ((*i1) != (*i2)) {
                ret = false;
                break;
            }
            ++i1;
            ++i2;
        }
    }
    return ret;
}

inline void __add_to_array(string_array& a, const std::string& sSrc,
                           std::string::const_iterator iBeg, std::string::const_iterator iEnd)
{
    static const char szSpaces[] = CFUN_SPACES;
    if (iEnd > iBeg) {
        size_t nFirst = sSrc.find_first_not_of(szSpaces, iBeg - sSrc.begin());

        if (nFirst != CFUN_NOT_FOUND) {
            size_t nLast = sSrc.find_last_not_of(szSpaces, iEnd - sSrc.begin() - 1);

            if (nLast != CFUN_NOT_FOUND &&
                nLast >= nFirst) {
                a.push_back(std::string(sSrc.begin() + nFirst, sSrc.begin() + (nLast + 1)));
            }
        }
    }
}

inline void __parse_vars(const std::string& sVars, size_t nFirst, size_t nLast, string_array& saResult)
{
    size_t nL;
    __clean_array(saResult);
    for (;;) {
        nL = sVars.find(CFUN_COMMA, nFirst);
        if (nL == CFUN_NOT_FOUND || nL >= nLast) {
            __add_to_array(saResult, sVars, sVars.begin() + nFirst, sVars.begin() + nLast);
            break;
        }
        __add_to_array(saResult, sVars, sVars.begin() + nFirst, sVars.begin() + nL);
        nFirst = nL + 1;
    }
}

inline std::string __format_vars(const string_array& a, bool emptyBraces = true)
{
    std::stringstream stream;
    bool showBraces = emptyBraces || a.size() > 0;
    if (showBraces) {
        stream << CFUN_O_BRACE;
    }
    string_array::const_iterator it = a.begin();
    string_array::const_iterator end = a.end();
    while (it != end) {
        stream << *it;
        ++it;
        if (it != end)
            stream << CFUN_SCOMMA;
    }
    if (showBraces) {
        stream << CFUN_C_BRACE;
    }
    return stream.str();
}

// MAC is missing isalpha while linking...
inline int is_alpha(int c)
{
#if defined(MAC)
    if ((c >= 0141 && c <= 0172) || (c >= 0101 && c <= 0132)) {
        return 1;
    }
    return 0;
#else
    return isalpha(c);
#endif
}

// k*x & k=a+b ---> (a+b)*x
inline bool __subst_parameter(std::string& sBody, const string_array& saParameters,
                              const string_array& saMeanings) throw(cvmexception)
{
    bool ret = false;
    const size_t nPsz = saParameters.size();

    for (size_t i = 0; i < nPsz; i++) {
        if (sBody == saParameters[i]) {
            ret = true;
            if (i < saMeanings.size()) {
                const std::string& sMeaning = saMeanings[i];
                size_t nBeg = sMeaning.find_first_of(sBody);
                if (nBeg != CFUN_NOT_FOUND) {
                    size_t nEnd = nBeg + sBody.length();
                    if ((nBeg <= 0 || !is_alpha(sMeaning.at(nBeg - 1))) &&  // p -> {p+p}  ||  p -> {2*p-x}  ||  p -> {22-p}
                        (nEnd >= sMeaning.length() || !is_alpha(sMeaning.at(nEnd)))) {
                        throw cvmexception(CFUN_PARAMETER_RECURSION, sBody.c_str(), sMeaning.c_str());
                    }
                }
                sBody.assign(__trim_beg(sMeaning), __trim_end(sMeaning));
            }
            else {
                throw cvmexception(CFUN_SUBSTPARAMETERERROR, sBody.c_str());
            }
            break;
        }
    }
    return ret;
}

// returns true if body looks like 1.e+12 or 1.e-12
inline bool __ifbodynum(const std::string& sBody, size_t nBodyLength, size_t nSignPos)
{
    static const char ccExp = 'e', ccExpb = 'E', ccPoint = '.';
    return nSignPos < nBodyLength - 1 &&
           isdigit(sBody[nSignPos + 1]) &&
           nSignPos > 1 &&
           (sBody[nSignPos - 1] == ccExp || sBody[nSignPos - 1] == ccExpb) &&
           (sBody[nSignPos - 2] == ccPoint || isdigit(sBody[nSignPos - 2]));
}

inline size_t __tassign(std::string& sDest, const std::string& sSrc, size_t nFirst)
{
    return __tassign(sDest, sSrc, nFirst, sSrc.length());
}

inline size_t __tassign(std::string& sDest, const std::string& sSrc)
{
    return __tassign(sDest, sSrc, 0, sSrc.length());
}

template <typename T> class BaseFunction;
template <typename T> class UnaryFunction;
template <typename T> class BinaryFunction;
template <typename T> class Fconst;
template <typename T> class Finfinity;
template <typename T> class Fmninfinity;
template <typename T> class Fplus;
template <typename T> class Fminus;
template <typename T> class Fmult;
template <typename T> class Fdiv;
template <typename T> class Fpower;
template <typename T> class Fsat;
template <typename T> class Fexp;
template <typename T> class Fsqrt;
template <typename T> class Flog;
template <typename T> class Flog10;
template <typename T> class Fsin;
template <typename T> class Fcos;
template <typename T> class Ftan;
template <typename T> class Fasin;
template <typename T> class Facos;
template <typename T> class Fatan;
template <typename T> class Fsinh;
template <typename T> class Fcosh;
template <typename T> class Ftanh;
template <typename T> class Fsinint;
template <typename T> class Fcosint;
template <typename T> class Fuminus;
template <typename T> class Fsign;
template <typename T> class Fabs;
template <typename T> class Fdelta;
template <typename T> class Fvar;
template <typename T> class Fiif;

#ifdef CVM_USE_VARIADIC_TEMPLATES
#   define CFUN_NEW_FUNC(cls, ...) std::make_shared<cls<T>>(__VA_ARGS__)
#else
#   define CFUN_NEW_FUNC(cls, ...) BasePointer(new cls<T>(__VA_ARGS__))
#endif

//! @endcond


/**
@brief Function Factory (not end-user)

\c T type stands for \ref treal or \ref tcomplex. Please use inherited function classes.
@see basic_function
*/
template <typename T>
class FunctionFactory
{
public:
    using BasePointer = std::shared_ptr<BaseFunction<T>>; //!< Shared pointer to BaseFunction

//! @cond INTERNAL
    
#define CFUN_CREATOR_DEF(f) \
    struct CreatorF##f : public Creator { \
        BasePointer create(const std::string& sBody, size_t nLeft, \
                            size_t nBodyLength, const string_array& saVars, \
                            const string_array& saParameters, const string_array& saMeanings) \
                            const override { \
                                return CFUN_NEW_FUNC(F##f, sBody, nLeft, nBodyLength, saVars, \
                                                     saParameters, saMeanings); \
                            } \
        };

#define CFUN_CREATOR_INST(F,f) \
    mmap.emplace(F, std::unique_ptr<Creator>(new CreatorF##f()));
    
    
private:
    struct Creator {
        virtual BasePointer create(const std::string& sBody, size_t nLeft,
                                   size_t nBodyLength, const string_array& saVars,
                                   const string_array& saParameters, const string_array& saMeanings) const = 0;
    };

    CFUN_CREATOR_DEF(exp)
    CFUN_CREATOR_DEF(sqrt)
    CFUN_CREATOR_DEF(log)
    CFUN_CREATOR_DEF(log10)
    CFUN_CREATOR_DEF(sin)
    CFUN_CREATOR_DEF(cos)
    CFUN_CREATOR_DEF(tan)
    CFUN_CREATOR_DEF(asin)
    CFUN_CREATOR_DEF(acos)
    CFUN_CREATOR_DEF(atan)
    CFUN_CREATOR_DEF(sinh)
    CFUN_CREATOR_DEF(cosh)
    CFUN_CREATOR_DEF(tanh)
    CFUN_CREATOR_DEF(sinint)
    CFUN_CREATOR_DEF(cosint)
    CFUN_CREATOR_DEF(sign)
    CFUN_CREATOR_DEF(abs)
    CFUN_CREATOR_DEF(iif)
    CFUN_CREATOR_DEF(delta)
    CFUN_CREATOR_DEF(power)
    CFUN_CREATOR_DEF(sat)
    
    class Engine {
        std::unordered_map<std::string,std::unique_ptr<Creator>> mmap;
    public:
        Engine() {
            CFUN_CREATOR_INST(CFUN_EXP,exp)
            CFUN_CREATOR_INST(CFUN_SQRT,sqrt)
            CFUN_CREATOR_INST(CFUN_LOG,log)
            CFUN_CREATOR_INST(CFUN_LOG10,log10)
            CFUN_CREATOR_INST(CFUN_SIN,sin)
            CFUN_CREATOR_INST(CFUN_COS,cos)
            CFUN_CREATOR_INST(CFUN_TAN,tan)
            CFUN_CREATOR_INST(CFUN_ASIN,asin)
            CFUN_CREATOR_INST(CFUN_ACOS,acos)
            CFUN_CREATOR_INST(CFUN_ATAN,atan)
            CFUN_CREATOR_INST(CFUN_SINH,sinh)
            CFUN_CREATOR_INST(CFUN_COSH,cosh)
            CFUN_CREATOR_INST(CFUN_TANH,tanh)
            CFUN_CREATOR_INST(CFUN_SI,sinint)
            CFUN_CREATOR_INST(CFUN_CI,cosint)
            CFUN_CREATOR_INST(CFUN_SIGN,sign)
            CFUN_CREATOR_INST(CFUN_ABS,abs)
            CFUN_CREATOR_INST(CFUN_IIF,iif)
            CFUN_CREATOR_INST(CFUN_DELTA,delta)
            CFUN_CREATOR_INST(CFUN_POWERS,power)
            CFUN_CREATOR_INST(CFUN_SAT,sat)
        }
        BasePointer create(const std::string& name, const std::string& sBody, size_t nLeft,
                           size_t nBodyLength, const string_array& saVars,
                           const string_array& saParameters, const string_array& saMeanings) const {
            auto it = mmap.find(name);
            if (it != mmap.end()) {
                return it->second->create(sBody, nLeft, nBodyLength, saVars, saParameters, saMeanings);
            }
            return nullptr;
        }
    };
    
//! @endcond

public:

/**
@brief Parser and factory interface (not end-user)

Parses incoming strings and instantiates function object
*/
    static BasePointer compile(const std::string& sPar, size_t nFirst, size_t nLast,
                               const string_array& saVars,
                               const string_array& saParameters,
                               const string_array& saMeanings,
                               bool subst = true) throw(cvmexception)
    {
        size_t i;
        static const char pccSigns[] = CFUN_SIGNS;
        static const char pccInfinity[] = CFUN_INF;
        std::string sBody, sLeft;
        size_t nBodyLength = __tassign(sBody, sPar, nFirst, nLast);

        if (!nBodyLength) {
            BaseFunction<T>::parse_err(sPar, saVars);
        }

        // trying to handle real or complex number first
        BasePointer pfConst = CFUN_NEW_FUNC(Fconst, sBody, false); // std::make_shared<Fconst<T>>(sBody, false);
        if (pfConst->is_valid()) {
            return pfConst;
        }

        if (subst && __subst_parameter(sBody, saParameters, saMeanings)) { // k*x & k=a+b ---> (a+b)*x
            return FunctionFactory<T>::compile(sBody, 0, sBody.length(), saVars, saParameters, saMeanings, false);
        }

        int nLevel = 0;
        for (i = nBodyLength - 1; i < CFUN_NOT_FOUND; --i) {
            switch(sBody[i]) {
            case CFUN_O_PARENTH:
                ++nLevel;
                break;
            case CFUN_C_PARENTH:
                --nLevel;
                break;
            case CFUN_PLUS:
                if (!nLevel) {
                    if (!i) {                                              // +x -> x
                        return FunctionFactory<T>::compile(sBody, 1, nBodyLength, saVars, saParameters, saMeanings, subst);
                    }
                    if (__ifbodynum(sBody, nBodyLength, i)) {
                        break;
                    }
                    return CFUN_NEW_FUNC(Fplus, sBody, 0, i, i + 1, nBodyLength, saVars, saParameters, saMeanings);
                }
                break;
            case CFUN_MINUS:
                if (!nLevel) {
                    if (__ifbodynum(sBody, nBodyLength, i)) {
                        break;
                    }
                    if (!i) {
                        return CFUN_NEW_FUNC(Fuminus, sBody, 1, nBodyLength, saVars, saParameters, saMeanings);
                    }
                    return CFUN_NEW_FUNC(Fminus, sBody, 0, i, i + 1, nBodyLength, saVars, saParameters, saMeanings);
                }
                break;
            default:
                break;
            }
        }
        if (nLevel) {                                                       // ((...) or (...))
            BaseFunction<T>::parse_err(sBody, saVars);
        }

        for (i = nBodyLength - 1; i < CFUN_NOT_FOUND; --i) {
            switch(sBody[i]) {
            case CFUN_O_PARENTH:
                ++nLevel;
                break;
            case CFUN_C_PARENTH:
                --nLevel;
                break;
            case CFUN_MULT:                                                 // x * y
                if (!i) {
                    BaseFunction<T>::parse_err(sBody, saVars);
                }
                if (!nLevel) {
                    return CFUN_NEW_FUNC(Fmult, sBody, 0, i, i + 1, nBodyLength, saVars, saParameters, saMeanings);
                }
                break;
            case CFUN_DIV:                                                  // x / y
                if (!i) {
                    BaseFunction<T>::parse_err(sBody, saVars);
                }
                if (!nLevel) {
                    return CFUN_NEW_FUNC(Fdiv, sBody, 0, i, i + 1, nBodyLength, saVars, saParameters, saMeanings);
                }
                break;
            default:
                break;
            }
        }

        for (i = 0; i < nBodyLength; ++i) {
            switch(sBody[i]) {
            case CFUN_O_PARENTH:
                ++nLevel;
                break;
            case CFUN_C_PARENTH:
                --nLevel;
                break;
            case CFUN_POWER:                                                // x ^ y
                if (!i) {
                    BaseFunction<T>::parse_err(sBody, saVars);
                }
                if (!nLevel) {
                    return CFUN_NEW_FUNC(Fpower, sBody, 0, i, i + 1, nBodyLength, saVars, saParameters, saMeanings);
                }
                break;
            default:
                break;
            }
        }

        if (isdigit(sBody[0]) || sBody[0] == CFUN_POINT) {                  // 1.e-3 or .2e+12 or 99
            return CFUN_NEW_FUNC(Fconst, sBody);
        }
        if (sBody == pccInfinity) {
            return CFUN_NEW_FUNC(Finfinity);
        }
        for (i = 0; i < saVars.size(); ++i) {
            if (saVars[i] == sBody) {
                return std::make_shared<Fvar<T>>(sBody, i);
            }
        }

        size_t nLeft = sBody.find_first_of(pccSigns, 0);                    // sin(... cos(... etc. or x1 or y
        __tassign(sLeft, sBody, 0, nLeft);

        if (sLeft.length() == 0) {
            BaseFunction<T>::parse_err(sBody, saVars);
        }
        
        if (sLeft == CFUN_I) {
            return CFUN_NEW_FUNC(Fconst, cfun_one<T>());
        }
        
        static Engine eng;
        BasePointer ptr = eng.create(sLeft, sBody, nLeft, nBodyLength, saVars, saParameters, saMeanings);
        if (!ptr) {
            BaseFunction<T>::parse_err(sLeft, saVars);
        }
        return ptr;
 //       return CFUN_NEW_FUNC(Fconst, T(0));
    }
};


/**
@brief Generic function (not end-user)

\c T type stands for \ref treal or \ref tcomplex. Please use inherited function classes.
@see basic_function
*/
template <typename T>
class BaseFunction
{
protected:
    using BasePointer = typename FunctionFactory<T>::BasePointer; //!< Shared pointer to BaseFunction

//! @cond INTERNAL
    virtual BasePointer _simpl() const = 0;                     // returns simplified function as a new object
    virtual int _depth(bool) const = 0;                         // call stack acquire and release
//! @endcond

public:
    /**
     * @brief Default constructor
     *
     * Creates empty function. No memory gets allocated.
     */
    BaseFunction()
    {
    }

    /**
     * @brief Destructor
     */
    virtual ~BaseFunction()
    {
    }

//! @cond INTERNAL
    virtual Kind _kind() const = 0;                             // a kind of function type id
    virtual T _value(const T* pdVars) const = 0;                // value of scalar function of few variables
    virtual BasePointer _clone() const = 0;                     // a new clone of an object
    virtual BasePointer _deriv(size_t nVarNum) const = 0;       // _derivative by given variable as a new object
    virtual bool _equals(const BasePointer& pfSrc) const = 0;   // checks whether they are identical
    virtual std::string _format(int nPrecision) const = 0;      // returns formatted body

    virtual bool is_valid() const {                             // to override in FConst
        return true;
    }

    virtual BasePointer _getArg(size_t) const {                 // nArgNum-th variable, 0-based, const version
        return nullptr;
    }

    BasePointer _simp() const
    {
        BasePointer pRet;
        if (_depth(true) > CFUN_SIMPS_STACK_DEPTH) {
            pRet = this->_clone();
        }
        else {
            pRet = this->_simpl();
        }
        _depth(false);
        return pRet;
    }

    static void parse_err(const std::string& sBody, const char* var = nullptr) throw(cvmexception) {
        throw cvmexception(CFUN_PARSEERROR, sBody.c_str(), var == nullptr ? "n/a" : var);
    }

    static void parse_err(const std::string& sBody, const string_array& saVars) throw(cvmexception) {
        parse_err(sBody, __format_vars(saVars).c_str());
    }
    
//! @endcond
};


/**
@brief Generic unary function (not end-user)

\c T type stands for \ref treal or \ref tcomplex. Please use inherited function classes.
@see basic_function
*/
template <typename T>
class UnaryFunction : public BaseFunction<T>
{
protected:
    using BasePointer = typename BaseFunction<T>::BasePointer; //!< Shared pointer to BaseFunction
    BasePointer mf0; //!< Pointer to function's argument. If this instance is sin(A) then mf0 points to A.

//! @cond INTERNAL
    virtual BasePointer _simpl() const = 0;                     // returns simplified function as a new object
    virtual int _depth(bool) const = 0;                         // call stack acquire and release
//! @endcond

public:
    /**
     * @brief Default constructor
     *
     * Creates empty function. No memory gets allocated.
     */
    UnaryFunction()
      : mf0()
    {
    }

    /**
     * @brief Copy constructor
     */
    UnaryFunction(const UnaryFunction<T>& rfSrc)
      : mf0(rfSrc.mf0)
    {
    }

    /**
     * @brief Move constructor
     */
    UnaryFunction(UnaryFunction<T>&& rfSrc)
      : mf0(std::move(rfSrc.mf0))
    {
    }

    /**
     * @brief Constructor
     */
    UnaryFunction(const BasePointer& pfSrc) 
      : mf0(pfSrc)
    {
    }

    UnaryFunction(BasePointer&& pfSrc)
      : mf0(std::move(pfSrc))
    {
    }

    /**
     * @brief Constructor
     */
    UnaryFunction(const std::string& sArg, const string_array& saVars,
                  const string_array& saParameters, const string_array& saMeanings)
      : mf0(FunctionFactory<T>::compile(sArg, 0, sArg.length(), saVars, saParameters, saMeanings))
    {
    }

    /**
     * @brief Constructor
     */
    UnaryFunction(const std::string& sArg, size_t nF, size_t nL, const string_array& saVars,
                  const string_array& saParameters, const string_array& saMeanings)
      : mf0(FunctionFactory<T>::compile(sArg, nF, nL, saVars, saParameters, saMeanings))
    {
    }

    /**
     * @brief Assignment operator
     */
    UnaryFunction& operator = (const UnaryFunction<T>& rfSrc)
    {
        mf0 = rfSrc.mf0;
        return *this;
    }

    /**
     * @brief Move assignment operator
     */
    UnaryFunction& operator = (UnaryFunction<T>&& rfSrc)
    {
        mf0 = std::move(rfSrc.mf0);
        return *this;
    }

//! @cond INTERNAL
    virtual Kind _kind() const = 0;
    virtual T _value(const T* pdVars) const = 0;
    virtual BasePointer _clone() const = 0;
    virtual BasePointer _deriv(size_t nVarNum) const = 0;
    virtual const char* _name() const = 0;

    bool _equals(const BasePointer& pfSrc) const override
    {
        return pfSrc &&
               mf0 &&
               this->_kind() == pfSrc->_kind() &&
               mf0->_equals(pfSrc->_getArg(0));
    }

    BasePointer _getArg(size_t nArgNum) const override
    {
        return nArgNum ? nullptr : mf0;
    }

    std::string _format(int nPrecision) const override
    {
        std::ostringstream osBuf;
        osBuf << this->_name() << CFUN_O_PARENTH << mf0->_format(nPrecision) << CFUN_C_PARENTH;
        return osBuf.str();
    }
//! @endcond
};


/**
@brief Generic binary function (not end-user)

\c T type stands for \ref treal or \ref tcomplex. Please use inherited function classes.
@see basic_function
*/
template <typename T>
class BinaryFunction : public UnaryFunction<T>
{
protected:
    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction
    BasePointer mf1; //!< Pointer to function's second argument. If this instance is A+B then mf0 points to A and mf1 points to B.

//! @cond INTERNAL
    void BinCtr(const std::string& sBodyCommaBody,              // for bodies like pow(a,b) and sat(a,b)
                size_t nFirst, size_t nLast,
                const string_array& saVars,
                const string_array& saParameters, const string_array& saMeanings) throw(cvmexception)
    {
        std::string sLeft, sRight;
        if (__separate(sBodyCommaBody, nFirst, nLast, sLeft, sRight) == 1) {
            this->mf0 = FunctionFactory<T>::compile(sLeft, 0, sLeft.length(), saVars, saParameters, saMeanings);
            this->mf1 = FunctionFactory<T>::compile(sRight, 0, sRight.length(), saVars, saParameters, saMeanings);
        }
        else {
            BaseFunction<T>::parse_err(sBodyCommaBody, saVars);
        }
    }

    virtual BasePointer _simpl() const = 0;                     // returns simplified function as a new object
    virtual int _depth(bool) const = 0;                         // call stack acquire and release
//! @endcond

public:
    /**
     * @brief Constructor
     */
    BinaryFunction()
      : mf1()
    {
    }

    /**
     * @brief Copy constructor
     */
    BinaryFunction(const BinaryFunction<T>& rfSrc)
      : UnaryFunction<T>(rfSrc.mf0),
        mf1(rfSrc.mf1)
    {
    }

    /**
     * @brief Move constructor
     */
    BinaryFunction(BinaryFunction<T>&& rfSrc)
      : UnaryFunction<T>(std::move(rfSrc.mf0)),
        mf1(std::move(rfSrc.mf1))
    {
    }

    /**
     * @brief Constructor
     */
    BinaryFunction(const BaseFunction<T>& rfSrc0, const BaseFunction<T>& rfSrc1)
      : UnaryFunction<T>(rfSrc0),
        mf1(rfSrc1.mf1)
    {
    }

    /**
     * @brief Constructor
     */
    BinaryFunction(const BasePointer& pfSrc0, const BasePointer& pfSrc1)
      : UnaryFunction<T>(pfSrc0),
        mf1(pfSrc1)
    {
    }

    /**
     * @brief Constructor
     */
    BinaryFunction(const std::string& sArg1, const std::string& sArg2,
                   const string_array& saVars,
                   const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg1, 0, sArg1.length(), saVars, saParameters, saMeanings),
        mf1(FunctionFactory<T>::compile(sArg2, 0, sArg2.length(), saVars, saParameters, saMeanings))
    {
    }

    /**
     * @brief Constructor
     */
    BinaryFunction(const std::string& sArgs, 
                   size_t nF1, size_t nL1, size_t nF2, size_t nL2,
                   const string_array& saVars,
                   const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArgs, nF1, nL1, saVars, saParameters, saMeanings),
        mf1(FunctionFactory<T>::compile(sArgs, nF2, nL2, saVars, saParameters, saMeanings))
    {
    }

    /**
     * @brief Constructor
     */
    BinaryFunction(const std::string& sArg, size_t nFirst, size_t nLast,
                   const string_array& saVars,
                   const string_array& saParameters, const string_array& saMeanings)
        : mf1()
    {
        std::string sArg1, sArg2;
        if (__separate(sArg, nFirst, nLast, sArg1, sArg2) == 1) {
            this->mf0 = FunctionFactory<T>::compile(sArg1, 0, sArg1.length(), saVars, saParameters, saMeanings);
            this->mf1 = FunctionFactory<T>::compile(sArg2, 0, sArg2.length(), saVars, saParameters, saMeanings);
        }
    }

    /**
     * @brief Assignment operator
     */
    BinaryFunction& operator = (const BinaryFunction<T>& rfSrc)
    {
        this->mf0 = rfSrc.mf0;
        this->mf1 = rfSrc.mf1;
        return *this;
    }

    /**
     * @brief Move assignment operator
     */
    BinaryFunction& operator = (BinaryFunction<T>&& rfSrc)
    {
        this->mf0 = std::move(rfSrc.mf0);
        this->mf1 = std::move(rfSrc.mf1);
        return *this;
    }

//! @cond INTERNAL
    virtual Kind _kind() const = 0;
    virtual T _value(const T* pdVars) const = 0;
    virtual BasePointer _clone() const = 0;
    virtual BasePointer _deriv(size_t nVarNum) const = 0;
    virtual std::string _format(int nPrecision) const = 0;

    bool _equals(const BasePointer& pfSrc) const override
    {
        return pfSrc && this->mf0 && this->mf1 &&
               this->_kind() == pfSrc->_kind() &&
               this->mf0->_equals(pfSrc->_getArg(0)) &&
               this->mf1->_equals(pfSrc->_getArg(1));
    }

    BasePointer _getArg(size_t nArgNum) const override
    {
        return nArgNum ? this->mf1 : this->mf0;
    }

    const char* _name() const override
    {
#ifdef _DEBUG
        assert(false);
#endif
        static const char sz_exp[] = "";         // no name for binaries
        return sz_exp;
    }
//! @endcond
};


//! @cond INTERNAL
template <typename T>
class Fconst : public BaseFunction<T>
{
private:
    T mdConst;
    bool mbValid;

    Kind _kind() const override {
        return Kind::cfun_const;
    }

protected:
    using BasePointer = typename BaseFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override {
        return this->_clone();
    }

    int _depth(bool) const override {
        return 1;
    }

public:
    Fconst()
      : mdConst(cfun_zero<T>()),
        mbValid(true)
    {
    }

    Fconst(const Fconst& rfSrc)
      : BaseFunction<T>(),
        mdConst(rfSrc.mdConst),
        mbValid(rfSrc.mbValid)
    {
    }

    Fconst(const T& rdSrc)
      : mdConst(rdSrc),
        mbValid(true)
    {
    }

    Fconst(const std::string& sConst, bool bThrow = true)
      : mdConst(cfun_zero<T>()),
        mbValid(Helper<T>::convert(sConst, mdConst, bThrow))
    {
    }

    bool is_valid() const override {
        return mbValid;
    }

    T _value(const T*) const override {
        return mdConst;
    }

    BasePointer _clone() const override {
        return std::make_shared<Fconst<T>>(*this);
    }

    BasePointer _deriv(size_t) const override {
        return std::make_shared<Fconst<T>>();
    }

    std::string _format(int nPrecision) const override {
        return Helper<T>::_format(mdConst, nPrecision);
    }

    bool _equals(const BasePointer& pfSrc) const override {
        return this->_kind() == pfSrc->_kind() && Comparator<T,T>::eq(mdConst, pfSrc->_value(nullptr));
    }

protected:
//! @cond HELPERS
    template<typename U>
    class Helper {
    public:
        static std::string _format(const U& v, int nPrecision)
        {
            std::ostringstream osBuf;
            if (nPrecision > 0) {
                osBuf.precision(nPrecision);
                osBuf.setf(std::ios::scientific | std::ios::showpoint | std::ios::left);
            }
            if (v < cfun_zero<U>()) {
                osBuf << CFUN_O_PARENTH << v << CFUN_C_PARENTH;
            }
            else {
                osBuf << v;
            }
            return osBuf.str();
        }
        static bool convert(const std::string& s, U& u, bool bThrow) throw(cvmexception)
        {
            bool ret = __parse_num<U>(s, u);
            if (bThrow && !ret) {
                BaseFunction<T>::parse_err(s);
            }
            return ret;
        }
    };

    template<typename U>
    class Helper<std::complex<U> > {
        using UC = std::complex<U>;
    public:
        static std::string _format(const UC& v, int nPrecision)
        {
            std::ostringstream osBuf;
            if (nPrecision > 0) {
                osBuf.precision(nPrecision);
                osBuf.setf(std::ios::scientific | std::ios::showpoint | std::ios::left);
            }
            osBuf << v;
            return osBuf.str();
        }
        static bool convert(const std::string& s, UC& uc, bool bThrow) throw(cvmexception)
        {
            bool ret1 = true, ret2 = true;
            std::string sRe, sIm;
            if (__separate(s, 0, s.length(), sRe, sIm) == 1) {
                U re, im;
                ret1 = __parse_num<U>(sRe, re);
                if (bThrow && !ret1) {
                    BaseFunction<T>::parse_err(sRe);
                }
                ret2 = __parse_num<U>(sIm, im);
                if (bThrow && !ret2) {
                    BaseFunction<T>::parse_err(sIm);
                }
                uc = UC(re, im);
            }
            else {
                U re;
                ret1 = __parse_num<U>(s, re);
                if (bThrow && !ret1) {
                    BaseFunction<T>::parse_err(s);
                }
                uc = UC(re, cfun_zero<U>());
            }
            return ret1 && ret2;
        }
    };
//! @endcond
};

template <typename T>
class Finfinity : public BaseFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_infinity;
    }

protected:
    using BasePointer = typename BaseFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override {
        return this->_clone();
    }

    int _depth(bool) const override {
        return 1;
    }

public:
    Finfinity()
      : BaseFunction<T>()
    {
    }

    T _value(const T*) const override {
        return basic_cvmMachMax<T>();
    }

    BasePointer _clone() const {
        return std::make_shared<Finfinity<T>>();
    }

    BasePointer _deriv(size_t) const override {
        return this->_clone();
    }

    std::string _format(int) const override {
        return CFUN_SINF;
    }

    bool _equals(const BasePointer& pfSrc) const override {
        return this->_kind() == pfSrc->_kind();
    }
};

template <typename T>
class Fmninfinity : public BaseFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_mninfinity;
    }

protected:
    using BasePointer = typename BaseFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override {
        return this->_clone();
    }

    int _depth(bool) const override {
        return 1;
    }

public:
    Fmninfinity()
      : BaseFunction<T>()
    {
    }

    T _value(const T*) const override {
        return -basic_cvmMachMax<T>();
    }

    BasePointer _clone() const override {
        return std::make_shared<Fmninfinity<T>>();
    }

    BasePointer _deriv(size_t) const override {
        return this->_clone();
    }

    std::string _format(int) const override {
        return CFUN_SMINF;
    }

    bool _equals(const BasePointer& pfSrc) const override {
        return this->_kind() == pfSrc->_kind();
    }
};

template <typename T>
class Fplus : public BinaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_plus;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename BinaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction
    using PointerPair = shared_ptr_pair<BaseFunction<T>>;

    BasePointer _simpl() const override
    {
        int i, j, k;
        BasePointer fZero = std::make_shared<Fconst<T>>();
        PointerPair fCopy(this->mf0->_simp(), this->mf1->_simp());

        for (i = 0; i <= 1; ++i)
        {
            if (fCopy[i]->_equals(fZero))
            {                                                               // 0 + f(x) = f(x) | f(x) + 0 = f(x)
                return fCopy[__not(i)];
            }
        }
        for (i = 0; i <= 1; ++i)
        {
            if (fCopy[i]->_kind() == Kind::cfun_infinity)
            {                                                               // inf + f(x) = inf | f(x) + inf = inf
                return std::make_shared<Finfinity<T>>();
            }
        }
        for (i = 0; i <= 1; ++i)
        {
            if (fCopy[i]->_kind() == Kind::cfun_mninfinity)
            {                                                               // -inf + f(x) = -inf | f(x) + -inf = -inf
                return std::make_shared<Fmninfinity<T>>();
            }
        }
        if (fCopy[0]->_kind() == Kind::cfun_const &&
            fCopy[1]->_kind() == Kind::cfun_const)
        {                                                                   // 1 + 2 = 3
            return std::make_shared<Fconst<T>>(fCopy[0]->_value(nullptr) + fCopy[1]->_value(nullptr));
        }
        for (i = 0; i <= 1; ++i)
        {
            if (fCopy[i]->_kind() == Kind::cfun_const)
            {                                                               // x + (-1) = x-1
                T val = fCopy[i]->_value(nullptr);
                if (Comparator<T,T>::lt(val, cfun_zero<T>()))
                {
                    return std::make_shared<Fminus<T>>(fCopy[__not(i)], std::make_shared<Fconst<T>>(-val))->_simp();
                }
            }
        }
        if (fCopy[0]->_equals(fCopy[1]))
        {                                                                   // x+x = 2*x
            return std::make_shared<Fmult<T>>(std::make_shared<Fconst<T>>(cfun_two<T>()), fCopy[0])->_simp();
        }

        for (i = 0; i <= 1; ++i)
        {
            if (fCopy[i]->_kind() == Kind::cfun_uminus)
            {                                                               // -a + b = b - a     a + -b = a - b
                return std::make_shared<Fminus<T>>(fCopy[__not(i)], fCopy[i]->_getArg(0))->_simp();
            }
        }

        if (fCopy[0]->_kind() == Kind::cfun_mult &&
            fCopy[1]->_kind() == Kind::cfun_mult)
        {
            for (i = 0; i <= 1; ++i)
            {                                                               // C*A+C*B = C*(A+B)
                for (j = 0; j <= 1; ++j)
                {                                                           // C*A+B*C = C*(A+B)
                    if (fCopy[0]->_getArg(i)->_equals(fCopy[1]->_getArg(j)))
                    {                                                       // A*C+C*B = C*(A+B)
                        return
                        std::make_shared<Fmult<T>>(fCopy[0]->_getArg(i),    // A*C+B*C = C*(A+B)
                                                   std::make_shared<Fplus<T>>(fCopy[0]->_getArg(__not(i)),
                                                                              fCopy[1]->_getArg(__not(j))))->_simp();
                    }
                }
            }
        }
        for (j = 0; j <= 1; ++j)
        {
            if (fCopy[j]->_kind() == Kind::cfun_mult)
            {                                                               // a*x + x = (a+1)*x
                for (i = 0; i <= 1; ++i)
                {
                    if (fCopy[j]->_getArg(i)->_equals(fCopy[__not(j)]))
                    {
                        return
                        std::make_shared<Fmult<T>>(fCopy[__not(j)],
                                                   std::make_shared<Fplus<T>>(fCopy[j]->_getArg(__not(i)),
                                                                              std::make_shared<Fconst<T>>(cfun_one<T>())))->_simp();
                    }
                }
            }
        }
        if (fCopy[0]->_kind() == Kind::cfun_div &&
            fCopy[1]->_kind() == Kind::cfun_div &&                              // x/a+y/a = (x+y)/a
            fCopy[0]->_getArg(1)->_equals(fCopy[1]->_getArg(1)))
        {
            return
            std::make_shared<Fdiv<T>>(std::make_shared<Fplus<T>>(fCopy[0]->_getArg(0),
                                                                 fCopy[1]->_getArg(0)),
                                      fCopy[0]->_getArg(1))->_simp();
        }
        for (j = 0; j <= 1; ++j)
        {
            if (fCopy[j]->_kind() == Kind::cfun_plus)
            {                                                               // x+a + a
                for (i = 0; i <= 1; ++i)
                {
                    BasePointer fTmp =
                    std::make_shared<Fplus<T>>(fCopy[j]->_getArg(i),
                                               fCopy[__not(j)]);
                    BasePointer fTmps(fTmp->_simp());
                    if (!fTmp->_equals(fTmps))
                    {
                        return
                        std::make_shared<Fplus<T>>(fTmps,
                                                   fCopy[j]->_getArg(__not(i)))->_simp();
                    }
                }
                if (fCopy[__not(j)]->_kind() == Kind::cfun_plus)
                {                                                           // x+a + x+b
                    for (i = 0; i <= 1; ++i)
                    {
                        for (k = 0; k <= 1; ++k)
                        {
                            BasePointer fTmp =
                            std::make_shared<Fplus<T>>(fCopy[j]->_getArg(i),
                                                       fCopy[__not(j)]->_getArg(k));
                            BasePointer fTmps(fTmp->_simp());
                            if (!fTmp->_equals(fTmps))
                            {
                                return
                                std::make_shared<Fplus<T>>(fTmps,
                                                           std::make_shared<Fplus<T>>(fCopy[j]->_getArg(__not(i)),
                                                                                      fCopy[__not(j)]->_getArg(__not(k))))->_simp();
                            }
                        }
                    }
                }
                if (fCopy[__not(j)]->_kind() == Kind::cfun_minus)
                {                                                           // x+a + x-b | x-a + x+b
                    for (i = 0; i <= 1; ++i)
                    {
                        BasePointer fTmp =
                        std::make_shared<Fminus<T>>(fCopy[j]->_getArg(i),
                                                    fCopy[__not(j)]->_getArg(1));
                        BasePointer fTmps(fTmp->_simp());
                        if (!fTmp->_equals(fTmps))
                        {
                            return
                            std::make_shared<Fplus<T>>(fTmps,
                                                       std::make_shared<Fplus<T>>(fCopy[j]->_getArg(__not(i)),
                                                                                  fCopy[__not(j)]->_getArg(0)))->_simp();
                        }
                    }
                    for (i = 0; i <= 1; ++i)
                    {
                        BasePointer fTmp =
                        std::make_shared<Fplus<T>>(fCopy[j]->_getArg(i),
                                                   fCopy[__not(j)]->_getArg(0));
                        BasePointer fTmps(fTmp->_simp());
                        if (!fTmp->_equals(fTmps))
                        {
                            return
                            std::make_shared<Fplus<T>>(fTmps,
                                                       std::make_shared<Fminus<T>>(fCopy[j]->_getArg(__not(i)),
                                                                                   fCopy[__not(j)]->_getArg(1)))->_simp();
                        }
                    }
                }
            }
            if (fCopy[j]->_kind() == Kind::cfun_minus)
            {
                {                                                           // a-x + a
                    BasePointer fTmp =
                    std::make_shared<Fplus<T>>(fCopy[j]->_getArg(0),
                                               fCopy[__not(j)]);
                    BasePointer fTmps(fTmp->_simp());
                    if (!fTmp->_equals(fTmps))
                    {
                        return
                        std::make_shared<Fminus<T>>(fTmps,
                                                    fCopy[j]->_getArg(1))->_simp();
                    }
                }
                {
                    BasePointer fTmp =
                    std::make_shared<Fminus<T>>(fCopy[__not(j)],            // x-a + a
                                                fCopy[j]->_getArg(1));
                    BasePointer fTmps(fTmp->_simp());
                    if (!fTmp->_equals(fTmps))
                    {
                        return
                        std::make_shared<Fplus<T>>(fTmps,
                                                   fCopy[j]->_getArg(0))->_simp();
                    }
                }

                if (fCopy[__not(j)]->_kind() == Kind::cfun_minus)
                {                                                           // x-a + x-b
                    for (i = 0; i <= 1; ++i)
                    {
                        BasePointer fTmp =
                        std::make_shared<Fplus<T>>(fCopy[j]->_getArg(i),
                                                   fCopy[__not(j)]->_getArg(i));
                        BasePointer fTmps(fTmp->_simp());
                        if (!fTmp->_equals(fTmps))
                        {
                            BasePointer fTmp =
                            i ?
                            std::make_shared<Fminus<T>>(std::make_shared<Fplus<T>>(fCopy[j]->_getArg(0),
                                                                                   fCopy[__not(j)]->_getArg(0)),
                                                        fTmps)
                            :
                            std::make_shared<Fminus<T>>(fTmps,
                                                        std::make_shared<Fplus<T>>(fCopy[j]->_getArg(1),
                                                                                   fCopy[__not(j)]->_getArg(1)));
                            return fTmp->_simp();
                        }
                    }
                    BasePointer fTmp =
                    std::make_shared<Fminus<T>>(fCopy[j]->_getArg(0),       // x-a + b-x
                                                fCopy[__not(j)]->_getArg(1));
                    BasePointer fTmps(fTmp->_simp());
                    if (!fTmp->_equals(fTmps))
                    {
                        return
                        std::make_shared<Fplus<T>>(std::make_shared<Fminus<T>>(fCopy[__not(j)]->_getArg(0),
                                                                               fCopy[j]->_getArg(1)),
                                                   fTmps)->_simp();
                    }
                }
            }
        }
        return std::make_shared<Fplus<T>>(fCopy[0], fCopy[1]);
    }

public:
    Fplus(const BasePointer& pfSrc0, const BasePointer& pfSrc1)
      : BinaryFunction<T>(pfSrc0, pfSrc1)
    {
    }

    Fplus(const std::string& sAddend1, const std::string& sAddend2,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sAddend1, sAddend2, saVars, saParameters, saMeanings)
    {
    }

    Fplus(const std::string& sArgs, size_t nF1, size_t nL1, size_t nF2, size_t nL2,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sArgs, nF1, nL1, nF2, nL2, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return this->mf0->_value(pdVars) + this->mf1->_value(pdVars);
    }

    BasePointer _clone() const override {
        return std::make_shared<Fplus<T>>(this->mf0, this->mf1);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fplus<T>>(this->mf0->_deriv(nVarNum),
                                   this->mf1->_deriv(nVarNum))->_simp();
    }

    std::string _format(int nPrecision) const override
    {
        std::ostringstream osBuf;
        osBuf << this->mf0->_format(nPrecision) << CFUN_SPLUS;
        if (this->mf1->_kind() == Kind::cfun_uminus) {
            osBuf << CFUN_O_PARENTH << this->mf1->_format(nPrecision) << CFUN_C_PARENTH;
        }
        else {
            osBuf << this->mf1->_format(nPrecision);
        }
        return osBuf.str();
    }
};

template <typename T>
class Fminus : public BinaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_minus;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename BinaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction
    using PointerPair = shared_ptr_pair<BaseFunction<T>>;

    BasePointer _simpl() const override
    {
        int i, j, k;
        BasePointer fZero = std::make_shared<Fconst<T>>();
        PointerPair fCopy(this->mf0->_simp(), this->mf1->_simp());
        if (fCopy[0]->_equals(fZero))
        {                                                                   // 0 - x = -x
            return std::make_shared<Fuminus<T>>(fCopy[1])->_simp();
        }
        if (fCopy[1]->_equals(fZero))                                       // x - 0 = x
        {
            return fCopy[0];
        }
        if (fCopy[0]->_kind() == Kind::cfun_infinity)                           // inf - x = inf
        {
            return std::make_shared<Finfinity<T>>();
        }
        if (fCopy[1]->_kind() == Kind::cfun_mninfinity)                         // x - -inf = inf
        {
            return std::make_shared<Finfinity<T>>();
        }
        if (fCopy[0]->_kind() == Kind::cfun_mninfinity)                         // -inf - x = -inf
        {
            return std::make_shared<Fmninfinity<T>>();
        }
        if (fCopy[1]->_kind() == Kind::cfun_infinity)                           // x - inf = -inf
        {
            return std::make_shared<Fmninfinity<T>>();
        }
        if (fCopy[0]->_kind() == Kind::cfun_const &&
            fCopy[1]->_kind() == Kind::cfun_const)                              // 3 - 2 = 1
        {
            return std::make_shared<Fconst<T>>(fCopy[0]->_value(nullptr)-
                                               fCopy[1]->_value(nullptr));
        }
        if (fCopy[0]->_equals(fCopy[1]))                                    // x - x = 0
        {
            return std::make_shared<Fconst<T>>();
        }
        if (fCopy[0]->_kind() == Kind::cfun_uminus)                             // -a - b = -(a + b)
        {
            return
            std::make_shared<Fuminus<T>>(std::make_shared<Fplus<T>>(fCopy[0]->_getArg(0),
                                                                    fCopy[1])->_simp())->_simp();
        }
        if (fCopy[1]->_kind() == Kind::cfun_uminus)                              // a - -b = a + b
        {
            return
            std::make_shared<Fplus<T>>(fCopy[0],
                                       fCopy[1]->_getArg(0))->_simp();
        }
        if (fCopy[0]->_kind() == Kind::cfun_mult &&
            fCopy[1]->_kind() == Kind::cfun_mult)
        {
            for (i = 0; i <= 1; ++i)                                        // C*A-C*B = C*(A-B)
            {
                for (j = 0; j <= 1; ++j)                                    // C*A-B*C = C*(A-B)
                {
                    if (fCopy[0]->_getArg(i)->_equals(fCopy[1]->_getArg(j))) // A*C-C*B = C*(A-B)
                    {
                        return
                        std::make_shared<Fmult<T>>(fCopy[0]->_getArg(i),    // A*C-B*C = C*(A-B)
                                                  std::make_shared<Fminus<T>>(fCopy[0]->_getArg(__not(i)),
                                                                              fCopy[1]->_getArg(__not(j))))->_simp();
                    }
                }
            }
        }
        for (j = 0; j <= 1; ++j)
        {
            if (fCopy[j]->_kind() == Kind::cfun_mult)
            {                                                               // a*x - x = (a-1)*x
                for (i = 0; i <= 1; ++i)
                {
                    if (fCopy[j]->_getArg(i)->_equals(fCopy[__not(j)]))
                    {
                        BasePointer fTmp =
                        std::make_shared<Fmult<T>>(fCopy[__not(j)],
                                                   j ?
                                                   std::make_shared<Fminus<T>>(std::make_shared<Fconst<T>>(cfun_one<T>()),
                                                                               fCopy[j]->_getArg(__not(i)))
                                                   :
                                                   std::make_shared<Fminus<T>>(fCopy[j]->_getArg(__not(i)),
                                                                               std::make_shared<Fconst<T>>(cfun_one<T>())));
                        return fTmp->_simp();
                    }
                }
            }
        }
        if (fCopy[0]->_kind() == Kind::cfun_div &&
            fCopy[1]->_kind() == Kind::cfun_div &&                              // x/a-y/a = (x+y)/a
            fCopy[0]->_getArg(1)->_equals(fCopy[1]->_getArg(1)))
        {
            return
            std::make_shared<Fdiv<T>>(std::make_shared<Fminus<T>>(fCopy[0]->_getArg(0),
                                                                  fCopy[1]->_getArg(0)),
                                      fCopy[0]->_getArg(1))->_simp();
        }
        for (j = 0; j <= 1; ++j)
        {
            if (fCopy[j]->_kind() == Kind::cfun_plus)                           // [x+a] - a  |  a - [x+a]
            {
                for (i = 0; i <= 1; ++i)
                {
                    BasePointer fTmp = std::make_shared<Fminus<T>>(fCopy[j]->_getArg(i),
                                                                   fCopy[__not(j)]);
                    BasePointer fTmps(fTmp->_simp());
                    if (!fTmp->_equals(fTmps))
                    {
                        if (j)
                        {
                            return
                            std::make_shared<Fminus<T>>(fTmps,
                                                        fCopy[j]->_getArg(__not(i)))->_simp();
                        }
                        else {
                            return
                            std::make_shared<Fplus<T>>(fCopy[j]->_getArg(__not(i)),
                                                       fTmps)->_simp();
                        }
                    }
                }
                if (fCopy[__not(j)]->_kind() == Kind::cfun_plus)                // x+a - x+b
                {
                    for (i = 0; i <= 1; ++i)
                    {
                        for (k = 0; k <= 1; ++k)
                        {
                            BasePointer fTmp = std::make_shared<Fminus<T>>(fCopy[j]->_getArg(i),
                                                                           fCopy[__not(j)]->_getArg(k));
                            BasePointer fTmps(fTmp->_simp());
                            if (!fTmp->_equals(fTmps))
                            {
                                return
                                std::make_shared<Fminus<T>>(fTmps,
                                                            std::make_shared<Fplus<T>>(fCopy[j]->_getArg(__not(i)),
                                                                                       fCopy[__not(j)]->_getArg(__not(k))))->_simp();
                            }
                        }
                    }
                }
                if (fCopy[__not(j)]->_kind() == Kind::cfun_minus)               // x+a - x-b | x-a - x+b
                {
                    if (!j)
                    {
                        return
                        std::make_shared<Fplus<T>>(fCopy[0],
                                                   std::make_shared<Fminus<T>>(fCopy[1]->_getArg(1),
                                                                               fCopy[1]->_getArg(0)))->_simp();
                    }
                    else                                                    // j==1, i.e.    x-a - x+b
                    {
                        for (i = 0; i <= 1; ++i)
                        {
                            BasePointer fTmp = std::make_shared<Fminus<T>>(fCopy[0]->_getArg(0),
                                                                           fCopy[1]->_getArg(i));
                            BasePointer fTmps(fTmp->_simp());
                            if (!fTmp->_equals(fTmps))
                            {
                                return
                                std::make_shared<Fminus<T>>(fTmps,
                                                            std::make_shared<Fplus<T>>(fCopy[0]->_getArg(1),
                                                                                       fCopy[1]->_getArg(__not(i))))->_simp();
                            }
                        }
                        for (i = 0; i <= 1; ++i)                            // a-x - x+b
                        {
                            BasePointer fTmp = std::make_shared<Fplus<T>>(fCopy[0]->_getArg(1),
                                                                          fCopy[1]->_getArg(i));
                            BasePointer fTmps(fTmp->_simp());
                            if (!fTmp->_equals(fTmps))
                            {
                                return
                                std::make_shared<Fminus<T>>(std::make_shared<Fminus<T>>(fCopy[0]->_getArg(0),
                                                                                        fCopy[1]->_getArg(__not(i))),
                                                            fTmps)->_simp();
                            }
                        }
                    }
                }
            }
            if (fCopy[j]->_kind() == Kind::cfun_minus)                          // a-x - a  |  a - a-x
            {
                if (j)                                                      // a - a-x == a + x-a
                {
                    return
                    std::make_shared<Fplus<T>>(fCopy[0],
                                               std::make_shared<Fminus<T>>(fCopy[1]->_getArg(1),
                                                                           fCopy[1]->_getArg(0)))->_simp();
                }
                else
                {
                    {                                                       // j==0 i.e. a-x - a
                        BasePointer fTmp = std::make_shared<Fminus<T>>(fCopy[j]->_getArg(0),
                                                                       fCopy[__not(j)]);
                        BasePointer fTmps(fTmp->_simp());
                        if (!fTmp->_equals(fTmps))
                        {
                            return
                            std::make_shared<Fminus<T>>(fTmps,
                                                        fCopy[j]->_getArg(1))->_simp();
                        }
                    }

                    {                                                       // x-a - a
                        BasePointer fTmp = std::make_shared<Fplus<T>>(fCopy[j]->_getArg(1),
                                                                      fCopy[__not(j)]);
                        BasePointer fTmps(fTmp->_simp());
                        if (!fTmp->_equals(fTmps))
                        {
                            return
                            std::make_shared<Fminus<T>>(fCopy[j]->_getArg(0),
                                                        fTmps)->_simp();
                        }
                    }
                }
            }
        }
        return std::make_shared<Fminus<T>>(fCopy[0], fCopy[1]);
    }

public:
    Fminus(const BasePointer& pfSrc0, const BasePointer& pfSrc1)
      : BinaryFunction<T>(pfSrc0, pfSrc1)
    {
    }

    Fminus(const std::string& sMinuend, const std::string& sSubtrahend,
           const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sMinuend, sSubtrahend, saVars, saParameters, saMeanings)
    {
    }

    Fminus(const std::string& sArgs, size_t nF1, size_t nL1, size_t nF2, size_t nL2,
           const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sArgs, nF1, nL1, nF2, nL2, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return this->mf0->_value(pdVars) - this->mf1->_value(pdVars);
    }

    BasePointer _clone() const override {
        return std::make_shared<Fminus<T>>(this->mf0, this->mf1);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fminus<T>>(this->mf0->_deriv(nVarNum),
                                    this->mf1->_deriv(nVarNum))->_simp();
    }

    std::string _format(int nPrecision) const override
    {
        std::ostringstream osBuf;
        osBuf << this->mf0->_format(nPrecision) << CFUN_SMINUS;
        if (this->mf1->_kind() == Kind::cfun_uminus) {
            osBuf << CFUN_O_PARENTH << this->mf1->_format(nPrecision) << CFUN_C_PARENTH;
        }
        else {
            osBuf << this->mf1->_format(nPrecision);
        }
        return osBuf.str();
    }
};

template <typename T>
class Fmult : public BinaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_mult;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename BinaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction
    using PointerPair = shared_ptr_pair<BaseFunction<T>>;

    BasePointer _simpl() const override
    {
        int i, j, k;
        BasePointer fZero = std::make_shared<Fconst<T>>();
        BasePointer fOne = std::make_shared<Fconst<T>>(cfun_one<T>());
        BasePointer fMOne = std::make_shared<Fconst<T>>(cfun_mone<T>());
        PointerPair fCopy(this->mf0->_simp(), this->mf1->_simp());
        for (i = 0; i <= 1; ++i)
        {
            if (fCopy[i]->_kind() == Kind::cfun_infinity)
            {
                return std::make_shared<Finfinity<T>>();
            }
            if (fCopy[i]->_kind() == Kind::cfun_mninfinity)
            {
                return std::make_shared<Fmninfinity<T>>();
            }
        }
        if (fCopy[0]->_equals(fZero) ||
            fCopy[1]->_equals(fZero))                                       // 0 * x = 0 | x * 0 = 0
        {
            return fZero;
        }
        for (i = 0; i <= 1; ++i)
        {
            if (fCopy[i]->_equals(fOne)) {                                  // 1 * x = x
                return fCopy[__not(i)];
            }
        }
        for (i = 0; i <= 1; ++i)
        {
            if (fCopy[i]->_equals(fMOne))                                   // -1 * x = -x
            {
                return
                std::make_shared<Fuminus<T>>(fCopy[__not(i)])->_simp();
            }
        }
        if (fCopy[0]->_kind() == Kind::cfun_const &&
            fCopy[1]->_kind() == Kind::cfun_const)                              // 3 * 2 = 6
        {
            return std::make_shared<Fconst<T>>(fCopy[0]->_value(nullptr) * fCopy[1]->_value(nullptr));
        }
        if (fCopy[0]->_equals(fCopy[1]))                                    // x * x = x^2
        {
            return
            std::make_shared<Fpower<T>>(fCopy[0],
                                        std::make_shared<Fconst<T>>(cfun_two<T>()))->_simp();
        }
        if (fCopy[0]->_kind() == Kind::cfun_uminus &&
            fCopy[1]->_kind() == Kind::cfun_uminus)                             // (-a)*(-b) = a*b)
        {
            return
            std::make_shared<Fmult<T>>(fCopy[0]->_getArg(0),
                                       fCopy[1]->_getArg(0))->_simp();
        }
        for (i = 0; i <= 1; ++i)
        {
            if (fCopy[i]->_kind() == Kind::cfun_uminus)                         // (-a)*b = -a*b
            {
                return
                std::make_shared<Fuminus<T>>(std::make_shared<Fmult<T>>(fCopy[i]->_getArg(0),
                                                                        fCopy[__not(i)]))->_simp();
            }
        }
        if (fCopy[0]->_kind() == Kind::cfun_power &&                            // x^a*x^b=x^(a+b)
            fCopy[1]->_kind() == Kind::cfun_power &&
            fCopy[0]->_getArg(0)->_equals(fCopy[1]->_getArg(0)))
        {
            return
            std::make_shared<Fpower<T>>(fCopy[0]->_getArg(0),
                                        std::make_shared<Fplus<T>>(fCopy[0]->_getArg(1),
                                                                   fCopy[1]->_getArg(1)))->_simp();
        }
        if (fCopy[0]->_kind() == Kind::cfun_exp &&                              // exp(a)*exp(b)=exp(a+b)
            fCopy[1]->_kind() == Kind::cfun_exp)
        {
            return
            std::make_shared<Fexp<T>>(std::make_shared<Fplus<T>>(fCopy[0]->_getArg(0),
                                                                 fCopy[1]->_getArg(0)))->_simp();
        }
        if (fCopy[0]->_kind() == Kind::cfun_power &&                            // x^a*x=x^(a+1)
            fCopy[0]->_getArg(0)->_equals(fCopy[1]))
        {
            return
            std::make_shared<Fpower<T>>(fCopy[1],
                                        std::make_shared<Fplus<T>>(fCopy[0]->_getArg(1),
                                                                   std::make_shared<Fconst<T>>(cfun_one<T>())))->_simp();
        }
        if (fCopy[1]->_kind() == Kind::cfun_power &&                            // x*x^a=x^(a+1)
            fCopy[1]->_getArg(0)->_equals(fCopy[0]))
        {
            return
            std::make_shared<Fpower<T>>(fCopy[0],
                                        std::make_shared<Fplus<T>>(fCopy[1]->_getArg(1),
                                                                   fOne))->_simp();
        }

        for (j = 0; j <= 1; ++j)
        {
            if (fCopy[j]->_kind() == Kind::cfun_mult)                           // x*a * a
            {
                for (i = 0; i <= 1; ++i)
                {
                    BasePointer fTmp = std::make_shared<Fmult<T>>(fCopy[j]->_getArg(i),
                                                                  fCopy[__not(j)]);
                    BasePointer fTmps(fTmp->_simp());
                    if (!fTmp->_equals(fTmps))
                    {
                        return
                        std::make_shared<Fmult<T>>(fTmps,
                                                   fCopy[j]->_getArg(__not(i)))->_simp();
                    }
                }
                if (fCopy[__not(j)]->_kind() == Kind::cfun_mult)                // x*a * x*b
                {
                    for (i = 0; i <= 1; ++i)
                    {
                        for (k = 0; k <= 1; ++k)
                        {
                            BasePointer fTmp = std::make_shared<Fmult<T>>(fCopy[j]->_getArg(i),
                                                                          fCopy[__not(j)]->_getArg(k));
                            BasePointer fTmps(fTmp->_simp());
                            if (!fTmp->_equals(fTmps))
                            {
                                return
                                std::make_shared<Fmult<T>>(fTmps,
                                                           std::make_shared<Fmult<T>>(fCopy[j]->_getArg(__not(i)),
                                                                                      fCopy[__not(j)]->_getArg(__not(k))))->_simp();
                            }
                        }
                    }
                }
                if (fCopy[__not(j)]->_kind() == Kind::cfun_div)                 // x*a * x/b | x/a * x*b
                {

                    for (i = 0; i <= 1; ++i)
                    {
                        BasePointer fTmp = std::make_shared<Fdiv<T>>(fCopy[j]->_getArg(i),
                                                                     fCopy[__not(j)]->_getArg(1));
                        BasePointer fTmps(fTmp->_simp());
                        if (!fTmp->_equals(fTmps))
                        {
                            return
                            std::make_shared<Fmult<T>>(fTmps,
                                                       std::make_shared<Fmult<T>>(fCopy[j]->_getArg(__not(i)),
                                                                                  fCopy[__not(j)]->_getArg(0)))->_simp();
                        }
                    }
                    for (i = 0; i <= 1; ++i)
                    {
                        BasePointer fTmp = std::make_shared<Fmult<T>>(fCopy[j]->_getArg(i),
                                                                      fCopy[__not(j)]->_getArg(0));
                        BasePointer fTmps(fTmp->_simp());
                        if (!fTmp->_equals(fTmps))
                        {
                            return
                            std::make_shared<Fmult<T>>(fTmps,
                                                       std::make_shared<Fdiv<T>>(fCopy[j]->_getArg(__not(i)),
                                                                                 fCopy[__not(j)]->_getArg(1)))->_simp();
                        }
                    }
                }
            }
            if (fCopy[j]->_kind() == Kind::cfun_div)                            // a/x * a
            {
                {
                    BasePointer fTmp = std::make_shared<Fmult<T>>(fCopy[j]->_getArg(0),
                                                                  fCopy[__not(j)]);
                    BasePointer fTmps(fTmp->_simp());
                    if (!fTmp->_equals(fTmps))
                    {
                        return
                        std::make_shared<Fdiv<T>>(fTmps,
                                                  fCopy[j]->_getArg(1))->_simp();
                    }
                }
                {                                                           // x/a * a
                    BasePointer fTmp = std::make_shared<Fdiv<T>>(fCopy[__not(j)],
                                                                 fCopy[j]->_getArg(1));
                    BasePointer fTmps(fTmp->_simp());
                    if (!fTmp->_equals(fTmps))
                    {
                        return
                        std::make_shared<Fmult<T>>(fTmps,
                                                   fCopy[j]->_getArg(0))->_simp();
                    }
                }

                if (fCopy[__not(j)]->_kind() == Kind::cfun_div)                 //   x/a * x/b
                {
                    for (i = 0; i <= 1; ++i)
                    {
                        BasePointer fTmp = std::make_shared<Fmult<T>>(fCopy[j]->_getArg(i),
                                                                      fCopy[__not(j)]->_getArg(i));
                        BasePointer fTmps(fTmp->_simp());
                        if (!fTmp->_equals(fTmps))
                        {
                            BasePointer fTmp =
                            i ?
                            std::make_shared<Fdiv<T>>(std::make_shared<Fmult<T>>(fCopy[j]->_getArg(0),
                                                                                 fCopy[__not(j)]->_getArg(0)),
                                                      fTmps)
                            :
                            std::make_shared<Fdiv<T>>(fTmps,
                                                      std::make_shared<Fmult<T>>(fCopy[j]->_getArg(1),
                                                                                 fCopy[__not(j)]->_getArg(1)));
                            return fTmp->_simp();
                        }
                    }                                                       // x/a * b/x
                    BasePointer fTmp = std::make_shared<Fdiv<T>>(fCopy[j]->_getArg(0),
                                                                 fCopy[__not(j)]->_getArg(1));
                    BasePointer fTmps(fTmp->_simp());
                    if (!fTmp->_equals(fTmps))
                    {
                        return
                        std::make_shared<Fmult<T>>(std::make_shared<Fdiv<T>>(fCopy[__not(j)]->_getArg(0),
                                                                             fCopy[j]->_getArg(1)),
                                                   fTmps)->_simp();
                    }
                }
            }
        }
        return std::make_shared<Fmult<T>>(fCopy[0], fCopy[1]);
    }

public:
    Fmult(const BasePointer& pfSrc0, const BasePointer& pfSrc1)
      : BinaryFunction<T>(pfSrc0, pfSrc1)
    {
    }

    Fmult(const std::string& sMultiplicand, const std::string& sMultiplier,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sMultiplicand, sMultiplier, saVars, saParameters, saMeanings)
    {
    }

    Fmult(const std::string& sArgs, size_t nF1, size_t nL1, size_t nF2, size_t nL2,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sArgs, nF1, nL1, nF2, nL2, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return this->mf0->_value(pdVars) * this->mf1->_value(pdVars);
    }

    BasePointer _clone() const override {
        return std::make_shared<Fmult<T>>(this->mf0, this->mf1);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fplus<T>>(std::make_shared<Fmult<T>>(this->mf0->_deriv(nVarNum),
                                                              this->mf1),
                                   std::make_shared<Fmult<T>>(this->mf0,
                                                              this->mf1->_deriv(nVarNum)))->_simp();
    }

    std::string _format(int nPrecision) const override
    {
        std::ostringstream osBuf;
        switch (this->mf0->_kind()) {
        case Kind::cfun_plus:
        case Kind::cfun_minus:
            osBuf << CFUN_O_SPARENTH << this->mf0->_format(nPrecision) << CFUN_C_PARENTH << CFUN_SMULT;
            break;
        default:
            osBuf << this->mf0->_format(nPrecision) << CFUN_SMULT;
            break;
        }
        switch (this->mf1->_kind()) {
        case Kind::cfun_plus:
        case Kind::cfun_minus:
        case Kind::cfun_uminus:
            osBuf << CFUN_O_SPARENTH << this->mf1->_format(nPrecision) << CFUN_C_PARENTH;
            break;
        default:
            osBuf << this->mf1->_format(nPrecision);
            break;
        }
        return osBuf.str();
    }
};

template <typename T>
class Fdiv : public BinaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_div;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename BinaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction
    using PointerPair = shared_ptr_pair<BaseFunction<T>>;

    BasePointer _simpl() const override
    {
        int i, j, k;
        BasePointer fZero = std::make_shared<Fconst<T>>();
        BasePointer fOne = std::make_shared<Fconst<T>>(cfun_one<T>());
        BasePointer fMOne = std::make_shared<Fconst<T>>(cfun_mone<T>());
        PointerPair fCopy(this->mf0->_simp(), this->mf1->_simp());
        if (fCopy[0]->_equals(fZero))                                       // 0 / x = 0
        {
            return fZero;
        }
        if (fCopy[1]->_equals(fZero))
        {                                                                   // x / 0 = inf
            return std::make_shared<Finfinity<T>>();
        }
        if (fCopy[1]->_kind() == Kind::cfun_infinity ||
            fCopy[1]->_kind() == Kind::cfun_mninfinity)                         // x / inf = 0 | x / -inf = 0
        {
            return fZero;
        }
        if (fCopy[1]->_equals(fOne))
        {                                                                   // x / 1 = x
            return fCopy[0];
        }
        if (fCopy[1]->_equals(fMOne))
        {                                                                   // x / -1 = -x
            return std::make_shared<Fuminus<T>>(fCopy[0]);
        }
        if (fCopy[0]->_kind() == Kind::cfun_const &&
            fCopy[1]->_kind() == Kind::cfun_const)
        {                                                                   // 6 / 2 = 3
            T dNomin = fCopy[0]->_value(nullptr);
            T dDenom = fCopy[1]->_value(nullptr);
            if (cvm::_abs(dDenom) <= cvm::cvmMachMin())
            {
                if (Comparator<T,T>::lt(dNomin, cfun_zero<T>()))
                {
                    return std::make_shared<Fmninfinity<T>>();
                }
                else
                {
                    return std::make_shared<Finfinity<T>>();
                }
            }
            else
            {
                return std::make_shared<Fconst<T>>(dNomin / dDenom);
            }
        }
        if (fCopy[0]->_equals(fCopy[1]))
        {                                                                   // x / x = 1
            return std::make_shared<Fconst<T>>(cfun_one<T>());
        }
        if (fCopy[0]->_kind() == Kind::cfun_uminus &&
            fCopy[1]->_kind() == Kind::cfun_uminus)
        {                                                                   // (-a)/(-b) = a/b)
            return
            std::make_shared<Fdiv<T>>(fCopy[0]->_getArg(0),
                                      fCopy[1]->_getArg(0))->_simp();
        }
        for (i = 0; i <= 1; ++i)
        {
            if (fCopy[i]->_kind() == Kind::cfun_uminus)
            {                                                               // (-a)/b = -a/b
                BasePointer fTmp =
                i ?
                std::make_shared<Fuminus<T>>(std::make_shared<Fdiv<T>>(fCopy[__not(i)],
                                                                       fCopy[i]->_getArg(0)))
                :
                std::make_shared<Fuminus<T>>(std::make_shared<Fdiv<T>>(fCopy[i]->_getArg(0),
                                                                       fCopy[__not(i)]));
                return fTmp->_simp();
            }
        }
        if (fCopy[0]->_kind() == Kind::cfun_power &&                            // x^a/x^b=x^(a-b)
            fCopy[1]->_kind() == Kind::cfun_power &&
            fCopy[0]->_getArg(0)->_equals(fCopy[1]->_getArg(0)))
        {
            return
            std::make_shared<Fpower<T>>(fCopy[0]->_getArg(0),
                                        std::make_shared<Fminus<T>>(fCopy[0]->_getArg(1),
                                                                    fCopy[1]->_getArg(1)))->_simp();
        }
        if (fCopy[0]->_kind() == Kind::cfun_power &&                            // x^a/x=x^(a-1)
            fCopy[0]->_getArg(0)->_equals(fCopy[1]))
        {
            return
            std::make_shared<Fpower<T>>(fCopy[1],
                                        std::make_shared<Fminus<T>>(fCopy[0]->_getArg(1),
                                                                    std::make_shared<Fconst<T>>(cfun_one<T>())))->_simp();
        }
        if (fCopy[1]->_kind() == Kind::cfun_power &&                            // x/x^a=x^(1-a)
            fCopy[1]->_getArg(0)->_equals(fCopy[0]))
        {
            return
            std::make_shared<Fpower<T>>(fCopy[0],
                                        std::make_shared<Fminus<T>>(std::make_shared<Fconst<T>>(cfun_one<T>()),
                                                                    fCopy[1]->_getArg(1)))->_simp();
        }

        for (j = 0; j <= 1; ++j)
        {
            if (fCopy[j]->_kind() == Kind::cfun_mult)
            {                                                               // [x*a] / a  |  a / [x*a]
                for (i = 0; i <= 1; ++i)
                {
                    BasePointer pNum = fCopy[j]->_getArg(i);
                    BasePointer pDen = fCopy[__not(j)];
                    BasePointer fTmp = std::make_shared<Fdiv<T>>(j ? pDen : pNum,
                                                                 j ? pNum : pDen);
                    BasePointer fTmps(fTmp->_simp());
                    if (!fTmp->_equals(fTmps))
                    {
                        if (j)
                        {
                            return
                            std::make_shared<Fdiv<T>>(fTmps,
                                                      fCopy[j]->_getArg(__not(i)))->_simp();
                        }
                        else
                        {
                            return
                            std::make_shared<Fmult<T>>(fCopy[j]->_getArg(__not(i)),
                                                       fTmps)->_simp();
                        }
                    }
                }
                if (fCopy[__not(j)]->_kind() == Kind::cfun_mult)
                {                                                           //   x*a / x*b
                    for (i = 0; i <= 1; ++i)
                    {
                        for (k = 0; k <= 1; ++k)
                        {
                            BasePointer fTmp = std::make_shared<Fdiv<T>>(fCopy[j]->_getArg(i),
                                                                         fCopy[__not(j)]->_getArg(k));
                            BasePointer fTmps(fTmp->_simp());
                            if (!fTmp->_equals(fTmps))
                            {
                                return
                                std::make_shared<Fdiv<T>>(fTmps,
                                                          std::make_shared<Fmult<T>>(fCopy[j]->_getArg(__not(i)),
                                                                                     fCopy[__not(j)]->_getArg(__not(k))))->_simp();
                            }
                        }
                    }
                }

                if (fCopy[__not(j)]->_kind() == Kind::cfun_div)
                {                                                           // x*a / x/b | x/a / x*b
                    if (!j)
                    {
                        return
                        std::make_shared<Fmult<T>>(fCopy[0],
                                                   std::make_shared<Fdiv<T>>(fCopy[1]->_getArg(1),
                                                                             fCopy[1]->_getArg(0)))->_simp();
                    }
                    else
                    {                                                       // j==1, i.e. x/a / x*b
                        for (i = 0; i <= 1; ++i)
                        {
                            BasePointer fTmp = std::make_shared<Fdiv<T>>(fCopy[0]->_getArg(0),
                                                                         fCopy[1]->_getArg(i));
                            BasePointer fTmps(fTmp->_simp());
                            if (!fTmp->_equals(fTmps))
                            {
                                return
                                std::make_shared<Fdiv<T>>(fTmps,
                                                          std::make_shared<Fmult<T>>(fCopy[0]->_getArg(1),
                                                                                     fCopy[1]->_getArg(__not(i))))->_simp();
                            }
                        }
                        for (i = 0; i <= 1; ++i)
                        {                                                   //  a/x / x*b
                            BasePointer fTmp = std::make_shared<Fmult<T>>(fCopy[0]->_getArg(1),
                                                                          fCopy[1]->_getArg(i));
                            BasePointer fTmps(fTmp->_simp());
                            if (!fTmp->_equals(fTmps))
                            {
                                return
                                std::make_shared<Fdiv<T>>(std::make_shared<Fdiv<T>>(fCopy[0]->_getArg(0),
                                                                                    fCopy[1]->_getArg(__not(i))),
                                                          fTmps)->_simp();
                            }
                        }
                    }
                }
            }
            if (fCopy[j]->_kind() == Kind::cfun_div)
            {                                                               // a/x / a  |  a / a/x
                if (j)
                {                                                           // a / a/x == a * x/a
                    return
                    std::make_shared<Fmult<T>>(fCopy[0],
                                               std::make_shared<Fdiv<T>>(fCopy[1]->_getArg(1),
                                                                         fCopy[1]->_getArg(0)))->_simp();
                }
                else
                {
                    {                                                       // j==0 i.e. a/x / a
                        BasePointer fTmp = std::make_shared<Fdiv<T>>(fCopy[j]->_getArg(0),
                                                                     fCopy[__not(j)]);
                        BasePointer fTmps(fTmp->_simp());
                        if (!fTmp->_equals(fTmps))
                        {
                            return
                            std::make_shared<Fdiv<T>>(fTmps,
                                                      fCopy[j]->_getArg(1))->_simp();
                        }
                    }
                    {
                        BasePointer fTmp = std::make_shared<Fmult<T>>(fCopy[j]->_getArg(1),    // x/a / a
                                                                      fCopy[__not(j)]);
                        BasePointer fTmps(fTmp->_simp());
                        if (!fTmp->_equals(fTmps))
                        {
                            return
                            std::make_shared<Fdiv<T>>(fCopy[j]->_getArg(0),
                                                      fTmps)->_simp();
                        }
                    }
                }
            }
        }
        return std::make_shared<Fdiv<T>>(fCopy[0], fCopy[1]);
    }

public:
    Fdiv(const BasePointer& pfSrc0, const BasePointer& pfSrc1)
      : BinaryFunction<T>(pfSrc0, pfSrc1)
    {
    }

    Fdiv(const std::string& sDividend, const std::string& sDivisor,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sDividend, sDivisor, saVars, saParameters, saMeanings)
    {
    }

    Fdiv(const std::string& sArgs, size_t nF1, size_t nL1, size_t nF2, size_t nL2,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sArgs, nF1, nL1, nF2, nL2, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override
    {
        T dDenom = this->mf1->_value(pdVars);
        if (Comparator<T,T>::eq(dDenom, cfun_zero<T>())) {
            throw cvmexception(CVM_DIVISIONBYZERO);
        }
        return this->mf0->_value(pdVars) / dDenom;
    }

    BasePointer _clone() const override {
        return std::make_shared<Fdiv<T>>(this->mf0, this->mf1);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fdiv<T>>(std::make_shared<Fminus<T>>(std::make_shared<Fmult<T>>(this->mf0->_deriv(nVarNum),
                                                                                         this->mf1),
                                                              std::make_shared<Fmult<T>>(this->mf0,
                                                                                         this->mf1->_deriv(nVarNum))),
                                  std::make_shared<Fpower<T>>(this->mf1,
                                                              std::make_shared<Fconst<T>>(cfun_two<T>())))->_simp();
    }

    std::string _format(int nPrecision) const override
    {
        std::ostringstream osBuf;
        switch (this->mf0->_kind()) {
        case Kind::cfun_plus:
        case Kind::cfun_minus:
        case Kind::cfun_div:
            osBuf << CFUN_O_SPARENTH << this->mf0->_format(nPrecision) << CFUN_C_PARENTH << CFUN_SDIV;
            break;
        default:
            osBuf << this->mf0->_format(nPrecision) << CFUN_SDIV;
            break;
        }
        switch (this->mf1->_kind()) {
        case Kind::cfun_plus:
        case Kind::cfun_minus:
        case Kind::cfun_uminus:
        case Kind::cfun_mult:
        case Kind::cfun_div:
            osBuf << CFUN_O_SPARENTH << this->mf1->_format(nPrecision) << CFUN_C_PARENTH;
            break;
        default:
            osBuf << this->mf1->_format(nPrecision);
            break;
        }
        return osBuf.str();
    }
};

template <typename T>
class Fpower : public BinaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_power;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename BinaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction
    using PointerPair = shared_ptr_pair<BaseFunction<T>>;

    BasePointer _simpl() const override
    {
        BasePointer fZero = std::make_shared<Fconst<T>>();
        BasePointer fOne = std::make_shared<Fconst<T>>(cfun_one<T>());
        BasePointer fMOne = std::make_shared<Fconst<T>>(cfun_mone<T>());
        PointerPair fCopy(this->mf0->_simp(), this->mf1->_simp());
        if (fCopy[1]->_equals(fZero))
        {                                                                   // x^0 = 1
            return fOne;
        }
        else if (fCopy[0]->_equals(fZero))
        {                                                                   // 0^x = 0
            return fZero;
        }
        else if (fCopy[1]->_equals(fOne))
        {                                                                   // x^1 = x
            return fCopy[0];
        }
        else if (fCopy[0]->_kind() == Kind::cfun_const &&
                 fCopy[1]->_kind() == Kind::cfun_const)
        {                                                                   // 6^2 = 36
            return
            std::make_shared<Fconst<T>>(std::pow(fCopy[0]->_value(nullptr), fCopy[1]->_value(nullptr)));
        }
        else if (fCopy[1]->_equals(fMOne))
        {                                                                   // x^-1 = 1/x
            return
            std::make_shared<Fdiv<T>>(fOne,
                                      fCopy[0]);
        }
        else if (fCopy[0]->_kind() == Kind::cfun_power)
        {                                                                   // [x^a]^b = x^[a*b]
            return
            std::make_shared<Fpower<T>>(fCopy[0]->_getArg(0),
                                        std::make_shared<Fmult<T>>(fCopy[0]->_getArg(1),
                                                                   fCopy[1]))->_simp();
        }
        return std::make_shared<Fpower<T>>(fCopy[0], fCopy[1]);
    }

public:
    Fpower(const BasePointer& pfSrc0, const BasePointer& pfSrc1)
      : BinaryFunction<T>(pfSrc0, pfSrc1)
    {
    }

    Fpower(const std::string& sBasis, const std::string& sExponent,
           const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sBasis, sExponent, saVars, saParameters, saMeanings)
    {
    }

    Fpower(const std::string& sBasisCommaExponent, size_t nFirst, size_t nLast,
           const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
    {
        this->BinCtr(sBasisCommaExponent, nFirst, nLast, saVars, saParameters, saMeanings);
    }

    Fpower(const std::string& sArgs, size_t nF1, size_t nL1, size_t nF2, size_t nL2,
           const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sArgs, nF1, nL1, nF2, nL2, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return std::pow(this->mf0->_value(pdVars), this->mf1->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Fpower<T>>(this->mf0, this->mf1);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fplus<T>>(std::make_shared<Fmult<T>>(this->mf0->_deriv(nVarNum),
                                                              std::make_shared<Fmult<T>>(this->mf1,
                                                                                         std::make_shared<Fpower<T>>(this->mf0,
                                                                                                                     std::make_shared<Fplus<T>>(this->mf1,
                                                                                                                                                std::make_shared<Fconst<T>>(cfun_mone<T>()))))),
                                   std::make_shared<Fmult<T>>(this->mf1->_deriv(nVarNum),
                                                              std::make_shared<Fmult<T>>(std::make_shared<Fpower<T>>(this->mf0,
                                                                                                                     this->mf1),
                                                                                         std::make_shared<Flog<T>>(this->mf0))))->_simp();
    }

    std::string _format(int nPrecision) const override
    {
        std::ostringstream osBuf;
        switch (this->mf0->_kind()) {
        case Kind::cfun_plus:
        case Kind::cfun_minus:
        case Kind::cfun_mult:
        case Kind::cfun_div:
        case Kind::cfun_power:
        case Kind::cfun_uminus:
            osBuf << CFUN_O_SPARENTH << this->mf0->_format(nPrecision) << CFUN_C_SPARENTH << CFUN_SPOWER;
            break;
        default:
            osBuf << this->mf0->_format(nPrecision) << CFUN_SPOWER;
            break;
        }
        switch (this->mf1->_kind()) {
        case Kind::cfun_plus:
        case Kind::cfun_minus:
        case Kind::cfun_mult:
        case Kind::cfun_div:
        case Kind::cfun_power:
        case Kind::cfun_uminus:
            osBuf << CFUN_O_SPARENTH << this->mf1->_format(nPrecision) << CFUN_C_SPARENTH;
            break;
        default:
            osBuf << this->mf1->_format(nPrecision);
            break;
        }
        return osBuf.str();
    }
};

template <typename T>
class Fsat : public BinaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_sat;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename BinaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction
    using PointerPair = shared_ptr_pair<BaseFunction<T>>;

    BasePointer _simpl() const override
    {
        BasePointer fZero = std::make_shared<Fconst<T>>();
        PointerPair fCopy(this->mf0->_simp(), this->mf1->_simp());
        if (fCopy[0]->_equals(fZero)) {
            return fZero;
        }
        return std::make_shared<Fsat<T>>(fCopy[0], fCopy[1]);
    }

public:
    Fsat(const BasePointer& pfSrc0, const BasePointer& pfSrc1)
      : BinaryFunction<T>(pfSrc0, pfSrc1)
    {
    }

    Fsat(const std::string& sFun, const std::string& sDelta,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sFun, sDelta, saVars, saParameters, saMeanings)
    {
    }

    Fsat(const std::string& sFunCommaDelta, size_t nFirst, size_t nLast,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
    {
        this->BinCtr(sFunCommaDelta, nFirst, nLast, saVars, saParameters, saMeanings);
    }

    Fsat(const std::string& sArgs, size_t nF1, size_t nL1, size_t nF2, size_t nL2,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sArgs, nF1, nL1, nF2, nL2, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return Helper<T>::_value(this->mf0.get(), this->mf1.get(), pdVars);
    }

    BasePointer _clone() const override {
        return std::make_shared<Fsat<T>>(this->mf0, this->mf1);
    }

    BasePointer _deriv(size_t) const override
    {
        return
        std::make_shared<Fplus<T>>(std::make_shared<Fdelta<T>>(this->mf0,
                                                               this->mf1),
                                   std::make_shared<Fdelta<T>>(this->mf0,
                                                               std::make_shared<Fuminus<T>>(this->mf1)))->_simp();
    }

    std::string _format(int nPrecision) const override
    {
        std::ostringstream osBuf;
        osBuf << CFUN_SAT << CFUN_O_PARENTH << this->mf0->_format(nPrecision) << CFUN_SCOMMA <<
            this->mf1->_format(nPrecision) << CFUN_C_PARENTH;
        return osBuf.str();
    }

protected:
//! @cond HELPERS
    template<typename U>
    class Helper {
    public:
        static U _value(const BaseFunction<U>* pfArg0, BaseFunction<U>* pfArg1, const U* pdVars)
        {
            U dRes = cfun_zero<U>();
            const U dMeans = pfArg0->_value(pdVars);
            const U dDelta = cvm::_abs(pfArg1->_value(pdVars));
            if (dMeans > dDelta) {
                dRes = cfun_one<U>();
            }
            else if (dMeans < -dDelta) {
                dRes = cfun_mone<U>();
            }
            return dRes;
        }
    };

    template<typename U>
    class Helper<std::complex<U> > {
        using UC = std::complex<U>;
    public:
        static UC _value(const BaseFunction<UC>* pfArg0, BaseFunction<UC>* pfArg1, const UC* pdVars)
        {
            UC dRes = cfun_zero<U>();
            const U dMeans = pfArg0->_value(pdVars).real();
            const U dDelta = cvm::_abs(pfArg1->_value(pdVars).real());
            if (dMeans > dDelta) {
                dRes = cfun_one<U>();
            }
            else if (dMeans < -dDelta) {
                dRes = cfun_mone<U>();
            }
            return dRes;
        }
    };
//! @endcond
};

template <typename T>
class Fexp : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_exp;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(std::exp(fCopy->_value(nullptr)));
        }
        return std::make_shared<Fexp<T>>(fCopy);
    }

public:
    Fexp(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fexp(const std::string& sArg,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fexp(const std::string& sArg, size_t nF, size_t nL,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return std::exp(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Fexp<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fmult<T>>(this->mf0->_deriv(nVarNum),
                                   std::make_shared<Fexp<T>>(this->mf0))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_EXP;
    }
};

template <typename T>
class Fsqrt : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_sqrt;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(ElementaryFunctions<T>::sqrt(fCopy->_value(nullptr)));
        }
        return std::make_shared<Fsqrt<T>>(fCopy);
    }

public:
    Fsqrt(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fsqrt(const std::string& sArg,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fsqrt(const std::string& sArg, size_t nF, size_t nL,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return ElementaryFunctions<T>::sqrt(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Fsqrt<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fdiv<T>>(this->mf0->_deriv(nVarNum),
                                  std::make_shared<Fmult<T>>(std::make_shared<Fconst<T>>(cfun_two<T>()),
                                                             std::make_shared<Fsqrt<T>>(this->mf0)))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_SQRT;
    }
};

template <typename T>
class Flog : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_log;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fZero = std::make_shared<Fconst<T>>();
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_equals(fZero))
        {                                                                   // log 0 = - inf
            return std::make_shared<Fmninfinity<T>>();
        }
        else if (fCopy->_kind() == Kind::cfun_const)
        {                                                                   // log 1 = 0
            return std::make_shared<Fconst<T>>(ElementaryFunctions<T>::log(fCopy->_value(nullptr)));
        }
        else if (fCopy->_kind() == Kind::cfun_sqrt)
        {                                                                   // log sqrt (x) = 0.5 * log (x)
            return
            std::make_shared<Fmult<T>>(std::make_shared<Flog<T>>(fCopy->_getArg(0)),
                                       std::make_shared<Fconst<T>>(cfun_half<T>()))->_simp();
        }
        else if (fCopy->_kind() == Kind::cfun_power)
        {                                                                   // log (x^y) = y * log (x)
            return
            std::make_shared<Fmult<T>>(std::make_shared<Flog<T>>(fCopy->_getArg(0)),
                                       fCopy->_getArg(1))->_simp();
        }
        return std::make_shared<Flog<T>>(fCopy);
    }

public:
    Flog(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Flog(const std::string& sArg,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Flog(const std::string& sArg, size_t nF, size_t nL,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return ElementaryFunctions<T>::log(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Flog<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fdiv<T>>(this->mf0->_deriv(nVarNum),
                                  this->mf0)->_simp();
    }

    const char* _name() const override
    {
        return CFUN_LOG;
    }
};

template <typename T>
class Flog10 : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_log10;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fZero = std::make_shared<Fconst<T>>();
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_equals(fZero))
        {
            return std::make_shared<Fmninfinity<T>>();
        }
        else if (fCopy->_kind() == Kind::cfun_const)
        {
            return std::make_shared<Fconst<T>>(ElementaryFunctions<T>::log10(fCopy->_value(nullptr)));
        }
        else if (fCopy->_kind() == Kind::cfun_sqrt)
        {                                                                   // log sqrt (x) = 0.5 * log (x)
            return
            std::make_shared<Fmult<T>>(std::make_shared<Flog10<T>>(fCopy->_getArg(0)),
                                       std::make_shared<Fconst<T>>(cfun_half<T>()))->_simp();
        }
        else if (fCopy->_kind() == Kind::cfun_power)
        {                                                                   // log (x^y) = y * log (x)
            return
            std::make_shared<Fmult<T>>(std::make_shared<Flog10<T>>(fCopy->_getArg(0)),
                                       fCopy->_getArg(1))->_simp();
        }
        return std::make_shared<Flog10<T>>(fCopy);
    }

public:
    Flog10(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Flog10(const std::string& sArg,
           const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Flog10(const std::string& sArg, size_t nF, size_t nL,
           const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return ElementaryFunctions<T>::log10(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Flog10<T>>(*this);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fdiv<T>>(this->mf0->_deriv(nVarNum),
                                  std::make_shared<Fmult<T>>(std::make_shared<Fconst<T>>(cfun_ln10<T>()),
                                                             this->mf0))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_LOG10;
    }
};

template <typename T>
class Fsin : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_sin;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(std::sin(fCopy->_value(nullptr)));
        }
        return std::make_shared<Fsin<T>>(fCopy);
    }

public:
    Fsin(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fsin(const std::string& sArg,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fsin(const std::string& sArg, size_t nF, size_t nL,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return std::sin(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Fsin<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fmult<T>>(this->mf0->_deriv(nVarNum),
                                   std::make_shared<Fcos<T>>(this->mf0))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_SIN;
    }
};

template <typename T>
class Fcos : public UnaryFunction<T> {
private:
    Kind _kind() const override {
        return Kind::cfun_cos;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(std::cos(fCopy->_value(nullptr)));
        }
        return std::make_shared<Fcos<T>>(fCopy);
    }

public:
    Fcos(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fcos(const std::string& sArg,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fcos(const std::string& sArg, size_t nF, size_t nL,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return std::cos(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Fcos<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fmult<T>>(this->mf0->_deriv(nVarNum),
                                   std::make_shared<Fuminus<T>>(std::make_shared<Fsin<T>>(this->mf0)))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_COS;
    }
};

template <typename T>
class Ftan : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_tan;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(ElementaryFunctions<T>::tan(fCopy->_value(nullptr)));
        }
        return std::make_shared<Ftan<T>>(fCopy);
    }

public:
    Ftan(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Ftan(const std::string& sArg,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Ftan(const std::string& sArg, size_t nF, size_t nL,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return ElementaryFunctions<T>::tan(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Ftan<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fdiv<T>>(this->mf0->_deriv(nVarNum),
                                  std::make_shared<Fpower<T>>(std::make_shared<Fcos<T>>(this->mf0),
                                                              std::make_shared<Fconst<T>>(cfun_two<T>())))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_TAN;
    }
};

template <typename T>
class Fasin : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_arcsin;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(ElementaryFunctions<T>::asin(fCopy->_value(nullptr)));
        }
        return std::make_shared<Fasin<T>>(fCopy);
    }

public:
    Fasin(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fasin(const std::string& sArg,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fasin(const std::string& sArg, size_t nF, size_t nL,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return ElementaryFunctions<T>::asin(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Fasin<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fdiv<T>>(this->mf0->_deriv(nVarNum),
                                  std::make_shared<Fsqrt<T>>(std::make_shared<Fminus<T>>(std::make_shared<Fconst<T>>(cfun_one<T>()),
                                                                                         std::make_shared<Fpower<T>>(this->mf0,
                                                                                                                     std::make_shared<Fconst<T>>(cfun_two<T>())))))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_ASIN;
    }
};

template <typename T>
class Facos : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_arccos;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(ElementaryFunctions<T>::acos(fCopy->_value(nullptr)));
        }
        return std::make_shared<Facos<T>>(fCopy);
    }

public:
    Facos(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Facos(const std::string& sArg,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Facos(const std::string& sArg, size_t nF, size_t nL,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return ElementaryFunctions<T>::acos(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Facos<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fuminus<T>>(std::make_shared<Fasin<T>>(this->mf0)->_deriv(nVarNum))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_ACOS;
    }
};

template <typename T>
class Fatan : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_arctan;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(ElementaryFunctions<T>::atan(fCopy->_value(nullptr)));
        }
        return std::make_shared<Fatan<T>>(fCopy);
    }

public:
    Fatan(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fatan(const std::string& sArg,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fatan(const std::string& sArg, size_t nF, size_t nL,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return ElementaryFunctions<T>::atan(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Fatan<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fdiv<T>>(this->mf0->_deriv(nVarNum),
                                  std::make_shared<Fplus<T>>(std::make_shared<Fconst<T>>(cfun_one<T>()),
                                                             std::make_shared<Fpower<T>>(this->mf0,
                                                                                         std::make_shared<Fconst<T>>(cfun_two<T>()))))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_ATAN;
    }
};


template <typename T>
class Fsinh : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_sinh;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(ElementaryFunctions<T>::sinh(fCopy->_value(nullptr)));
        }
        return std::make_shared<Fsinh<T>>(fCopy);
    }

public:
    Fsinh(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fsinh(const std::string& sArg,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fsinh(const std::string& sArg, size_t nF, size_t nL,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return ElementaryFunctions<T>::sinh(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Fsinh<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fmult<T>>(this->mf0->_deriv(nVarNum),
                                   std::make_shared<Fcosh<T>>(this->mf0))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_SINH;
    }
};


template <typename T>
class Fcosh : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_cosh;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(ElementaryFunctions<T>::cosh(fCopy->_value(nullptr)));
        }
        return std::make_shared<Fcosh<T>>(fCopy);
    }

public:
    Fcosh(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fcosh(const std::string& sArg,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fcosh(const std::string& sArg, size_t nF, size_t nL,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return ElementaryFunctions<T>::cosh(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Fcosh<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fmult<T>>(this->mf0->_deriv(nVarNum),
                                   std::make_shared<Fuminus<T>>(std::make_shared<Fsinh<T>>(this->mf0)))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_COSH;
    }
};


template <typename T>
class Ftanh : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_tanh;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(ElementaryFunctions<T>::tanh(fCopy->_value(nullptr)));
        }
        return std::make_shared<Ftanh<T>>(fCopy);
    }

public:
    Ftanh(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Ftanh(const std::string& sArg,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Ftanh(const std::string& sArg, size_t nF, size_t nL,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return ElementaryFunctions<T>::tanh(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Ftanh<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fdiv<T>>(this->mf0->_deriv(nVarNum),
                                  std::make_shared<Fpower<T>>(std::make_shared<Fcosh<T>>(this->mf0),
                                                              std::make_shared<Fconst<T>>(cfun_two<T>())))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_TANH;
    }
};

template <typename T>
class Fsinint : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_intsin;
    }

protected:
    int _depth(bool bAcquire) const override {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(Helper<T>::_value(fCopy.get(), nullptr));
        }
        return std::make_shared<Fsinint<T>>(fCopy);
    }

public:
    Fsinint(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fsinint(const std::string& sArg,
            const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fsinint(const std::string& sArg, size_t nF, size_t nL,
            const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return Helper<T>::_value(this->mf0.get(), pdVars);
    }

    BasePointer _clone() const override {
        return std::make_shared<Fsinint<T>>(this->mf0);
    }

    BasePointer _deriv(size_t) const override
    {
        return
        std::make_shared<Fdiv<T>>(std::make_shared<Fsin<T>>(this->mf0),
                                  this->mf0)->_simp();
    }

    const char* _name() const override
    {
        return CFUN_SI;
    }

protected:
//! @cond HELPERS
    template<typename U>
    class Helper {
    public:
        static U _value(const BaseFunction<U>* pfArg, const U* pdVars)
        {
            return ElementaryFunctions<U>::sinint(pfArg->_value(pdVars), basic_cvmMachSp<U>());
        }
    };
    template<typename U>
    class Helper<std::complex<U> > {
        using UC = std::complex<U>;
    public:
        static UC _value(const BaseFunction<UC>* pfArg, const UC* pdVars)
        {
            return ElementaryFunctions<UC>::sinint(pfArg->_value(pdVars), basic_cvmMachSp<U>());
        }
    };
//! @endcond
};

template <typename T>
class Fcosint : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_intcos;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(Helper<T>::_value(fCopy.get(), nullptr));
        }
        return std::make_shared<Fcosint<T>>(fCopy);
    }

public:
    Fcosint(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fcosint(const std::string& sArg,
            const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fcosint(const std::string& sArg, size_t nF, size_t nL,
            const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return Helper<T>::_value(this->mf0.get(), pdVars);
    }

    BasePointer _clone() const override {
        return std::make_shared<Fcosint<T>>(this->mf0);
    }

    BasePointer _deriv(size_t) const override
    {
        return
        std::make_shared<Fdiv<T>>(std::make_shared<Fcos<T>>(this->mf0),
                                  this->mf0)->_simp();
    }

    const char* _name() const override
    {
        return CFUN_CI;
    }

protected:
//! @cond HELPERS
    template<typename U>
    class Helper {
    public:
        static U _value(const BaseFunction<U>* pfArg, const U* pdVars)
        {
            return ElementaryFunctions<U>::cosint(pfArg->_value(pdVars), basic_cvmMachSp<U>());
        }
    };
    template<typename U>
    class Helper<std::complex<U> > {
        using UC = std::complex<U>;
    public:
        static UC _value(const BaseFunction<UC>* pfArg, const UC* pdVars)
        {
            return ElementaryFunctions<UC>::cosint(pfArg->_value(pdVars), basic_cvmMachSp<U>());
        }
    };
//! @endcond
};

template <typename T>
class Fuminus : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_uminus;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_uminus)
        {                                                                   //  - - b
            return fCopy->_getArg(0);
        }
        if (fCopy->_kind() == Kind::cfun_infinity)
        {
            return std::make_shared<Fmninfinity<T>>();
        }
        if (fCopy->_kind() == Kind::cfun_mninfinity)
        {
            return std::make_shared<Finfinity<T>>();
        }
        if (fCopy->_kind() == Kind::cfun_const)
        {
            return std::make_shared<Fconst<T>>(- fCopy->_value(nullptr));
        }
        if (fCopy->_kind() == Kind::cfun_minus)
        {                                                                   // - (a - b) = b - a
            return
            std::make_shared<Fminus<T>>(fCopy->_getArg(1),
                                        fCopy->_getArg(0));
        }
        if (fCopy->_kind() == Kind::cfun_mult)
        {                                                                   // - x*y
            for (size_t i = 0; i <= 1; ++i)
            {
                BasePointer fTmp = std::make_shared<Fuminus<T>>(fCopy->_getArg(i));
                BasePointer fTmps(fTmp->_simp());
                if (!fTmp->_equals(fTmps))
                {
                    return
                    std::make_shared<Fmult<T>>(fTmps,
                                               fCopy->_getArg(__not(i)))->_simp();
                }
            }
        }
        if (fCopy->_kind() == Kind::cfun_div)
        {                                                                   // - x/y
            for (size_t i = 0; i <= 1; ++i)
            {
                BasePointer fTmp = std::make_shared<Fuminus<T>>(fCopy->_getArg(i));
                BasePointer fTmps(fTmp->_simp());
                if (!fTmp->_equals(fTmps))
                {
                    BasePointer fTmp(
                    i ?
                    std::make_shared<Fdiv<T>>(fCopy->_getArg(__not(i)),
                                              fTmps)
                    :
                    std::make_shared<Fdiv<T>>(fTmps,
                                              fCopy->_getArg(__not(i))));
                    return fTmp->_simp();
                }
            }
        }
        return std::make_shared<Fuminus<T>>(fCopy);
    }

public:
    Fuminus(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fuminus(const std::string& sArg,
            const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fuminus(const std::string& sArg, size_t nF, size_t nL,
            const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return - this->mf0->_value(pdVars);
    }

    BasePointer _clone() const override {
        return std::make_shared<Fuminus<T>>(this->mf0);
    }

    std::string _format(int nPrecision) const override
    {
        std::ostringstream osBuf;
        osBuf << this->_name();
        switch (this->mf0->_kind()) {
        case Kind::cfun_plus:
        case Kind::cfun_minus:
        case Kind::cfun_uminus:
            osBuf << CFUN_O_SPARENTH << this->mf0->_format(nPrecision) << CFUN_C_PARENTH;
            break;
        default:
            osBuf << this->mf0->_format(nPrecision);
            break;
        }
        return osBuf.str();
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fuminus<T>>(this->mf0->_deriv(nVarNum))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_SMINUS;
    }
};

template <typename T>
class Fsign : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_sign;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const)
        {
            return std::make_shared<Fconst<T>>(Helper<T>::_value(fCopy.get(), nullptr));
        }
        return std::make_shared<Fsign<T>>(fCopy);
    }

public:
    Fsign(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fsign(const std::string& sArg,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fsign(const std::string& sArg, size_t nF, size_t nL,
          const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return Helper<T>::_value(this->mf0.get(), pdVars);
    }

    BasePointer _clone() const override {
        return std::make_shared<Fsign<T>>(this->mf0);
    }

    BasePointer _deriv(size_t) const override
    {
        return
        std::make_shared<Fdelta<T>>(this->mf0,
                                    std::make_shared<Fconst<T>>())->_simp();
    }

    const char* _name() const override
    {
        return CFUN_SIGN;
    }

protected:
//! @cond HELPERS
    template<typename U>
    class Helper {
    public:
        static U _value(const BaseFunction<U>* pfArg, const U* pdVars)
        {
            const U dV = pfArg->_value(pdVars);
            return dV > cfun_zero<U>() ? cfun_one<U>() :
                                         (dV < cfun_zero<U>() ? cfun_mone<U>() : cfun_zero<U>());
        }
    };

    template<typename U>
    class Helper<std::complex<U> > {
        using UC = std::complex<U>;
    public:
        static UC _value(const BaseFunction<UC>* pfArg, const UC* pdVars)
        {
            const U dV = pfArg->_value(pdVars).real();
            return dV > cfun_zero<U>() ? cfun_one<U>() :
                                         (dV < cfun_zero<U>() ? cfun_mone<U>() : cfun_zero<U>());
        }
    };
//! @endcond
};

template <typename T>
class Fabs : public UnaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_abs;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    BasePointer _simpl() const override
    {
        BasePointer fCopy(this->mf0->_simp());
        if (fCopy->_kind() == Kind::cfun_const) {
            return std::make_shared<Fconst<T>>(cvm::_abs(fCopy->_value(nullptr)));
        }
        else if (fCopy->_kind() == Kind::cfun_abs) {
            return fCopy;
        }
        return std::make_shared<Fabs<T>>(fCopy);
    }

public:
    Fabs(const BasePointer& pfSrc)
      : UnaryFunction<T>(pfSrc)
    {
    }

    Fabs(const std::string& sArg,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, saVars, saParameters, saMeanings)
    {
    }

    Fabs(const std::string& sArg, size_t nF, size_t nL,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : UnaryFunction<T>(sArg, nF, nL, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return cvm::_abs(this->mf0->_value(pdVars));
    }

    BasePointer _clone() const override {
        return std::make_shared<Fabs<T>>(this->mf0);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fmult<T>>(std::make_shared<Fsign<T>>(this->mf0),
                                   this->mf0->_deriv(nVarNum))->_simp();
    }

    const char* _name() const override
    {
        return CFUN_ABS;
    }
};

template <typename T>
class Fdelta : public BinaryFunction<T>
{
private:
    Kind _kind() const override {
        return Kind::cfun_delta;
    }

protected:
    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    using BasePointer = typename BinaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction
    using PointerPair = shared_ptr_pair<BaseFunction<T>>;

    BasePointer _simpl() const override
    {
        PointerPair fCopy(this->mf0->_simp(), this->mf1->_simp());
        if (fCopy[0]->_kind() == Kind::cfun_const && fCopy[1]->_kind() == Kind::cfun_const) {
            T dM1 = fCopy[0]->_value(nullptr);
            T dM2 = fCopy[1]->_value(nullptr);
            if (cvm::_abs(dM1 - dM2) <= cvm::cvmMachMin()) {
                return std::make_shared<Finfinity<T>>();
            }
            else {
                return std::make_shared<Fconst<T>>();
            }
        }
        return std::make_shared<Fdelta<T>>(fCopy[0], fCopy[1]);
    }

public:
    Fdelta(const BasePointer& pfSrc0, const BasePointer& pfSrc1)
      : BinaryFunction<T>(pfSrc0, pfSrc1)
    {
    }

    Fdelta(const std::string& sArg1, const std::string& sArg2,
           const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sArg1, sArg2, saVars, saParameters, saMeanings)
    {
    }

    Fdelta(const std::string& sArgs, size_t nF1, size_t nL1, size_t nF2, size_t nL2,
           const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sArgs, nF1, nL1, nF2, nL2, saVars, saParameters, saMeanings)
    {
    }

    Fdelta(const std::string& sArg, size_t nFirst, size_t nLast,
           const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : BinaryFunction<T>(sArg, nFirst, nLast, saVars, saParameters, saMeanings)
    {
    }

    T _value(const T* pdVars) const override {
        return Helper<T>::_value(this->mf0.get(), this->mf1.get(), pdVars);
    }

    BasePointer _clone() const override {
        return std::make_shared<Fdelta<T>>(this->mf0, this->mf1);
    }

    BasePointer _deriv(size_t) const override
    {
        return
        std::make_shared<Fdelta<T>>(this->mf0,
                                    this->mf1)->_simp();
    }

    std::string _format(int nPrecision) const override
    {
        std::ostringstream osBuf;
        osBuf << CFUN_DELTA << CFUN_O_PARENTH << this->mf0->_format(nPrecision) << CFUN_SCOMMA <<
            this->mf1->_format(nPrecision) << CFUN_C_PARENTH;
        return osBuf.str();
    }

protected:
//! @cond HELPERS
    template<typename U>
    class Helper {
    public:
        static U _value(const BaseFunction<U>* pfArg0, const BaseFunction<U>* pfArg1, const U* pdVars)
        {
            return cvm::_abs(pfArg0->_value(pdVars) - pfArg1->_value(pdVars)) <= basic_cvmMachMin<U>() ?
                   basic_cvmMachMax<U>() : cfun_zero<U>();
        }
    };

    template<typename U>
    class Helper<std::complex<U> > {
        using UC = std::complex<U>;
    public:
        static UC _value(const BaseFunction<UC>* pfArg0, const BaseFunction<UC>* pfArg1, const UC* pdVars)
        {
            return cvm::_abs((pfArg0->_value(pdVars) - pfArg1->_value(pdVars)).real()) <= basic_cvmMachMin<U>() ?
                   basic_cvmMachMax<U>() : cfun_zero<U>();
        }
    };
//! @endcond
};

template <typename T>
class Fvar : public BaseFunction<T>
{
private:
    size_t mnVarPos;
    std::string msVarName;

    Kind _kind() const override {
        return Kind::cfun_var;
    }

protected:
    int _depth(bool) const override {
        return 1;
    }

    using BasePointer = typename BaseFunction<T>::BasePointer; //!< Shared pointer to BaseFunction


    BasePointer _simpl() const override
    {
        return this->_clone();
    }

public:
    Fvar()
      : mnVarPos(0),
        msVarName()
    {
    }

    Fvar(const Fvar<T>& rfSrc)
      : mnVarPos(rfSrc.mnVarPos),
        msVarName(rfSrc.msVarName)
    {
    }

    Fvar(std::string& sVN, size_t nVP)
      : mnVarPos(nVP),
        msVarName(sVN)
    {
    }

    T _value(const T* pdVars) const override
    {
        if (pdVars == nullptr) {
            throw cvmexception(CFUN_NULLPOINTERERROR, "Fvar<T>::_value");
        }
        return pdVars[mnVarPos];
    }

    BasePointer _clone() const override {
        return std::make_shared<Fvar<T>>(*this);
    }

    std::string _format(int) const override {
        return msVarName;
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        if (mnVarPos == nVarNum) {
            return std::make_shared<Fconst<T>>(cfun_one<T>());
        }
        return std::make_shared<Fconst<T>>();
    }

    bool _equals(const BasePointer& pfSrc) const override
    {
        return this->_kind() == pfSrc->_kind() &&
               // since kind() is the same we can do this downcasting
               mnVarPos == reinterpret_cast<const Fvar<T>*>(pfSrc.get())->mnVarPos &&
               msVarName == reinterpret_cast<const Fvar<T>*>(pfSrc.get())->msVarName;
    }

    BasePointer _getArg(size_t) const override {
        return nullptr;
    }
};

template <typename T>
class Fiif : public UnaryFunction<T>
{
private:
    using BasePointer = typename UnaryFunction<T>::BasePointer; //!< Shared pointer to BaseFunction

    Kind _kind() const override {
        return Kind::cfun_iif;
    }

    BasePointer mpflt;  // if mf0 < 0
    BasePointer mpfge;  // if mf0 >= 0

protected:
    void TriCtr(const std::string& sBodyCommaBodyCommaBody, size_t nFirst, size_t nLast,
                const string_array& saVars,
                const string_array& saParameters, const string_array& saMeanings) throw(cvmexception)
    {
        std::string sF, sLeft, sRight;
        if (__separate(sBodyCommaBodyCommaBody, nFirst, nLast, sF, sRight) == 1) {
            this->mf0 = FunctionFactory<T>::compile(sF, 0, sF.length(), saVars, saParameters, saMeanings);
            if (__separate(sRight, 0, sRight.length(), sLeft, sRight) == 1) {
                mpflt = FunctionFactory<T>::compile(sLeft, 0, sLeft.length(), saVars, saParameters, saMeanings);
                mpfge = FunctionFactory<T>::compile(sRight, 0, sRight.length(), saVars, saParameters, saMeanings);
                return;
            }
        }
        BaseFunction<T>::parse_err(sBodyCommaBodyCommaBody, saVars);
    }

    int _depth(bool bAcquire) const override
    {
        static int nDepth;
        return bAcquire ? ++nDepth : --nDepth;
    }

    BasePointer _simpl() const override
    {
        return std::make_shared<Fiif<T>>(this->mf0->_simp(), this->mpflt->_simp(), this->mpfge->_simp());
    }

public:
    Fiif(const BasePointer& rfSrc, const BasePointer& rfLT, const BasePointer& rfGE)
      : UnaryFunction<T>(rfSrc),
        mpflt(rfLT),
        mpfge(rfGE)
    {
    }

    Fiif(const std::string& sBody,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : mpflt(nullptr),
        mpfge(nullptr)
    {
        TriCtr(sBody, 0, sBody.length(), saVars, saParameters, saMeanings);
    }

    Fiif(const std::string& sBody, size_t nFirst, size_t nLast,
         const string_array& saVars, const string_array& saParameters, const string_array& saMeanings)
      : mpflt(nullptr),
        mpfge(nullptr)
    {
        TriCtr(sBody, nFirst, nLast, saVars, saParameters, saMeanings);
    }

    T _value(const T* pdVars) const override {
        return Helper<T>::left(this->mf0.get(), pdVars) ? this->mpflt->_value(pdVars) :
                                                          this->mpfge->_value(pdVars);
    }

    BasePointer _clone() const override {
        return std::make_shared<Fiif<T>>(*this);
    }

    BasePointer _deriv(size_t nVarNum) const override
    {
        return
        std::make_shared<Fiif<T>>(this->mf0, this->mpflt->_deriv(nVarNum), this->mpfge->_deriv(nVarNum));
    }

    std::string _format(int nPrecision) const override
    {
        std::ostringstream osBuf;
        osBuf << this->_name() << CFUN_O_PARENTH << this->mf0->_format(nPrecision) << CFUN_SCOMMA <<
            this->mpflt->_format(nPrecision) << CFUN_SCOMMA <<
            this->mpfge->_format(nPrecision) << CFUN_C_PARENTH;
        return osBuf.str();
    }

    const char* _name() const override
    {
        return CFUN_IIF;
    }

protected:
//! @cond HELPERS
    template<typename U>
    class Helper {
    public:
        static bool left(const BaseFunction<U>* pfArg0, const U* pdVars)
        {
            return pfArg0->_value(pdVars) < cfun_zero<U>();
        }
    };

    template<typename U>
    class Helper<std::complex<U> > {
        using UC = std::complex<U>;
    public:
        static bool left(const BaseFunction<UC>* pfArg0, const UC* pdVars)
        {
            return pfArg0->_value(pdVars).real() < cfun_zero<U>();
        }
    };
//! @endcond
};

//! @endcond


/**
@brief End-user class encapsulating elementary function of real
       or complex numbers (depending on \c T type).

Typically used to convert strings entered by a user to
computable expressions without need to parse same expression
over and over again. Also might be used to simplify computable
expressions and to compute derivatives analytically, i.e.
without using numerical methods. Functions can have zero, one
or more than one variables and can be parameterized.

\par Elementary functions supported


\f[
\begin{aligned}
+\ -\ *\ /\ \verb|^| \quad & \text{arithmetic operators: add, subtract, multiply, divide, power}\\

\texttt{exp(x)}\quad & e^x \\

\texttt{sqrt(x)}\quad & \sqrt x\\

\texttt{log(x)}\quad & \text{natural logarithm of}\ x\\

\texttt{log10(x)}\quad & \text{common (base 10) logarithm of}\ x\\

\texttt{sin(x)}\quad & \text{sine of}\ x\\

\texttt{cos(x)}\quad & \text{cosine of}\ x\\

\texttt{tan(x)}\quad & \text{tangent of}\ x\\

\texttt{asin(x)}\quad & \text{arc sine of}\ x\\

\texttt{acos(x)}\quad & \text{arc cosine of}\ x\\

\texttt{atan(x)}\quad & \text{arc tangent of}\ x\\

\texttt{sinh(x)}\quad & \text{hyperbolic sine of}\ x\\

\texttt{cosh(x)}\quad & \text{hyperbolic cosine of}\ x\\

\texttt{tanh(x)}\quad & \text{hyperbolic tangent of}\ x\\

\texttt{sinint(x)}\quad & \text{sine integral of}\ x,\quad

\int_0^x \frac{\sin t}{t}dt\\

\texttt{cosint(x)}\quad & \text{cosine integral of}\ x,\quad

-\int_x^\infty \frac{\cos t}{t}dt \\

\texttt{sign(x)}\quad & \text{sign of}\ x,\quad

 \left\{
  \begin{aligned}
-1 & \text{ if } x < 0\\
 0 & \text{ if } x = 0\\
 1 & \text{ if } x > 0
  \end{aligned}
 \right.\\

\texttt{abs(x)}\quad & |x| \\

\texttt{iif(x,expr1,expr2)}\quad & \text{immediate if}\quad

 \left\{
  \begin{aligned}
expr1 & \text{ if } x < 0\\
expr2 & \text{ if } x \ge 0
  \end{aligned}
 \right.\\

\texttt{sat(x,y)}\quad & \text{satellite function of}\ x\
\text{and}\ y,\quad

 \left\{
  \begin{aligned}
1 & \text{ if } x > |y|\\
0 & \text{ if } -|y| \le x \le |y|\\
-1 & \text{ if } x < -|y|
  \end{aligned}
 \right.\\

\texttt{i}\quad & 1\ \text{(one)}\\

 & 0\ \text{(empty field means zero)}

\end{aligned}
\f]




\par Example:
\code
try{
    rfunction rf("{x,y} cos(x)*cos(y) + x + x + x");
    std::cout << rf.simp() << std::endl;
    std::cout << rf.drv(0) << std::endl;
    std::cout << rf.drv(1) << std::endl;
    double vars[] = {0., 0.};
    std::cout << rf(vars) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x*3+cos(x)*cos(y)
{x,y} 3-sin(x)*cos(y)
{x,y} -sin(y)*cos(x)
1
\endcode
@see BaseFunction, UnaryFunction, BinaryFunction
*/
template <typename T>
class basic_function
{
public:
    using BasePointer = typename FunctionFactory<T>::BasePointer; //!< Shared pointer to BaseFunction

protected:
    mutable string_array mvars; //!< Array of variables
    BasePointer mp; //!< Pointer to function's instance

//! @cond INTERNAL
    const string_array& _check_vars(const string_array& saVars) const throw(cvmexception)
    {
        if (this->vars_num() > 0 && saVars.size() > 0 && !__arrays_equal(mvars, saVars)) {
            throw cvmexception(CFUN_VARSDONTMATCH, __format_vars(mvars).c_str(), __format_vars(saVars).c_str());
        }
        return this->vars_num() > 0 ? mvars : saVars;      // here we cover cases like '5+x'
    }

    // protected utility constructor
    basic_function(const string_array& saVars, const BasePointer& pf)
      : mvars(saVars),
        mp(pf)
    {
    }

    basic_function(string_array&& saVars, BasePointer&& pf)
      : mvars(std::move(saVars)),
        mp(std::move(pf))
    {
    }
//! @endcond

public:
/**
@brief Default constructor

Creates zero function of no variables.
\par Example:
\code
using namespace cvm;
rfunction rf;
std::cout << rf << std::endl;
std::cout << rf() << std::endl;

cfunction cf;
std::cout << cf << std::endl;
std::cout << cf() << std::endl;
\endcode
prints
\code
0
0
(0,0)
(0,0)
\endcode
*/
    basic_function()
      : mvars(),
        mp(std::make_shared<Fconst<T>>())
    {
    }

/**
@brief Constructor

Creates zero function with given list of variables.
\par Example:
\code
using namespace cvm;
string_array args;
args.push_back("x");
args.push_back("y");
rfunction rf(args);
std::cout << rf << std::endl;
std::cout << rf() << std::endl;

cfunction cf(args);
std::cout << cf << std::endl;
std::cout << cf() << std::endl;
\endcode
prints
\code
{x,y} 0
0
{x,y} (0,0)
(0,0)
\endcode
@param[in] saVars \ref string_array of variables.
*/
    explicit basic_function(const string_array& saVars)
      : mvars(saVars),
        mp(std::make_shared<Fconst<T>>())
    {
    }

/**
@brief Constructor

Creates constant number function of no variables.
\par Example:
\code
using namespace cvm;
using namespace std;

rfunction rf(1.234);
std::cout << rf << std::endl;
std::cout << rf() << std::endl;

cfunction cf(complex<double>(1.,2.));
std::cout << cf << std::endl;
std::cout << cf() << std::endl;
\endcode
prints
\code
1.234
1.234
(1,2)
(1,2)
\endcode
@param[in] dConst real (for \ref rfunction) or complex (for \ref
      cfunction) number.
*/
    explicit basic_function(const T& dConst)
      : mvars(),
        mp(std::make_shared<Fconst<T>>(dConst))
    {
    }

/**
@brief Copy Constructor

Creates deep (default) or shallow (if <tt>bDeep = false</tt>)
copy of a function referred by <tt>rf</tt>. Shallow copying is
reserved for internal use only.
\par Example:
\code
using namespace cvm;
rfunction rf(1.234);
rfunction rf1(rf);
rf = 2.5;
std::cout << rf << std::endl;
std::cout << rf1 << std::endl;
\endcode
prints
\code
2.5
1.234
\endcode
@param[in] rf function (\ref rfunction or \ref
      cfunction) to copy from.
*/
    basic_function(const basic_function& rf)
      : mvars(rf.mvars),
        mp(rf.mp)
    {
    }

/**
@brief Move Constructor
*/
    basic_function(basic_function&& rf)
      : mvars(std::move(rf.mvars)),
        mp(std::move(rf.mp))
    {
    }

/**
@brief Constructor

Creates function of one or more variables using Wolfram's Mathemaca syntax
<tt>{var1[,var2,...]} expr</tt>. Throws \ref cvmexception in
case of syntax or memory allocation error.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    rfunction rf("{x,y} cos(x)*sin(y)");
    double rvars[] = {0., 3.14159 / 2.};
    std::cout << rf << std::endl;
    std::cout << rf(rvars) << std::endl;

    cfunction cf("{x,y} sqrt(x) + sqrt(y)");
    complex<double> cvars[] = {complex<double>(0., 1.), complex<double>(1., 0.)};
    std::cout << cf << std::endl;
    std::cout << cf(cvars) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} cos(x)*sin(y)
1
{x,y} sqrt(x)+sqrt(y)
(1.70711,0.707107)
\endcode
@param[in] sInput String representing a function as
      <tt>{var1[,var2,...]} expr</tt>. For expression syntax
       look at \ref basic_function description.
*/
    explicit basic_function(const std::string& sInput)
      : mvars(),
        mp()
    {
        string_array saParameters, saMeanings;
        size_t nAF = sInput.find(CFUN_O_BRACE, 0);
        size_t nAL = sInput.find(CFUN_C_BRACE, 0);
        if (nAL != CFUN_NOT_FOUND && nAL > nAF) {
            __parse_vars(sInput, nAF + 1, nAL, mvars);
        }
        mp = FunctionFactory<T>::compile(sInput, nAL + 1, sInput.length(), mvars, saParameters, saMeanings);
    }

/**
@brief Constructor

Creates function of one or more variables. Throws \ref
cvmexception in case of syntax or memory allocation error.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    string_array vars;
    vars.push_back("x");
    vars.push_back("y");

    rfunction rf(vars, "cos(x)*sin(y)");
    double rvars[] = {0., 3.14159 / 2.};
    std::cout << rf << std::endl;
    std::cout << rf(rvars) << std::endl;

    cfunction cf(vars, "sqrt(x) + sqrt(y)");
    complex<double> cvars[] = {complex<double>(0., 1.), complex<double>(1., 0.)};
    std::cout << cf << std::endl;
    std::cout << cf(cvars) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} cos(x)*sin(y)
1
{x,y} sqrt(x)+sqrt(y)
(1.70711,0.707107)
\endcode
@param[in] saVars String array with variables (may be
      empty).
@param[in] sBody String with function expression. For expression
      syntax look at \ref basic_function description.
*/
    basic_function(const string_array& saVars, const std::string& sBody)
      : mvars(saVars),
        mp()
    {
        string_array saParameters, saMeanings;
        mp = FunctionFactory<T>::compile(sBody, 0, sBody.length(), mvars, saParameters, saMeanings);
    }

/**
@brief Constructor

Creates parameterized function of one or more variables. Throws
\ref cvmexception in case of syntax or memory allocation error.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    string_array vars, params, meanings;
    vars.push_back("x");
    vars.push_back("y");
    params.push_back("p");
    meanings.push_back("sqrt(x) + sqrt(y)");

    rfunction rf(vars, "p^2 + p + x + y", params, meanings);
    double rvars[] = {0., 3.14159 / 2.};
    std::cout << rf << std::endl;
    std::cout << rf(rvars) << std::endl;

    cfunction cf(vars, "p^2 + p + x + y", params, meanings);
    complex<double> cvars[] = {complex<double>(0., 1.), complex<double>(1., 0.)};
    std::cout << cf << std::endl;
    std::cout << cf(cvars) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} (sqrt(x)+sqrt(y))^2+sqrt(x)+sqrt(y)+x+y
4.3949
{x,y} (sqrt(x)+sqrt(y))^(2,0)+sqrt(x)+sqrt(y)+x+y
(5.12132,4.12132)
\endcode
@param[in] saVars String array with variables (may be
      empty).
@param[in] sBody String with function expression. For expression
      syntax look at \ref basic_function description.
@param[in] saParameters String array with parameters (may be
      empty).
@param[in] saMeanings String array with parameters' meanings
      (may be empty, must have the same size as saParameters).
*/
    basic_function(const string_array& saVars, const std::string& sBody,
                   const string_array& saParameters, const string_array& saMeanings)
        : mvars(saVars),
          mp(FunctionFactory<T>::compile(sBody, 0, sBody.length(), mvars, saParameters, saMeanings))
    {
    }

/**
@brief Destructor
*/
    virtual ~basic_function()
    {
    }

/**
@brief Variables

Returns constant referense to array of variables.
\par Example:
\code
rfunction rf("{x,y} cos(x)*sin(y)");
const string_array vars = rf.vars();
std::cout << vars[0] << " " << vars[1] << std::endl;
\endcode
prints
\code
x y
\endcode
@return Referernce to array of strings.
*/
    const string_array& vars() const {
        return mvars;
    }

/**
@brief Number of variables

Returns number of variables.
\par Example:
\code
rfunction rf("{x,y} cos(x)*sin(y)");
std::cout << rf.vars_num() << std::endl;
\endcode
prints
\code
2
\endcode
@return size_t number.
*/
    size_t vars_num() const {
        return mvars.size();
    }

/**
@brief Numerical value

Returns numerical value of function for given values of
variables. Array's dimension is not verified.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    rfunction rf("{x,y} x + 2*y");
    const double rvars[] = {1., 2.};
    std::cout << rf.value(rvars) << std::endl;

    cfunction cf("{x,y} x + 2*y");
    const complex<double> cvars[] = {complex<double>(1., 0.), complex<double>(1., 1.)};
    std::cout << cf.value(cvars) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception: " << e.what () << std::endl;
}
\endcode
prints
\code
5
(3,2)
\endcode
@param[in] pd Array of variables' values.
@return real or complex number.
*/
    T value(const T* pd) const {
        return mp->_value(pd);
    }


/**
@brief Numerical value

Returns numerical value of function for given values of
variables. Array's dimension is not verified.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    rfunction rf("{x,y} x + 2*y");
    const double rvars[] = {1., 2.};
    std::cout << rf(rvars) << std::endl;

    cfunction cf("{x,y} x + 2*y");
    const complex<double> cvars[] = {complex<double>(1., 0.), complex<double>(1., 1.)};
    std::cout << cf(cvars) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception: " << e.what () << std::endl;
}
\endcode
prints
\code
5
(3,2)
\endcode
@param[in] pd Array of variables' values.
@return real or complex number.
*/
    T operator () (const T* pd) const {
        return mp->_value(pd);
    }

/**
@brief Numerical value

Returns numerical value of function for given value of single
variable. Result is unpredictable if function is of more than
one variable.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    rfunction rf("{x} 2*x+1");
    std::cout << rf(1.) << std::endl;

    cfunction cf("{x} 2*x+(1,1)");
    std::cout << cf(complex<double>(2., 1.)) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception: " << e.what () << std::endl;
}
\endcode
prints
\code
3
(5,3)
\endcode
@param[in] d Variable's value.
@return real or complex number.
*/
    T operator () (const T& d) const {
        return mp->_value(&d);
    }

/**
@brief Numerical value

Returns numerical value of function for given two variables' values.
Result is unpredictable if function is of more than
two variables.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} sin(x)^2 + cos(y)^2");
std::cout << rf(1.,1.) << std::endl;
std::cout << rf(3.1459,0.) << std::endl;

cfunction cf("{x,y} sqrt(x) + sqrt(y)");
std::cout << cf(complex<double>(-1., 0.), complex<double>(-4., 0.)) << std::endl;
std::cout << cf(complex<double>(9., 0.), complex<double>(-4., 0.)) << std::endl;
\endcode
prints
\code
1
1.00002
(0,3)
(3,2)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@return real or complex number.
*/
    T operator () (const T& d1, const T& d2) const
    {
        T dVals[2];
        dVals[0] = d1;
        dVals[1] = d2;
        return mp->_value(dVals);
    }

/**
@brief Numerical value

Returns numerical value of function for given three variables' values.
Result is unpredictable if function is of more than
three variables.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y,z} x*y*z");
std::cout << rf(2.,3.,4.) << std::endl;
cfunction cf("{x,y,z} x*y*z");
std::cout << cf(complex<double>(1., 1.), complex<double>(2., 1.), complex<double>(3., 1.)) << std::endl;
\endcode
prints
\code
24
(0,10)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@param[in] d3 Third variable's value.
@return real or complex number.
*/
    T operator () (const T& d1, const T& d2, const T& d3) const
    {
        T dVals[3];
        dVals[0] = d1;
        dVals[1] = d2;
        dVals[2] = d3;
        return mp->_value(dVals);
    }

/**
@brief Numerical value

Returns numerical value of function for given zero value of
single variable or function of no variables.
Result is unpredictable if function is of more
than one variable.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf("{x} 2*x+1");
    std::cout << rf() << std::endl;
    rfunction rf0("sqrt(9)");
    std::cout << rf0() << std::endl;

    cfunction cf("{x} 2*x+(1,1)");
    std::cout << cf() << std::endl;
    cfunction cf0("sqrt(-1)");
    std::cout << cf0() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception: " << e.what () << std::endl;
}
\endcode
prints
\code
1
3
(1,1)
(0,1)
\endcode
@return real or complex number.
@return %Function object.
@return boolean value.
@return Reference to changed calling function.
*/
    T operator () () const
    {
        static const T dZero = T();
        return mp->_value(&dZero);
    }

/**
@brief Function body as a string

Returns function body formatted as a string.
\par Example:
\code
using namespace cvm;
try{
    string_array vars, params, meanings;
    vars.push_back("x");
    vars.push_back("y");
    params.push_back("p");
    meanings.push_back("sqrt(x) + sqrt(y)");

    rfunction rf(vars, "p^2 + p + x + y", params, meanings);
    std::cout << rf.format() << std::endl;

    cfunction cf(vars, "p^2 + p + x + y", params, meanings);
    std::cout << cf.format() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
(sqrt(x)+sqrt(y))^2+sqrt(x)+sqrt(y)+x+y
(sqrt(x)+sqrt(y))^(2,0)+sqrt(x)+sqrt(y)+x+y
\endcode
@param[in] nPrecision Precision of numbers printed. Zero for
      auto mode (default).
@return std::string object.
*/
    std::string format(int nPrecision = 0) const {
        return mp->_format(nPrecision);
    }

/**
@brief Function variables and body as a string

Returns function variables and body formatted as a string in
<tt>[{var1,var2,...}] expr</tt> format.
\par Example:
\code
using namespace cvm;
try{
    string_array vars, params, meanings;
    vars.push_back("x");
    vars.push_back("y");
    params.push_back("p");
    meanings.push_back("sqrt(x) + sqrt(y)");

    rfunction rf(vars, "p^2 + p + x + y", params, meanings);
    std::cout << rf.vformat() << std::endl;

    cfunction cf(vars, "p^2 + p + x + y", params, meanings);
    std::cout << cf.vformat() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} (sqrt(x)+sqrt(y))^2+sqrt(x)+sqrt(y)+x+y
{x,y} (sqrt(x)+sqrt(y))^(2,0)+sqrt(x)+sqrt(y)+x+y
\endcode
@param[in] nPrecision Precision of numbers printed. Zero for
      auto mode (default).
@return std::string object.
*/
    std::string vformat(int nPrecision = 0) const
    {
        std::stringstream stream;
        std::string vars = __format_vars(this->mvars, false);
        if (vars.size() > 0) {
            stream << vars << CFUN_SSPACE;
        }
        stream << mp->_format(nPrecision);
        return stream.str();
    }

/**
@brief Functions comparison

Returns true if functions are equal and false otherwise.
\par Example:
\code
using namespace cvm;
try{
    string_array vars;
    vars.push_back("x");
    vars.push_back("y");

    rfunction rf1(vars, "x + y");
    rfunction rf2("{x,y} x + y");
    std::cout << (rf1 == rf2) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
1
\endcode
@param[in] rf Const reference to function to compare with.
@return boolean value.
*/
    bool operator == (const basic_function& rf) const {
        return vars_num() == rf.vars_num() && mp->_equals(rf.mp);
    }

/**
@brief Functions comparison

Returns true if functions are equal and false otherwise.
\par Example:
\code
using namespace cvm;
try{
    string_array vars;
    vars.push_back("x");
    vars.push_back("z");

    rfunction rf1(vars, "x + z");
    rfunction rf2("{x,y} x + y");
    std::cout << (rf1 != rf2) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
1
\endcode
@param[in] rf Const reference to function to compare with.
@return boolean value.
*/
    bool operator != (const basic_function& rf) const {
        return !operator == (rf);
    }

/**
@brief Partial derivative

Creates basic_function object as a partial derivative of calling
function. Variables set remains the same even if the derivative
doesn't depend on some of them. Partial derivative is computed
by <tt>nVarNum</tt>'s variable (0-based, 0 by default).
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x} x + log(x) + x^x");
    std::cout << rf1.drv() << std::endl;

    cfunction cf1("{x} x + log(x) + x^x");
    std::cout << cf1.drv() << std::endl;

    rfunction rf2("{x,y} sin(x)*cos(y) + sin(y)*cos(x) + x*y");
    std::cout << rf2.drv(0) << std::endl;
    std::cout << rf2.drv(1) << std::endl;

    cfunction cf2("{x,y} sin(x)*cos(y) + sin(y)*cos(x) + x*y");
    std::cout << cf2.drv(0) << std::endl;
    std::cout << cf2.drv(1) << std::endl;

    rfunction rf3("{x,y} x");
    std::cout << rf3.drv(0) << std::endl;
    std::cout << rf3.drv(1) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} 1+1/x+x^x*(log(x)+1)
{x} (1,0)+(1,0)/x+x^x*(log(x)+(1,0))
{x,y} cos(x)*cos(y)-sin(x)*sin(y)+y
{x,y} cos(y)*cos(x)-sin(y)*sin(x)+x
{x,y} cos(x)*cos(y)-sin(x)*sin(y)+y
{x,y} cos(y)*cos(x)-sin(y)*sin(x)+x
{x,y} 1
{x,y} 0
\endcode
@param[in] nVarNum Variable's index (0-based, 0 by default).
@return %Function object.
*/
    basic_function drv(size_t nVarNum = 0) const {
        return basic_function(mvars, mp->_deriv(nVarNum));
    }

/**
@brief Simplifier

Simplifies basic_function for fatsest numerical computation
possible and returns reference to the object changed.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x} x*2 + 3*x");
    std::cout << rf1.simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} x*5
\endcode
@return Reference to changed calling function.
*/
    basic_function& simp()
    {
        mp = mp->_simp();
        return *this;
    }

/**
@brief Assignment operator

Assigns basic_function object to calling one by performing deep
copying. No verifications made, it just copies over and returns
reference to the object changed.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x} x*2");
    rfunction rf2("{y} y*3");
    rf1 = rf2;
    std::cout << rf1 << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{y} y*3
\endcode
@param[in] rfSrc Const reference to function to assign.
@return Reference to changed calling function.
*/
    basic_function& operator = (const basic_function& rfSrc)
    {
        if (mp != rfSrc.mp) {
            // we don't check vars here, we just override...
            mvars = rfSrc.mvars;
            mp = rfSrc.mp;
        }
        return *this;
    }

/**
@brief Move assignment operator
*/
    basic_function& operator = (basic_function&& rfSrc)
    {
        if (mp != rfSrc.mp) {
            // we don't check vars here, we just override...
            mvars = std::move(rfSrc.mvars);
            mp = std::move(rfSrc.mp);
        }
        return *this;
    }

/**
@brief Assignment operator

Assigns real or complex number to calling basic_function object
and makes it to be constant function of variables originally
set. Returns reference to the object changed.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf1("{x} x*2");
rf1 = 1.234;
std::cout << rf1 << std::endl;
rfunction rf2;
rf2 = 1.234;
std::cout << rf2 << std::endl;
cfunction cf("{x} x*2");
cf = complex<double>(1., 2.);
std::cout << cf << std::endl;
\endcode
prints
\code
{x} 1.234
1.234
{x} (1,2)
\endcode
@param[in] rdSrc Const reference to number to assign.
@return Reference to changed calling function.
*/
    basic_function& operator = (const T& rdSrc)
    {
        mp = std::make_shared<Fconst<T>>(rdSrc);
        return *this;
    }

/**
@brief Unary minus operator

Creates basic_function object as calling function with opposite
sign.
\par Example:
\code
using namespace cvm;
rfunction rf("{x} x*2");
std::cout << -rf << std::endl;
std::cout << (-rf).simp() << std::endl;
cfunction cf("{x} x*(1,2)");
std::cout << -cf << std::endl;
std::cout << (-cf).simp() << std::endl;
\endcode
prints
\code
{x} -x*2
{x} (-2)*x
{x} -x*(1,2)
{x} (-1,-2)*x
\endcode
@return %Function object.
*/
    basic_function operator - () const {
        return basic_function(mvars, std::make_shared<Fuminus<T>>(mp));
    }

/**
@brief Addition operator

Creates basic_function object as sum of calling function and
function referred by <tt>rf</tt>. Operator throws \ref
cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x,y} x*2");
    rfunction rf2("{x,y} y*3");
    std::cout << (rf1 + rf2) << std::endl;

    cfunction cf1("{x,y} x*(1,2)");
    cfunction cf2("{x,y} y*(3,4)");
    std::cout << (cf1 + cf2) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x*2+y*3
{x,y} x*(1,2)+y*(3,4)
\endcode
@param[in] rf Const reference to function to add.
@return %Function object.
*/
    basic_function operator + (const basic_function& rf) const throw(cvmexception) {
        return basic_function(this->_check_vars(rf.mvars), std::make_shared<Fplus<T>>(mp, rf.mp));
    }

/**
@brief Subtraction operator

Creates basic_function object as difference of calling function
and function referred by <tt>rf</tt>. Operator throws \ref
cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try {
    rfunction rf1("{x,y} x*2");
    rfunction rf2("{x,y} y*3");
    std::cout << (rf1 - rf2) << std::endl;

    cfunction cf1("{x,y} x*(1,2)");
    cfunction cf2("{x,y} y*(3,4)");
    std::cout << (cf1 - cf2) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x*2-y*3
{x,y} x*(1,2)-y*(3,4)
\endcode
@param[in] rf Const reference to function to subtract.
@return %Function object.
*/
    basic_function operator - (const basic_function& rf) const throw(cvmexception) {
        return basic_function(this->_check_vars(rf.mvars), std::make_shared<Fminus<T>>(mp, rf.mp));
    }

/**
@brief Multiplication operator

Creates basic_function object as product of calling function
and function referred by <tt>rf</tt>. Operator throws \ref
cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x,y} x*2");
    rfunction rf2("{x,y} y*3");
    std::cout << (rf1 * rf2) << std::endl;
    std::cout << (rf1 * rf2).simp() << std::endl;

    cfunction cf1("{x,y} x*(1,2)");
    cfunction cf2("{x,y} y*(3,4)");
    std::cout << (cf1 * cf2) << std::endl;
    std::cout << (cf1 * cf2).simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x*2*y*3
{x,y} 6*y*x
{x,y} x*(1,2)*y*(3,4)
{x,y} (-5,10)*y*x
\endcode
@param[in] rf Const reference to function to multiply by.
@return %Function object.
*/
    basic_function operator *(const basic_function& rf) const throw(cvmexception) {
        return basic_function(this->_check_vars(rf.mvars), std::make_shared<Fmult<T>>(mp, rf.mp));
    }

/**
@brief Division operator

Creates basic_function object as calling function divided
by function referred by <tt>rf</tt>. Operator throws \ref
cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x,y} x*2");
    rfunction rf2("{x,y} y*2");
    std::cout << (rf1 / rf2) << std::endl;
    std::cout << (rf1 / rf2).simp() << std::endl;

    cfunction cf1("{x,y} x*(1,2)");
    cfunction cf2("{x,y} y*(3,4)");
    std::cout << (cf1 / cf2) << std::endl;
    std::cout << (cf1 / cf2).simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x*2/(y*2)
{x,y} x/y
{x,y} x*(1,2)/(y*(3,4))
{x,y} x*(0.44,0.08)/y
\endcode
@param[in] rf Const reference to function to divide by.
@return %Function object.
*/
    basic_function operator / (const basic_function& rf) const throw(cvmexception) {
        return basic_function(this->_check_vars(rf.mvars), std::make_shared<Fdiv<T>>(mp, rf.mp));
    }

/**
@brief Power to operator

Creates basic_function object as calling function
powered to function referred by <tt>rf</tt>. Operator throws
\ref cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x,y} x*2");
    rfunction rf2("{x,y} y*2");
    std::cout << (rf1 ^ rf2) << std::endl;

    cfunction cf1("{x,y} x*(1,2)");
    cfunction cf2("{x,y} y*(3,4)");
    std::cout << (cf1 ^ cf2) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} (x*2)^(y*2)
{x,y} (x*(1,2))^(y*(3,4))
\endcode
@param[in] rf Const reference to function to power to.
@return %Function object.
*/
    basic_function operator ^(const basic_function& rf) const throw(cvmexception) {
        return basic_function(this->_check_vars(rf.mvars), std::make_shared<Fpower<T>>(mp, rf.mp));
    }

/**
@brief Increment operator

Adds to calling basic_function
function referred by <tt>rf</tt> and returns reference to the
object changed. Operator throws
\ref cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x,y} x*3");
    rfunction rf2("{x,y} y*2");
    rf1 += rf2;
    std::cout << rf1 << std::endl;

    cfunction cf1("{x,y} x*(1,2)");
    cfunction cf2("{x,y} y*(3,4)");
    cf1 += cf2;
    std::cout << cf1 << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x*3+y*2
{x,y} x*(1,2)+y*(3,4)
\endcode
@param[in] rf Const reference to function to add.
@return Reference to changed calling function.
*/
    basic_function& operator += (const basic_function& rf) throw(cvmexception)
    {
        mvars = this->_check_vars(rf.mvars);
        mp = std::make_shared<Fplus<T>>(mp, rf.mp);
        return *this;
    }

/**
@brief Decrement operator

Subtracts from calling basic_function
function referred by <tt>rf</tt> and returns reference to the
object changed. Operator throws
\ref cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x,y} x*3");
    rfunction rf2("{x,y} y*2");
    rf1 -= rf2;
    std::cout << rf1 << std::endl;

    cfunction cf1("{x,y} x*(1,2)");
    cfunction cf2("{x,y} y*(3,4)");
    cf1 -= cf2;
    std::cout << cf1 << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x*3-y*2
{x,y} x*(1,2)-y*(3,4)
\endcode
@param[in] rf Const reference to function to subtract.
@return Reference to changed calling function.
*/
    basic_function& operator -= (const basic_function& rf) throw(cvmexception)
    {
        mvars = this->_check_vars(rf.mvars);
        mp = std::make_shared<Fminus<T>>(mp, rf.mp);
        return *this;
    }

/**
@brief Multiply and assign operator

Multiplies calling basic_function
by function referred by <tt>rf</tt> and returns reference to the
object changed. Operator throws
\ref cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x,y} x*3");
    rfunction rf2("{x,y} y*2");
    rf1 *= rf2;
    std::cout << rf1 << std::endl;
    std::cout << rf1.simp() << std::endl;

    cfunction cf1("{x,y} x*(1,2)");
    cfunction cf2("{x,y} y*(3,4)");
    cf1 *= cf2;
    std::cout << cf1 << std::endl;
    std::cout << cf1.simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x*3*y*2
{x,y} 6*y*x
{x,y} x*(1,2)*y*(3,4)
{x,y} (-5,10)*y*x
\endcode
@param[in] rf Const reference to function to multiply by.
@return Reference to changed calling function.
*/
    basic_function& operator *= (const basic_function& rf) throw(cvmexception)
    {
        mvars = this->_check_vars(rf.mvars);
        mp = std::make_shared<Fmult<T>>(mp, rf.mp);
        return *this;
    }

/**
@brief Divide and assign operator

Divides calling basic_function
by function referred by <tt>rf</tt> and returns reference to the
object changed. Operator throws
\ref cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    rfunction rf1("{x,y} x*3");
    rfunction rf2("{x,y} y*2");
    rf1 /= rf2;
    std::cout << rf1 << std::endl;
    std::cout << rf1.simp() << std::endl;

    cfunction cf1("{x,y} x*(1,2)");
    cfunction cf2("{x,y} y*(3,4)");
    cf1 /= cf2;
    std::cout << cf1 << std::endl;
    std::cout << cf1.simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x*3/(y*2)
{x,y} x*1.5/y
{x,y} x*(1,2)/(y*(3,4))
{x,y} x*(0.44,0.08)/y
\endcode
@param[in] rf Const reference to function to multiply by.
@return Reference to changed calling function.
*/
    basic_function& operator /= (const basic_function& rf) throw(cvmexception)
    {
        mvars = this->_check_vars(rf.mvars);
        mp = std::make_shared<Fdiv<T>>(mp, rf.mp);
        return *this;
    }

/**
@brief Addition operator

Creates basic_function object as sum of calling function and
real or complex number referred by <tt>d</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
std::cout << (rf + 2.) << std::endl;
cfunction cf("{x,y} x*(1,2)");
std::cout << (cf + complex<double>(3., 4.)) << std::endl;
\endcode
prints
\code
{x,y} x*3+2
{x,y} x*(1,2)+(3,4)
\endcode
@param[in] d Const reference to number to add.
@return %Function object.
*/
    basic_function operator + (const T& d) const throw(cvmexception) {
        return basic_function(mvars, std::make_shared<Fplus<T>>(mp, std::make_shared<Fconst<T>>(d)));
    }

/**
@brief Subtraction operator

Creates basic_function object as difference of calling function
and
real or complex number referred by <tt>d</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
std::cout << (rf - 2.) << std::endl;
cfunction cf("{x,y} x*(1,2)");
std::cout << (cf - complex<double>(3., 4.)) << std::endl;
\endcode
prints
\code
{x,y} x*3-2
{x,y} x*(1,2)-(3,4)
\endcode
@param[in] d Const reference to number to subtract.
@return %Function object.
*/
    basic_function operator - (const T& d) const throw(cvmexception) {
        return basic_function(mvars, std::make_shared<Fminus<T>>(mp, std::make_shared<Fconst<T>>(d)));
    }

/**
@brief Multiplication operator

Creates basic_function object as product of calling function
and
real or complex number referred by <tt>d</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
std::cout << rf * 2. << std::endl;
std::cout << (rf * 2.).simp() << std::endl;
cfunction cf("{x,y} x*(1,2)");
std::cout << cf * complex<double>(3., 4.) << std::endl;
std::cout << (cf * complex<double>(3., 4.)).simp() << std::endl;
\endcode
prints
\code
{x,y} x*3*2
{x,y} 6*x
{x,y} x*(1,2)*(3,4)
{x,y} (-5,10)*x
\endcode
@param[in] d Const reference to number to multiply by.
@return %Function object.
*/
    basic_function operator *(const T& d) const throw(cvmexception) {
        return basic_function(mvars, std::make_shared<Fmult<T>>(mp, std::make_shared<Fconst<T>>(d)));
    }

/**
@brief Division operator

Creates basic_function object as division of calling function
by real or complex number referred by <tt>d</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
std::cout << rf / 2. << std::endl;
std::cout << (rf / 2.).simp() << std::endl;
cfunction cf("{x,y} x*(1,2)");
std::cout << cf / complex<double>(3., 4.) << std::endl;
std::cout << (cf / complex<double>(3., 4.)).simp() << std::endl;
\endcode
prints
\code
{x,y} x*3/2
{x,y} x*1.5
{x,y} x*(1,2)/(3,4)
{x,y} x*(0.44,0.08)
\endcode
@param[in] d Const reference to number to multiply by.
@return %Function object.
*/
    basic_function operator / (const T& d) const throw(cvmexception) {
        return basic_function(mvars, std::make_shared<Fdiv<T>>(mp, std::make_shared<Fconst<T>>(d)));
    }

/**
@brief Power to operator

Creates basic_function object as power of calling function
to real or complex number referred by <tt>d</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
std::cout << (rf ^ 2.) << std::endl;
cfunction cf("{x,y} x*(1,2)");
std::cout << (cf ^ complex<double>(3., 4.)) << std::endl;
\endcode
prints
\code
{x,y} (x*3)^2
{x,y} (x*(1,2))^(3,4)
\endcode
@param[in] d Const reference to number to power to.
@return %Function object.
*/
    basic_function operator ^(const T& d) const throw(cvmexception) {
        return basic_function(mvars, std::make_shared<Fpower<T>>(mp, std::make_shared<Fconst<T>>(d)));
    }

/**
@brief Increment operator

Adds to calling basic_function
real or complex number referred by <tt>d</tt> and returns
reference to the object changed.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
rf += 2.;
std::cout << rf << std::endl;
cfunction cf("{x,y} x*(1,2)");
cf += complex<double>(3., 4.);
std::cout << cf << std::endl;
\endcode
prints
\code
{x,y} x*3+2
{x,y} x*(1,2)+(3,4)
\endcode
@param[in] d Number to add.
@return Reference to changed calling function.
*/
    basic_function& operator += (const T& d) throw(cvmexception)
    {
        mp = std::make_shared<Fplus<T>>(mp, std::make_shared<Fconst<T>>(d));
        return *this;
    }

/**
@brief Decrement operator

Subtracts from calling basic_function
real or complex number referred by <tt>d</tt> and returns
reference to the object changed.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
rf -= 2.;
std::cout << rf << std::endl;
cfunction cf("{x,y} x*(1,2)");
cf -= complex<double>(3., 4.);
std::cout << cf << std::endl;
\endcode
prints
\code
{x,y} x*3-2
{x,y} x*(1,2)-(3,4)
\endcode
@param[in] d Number to subtract.
@return Reference to changed calling function.
*/
    basic_function& operator -= (const T& d) throw(cvmexception)
    {
        mp = std::make_shared<Fminus<T>>(mp, std::make_shared<Fconst<T>>(d));
        return *this;
    }

/**
@brief Multiply and assign operator

Multiplies calling basic_function by
real or complex number referred by <tt>d</tt> and returns
reference to the object changed.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
rf *= 2.;
std::cout << rf << std::endl;
std::cout << rf.simp() << std::endl;
cfunction cf("{x,y} x*(1,2)");
cf *= complex<double>(3., 4.);
std::cout << cf << std::endl;
std::cout << cf.simp() << std::endl;
\endcode
prints
\code
{x,y} x*3*2
{x,y} 6*x
{x,y} x*(1,2)*(3,4)
{x,y} (-5,10)*x
\endcode
@param[in] d Number to multiply by.
@return Reference to changed calling function.
*/
    basic_function& operator *= (const T& d) throw(cvmexception)
    {
        mp = std::make_shared<Fmult<T>>(mp, std::make_shared<Fconst<T>>(d));
        return *this;
    }

/**
@brief Divide and assign operator

Divides calling basic_function by
real or complex number referred by <tt>d</tt> and returns
reference to the object changed.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
rf /= 2.;
std::cout << rf << std::endl;
std::cout << rf.simp() << std::endl;
cfunction cf("{x,y} x*(1,2)");
cf /= complex<double>(3., 4.);
std::cout << cf << std::endl;
std::cout << cf.simp() << std::endl;
\endcode
prints
\code
{x,y} x*3/2
{x,y} x*1.5
{x,y} x*(1,2)/(3,4)
{x,y} x*(0.44,0.08)
\endcode
@param[in] d Number to divide by.
@return Reference to changed calling function.
*/
    basic_function& operator /= (const T& d) throw(cvmexception)
    {
        mp = std::make_shared<Fdiv<T>>(mp, std::make_shared<Fconst<T>>(d));
        return *this;
    }

/**
@brief Power to and assign operator

Powers calling basic_function to
real or complex number referred by <tt>d</tt> and returns
reference to the object changed.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
rf ^= 2.;
std::cout << rf << std::endl;
cfunction cf("{x,y} x*(1,2)");
cf ^= complex<double>(3., 4.);
std::cout << cf << std::endl;
\endcode
prints
\code
{x,y} (x*3)^2
{x,y} (x*(1,2))^(3,4)
\endcode
@param[in] d Number to power to.
@return Reference to changed calling function.
*/
    basic_function& operator ^= (const T& d) throw(cvmexception)
    {
        mp = std::make_shared<Fpower<T>>(mp, std::make_shared<Fconst<T>>(d));
        return *this;
    }

/**
@brief Satellite function

Creates basic_function object as satellite of calling
basic_function
and function referred by <tt>rf</tt>.
For complex numbers real part is used only.
Function throws \ref cvmexception if variables don't match.
\f[
\texttt{sat(x,y)}=

 \left\{
  \begin{aligned}
1 & \text{ if } x > |y|\\
0 & \text{ if } -|y| \le x \le |y|\\
-1 & \text{ if } x < -|y|
  \end{aligned}
 \right.\\
\f]

\par Example:
\code
using namespace cvm;
using namespace std;
try{
    rfunction rf1("{x,y} x");
    rfunction rf2("{x,y} y");
    rfunction rs = rf1.sat(rf2);
    std::cout << rs << std::endl;
    std::cout << rs(1.5,1.) << std::endl;
    std::cout << rs(0.5,1.) << std::endl;
    std::cout << rs(-1.5,1.) << std::endl;

    cfunction cf1("{x,y} x");
    cfunction cf2("{x,y} y");
    cfunction cs = cf1.sat(cf2);
    std::cout << cs << std::endl;
    std::cout << cs(complex<double>(1.5, 0.), complex<double>(1., 1.)) << std::endl;
    std::cout << cs(complex<double>(0.5, 0.), complex<double>(1., 1.)) << std::endl;
    std::cout << cs(complex<double>(-1.5, 0.), complex<double>(1., 1.)) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} sat(x,y)
1
0
-1
{x,y} sat(x,y)
(1,0)
(0,0)
(-1,0)
\endcode
@param[in] rf Const reference to other function.
@return %Function object.
*/
    basic_function sat(const basic_function& rf) const throw(cvmexception) {
        return basic_function(this->_check_vars(rf.mvars), std::make_shared<Fsat<T>>(mp, rf.mp));
    }

/**
@brief Exponent

Creates basic_function object as exponent of calling function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.exp();
std::cout << re << std::endl;
std::cout << re(1.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.exp();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0., 3.141592653589793)) << std::endl;
\endcode
prints
\code
{x} exp(x)
2.71828
{x} exp(x)
(-1,1.22465e-016)
\endcode
@return %Function object.
*/
    basic_function exp() const {
        return basic_function(mvars, std::make_shared<Fexp<T>>(mp));
    }

/**
@brief Square root

Creates basic_function object as square root of calling
function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.sqrt();
std::cout << re << std::endl;
std::cout << re(9.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.sqrt();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(-1.,0.)) << std::endl;
\endcode
prints
\code
{x} sqrt(x)
3
{x} sqrt(x)
(0,1)
\endcode
@return %Function object.
*/
    basic_function sqrt() const {
        return basic_function(mvars, std::make_shared<Fsqrt<T>>(mp));
    }

/**
@brief Natural logarithm

Creates basic_function object as natural logarithm of calling
function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.log();
std::cout << re << std::endl;
std::cout << re(::exp(1.)) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.log();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(-1.,0.)) << std::endl;
\endcode
prints
\code
{x} log(x)
1
{x} log(x)
(0,3.14159)
\endcode
@return %Function object.
*/
    basic_function log() const {
        return basic_function(mvars, std::make_shared<Flog<T>>(mp));
    }

/**
@brief Common (base 10) logarithm

Creates basic_function object as common (base 10) logarithm of
calling function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.log10();
std::cout << re << std::endl;
std::cout << re(100.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.log10();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(1.,0.)) << std::endl;
\endcode
prints
\code
{x} log10(x)
2
{x} log10(x)
(0,0)
\endcode
@return %Function object.
*/
    basic_function log10() const {
        return basic_function(mvars, std::make_shared<Flog10<T>>(mp));
    }

/**
@brief Sine

Creates basic_function object as sine of
calling function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.sin();
std::cout << re << std::endl;
std::cout << re(3.141592653589793 / 2.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.sin();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,3.141592653589793)) << std::endl;
\endcode
prints
\code
{x} sin(x)
1
{x} sin(x)
(0,11.5487)
\endcode
@return %Function object.
*/
    basic_function sin() const {
        return basic_function(mvars, std::make_shared<Fsin<T>>(mp));
    }

/**
@brief Cosine

Creates basic_function object as cosine of
calling function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.cos();
std::cout << re << std::endl;
std::cout << re(0.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.cos();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,3.141592653589793)) << std::endl;
\endcode
prints
\code
{x} cos(x)
1
{x} cos(x)
(11.592,-0)
\endcode
@return %Function object.
*/
    basic_function cos() const {
        return basic_function(mvars, std::make_shared<Fcos<T>>(mp));
    }

/**
@brief Tangent

Creates basic_function object as tangent of calling function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.tan();
std::cout << re << std::endl;
std::cout << re(3.141592653589793 / 4.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.tan();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,3.141592653589793)) << std::endl;
\endcode
prints
\code
{x} tan(x)
1
{x} tan(x)
(0,0.996272)
\endcode
@return %Function object.
*/
    basic_function tan() const {
        return basic_function(mvars, std::make_shared<Ftan<T>>(mp));
    }

/**
@brief Arcsine

Creates basic_function object as arcsine of calling function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.asin();
std::cout << re << std::endl;
std::cout << re(-1.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.asin();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,2.)) << std::endl;
\endcode
prints
\code
{x} asin(x)
-1.5708
{x} asin(x)
(0,1.44364)
\endcode
@return %Function object.
*/
    basic_function asin() const {
        return basic_function(mvars, std::make_shared<Fasin<T>>(mp));
    }

/**
@brief Arc cosine

Creates basic_function object as arc cosine of calling function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.acos();
std::cout << re << std::endl;
std::cout << re(-1.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.acos();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,2.)) << std::endl;
\endcode
prints
\code
{x} acos(x)
3.14159
{x} acos(x)
(1.5708,-1.44364)
\endcode
@return %Function object.
*/
    basic_function acos() const {
        return basic_function(mvars, std::make_shared<Facos<T>>(mp));
    }

/**
@brief Arc tangent

Creates basic_function object as arc tangent of calling
function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.atan();
std::cout << re << std::endl;
std::cout << re(1.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.atan();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,0.5)) << std::endl;
\endcode
prints
\code
{x} atan(x)
0.785398
{x} atan(x)
(0,0.549306)
\endcode
@return %Function object.
*/
    basic_function atan() const {
        return basic_function(mvars, std::make_shared<Fatan<T>>(mp));
    }

/**
@brief Hyperbolic sine

Creates basic_function object as hyperbolic sine of calling
function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.sinh();
std::cout << re << std::endl;
std::cout << re(1.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.sinh();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,1.)) << std::endl;
\endcode
prints
\code
{x} sinh(x)
1.1752
{x} sinh(x)
(0,0.841471)
\endcode
@return %Function object.
*/
    basic_function sinh() const {
        return basic_function(mvars, std::make_shared<Fsinh<T>>(mp));
    }

/**
@brief Hyperbolic cosine

Creates basic_function object as hyperbolic cosine of calling
function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.cosh();
std::cout << re << std::endl;
std::cout << re(1.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.cosh();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,1.)) << std::endl;
\endcode
prints
\code
{x} cosh(x)
1.54308
{x} cosh(x)
(0.540302,0)
\endcode
@return %Function object.
*/
    basic_function cosh() const {
        return basic_function(mvars, std::make_shared<Fcosh<T>>(mp));
    }

/**
@brief Hyperbolic tangent

Creates basic_function object as hyperbolic tangent of calling
function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.tanh();
std::cout << re << std::endl;
std::cout << re(1.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.tanh();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,1.)) << std::endl;
\endcode
prints
\code
{x} tanh(x)
0.761594
{x} tanh(x)
(0,1.55741)
\endcode
@return %Function object.
*/
    basic_function tanh() const {
        return basic_function(mvars, std::make_shared<Ftanh<T>>(mp));
    }

/**
@brief Integral sine

Creates basic_function object as integral sine of calling
function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.sinint();
std::cout << re << std::endl;
std::cout << re(1000.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.sinint();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,1.)) << std::endl;
\endcode
prints
\code
{x} sinint(x)
1.57023
{x} sinint(x)
(0,1.05725)
\endcode
@return %Function object.
*/
    basic_function sinint() const {
        return basic_function(mvars, std::make_shared<Fsinint<T>>(mp));
    }

/**
@brief Integral cosine

Creates basic_function object as integral cosine of calling
function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.cosint();
std::cout << re << std::endl;
std::cout << re(1000.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.cosint();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(0.,1.)) << std::endl;
\endcode
prints
\code
{x} cosint(x)
0.000826316
{x} cosint(x)
(0.837867,1.5708)
\endcode
@return %Function object.
*/
    basic_function cosint() const {
        return basic_function(mvars, std::make_shared<Fcosint<T>>(mp));
    }

/**
@brief Sign

Creates basic_function object as sign of calling
function. For complex numbers real part is used only.
\f[
\texttt{sign(x)}=

 \left\{
  \begin{aligned}
-1 & \text{ if } x < 0\\
 0 & \text{ if } x = 0\\
 1 & \text{ if } x > 0
  \end{aligned}
 \right.\\
\f]

\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.sign();
std::cout << re << std::endl;
std::cout << re(-2.) << std::endl;
std::cout << re(0.) << std::endl;
std::cout << re(3.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.sign();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(-2.,1.)) << std::endl;
std::cout << ce(complex<double>(0.,1.)) << std::endl;
std::cout << ce(complex<double>(3.,1.)) << std::endl;
\endcode
prints
\code
{x} sign(x)
-1
0
1
{x} sign(x)
(-1,0)
(0,0)
(1,0)
\endcode
@return %Function object.
*/
    basic_function sign() const {
        return basic_function(mvars, std::make_shared<Fsign<T>>(mp));
    }

/**
@brief Absolute value

Creates basic_function object as absolute value of calling
function.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x} x");
rfunction re = rf.abs();
std::cout << re << std::endl;
std::cout << re(-2.) << std::endl;
std::cout << re(0.) << std::endl;
std::cout << re(3.) << std::endl;
cfunction cf("{x} x");
cfunction ce = cf.abs();
std::cout << ce << std::endl;
std::cout << ce(complex<double>(-1.,1.)) << std::endl;
std::cout << ce(complex<double>(0.,1.)) << std::endl;
std::cout << ce(complex<double>(1.,1.)) << std::endl;
\endcode
prints
\code
{x} abs(x)
2
0
3
{x} abs(x)
(1.41421,0)
(1,0)
(1.41421,0)
\endcode
@return %Function object.
*/
    basic_function abs() const {
        return basic_function(mvars, std::make_shared<Fabs<T>>(mp));
    }

/**
@brief Delta function

Creates basic_function object as delta function of calling
basic_function
and function referred by <tt>rf</tt>.
For complex numbers real part is used only.
Function throws \ref cvmexception if variables don't match.
\f[
\texttt{delta(x,y)}=

 \left\{
  \begin{aligned}
0 & \text{ if } |x - y| = 0\\
\infty & \text{ if } |x - y| \ne 0
  \end{aligned}
 \right.\\
\f]

\par Example:
\code
using namespace cvm;
using namespace std;
try{
    rfunction rf1("{x,y} x");
    rfunction rf2("{x,y} y");
    rfunction rd = rf1.delta(rf2);
    std::cout << rd << std::endl;
    std::cout << rd(1.5,1.) << std::endl;
    std::cout << rd(1.,1.) << std::endl;

    cfunction cf1("{x,y} x");
    cfunction cf2("{x,y} y");
    cfunction cd = cf1.delta(cf2);
    std::cout << cd << std::endl;
    std::cout << cd(complex<double>(1., 1.), complex<double>(1., 0.)) << std::endl;
    std::cout << cd(complex<double>(0.5, 1.), complex<double>(1., 0.)) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what() << std::endl;
}
\endcode
prints
\code
{x,y} delta(x,y)
0
1.79769e+308
{x,y} delta(x,y)
(1.79769e+308,0)
(0,0)
\endcode
@param[in] rf Const reference to other function.
@return %Function object.
*/
    basic_function delta(const basic_function& rf) const throw(cvmexception) {
        return basic_function(this->_check_vars(rf.mvars), std::make_shared<Fdelta<T>>(mp, rf.mp));
    }

/**
@brief Immediate If function

Creates basic_function object as Immediate If function of
calling basic_function
and functions referred by <tt>rfNeg</tt> and <tt>rfNotNeg</tt>.
For complex numbers real part of calling function is used only.
Function throws
\ref cvmexception if variables don't match.
\f[
\texttt{iif(x,expr1,expr2)}=

 \left\{
  \begin{aligned}
expr1 & \text{ if } x < 0\\
expr2 & \text{ if } x \ge 0
  \end{aligned}
 \right.\\
\f]

\par Example:
\code
using namespace cvm;
using namespace std;
try{
    rfunction rf("{x,y,z} x");
    rfunction rf1("{x,y,z} y");
    rfunction rf2("{x,y,z} z");
    rfunction ri = rf.iif(rf1,rf2);
    std::cout << ri << std::endl;
    std::cout << ri(-1.,1.,2.) << std::endl;
    std::cout << ri( 0.,1.,2.) << std::endl;

    cfunction cf("{x,y,z} x");
    cfunction cf1("{x,y,z} y");
    cfunction cf2("{x,y,z} z");
    cfunction ci = cf.iif(cf1,cf2);
    std::cout << ci << std::endl;
    std::cout << ci(complex<double>(-1.,1.), complex<double>(1.,1.), complex<double>(2.,2.)) << std::endl;
    std::cout << ci(complex<double>( 0.,1.), complex<double>(1.,1.), complex<double>(2.,2.)) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what() << std::endl;
}
\endcode
prints
\code
{x,y,z} iif(x,y,z)
1
2
{x,y,z} iif(x,y,z)
(1,1)
(2,2)
\endcode
@param[in] rfNeg Const reference to function for negative calling function value.
@param[in] rfNotNeg Const reference to function for non-negative calling function value.
@return %Function object.
*/
    basic_function iif (const basic_function& rfNeg,
                        const basic_function& rfNotNeg) const throw(cvmexception)
    {
        const string_array& saVarsT = this->_check_vars(rfNeg.mvars);
        const string_array& saVarsF = this->_check_vars(rfNotNeg.mvars);
        const string_array& saVars = saVarsT.size() > saVarsF.size() ? saVarsT : saVarsF;
        return basic_function(saVars, std::make_shared<Fiif<T>>(mp, rfNeg.mp, rfNotNeg.mp));
    }

/**
@brief Addition operator (left sided)

Creates basic_function object as sum of real or complex number referred by <tt>d</tt>
and function referred by <tt>rf</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
std::cout << (2. + rf) << std::endl;
cfunction cf("{x,y} x*(1,2)");
std::cout << (complex<double>(3., 4.) + cf) << std::endl;
\endcode
prints
\code
{x,y} x*3+2
{x,y} x*(1,2)+(3,4)
\endcode
@param[in] d Const reference to number to add to.
@param[in] rf Const reference to function to add.
@return %Function object.
*/
    friend basic_function<T> operator + (const T& d, const basic_function<T>& rf) {
        return rf + d;
    }

/**
@brief Subtraction operator (left sided)

Creates basic_function object as difference of real or complex number referred by <tt>d</tt>
and function referred by <tt>rf</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
std::cout << (2. - rf) << std::endl;
cfunction cf("{x,y} x*(1,2)");
std::cout << (complex<double>(3., 4.) - cf) << std::endl;
\endcode
prints
\code
{x,y} 2-x*3
{x,y} (3,4)-x*(1,2)
\endcode
@param[in] d Const reference to number to subtract from.
@param[in] rf Const reference to function to subtract.
@return %Function object.
*/
    friend basic_function<T> operator - (const T& d, const basic_function<T>& rf) {
        return basic_function(rf.mvars, std::make_shared<Fminus<T>>(std::make_shared<Fconst<T>>(d), rf.mp));
    }

/**
@brief Multiplication operator (left sided)

Creates basic_function object as product of real or complex number referred by <tt>d</tt>
and function referred by <tt>rf</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*3");
std::cout << 2. * rf << std::endl;
std::cout << (2. * rf).simp() << std::endl;
cfunction cf("{x,y} x*(1,2)");
std::cout << complex<double>(3., 4.) * cf << std::endl;
std::cout << (complex<double>(3., 4.) * cf).simp() << std::endl;
\endcode
prints
\code
{x,y} x*3*2
{x,y} 6*x
{x,y} x*(1,2)*(3,4)
{x,y} (-5,10)*x
\endcode
@param[in] d Const reference to number to multiply.
@param[in] rf Const reference to function to multiply by.
@return %Function object.
*/
    friend basic_function<T> operator *(const T& d, const basic_function<T>& rf) {
        return rf * d;
    }

/**
@brief Division operator (left sided)

Creates basic_function object as division of real or complex number referred by <tt>d</tt>
by function referred by <tt>rf</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*2");
std::cout << 3. / rf << std::endl;
std::cout << (3. / rf).simp() << std::endl;
cfunction cf("{x,y} x*(3,4)");
std::cout << complex<double>(1., 2.) / cf << std::endl;
std::cout << (complex<double>(1., 2.) / cf).simp() << std::endl;
\endcode
prints
\code
{x,y} 3/(x*2)
{x,y} 1.5/x
{x,y} (1,2)/(x*(3,4))
{x,y} (0.44,0.08)/x
\endcode
@param[in] d Const reference to number to divide.
@param[in] rf Const reference to function to divide by.
@return %Function object.
*/
    friend basic_function<T> operator / (const T& d, const basic_function<T>& rf) {
        return basic_function(rf.mvars, std::make_shared<Fdiv<T>>(std::make_shared<Fconst<T>>(d), rf.mp));
    }

/**
@brief Power to operator (left sided)

Creates basic_function object as power of real or complex number referred by <tt>d</tt>
to function referred by <tt>rf</tt>
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x");
std::cout << (2. ^ rf) << std::endl;
cfunction cf("{x,y} x");
std::cout << (complex<double>(3., 4.) ^ cf) << std::endl;
\endcode
prints
\code
{x,y} 2^x
{x,y} (3,4)^x
\endcode
@param[in] d Const reference to number to power.
@param[in] rf Const reference to function to power to.
@return %Function object.
*/
    friend basic_function<T> operator ^(const T& d, const basic_function<T>& rf) {
        return basic_function(rf.mvars, std::make_shared<Fpower<T>>(std::make_shared<Fconst<T>>(d), rf.mp));
    }

/**
@brief Output operator

Prints function referred by <tt>rf</tt>
to output stream referred by <tt>os</tt>
using Wolfram's Mathemaca syntax
<tt>{var1[,var2,...]} expr</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
rfunction rf("{x,y} x*y + x^2");
std::cout << rf << std::endl;
cfunction cf("{x,y} x*y + x^2");
std::cout << cf << std::endl;
\endcode
prints
\code
{x,y} x*y+x^2
{x,y} x*y+x^(2,0)
\endcode
@param[in] os Reference to output stream.
@param[in] rf Const reference to function to print.
@return Reference to output stream.
*/
    friend std::ostream& operator <<(std::ostream& os, const basic_function& rf)
    {
        std::string vars = __format_vars(rf.mvars, false);
        if (vars.size() > 0) {
            os << vars << CFUN_SSPACE;
        }
        os << rf.format(0);
        return os;
    }

//! @cond INTERNAL
    // sets caller to be equal to scalar product of fa1 and fa2
    basic_function& _scalar_product(size_t nSize,
                                    const basic_function* fa1, size_t incr1,
                                    const basic_function* fa2, size_t incr2) throw(cvmexception)
    {
        const string_array& saVars1 = this->_check_vars(fa1->vars());
        const string_array& saVars2 = this->_check_vars(fa2->vars());
        const string_array& saVars = saVars1.size() > saVars2.size() ? saVars1 : saVars2;

        mvars = saVars;
        mp = std::make_shared<Fconst<T>>();

        for (size_t i = 0; i < nSize; ++i, fa1 += incr1, fa2 += incr2)
        {
            *this += (*fa1) * (*fa2);
        }
        return this->simp();
    }
//! @endcond
};


//! @cond INTERNAL
template<typename BaseFunction>
inline void _copy(size_t nSize,
                  const BaseFunction* faFrom, size_t incrFrom,
                  BaseFunction* faTo, size_t incrTo)
{
    for (size_t i = 0; i < nSize; ++i, faFrom += incrFrom, faTo += incrTo) {
        *faTo = *faFrom;
    }
}
//! @endcond


/**
@brief Generalized array of functions class (not end-user)

\c T type stands for \ref treal or \ref tcomplex. Please use inherited verctor and matrix classes.
This one provides some member functions which are common for all arrays.
@see basic_fvector
@see basic_fmatrix
*/
template<typename T>
class FArray
{
protected:
    using BaseFunction = basic_function<T>; //!< Vector element, i.e. \ref rfunction or \ref cfunction

    std::vector<BaseFunction> mv; //!< Internal storage

//! @cond INTERNAL
    void _assign(const FArray& a)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) = a.mv.at(i);
        }
    }
//! @endcond

public:
    /**
     * @brief Default constructor
     *
     * Creates empty array. No memory gets allocated.
     */
    FArray()
      : mv()
    {
    }

    /**
     * @brief Constructor
     *
     * Creates function array of \c nSize size.
     * @param[in] nSize Number of functions in array.
     */
    explicit FArray(size_t nSize)
      : mv(nSize)
    {
    }

    /**
     * @brief Copy constructor
     *
     * Creates function array as a copy of array \c a.
     * @param[in] a Function array to copy.
     */
    FArray(const FArray& a)
      : mv(a.size())
    {
        this->_assign(a);
    }

    /**
     * @brief Move constructor
     */
    FArray(FArray&& a)
      : mv(std::move(a.mv))
    {
    }

    /**
     * @brief Constructor
     *
     * Creates function array of \c saInput.size() size.
     * Each function element is ininialized using appropriate string as
     * parameterless input in Wolfram's Mathemaca syntax <tt>{var1[,var2,...]} expr</tt>
     * @param[in] saInput Array of strings.
     */
    explicit FArray(const string_array& saInput) 
      : mv(saInput.size())
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) = BaseFunction(saInput[i]);
        }
    }

    /**
     * @brief Constructor
     *
     * Creates array of parameterized functions of one or more variables of \c saBodies.size() size.
     * @param[in] saVars String array with variables (may be empty).
     * @param[in] saBodies String array with function expressions.
     *            For expression syntax look at \ref basic_function description.
     * @param[in] saParameters String array with parameters (may be empty).
     * @param[in] saMeanings String array with parameters' meanings (may be empty,
     *            must have the same size as saParameters).
     */
    FArray(const string_array& saVars, const string_array& saBodies,
           const string_array& saParameters, const string_array& saMeanings)
      : mv(saBodies.size())
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) = BaseFunction(saVars, saBodies[i], saParameters, saMeanings);
        }
    }

    /**
     * @brief Destructor
     */
    virtual ~FArray()
    {
    }

/**
@brief Internal storage

Functional arrays are implemented as <tt>std::vector</tt>s. 
This function returns non-const reference to the storage.
@return Reference to internal <tt>std::vector</tt> storage.
*/
    std::vector<BaseFunction>& impl() {
        return mv;
    }

/**
@brief Internal storage

Functional arrays are implemented as <tt>std::vector</tt>s. 
This function returns const reference to the storage.
@return Reference to internal <tt>std::vector</tt> storage.
*/
    const std::vector<BaseFunction>& impl() const {
        return mv;
    }

/**
@brief Internal storage

Functional arrays are implemented as <tt>std::vector</tt>s. 
This function returns number of elements in the storage. 
For matrices it's a product of numbers of rows and 
columns.
@return Storage size as <tt>size_t</tt>. 
*/
    size_t size() const {
        return mv.size();
    }

/**
@brief Constant reference to element (\e not l-value, \e zero-based)

Returns constant reference to array's element by its index (\e 
zero based). 
@param[in] n Index of element to return reference to.
@return basic_function<T>
*/
    const BaseFunction& operator [] (size_t n) const {
        return mv.at(n);
    }

/**
@brief Reference to element (l-value, \e zero-based)

Returns reference to array's element by its index 
(\e zero based).
@param[in] n Index of element to return reference to.
@return basic_function<T>
*/
    BaseFunction& operator [] (size_t n) {
        return mv.at(n);
    }

/**
@brief Numerical value

Returns numerical value of array of functions of no variables.
Arrays' dimensions are not verified.
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("2*2");
funcs.push_back("3*3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector ry(2);
cvector cy(2);
rfv.value(ry);
cfv.value(cy);
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
2*2 3*3
4 9

(2,0)*(2,0) (3,0)*(3,0)
(4,0) (9,0)
\endcode
@param[out] pv Array of functions' values.
*/
    void value(T* pv) const {
        value(nullptr, pv);
    }

/**
@brief Numerical value

Returns numerical value of array of functions of one variable.
Arrays' dimensions are not verified.
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
rfvector rfv(funcs);
cfvector cfv(funcs);
rvector ry(3);
cvector cy(3);

rfv.value(2., ry);
cfv.value(complex<double>(2.,0.), cy);
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^2 {x} x^3
2 4 8

{x} x {x} x^(2,0) {x} x^(3,0)
(2,0) (4,0) (8,0)
\endcode
@param[in] d Variable's value.
@param[out] pv Array of functions' values.
*/
    void value(T d, T* pv) const {
        value(&d, pv);
    }

/**
@brief Numerical value

Returns numerical value of array of functions of two variables.
Arrays' dimensions are not verified.
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x,y} x+y");
funcs.push_back("{x,y} x*y");
funcs.push_back("{x,y} x^y");
rfvector rfv(funcs);
cfvector cfv(funcs);
rvector ry(3);
cvector cy(3);

rfv.value(2., 3., ry);
cfv.value(complex<double>(2.,0.), complex<double>(3.,0.), cy);
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x,y} x+y {x,y} x*y {x,y} x^y
5 6 8

{x,y} x+y {x,y} x*y {x,y} x^y
(5,0) (6,0) (8,0)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@param[out] pv Array of functions' values.
*/
    void value(T d1, T d2, T* pv) const {
        T d[2];
        d[0] = d1;
        d[1] = d2;
        value(d, pv);
    }

/**
@brief Numerical value

Returns numerical value of array of functions of three variables.
Arrays' dimensions are not verified.
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x,y,z} x+y+z");
funcs.push_back("{x,y,z} x*y*z");
funcs.push_back("{x,y,z} x^y^z");
rfvector rfv(funcs);
cfvector cfv(funcs);
rvector ry(3);
cvector cy(3);

rfv.value(3., 2., 2., ry);
cfv.value(complex<double>(3.,0.), complex<double>(2.,0.), complex<double>(2.,0.), cy);
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x,y,z} x+y+z {x,y,z} x*y*z {x,y,z} x^(y^z)
7 12 81

{x,y,z} x+y+z {x,y,z} x*y*z {x,y,z} x^(y^z)
(7,0) (12,0) (81,0)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@param[in] d3 Third variable's value.
@param[out] pv Array of functions' values.
*/
    void value(T d1, T d2, T d3, T* pv) const {
        T d[3];
        d[0] = d1;
        d[1] = d2;
        d[2] = d3;
        value(d, pv);
    }

/**
@brief Numerical value

Returns numerical value of array of functions for given values of
variables. Arrays' dimensions are not verified.
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector rx(1), ry(3);
cvector cx(1), cy(3);
rx(1) = 2.;
cx(1) = complex<double>(2.,0.);
rfv.value(rx, ry);
cfv.value(cx, cy);
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^2 {x} x^3
2 4 8

{x} x {x} x^(2,0) {x} x^(3,0)
(2,0) (4,0) (8,0)
\endcode
@param[in] pd Array of variables' values.
@param[out] pv Array of functions' values.
*/
    void value(const T* pd, T* pv) const
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            pv[i] = mv.at(i).value(pd);
        }
    }

//! @cond INTERNAL
protected:
    void _drv(const FArray& a, size_t nVarNum)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) = a.mv.at(i).drv(nVarNum);
        }
    }

    void _simp()
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i).simp();
        }
    }

    bool _equals(const FArray& a) const
    {
        bool bResult = true;
        if (mv.size() != a.size()) {
            bResult = false;
        }
        else
            for (size_t i = 0; i < mv.size(); ++i) {
                if (mv.at(i) != a.mv.at(i)) {
                    bResult = false;
                    break;
                }
            }
        return bResult;
    }

    void _add(const FArray& a)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) += a.mv.at(i);
        }
    }

    void _subtract(const FArray& a)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) -= a.mv.at(i);
        }
    }

    void _add(const FArray& a1, const FArray& a2)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) = a1.mv.at(i) + a2.mv.at(i);
        }
    }

    void _subtract(const FArray& a1, const FArray& a2)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) = a1.mv.at(i) - a2.mv.at(i);
        }
    }

    void _mult(const BaseFunction& f)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) *= f;
        }
    }

    void _div(const BaseFunction& f)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) /= f;
        }
    }

    void _mult(const FArray& a, const BaseFunction& f)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) = a.mv.at(i) * f;
        }
    }

    void _div(const FArray& a, const BaseFunction& f)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) = a.mv.at(i) / f;
        }
    }

    void _mult(const T& t)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) *= t;
        }
    }

    void _div(const T& t)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) /= t;
        }
    }

    void _mult(const FArray& a, const T& t)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) = a.mv.at(i) * t;
        }
    }

    void _div(const FArray& a, const T& t)
    {
        for (size_t i = 0; i < mv.size(); ++i) {
            mv.at(i) = a.mv.at(i) / t;
        }
    }
//! @endcond
};


template<typename T> class basic_fvector;
template<typename T> class basic_fmatrix;


/**
@brief Vector of functions class

\c T type stands for \ref treal or \ref tcomplex.
Please use \ref rfvector and \ref cfvector classes in your applications.
@see rfvector
@see cfvector
@see basic_fmatrix
*/
template<typename T>
class basic_fvector : public FArray<T>
{
    friend class basic_fmatrix<T>;  // for _mult

protected:
    using BaseFArray = FArray<T>; //!< Base class
    using BaseFunction = typename BaseFArray::BaseFunction; //!< Vector element, i.e. \ref rfunction or \ref cfunction

//! @cond INTERNAL
    void _check_size(size_t size) const throw(cvmexception)
    {
        _check_ne(CVM_SIZESMISMATCH, size, this->size());
    }
//! @endcond

public:
/**
@brief Default constructor

Creates empty vector of functions. No memory gets allocated.
*/
    basic_fvector()
      : BaseFArray()
    {
    }

/**
@brief Constructor

Creates vector of zero functions of \c nSize size.
\par Example:
\code
using namespace cvm;
rfvector rfv(3);
std::cout << rfv;
cfvector cfv(3);
std::cout << cfv;
\endcode
prints
\code
0 0 0
(0,0) (0,0) (0,0)
\endcode
@param[in] nSize Number of functions in array.
*/
    explicit basic_fvector(size_t nSize)
      : BaseFArray(nSize)
    {
    }

/**
@brief Constructor

Creates vector of functions of \c saInput.size() size.
Each function element is ininialized using appropriate string as
parameterless input in Wolfram's Mathemaca syntax <tt>{var1[,var2,...]} expr</tt>
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector rx(1), ry(3);
cvector cx(1), cy(3);
rx(1) = 2.;
cx(1) = complex<double>(2.,0.);
rfv.value(rx, ry);
cfv.value(cx, cy);
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^2 {x} x^3
2 4 8

{x} x {x} x^(2,0) {x} x^(3,0)
(2,0) (4,0) (8,0)
\endcode
@param[in] saInput Array of strings.
*/
    explicit basic_fvector(const string_array& saInput)
      : BaseFArray(saInput)
    {
    }

/**
@brief Constructor

Creates vector of parameterized functions of one or more variables. Throws
\ref cvmexception in case of syntax or memory allocation error.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    string_array vars, bodies, params, meanings;
    vars.push_back("x");
    vars.push_back("y");
    params.push_back("p");
    meanings.push_back("sqrt(x) + sqrt(y)");
    bodies.push_back("p^2");
    bodies.push_back("p");
    bodies.push_back("x+y");

    rfvector rfv(vars, bodies, params, meanings);
    rvector rv(3);
    rfv.value(4., 1., rv);
    std::cout << rfv << rv;

    cfvector cfv(vars, bodies, params, meanings);
    cvector cv(3);
    cfv.value(complex<double>(4., 0.), complex<double>(1., 0.), cv);
    std::cout << cfv << cv;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} (sqrt(x)+sqrt(y))^2 {x,y} sqrt(x)+sqrt(y) {x,y} x+y
9 3 5
{x,y} (sqrt(x)+sqrt(y))^(2,0) {x,y} sqrt(x)+sqrt(y) {x,y} x+y
(9,0) (3,0) (5,0)
\endcode
@param[in] saVars String array with variables (may be empty).
@param[in] saBodies String array with functions' expressions. For expression syntax look at \ref basic_function description.
@param[in] saParameters String array with parameters (may be empty).
@param[in] saMeanings String array with parameters' meanings (may be empty, must have the same size as saParameters).
*/
    basic_fvector(const string_array& saVars, const string_array& saBodies,
                  const string_array& saParameters, const string_array& saMeanings)
      : BaseFArray(saVars, saBodies, saParameters, saMeanings)
    {
    }

/**
@brief Copy Constructor

Creates copy of vector of functions referred by <tt>fv</tt>.
@param[in] fv Vector of functions (\ref rfvector or \ref cfvector) to copy from.
*/
    basic_fvector(const basic_fvector& fv)
      : BaseFArray(fv)
    {
    }

/**
@brief Move Constructor
*/
    basic_fvector(basic_fvector&& fv)
      : BaseFArray(std::move(fv))
    {
    }

/**
@brief Assignment operator

Assigns basic_fvector object to calling one or throws \ref cvmexception
if objects have different sizes.
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x,y} x+y+1");
funcs.push_back("{x,y} x*y");
funcs.push_back("{x,y} x^y");
rfvector rfv(funcs);
cfvector cfv(funcs);
rfvector rfv2(3);
cfvector cfv2(3);

rfv2 = rfv;
cfv2 = cfv;
std::cout << rfv2 << cfv2;
\endcode
prints
\code
{x,y} x+y+1 {x,y} x*y {x,y} x^y
{x,y} x+y+(1,0) {x,y} x*y {x,y} x^y
\endcode
@param[in] fv Const reference to vector of functions to assign.
@return Reference to changed calling vector.
*/
    basic_fvector& operator = (const basic_fvector& fv) throw(cvmexception)
    {
        this->_check_size(fv.size());
        this->_assign(fv);
        return *this;
    }

/**
@brief Move sssignment operator
*/
    basic_fvector& operator = (basic_fvector&& fv) throw(cvmexception)
    {
        this->_check_size(fv.size());
        this->mv = std::move(fv.mv);
        return *this;
    }

/**
@brief Vectors of functions comparison

Returns true if vectors of functions are equal and false otherwise.
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x,y} x+y+1");
funcs.push_back("{x,y} x*y");
funcs.push_back("{x,y} x^y");
rfvector rfv(funcs);
cfvector cfv(funcs);
rfvector rfv2(3);
cfvector cfv2(3);
std::cout << (rfv2 == rfv) << std::endl;
std::cout << (cfv2 == cfv) << std::endl;
rfv2 = rfv;
cfv2 = cfv;
std::cout << (rfv2 == rfv) << std::endl;
std::cout << (cfv2 == cfv) << std::endl;
\endcode
prints
\code
0
0
1
1
\endcode
@param[in] fv Const reference to vector of functions to compare with.
@return boolean value.
*/
    bool operator == (const basic_fvector& fv) const {
        return this->_equals(fv);
    }

/**
@brief Vectors of functions comparison

Returns true if vectors of functions are not equal and false otherwise.
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x,y} x+y+1");
funcs.push_back("{x,y} x*y");
funcs.push_back("{x,y} x^y");
rfvector rfv(funcs);
cfvector cfv(funcs);
rfvector rfv2(3);
cfvector cfv2(3);
std::cout << (rfv2 != rfv) << std::endl;
std::cout << (cfv2 != cfv) << std::endl;
rfv2 = rfv;
cfv2 = cfv;
std::cout << (rfv2 != rfv) << std::endl;
std::cout << (cfv2 != cfv) << std::endl;
\endcode
prints
\code
1
1
0
0
\endcode
@param[in] fv Const reference to vector of functions to compare with.
@return boolean value.
*/
    bool operator != (const basic_fvector& fv) const {
        return !this->_equals(fv);
    }

/**
@brief Replacement operator

Assigns basic_fvector object to calling one without checking anything.
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x,y} x+y+1");
funcs.push_back("{x,y} x*y");
funcs.push_back("{x,y} x^y");
rfvector rfv(funcs);
cfvector cfv(funcs);
rfvector rfv2;
cfvector cfv2;

rfv2 << rfv;
cfv2 << cfv;
std::cout << rfv2 << cfv2;
\endcode
prints
\code
{x,y} x+y+1 {x,y} x*y {x,y} x^y
{x,y} x+y+(1,0) {x,y} x*y {x,y} x^y
\endcode
@param[in] fv Const reference to vector of functions to replace by.
@return Reference to changed calling vector.
*/
    basic_fvector& operator <<(const basic_fvector& fv)
    {
        this->mv.resize(fv.size());
        this->_assign(fv);
        return *this;
    }

/**
@brief Partial derivative

Creates basic_fvector object as a partial derivative of calling
vector. Variables set remains the same even if the derivative
doesn't depend on some of them. Partial derivative is computed
by <tt>nVarNum</tt>'s variable (0-based, 0 by default).
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y} x + log(x) + x^y");
funcs.push_back("{x,y} sin(x)*cos(y) + sin(y)*cos(x) + x*y");
rfvector rfv(funcs);
cfvector cfv(funcs);
std::cout << rfv.drv(0) << rfv.drv(1);
std::cout << cfv.drv(0) << cfv.drv(1);
\endcode
prints
\code
{x,y} 1+1/x+y*x^(y-1) {x,y} cos(x)*cos(y)-sin(x)*sin(y)+y
{x,y} x^y*log(x) {x,y} cos(y)*cos(x)-sin(y)*sin(x)+x
{x,y} (1,0)+(1,0)/x+y*x^(y-(1,-0)) {x,y} cos(x)*cos(y)-sin(x)*sin(y)+y
{x,y} x^y*log(x) {x,y} cos(y)*cos(x)-sin(y)*sin(x)+x
\endcode
@param[in] nVarNum Variable's index (0-based, 0 by default).
@return %Vector of functions object.
*/
    basic_fvector drv(size_t nVarNum) const
    {
        basic_fvector vRet(this->size());
        vRet._drv(*this, nVarNum);
        return vRet;
    }

/**
@brief Simplifier

Simplifies basic_fvector for fatsest numerical computation
possible and returns reference to the object changed.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y} x*2 + 3*x");
funcs.push_back("{x,y} x + y + x + y + x");
rfvector rfv(funcs);
cfvector cfv(funcs);
std::cout << rfv.simp() << cfv.simp();
\endcode
prints
\code
{x,y} x*5 {x,y} 2*(y+x)+x
{x,y} x*(5,0) {x,y} (2,0)*(y+x)+x
\endcode
@return Reference to changed calling vector.
*/
    basic_fvector& simp()
    {
        this->_simp();
        return *this;
    }

/**
@brief Addition operator

Creates basic_fvector object as sum of calling vector and
vector referred by <tt>fv</tt>. Operator throws \ref
cvmexception if sizes of vectors or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} y");
    funcs2.push_back("{x,y} x * y");
    rfvector rfv1(funcs1);
    cfvector cfv1(funcs1);
    rfvector rfv2(funcs2);
    cfvector cfv2(funcs2);
    std::cout << (rfv1 + rfv2);
    std::cout << (cfv1 + cfv2);
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x+y {x,y} x+y+x*y
{x,y} x+y {x,y} x+y+x*y
\endcode
@param[in] fv Const reference to vector to add.
@return %Vector of functions object.
*/
    basic_fvector operator + (const basic_fvector& fv) const throw(cvmexception)
    {
        this->_check_size(fv.size());
        basic_fvector vRet(this->size());
        vRet._add(*this, fv);
        return vRet;
    }

/**
@brief Subtraction operator

Creates basic_fvector object as difference of calling vector and
vector referred by <tt>fv</tt>. Operator throws \ref
cvmexception if sizes of vectors or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} y");
    funcs2.push_back("{x,y} x * y");
    rfvector rfv1(funcs1);
    cfvector cfv1(funcs1);
    rfvector rfv2(funcs2);
    cfvector cfv2(funcs2);
    std::cout << (rfv1 - rfv2);
    std::cout << (cfv1 - cfv2);
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x-y {x,y} x+y-x*y
{x,y} x-y {x,y} x+y-x*y
\endcode
@param[in] fv Const reference to vector to subtract.
@return %Vector of functions object.
*/
    basic_fvector operator - (const basic_fvector& fv) const throw(cvmexception)
    {
        this->_check_size(fv.size());
        basic_fvector vRet(this->size());
        vRet._subtract(*this, fv);
        return vRet;
    }

/**
@brief Increment operator

Adds to calling basic_fvector
vector of functions referred by <tt>fv</tt> and returns reference to the
object changed. Operator throws \ref
cvmexception if sizes of vectors or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} y");
    funcs2.push_back("{x,y} x * y");
    rfvector rfv1(funcs1);
    cfvector cfv1(funcs1);
    rfvector rfv2(funcs2);
    cfvector cfv2(funcs2);
    rfv1 += rfv2;
    cfv1 += cfv2;
    std::cout << rfv1;
    std::cout << cfv1;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x+y {x,y} x+y+x*y
{x,y} x+y {x,y} x+y+x*y
\endcode
@param[in] fv Const reference to vector to add.
@return Reference to changed calling vector.
*/
    basic_fvector& operator += (const basic_fvector& fv) throw(cvmexception)
    {
        this->_check_size(fv.size());
        this->_add(fv);
        return *this;
    }

/**
@brief Decrement operator

Subtracts from calling basic_fvector
vector of functions referred by <tt>fv</tt> and returns reference to the
object changed. Operator throws \ref
cvmexception if sizes of vectors or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} y");
    funcs2.push_back("{x,y} x * y");
    rfvector rfv1(funcs1);
    cfvector cfv1(funcs1);
    rfvector rfv2(funcs2);
    cfvector cfv2(funcs2);
    rfv1 -= rfv2;
    cfv1 -= cfv2;
    std::cout << rfv1;
    std::cout << cfv1;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x-y {x,y} x+y-x*y
{x,y} x-y {x,y} x+y-x*y
\endcode
@param[in] fv Const reference to vector to add.
@return Reference to changed calling vector.
*/
    basic_fvector& operator -= (const basic_fvector& fv) throw(cvmexception)
    {
        this->_check_size(fv.size());
        this->_subtract(fv);
        return *this;
    }

/**
@brief Multiplication operator

Creates basic_fvector object as product of calling vector
and function referred by <tt>f</tt>. Operator throws \ref
cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    rfvector rfv(funcs);
    cfvector cfv(funcs);
    rfunction rf("{x} x/2");
    cfunction cf("{x} x/2");
    std::cout << rfv * rf << (rfv * rf).simp();
    std::cout << cfv * cf << (cfv * cf).simp();
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)*x/2 {x} 2*x*x/2
{x} (x+x^2)*x/2 {x} x^2
{x} (x+x^(2,0))*x/(2,0) {x} (2,0)*x*x/(2,0)
{x} (x+x^(2,0))*x/(2,0) {x} x^(2,0)
\endcode
@param[in] f Const reference to function to multiply by.
@return %Vector of functions object.
*/
    basic_fvector operator *(const BaseFunction& f) const
    {
        basic_fvector vRet(this->size());
        vRet.BaseFArray::_mult(*this, f);
        return vRet;
    }

/**
@brief Division operator

Creates basic_fvector object as division of calling vector
by function referred by <tt>f</tt>. Operator throws \ref
cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    rfvector rfv(funcs);
    cfvector cfv(funcs);
    rfunction rf("{x} x/2");
    cfunction cf("{x} x/2");
    std::cout << rfv / rf << (rfv / rf).simp();
    std::cout << cfv / cf << (cfv / cf).simp();
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)/(x/2) {x} 2*x/(x/2)
{x} (x+x^2)*2/x {x} 4
{x} (x+x^(2,0))/(x/(2,0)) {x} (2,0)*x/(x/(2,0))
{x} (x+x^(2,0))*(2,0)/x {x} (4,0)
\endcode
@param[in] f Const reference to function to divide by.
@return %Vector of functions object.
*/
    basic_fvector operator / (const BaseFunction& f) const
    {
        basic_fvector vRet(this->size());
        vRet._div(*this, f);
        return vRet;
    }

/**
@brief Multiplication operator

Creates basic_fvector object as product of calling vector
and real or complex number referred by <tt>d</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    string_array funcs;
    funcs.push_back("{x} x / 2");
    funcs.push_back("{x} 2 * x");
    rfvector rfv(funcs);
    cfvector cfv(funcs);
    std::cout << rfv * 2. << (rfv * 2.).simp();
    std::cout << cfv * complex<double>(2., 0.) << (cfv * complex<double>(2., 0.)).simp();
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} x/2*2 {x} 2*x*2
{x} x {x} 4*x
{x} x/(2,0)*(2,0) {x} (2,0)*x*(2,0)
{x} x {x} (4,0)*x
\endcode
@param[in] d Number to multiply by.
@return %Vector of functions object.
*/
    basic_fvector operator *(const T& d) const
    {
        basic_fvector vRet(this->size());
        vRet.BaseFArray::_mult(*this, d);
        return vRet;
    }

/**
@brief Division operator

Creates basic_fvector object as division of calling vector
by real or complex number referred by <tt>d</tt>.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    string_array funcs;
    funcs.push_back("{x} x / 2");
    funcs.push_back("{x} 2 * x");
    rfvector rfv(funcs);
    cfvector cfv(funcs);
    std::cout << rfv / 2. << (rfv / 2.).simp();
    std::cout << cfv / complex<double>(2., 0.) << (cfv / complex<double>(2., 0.)).simp();
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x/2)/2 {x} 2*x/2
{x} x/4 {x} x
{x} (x/(2,0))/(2,0) {x} (2,0)*x/(2,0)
{x} x/(4,0) {x} x
\endcode
@param[in] d Number to divide by.
@return %Vector of functions object.
*/
    basic_fvector operator / (const T& d) const
    {
        basic_fvector vRet(this->size());
        vRet._div(*this, d);
        return vRet;
    }

/**
@brief Multiply and assign operator

Multiplies calling vector of functions
by function referred by <tt>f</tt> and returns reference to the
object changed. Operator throws
\ref cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    rfvector rfv(funcs);
    cfvector cfv(funcs);
    rfunction rf("{x} x/2");
    cfunction cf("{x} x/2");
    rfv *= rf;
    cfv *= cf;
    std::cout << rfv;
    std::cout << rfv.simp();
    std::cout << cfv;
    std::cout << cfv.simp();
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)*x/2 {x} 2*x*x/2
{x} (x+x^2)*x/2 {x} x^2
{x} (x+x^(2,0))*x/(2,0) {x} (2,0)*x*x/(2,0)
{x} (x+x^(2,0))*x/(2,0) {x} x^(2,0)
\endcode
@param[in] f Const reference to function to multiply by.
@return Reference to changed calling vector.
*/
    basic_fvector& operator *= (const BaseFunction& f)
    {
        this->BaseFArray::_mult(f);
        return *this;
    }

/**
@brief Divide and assign operator

Divides calling vector of functions
by function referred by <tt>f</tt> and returns reference to the
object changed. Operator throws
\ref cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    rfvector rfv(funcs);
    cfvector cfv(funcs);
    rfunction rf("{x} x/2");
    cfunction cf("{x} x/2");
    rfv /= rf;
    cfv /= cf;
    std::cout << rfv;
    std::cout << rfv.simp();
    std::cout << cfv;
    std::cout << cfv.simp();
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)/(x/2) {x} 2*x/(x/2)
{x} (x+x^2)*2/x {x} 4
{x} (x+x^(2,0))/(x/(2,0)) {x} (2,0)*x/(x/(2,0))
{x} (x+x^(2,0))*(2,0)/x {x} (4,0)
\endcode
@param[in] f Const reference to function to divide by.
@return Reference to changed calling vector.
*/
    basic_fvector& operator /= (const BaseFunction& f)
    {
        this->BaseFArray::_div(f);
        return *this;
    }

/**
@brief Multiply and assign operator

Multiplies calling vector of functions
by real or complex number referred by <tt>d</tt> and returns reference to the
object changed.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    rfvector rfv(funcs);
    cfvector cfv(funcs);
    rfunction rf("{x} x/2");
    cfunction cf("{x} x/2");
    rfv *= 2.;
    cfv *= 2.;
    std::cout << rfv;
    std::cout << rfv.simp();
    std::cout << cfv;
    std::cout << cfv.simp();
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)*2 {x} 2*x*2
{x} (x+x^2)*2 {x} 4*x
{x} (x+x^(2,0))*(2,0) {x} (2,0)*x*(2,0)
{x} (x+x^(2,0))*(2,0) {x} (4,0)*x
\endcode
@param[in] d Number to multiply by.
@return Reference to changed calling vector.
*/
    basic_fvector& operator *= (const T& d)
    {
        this->BaseFArray::_mult(d);
        return *this;
    }

/**
@brief Divide and assign operator

Divides calling vector of functions
by real or complex number referred by <tt>d</tt> and returns reference to the
object changed.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    rfvector rfv(funcs);
    cfvector cfv(funcs);
    rfunction rf("{x} x/2");
    cfunction cf("{x} x/2");
    rfv /= 2.;
    cfv /= 2.;
    std::cout << rfv;
    std::cout << rfv.simp();
    std::cout << cfv;
    std::cout << cfv.simp();
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)/2 {x} 2*x/2
{x} (x+x^2)/2 {x} x
{x} (x+x^(2,0))/(2,0) {x} (2,0)*x/(2,0)
{x} (x+x^(2,0))/(2,0) {x} x
\endcode
@param[in] d Number to divide by.
@return Reference to changed calling vector.
*/
    basic_fvector& operator /= (const T& d)
    {
        this->BaseFArray::_div(d);
        return *this;
    }

/**
@brief Scalar product

Creates basic_function object as scalar product of calling vector
and vector referred by <tt>fv</tt>. Operator throws
\ref cvmexception if sizes of vectors or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} y");
    funcs2.push_back("{x,y} x * y");
    rfvector rfv1(funcs1);
    cfvector cfv1(funcs1);
    rfvector rfv2(funcs2);
    cfvector cfv2(funcs2);
    rfunction rf = rfv1 * rfv2;
    cfunction cf = cfv1 * cfv2;
    std::cout << rf << std::endl;
    std::cout << cf << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x*y*(x+y+1)
{x,y} x*y*(x+y+(1,0))
\endcode
@param[in] fv Vector of functions to multiply by.
@return \ref basic_function object.
*/
    BaseFunction operator *(const basic_fvector& fv) const throw(cvmexception)
    {
        this->_check_size(fv.size());
        BaseFunction fRet;
        fRet._scalar_product(this->mv.size(), &this->mv.at(0), 1, &fv.mv.at(0), 1);
        return fRet;
    }

/**
@brief Vector by matrix product

Creates basic_fvector object as product of calling vector
and matrix of functions referred by <tt>fm</tt>. Operator throws
\ref cvmexception if sizes of objects or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} x");
    funcs2.push_back("{x,y} 1");
    funcs2.push_back("{x,y} 1");
    funcs2.push_back("{x,y} y");
    rfvector rfv(funcs1);
    cfvector cfv(funcs1);
    rfmatrix rfm(2,2,funcs2);
    cfmatrix cfm(2,2,funcs2);
    std::cout << rfm << std::endl;
    std::cout << rfv * rfm << std::endl;
    std::cout << cfm << std::endl;
    std::cout << cfv * cfm << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x {x,y} 1
{x,y} 1 {x,y} y

{x,y} x^2+x+y {x,y} x+(x+y)*y

{x,y} x {x,y} (1,0)
{x,y} (1,0) {x,y} y

{x,y} x^(2,0)+x+y {x,y} x+(x+y)*y
\endcode
@param[in] fm %Matrix of functions to multiply by.
@return %Vector of functions.
*/
    basic_fvector operator *(const basic_fmatrix<T>& fm) const throw(cvmexception)
    {
        this->_check_size(fm.msize());
        basic_fvector vRet(fm.nsize());
        vRet._mult(*this, fm);
        return vRet;
    }

/**
@brief Vector by matrix product

Sets basic_fvector object to be equal to product of vector of functions referred by <tt>fv</tt>
and matrix of functions referred by <tt>fm</tt> and returns reference to the object changed.
Function throws
\ref cvmexception if sizes of objects or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} x");
    funcs2.push_back("{x,y} 1");
    funcs2.push_back("{x,y} 2");
    funcs2.push_back("{x,y} y");
    rfvector rfv1(funcs1);
    cfvector cfv1(funcs1);
    rfmatrix rfm(2,2,funcs2);
    cfmatrix cfm(2,2,funcs2);
    rfvector rfv2(2);
    cfvector cfv2(2);
    rfv2.mult(rfv1,rfm);
    cfv2.mult(cfv1,cfm);
    std::cout << rfm << std::endl;
    std::cout << rfv2 << std::endl;
    std::cout << cfm << std::endl;
    std::cout << cfv2 << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x {x,y} 2
{x,y} 1 {x,y} y

{x,y} x^2+x+y {x,y} x*2+(x+y)*y

{x,y} x {x,y} (2,0)
{x,y} (1,0) {x,y} y

{x,y} x^(2,0)+x+y {x,y} x*(2,0)+(x+y)*y
\endcode
@param[in] fv %Vector of functions to multiply.
@param[in] fm %Matrix of functions to multiply by.
@return Reference to changed calling vector.
*/
    basic_fvector& mult(const basic_fvector& fv, const basic_fmatrix<T>& fm) throw(cvmexception)
    {
        fv._check_size(fm.msize());
        this->_check_size(fm.nsize());
        return this->_mult(fv, fm);
    }

/**
@brief %Matrix by vector product

Sets basic_fvector object to be equal to product of matrix of functions referred by <tt>fm</tt>
and vector of functions referred by <tt>fv</tt> and returns reference to the object changed.
FUnction throws
\ref cvmexception if sizes of objects or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} x");
    funcs2.push_back("{x,y} 1");
    funcs2.push_back("{x,y} 2");
    funcs2.push_back("{x,y} y");
    rfvector rfv1(funcs1);
    cfvector cfv1(funcs1);
    rfmatrix rfm(2,2,funcs2);
    cfmatrix cfm(2,2,funcs2);
    rfvector rfv2(2);
    cfvector cfv2(2);
    rfv2.mult(rfm,rfv1);
    cfv2.mult(cfm,cfv1);
    std::cout << rfm << std::endl;
    std::cout << rfv2 << std::endl;
    std::cout << cfm << std::endl;
    std::cout << cfv2 << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x {x,y} 2
{x,y} 1 {x,y} y

{x,y} x^2+2*(x+y) {x,y} x+y*(x+y)

{x,y} x {x,y} (2,0)
{x,y} (1,0) {x,y} y

{x,y} x^(2,0)+(2,0)*(x+y) {x,y} x+y*(x+y)
\endcode
@param[in] fm %Matrix of functions to multiply.
@param[in] fv %Vector of functions to multiply by.
@return Reference to changed calling vector.
*/
    basic_fvector& mult(const basic_fmatrix<T>& fm, const basic_fvector& fv) throw(cvmexception)
    {
        fv._check_size(fm.nsize());
        this->_check_size(fm.msize());
        return this->_mult(fm, fv);
    }

/**
@brief Jacobi matrix

Creates basic_fmatrix object as Jacobi matrix of calling vector of functions.
First variable to compute partial derivatives is set by <tt>nfrom</tt> index (0-based,
0 by default), the number of variables to use is set by <tt>vars</tt> argument (0 for all, default).
Number of rows of the Jacobi matrix built is equal to vector's size,
number of columns is the number of variables used to compute it.
Function throws \ref cvmexception if the numbers are out of boundaries.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x,y} x+y+1");
    funcs.push_back("{x,y} x*y");
    funcs.push_back("{x,y} x^y");
    rfvector rfv(funcs);
    cfvector cfv(funcs);
    std::cout << rfv.jacobian() << std::endl;
    std::cout << cfv.jacobian() << std::endl;
    std::cout << rfv.jacobian(0,1) << std::endl;
    std::cout << cfv.jacobian(0,1) << std::endl;
    std::cout << rfv.jacobian(1) << std::endl;
    std::cout << cfv.jacobian(1) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} 1 {x,y} 1
{x,y} y {x,y} x
{x,y} y*x^(y-1) {x,y} x^y*log(x)

{x,y} (1,0) {x,y} (1,0)
{x,y} y {x,y} x
{x,y} y*x^(y-(1,-0)) {x,y} x^y*log(x)

{x,y} 1
{x,y} y
{x,y} y*x^(y-1)

{x,y} (1,0)
{x,y} y
{x,y} y*x^(y-(1,-0))

{x,y} 1
{x,y} x
{x,y} x^y*log(x)

{x,y} (1,0)
{x,y} x
{x,y} x^y*log(x)
\endcode
@param[in] nfrom 0-based index (0 by default) of variable to start with.
@param[in] vars Number of variables to compute Jacobian metrix for (0 for all, default).
@return \ref basic_fmatrix Jacobi matrix object.
*/
    basic_fmatrix<T> jacobian(size_t nfrom = 0, size_t vars = 0) const
    {
        const size_t m = this->size();
        if (m <= 0) {
            return basic_fmatrix<T>();
        }
        const size_t sz = this->mv.at(0).vars().size();
        _check_ge(CVM_INDEX_GE, nfrom, sz);

        const size_t n = vars > 0 ? vars : sz - nfrom;
        _check_gt(CVM_INDEX_GT, nfrom + n, sz);

        basic_fmatrix<T> fmj(m, n);
        this->_jacobi(fmj, nfrom);
        return fmj;
    }

/**
@brief Jacobi matrix

Computes Jacobi matrix of calling vector of functions and sets <tt>fmj</tt> basic_fmatrix object to it.
First variable to compute partial derivatives is set by <tt>nfrom</tt> index (0-based,
0 by default), the number of variables to use is set by <tt>vars</tt> argument (0 for all, default).
Number of rows of the Jacobi matrix built must be equal to vector's size,
number of columns is the number of variables used to compute it. Function throws \ref cvmexception otherwise.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x,y} x+y+1");
    funcs.push_back("{x,y} x*y");
    funcs.push_back("{x,y} x^y");
    rfvector rfv(funcs);
    cfvector cfv(funcs);
    rfmatrix rm32(3,2), rm31(3,1);
    cfmatrix cm32(3,2), cm31(3,1);

    rfv.jacobian(rm32);
    std::cout << rm32 << std::endl;
    cfv.jacobian(cm32);
    std::cout << cm32 << std::endl;

    rfv.jacobian(rm31,0,1);
    std::cout << rm31 << std::endl;
    cfv.jacobian(cm31,0,1);
    std::cout << cm31 << std::endl;

    rfv.jacobian(rm31,1);
    std::cout << rm31 << std::endl;
    cfv.jacobian(cm31,1);
    std::cout << cm31 << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} 1 {x,y} 1
{x,y} y {x,y} x
{x,y} y*x^(y-1) {x,y} x^y*log(x)

{x,y} (1,0) {x,y} (1,0)
{x,y} y {x,y} x
{x,y} y*x^(y-(1,-0)) {x,y} x^y*log(x)

{x,y} 1
{x,y} y
{x,y} y*x^(y-1)

{x,y} (1,0)
{x,y} y
{x,y} y*x^(y-(1,-0))

{x,y} 1
{x,y} x
{x,y} x^y*log(x)

{x,y} (1,0)
{x,y} x
{x,y} x^y*log(x)
\endcode
@param[out] fmj Jacobi matrix to compute.
@param[in] nfrom 0-based index (0 by default) of variable to start with.
@param[in] vars Number of variables to compute Jacobian metrix for (0 for all, default).
*/
    void jacobian(basic_fmatrix<T>& fmj, size_t nfrom = 0, size_t vars = 0) const
    {
        const size_t m = this->size();
        _check_ne(CVM_SIZESMISMATCH, m, fmj.msize());
        if (m > 0) {
            const size_t sz = this->mv.at(0).vars().size();
            _check_ge(CVM_INDEX_GE, nfrom, sz);
            const size_t n = vars > 0 ? vars : sz - nfrom;
            _check_gt(CVM_INDEX_GT, nfrom + n, sz);
            _check_ne(CVM_SIZESMISMATCH, n, fmj.nsize());
            this->_jacobi(fmj, nfrom);
        }
    }

/**
@brief Output operator

Prints vector of functions referred by <tt>fv</tt>
to output stream referred by <tt>os</tt>
using Wolfram's Mathemaca syntax
<tt>{var1[,var2,...]} expr</tt> for each element.
\par Example:
\code
string_array funcs;
funcs.push_back("{x,y} x");
funcs.push_back("{x,y} x + y");
funcs.push_back("{x,y} x*y + 1");
rfvector rfv(funcs);
cfvector cfv(funcs);
std::cout << rfv;
std::cout << cfv;
\endcode
prints
\code
{x,y} x {x,y} x+y {x,y} x*y+1
{x,y} x {x,y} x+y {x,y} x*y+(1,0)
\endcode
@param[in] os Reference to output stream.
@param[in] fv Const reference to vector of functions to print.
@return Reference to output stream.
*/
    friend std::ostream& operator <<(std::ostream& os, const basic_fvector<T>& fv)
    {
        for (size_t i = 0; i < fv.size(); ++i) {
            os << fv.mv.at(i) << " ";
        }
        os << std::endl;
        return os;
    }

protected:
//! @cond INTERNAL
    // internal version, it doesn't verify anything
    basic_fvector& _mult(const basic_fvector& fv, const basic_fmatrix<T>& fm) throw(cvmexception)
    {
        for (size_t i = 0; i < fm.nsize(); ++i) {
            this->mv.at(i)._scalar_product(fv.size(), &fv.mv.at(0), 1, &fm.mv.at(fm.msize() * i), 1);
        }
        return *this;
    }
    // internal version, it doesn't verify anything
    basic_fvector& _mult(const basic_fmatrix<T>& fm, const basic_fvector& fv) throw(cvmexception)
    {
        for (size_t i = 0; i < fm.msize(); ++i) {
            this->mv.at(i)._scalar_product(fm.nsize(), &fm.mv.at(i), fm.msize(), &fv.mv.at(0), 1);
        }
        return *this;
    }
    // internal version, it doesn't verify anything
    // starts from nfrom-th variable (0-based)
    void _jacobi(basic_fmatrix<T>& fmj, size_t nfrom) const
    {
        for (size_t i = 0; i < fmj.msize(); ++i) {
            for (size_t j = 0; j < fmj.nsize(); ++j) {
                fmj.mv.at(fmj.msize() * j + i) = this->mv.at(i).drv(j + nfrom);
            }
        }
    }
//! @endcond
};


/**
@brief Vector of real functions

@see basic_fvector
@see cfvector
*/
template <typename TR>
class basic_rfvector : public basic_fvector<TR>
{
protected:
    using BaseFVector = basic_fvector<TR>; //!< Base class

public:
/**
@brief Default constructor

Creates empty vector of functions. No memory gets allocated.
*/
    basic_rfvector()
      : BaseFVector()
    {
    }

/**
@brief Constructor

Creates vector of zero functions of \c nSize size.
\par Example:
\code
using namespace cvm;
rfvector rfv(3);
std::cout << rfv;
cfvector cfv(3);
std::cout << cfv;
\endcode
prints
\code
0 0 0
(0,0) (0,0) (0,0)
\endcode
@param[in] nSize Number of functions in array.
*/
    explicit basic_rfvector(size_t nSize)
      : BaseFVector(nSize)
    {
    }

/**
@brief Constructor

Creates vector of functions of \c saInput.size() size.
Each function element is ininialized using appropriate string as
parameterless input in Wolfram's Mathemaca syntax <tt>{var1[,var2,...]} expr</tt>
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector rx(1), ry(3);
cvector cx(1), cy(3);
rx(1) = 2.;
cx(1) = complex<double>(2.,0.);
rfv.value(rx, ry);
cfv.value(cx, cy);
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^2 {x} x^3
2 4 8

{x} x {x} x^(2,0) {x} x^(3,0)
(2,0) (4,0) (8,0)
\endcode
@param[in] saInput Array of strings.
*/
    explicit basic_rfvector(const string_array& saInput)
      : BaseFVector(saInput)
    {
    }

/**
@brief Constructor

Creates vector of parameterized functions of one or more variables. Throws
\ref cvmexception in case of syntax or memory allocation error.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    string_array vars, bodies, params, meanings;
    vars.push_back("x");
    vars.push_back("y");
    params.push_back("p");
    meanings.push_back("sqrt(x) + sqrt(y)");
    bodies.push_back("p^2");
    bodies.push_back("p");
    bodies.push_back("x+y");

    rfvector rfv(vars, bodies, params, meanings);
    rvector rv(3);
    rfv.value(4., 1., rv);
    std::cout << rfv << rv;

    cfvector cfv(vars, bodies, params, meanings);
    cvector cv(3);
    cfv.value(complex<double>(4., 0.), complex<double>(1., 0.), cv);
    std::cout << cfv << cv;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} (sqrt(x)+sqrt(y))^2 {x,y} sqrt(x)+sqrt(y) {x,y} x+y
9 3 5
{x,y} (sqrt(x)+sqrt(y))^(2,0) {x,y} sqrt(x)+sqrt(y) {x,y} x+y
(9,0) (3,0) (5,0)
\endcode
@param[in] saVars String array with variables (may be empty).
@param[in] saBodies String array with functions' expressions. For expression syntax look at \ref basic_function description.
@param[in] saParameters String array with parameters (may be empty).
@param[in] saMeanings String array with parameters' meanings (may be empty, must have the same size as saParameters).
*/
    basic_rfvector(const string_array& saVars, const string_array& saBodies,
                  const string_array& saParameters, const string_array& saMeanings)
      : BaseFVector(saVars, saBodies, saParameters, saMeanings)
    {
    }

/**
@brief Copy Constructor

Creates copy of vector of functions referred by <tt>fv</tt>.
@param[in] fv Vector of functions (\ref rfvector or \ref cfvector) to copy from.
*/
    basic_rfvector(const BaseFVector& fv)
      : BaseFVector(fv)
    {
    }

/**
@brief Move Constructor
*/
    basic_rfvector(BaseFVector&& fv)
      : BaseFVector(std::move(fv))
    {
    }

/**
@brief Numerical value

Returns numerical value of vector of functions of no variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("2*2");
funcs.push_back("3*3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector ry = rfv();
cvector cy = cfv();
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
2*2 3*3
4 9

(2,0)*(2,0) (3,0)*(3,0)
(4,0) (9,0)
\endcode
@return vector of functions' values.
*/
    basic_rvector<TR> operator () () const {
        basic_rvector<TR> ret((tint)this->size());
        this->value(ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of vector of functions of one variable.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector ry = rfv(2.);
cvector cy = cfv(tcomplex(2.,0.));
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^2 {x} x^3
2 4 8

{x} x {x} x^(2,0) {x} x^(3,0)
(2,0) (4,0) (8,0)
\endcode
@param[in] d variable's value.
@return vector of functions' values.
*/
    basic_rvector<TR> operator () (TR d) const {
        basic_rvector<TR> ret((tint)this->size());
        this->value(d, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of vector of functions of two variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y} x+y");
funcs.push_back("{x,y} x*y");
funcs.push_back("{x,y} x^y");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector ry = rfv(2., 3.);
cvector cy = cfv(tcomplex(2.,0.), tcomplex(3.,0.));
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x,y} x+y {x,y} x*y {x,y} x^y
5 6 8

{x,y} x+y {x,y} x*y {x,y} x^y
(5,0) (6,0) (8,0)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@return vector of functions' values.
*/
    basic_rvector<TR> operator () (TR d1, TR d2) const {
        basic_rvector<TR> ret((tint)this->size());
        this->value(d1, d2, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of vector of functions of three variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y,z} x+y+z");
funcs.push_back("{x,y,z} x*y*z");
funcs.push_back("{x,y,z} x^y^z");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector ry = rfv(3., 2., 2.);
cvector cy = cfv(tcomplex(3.,0.), tcomplex(2.,0.), tcomplex(2.,0.));
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x,y,z} x+y+z {x,y,z} x*y*z {x,y,z} x^(y^z)
7 12 81

{x,y,z} x+y+z {x,y,z} x*y*z {x,y,z} x^(y^z)
(7,0) (12,0) (81,0)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@param[in] d3 Third variable's value.
@return vector of functions' values.
*/
    basic_rvector<TR> operator () (TR d1, TR d2, TR d3) const {
        basic_rvector<TR> ret((tint)this->size());
        this->value(d1, d2, d3, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of array of functions for given values of
variables. Array's dimension is not verified.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector rx(1);
cvector cx(1);
rx(1) = 2.;
cx(1) = tcomplex(2.,0.);
rvector ry = rfv(rx);
cvector cy = cfv(cx);
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^2 {x} x^3
2 4 8

{x} x {x} x^(2,0) {x} x^(3,0)
(2,0) (4,0) (8,0)
\endcode
@param[in] pd array of variables' values.
@return vector of functions' values.
*/
    basic_rvector<TR> operator () (const TR* pd) const {
        basic_rvector<TR> ret((tint)this->size());
        this->value(pd, ret);
        return ret;
    }
};


/**
@brief Vector of complex functions

@see basic_fvector
@see rfvector
*/
template <typename TR, typename TC>
class basic_cfvector : public basic_fvector<TC>
{
protected:
    using BaseFVector = basic_fvector<TC>; //!< Base class

public:
/**
@brief Default constructor

Creates empty vector of functions. No memory gets allocated.
*/
    basic_cfvector()
      : BaseFVector()
    {
    }

/**
@brief Constructor

Creates vector of zero functions of \c nSize size.
\par Example:
\code
using namespace cvm;
rfvector rfv(3);
std::cout << rfv;
cfvector cfv(3);
std::cout << cfv;
\endcode
prints
\code
0 0 0
(0,0) (0,0) (0,0)
\endcode
@param[in] nSize Number of functions in array.
*/
    explicit basic_cfvector(size_t nSize)
      : BaseFVector(nSize)
    {
    }

/**
@brief Constructor

Creates vector of functions of \c saInput.size() size.
Each function element is ininialized using appropriate string as
parameterless input in Wolfram's Mathemaca syntax <tt>{var1[,var2,...]} expr</tt>
\par Example:
\code
using namespace cvm;
using namespace std;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector rx(1), ry(3);
cvector cx(1), cy(3);
rx(1) = 2.;
cx(1) = complex<double>(2.,0.);
rfv.value(rx, ry);
cfv.value(cx, cy);
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^2 {x} x^3
2 4 8

{x} x {x} x^(2,0) {x} x^(3,0)
(2,0) (4,0) (8,0)
\endcode
@param[in] saInput Array of strings.
*/
    explicit basic_cfvector(const string_array& saInput)
      : BaseFVector(saInput)
    {
    }

/**
@brief Constructor

Creates vector of parameterized functions of one or more variables. Throws
\ref cvmexception in case of syntax or memory allocation error.
\par Example:
\code
using namespace cvm;
using namespace std;
try{
    string_array vars, bodies, params, meanings;
    vars.push_back("x");
    vars.push_back("y");
    params.push_back("p");
    meanings.push_back("sqrt(x) + sqrt(y)");
    bodies.push_back("p^2");
    bodies.push_back("p");
    bodies.push_back("x+y");

    rfvector rfv(vars, bodies, params, meanings);
    rvector rv(3);
    rfv.value(4., 1., rv);
    std::cout << rfv << rv;

    cfvector cfv(vars, bodies, params, meanings);
    cvector cv(3);
    cfv.value(complex<double>(4., 0.), complex<double>(1., 0.), cv);
    std::cout << cfv << cv;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} (sqrt(x)+sqrt(y))^2 {x,y} sqrt(x)+sqrt(y) {x,y} x+y
9 3 5
{x,y} (sqrt(x)+sqrt(y))^(2,0) {x,y} sqrt(x)+sqrt(y) {x,y} x+y
(9,0) (3,0) (5,0)
\endcode
@param[in] saVars String array with variables (may be empty).
@param[in] saBodies String array with functions' expressions. For expression syntax look at \ref basic_function description.
@param[in] saParameters String array with parameters (may be empty).
@param[in] saMeanings String array with parameters' meanings (may be empty, must have the same size as saParameters).
*/
    basic_cfvector(const string_array& saVars, const string_array& saBodies,
                  const string_array& saParameters, const string_array& saMeanings)
      : BaseFVector(saVars, saBodies, saParameters, saMeanings)
    {
    }

/**
@brief Copy Constructor

Creates copy of vector of functions referred by <tt>fv</tt>.
@param[in] fv Vector of functions (\ref rfvector or \ref cfvector) to copy from.
*/
    basic_cfvector(const BaseFVector& fv)
      : BaseFVector(fv)
    {
    }

/**
@brief Move Constructor
*/
    basic_cfvector(BaseFVector&& fv)
      : BaseFVector(std::move(fv))
    {
    }

/**
@brief Numerical value

Returns numerical value of vector of functions of no variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("2*2");
funcs.push_back("3*3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector ry = rfv();
cvector cy = cfv();
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
2*2 3*3
4 9

(2,0)*(2,0) (3,0)*(3,0)
(4,0) (9,0)
\endcode
*/
    basic_cvector<TR,TC> operator () () const {
        basic_cvector<TR,TC> ret((tint)this->size());
        this->value(ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of vector of functions of one variable.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector ry = rfv(2.);
cvector cy = cfv(tcomplex(2.,0.));
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^2 {x} x^3
2 4 8

{x} x {x} x^(2,0) {x} x^(3,0)
(2,0) (4,0) (8,0)
\endcode
@param[in] d variable's value.
@return vector of functions' values.
*/
    basic_cvector<TR,TC> operator () (TC d) const {
        basic_cvector<TR,TC> ret((tint)this->size());
        this->value(d, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of vector of functions of two variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y} x+y");
funcs.push_back("{x,y} x*y");
funcs.push_back("{x,y} x^y");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector ry = rfv(2., 3.);
cvector cy = cfv(tcomplex(2.,0.), tcomplex(3.,0.));
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x,y} x+y {x,y} x*y {x,y} x^y
5 6 8

{x,y} x+y {x,y} x*y {x,y} x^y
(5,0) (6,0) (8,0)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@return vector of functions' values.
*/
    basic_cvector<TR,TC> operator () (TC d1, TC d2) const {
        basic_cvector<TR,TC> ret((tint)this->size());
        this->value(d1, d2, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of vector of functions of three variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y,z} x+y+z");
funcs.push_back("{x,y,z} x*y*z");
funcs.push_back("{x,y,z} x^y^z");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector ry = rfv(3., 2., 2.);
cvector cy = cfv(tcomplex(3.,0.), tcomplex(2.,0.), tcomplex(2.,0.));
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x,y,z} x+y+z {x,y,z} x*y*z {x,y,z} x^(y^z)
7 12 81

{x,y,z} x+y+z {x,y,z} x*y*z {x,y,z} x^(y^z)
(7,0) (12,0) (81,0)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@param[in] d3 Third variable's value.
@return vector of functions' values.
*/
    basic_cvector<TR,TC> operator () (TC d1, TC d2, TC d3) const {
        basic_cvector<TR,TC> ret((tint)this->size());
        this->value(d1, d2, d3, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of array of functions for given values of
variables. Array's dimension is not verified.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
rfvector rfv(funcs);
cfvector cfv(funcs);

rvector rx(1);
cvector cx(1);
rx(1) = 2.;
cx(1) = tcomplex(2.,0.);
rvector ry = rfv(rx);
cvector cy = cfv(cx);
std::cout << rfv << ry << std::endl;
std::cout << cfv << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^2 {x} x^3
2 4 8

{x} x {x} x^(2,0) {x} x^(3,0)
(2,0) (4,0) (8,0)
\endcode
@param[in] pd array of variables' values.
@return vector of functions' values.
*/
    basic_cvector<TR,TC> operator () (const TC* pd) const {
        basic_cvector<TR,TC> ret((tint)this->size());
        this->value(pd, ret);
        return ret;
    }

};


/**
@brief %Matrix of functions class

\c T type stands for \ref treal or \ref tcomplex. Please use inherited \ref rfmatrix and \ref cfmatrix classes.
Fortran style storage is used here, i.e. elements are stored by columns (the same way as in \ref basic_rmatrix etc.).
@see rfmatrix
@see cfmatrix
@see basic_fvector
*/
template<typename T>
class basic_fmatrix : public FArray<T>
{
    friend class basic_fvector<T>;  // _mult

protected:
    using BaseFArray = FArray<T>; //!< Base class
    using BaseFunction = typename BaseFArray::BaseFunction; //!< %Matrix element, i.e. \ref rfunction or \ref cfunction

    size_t mM; //!< %Number of rows
    size_t mN; //!< %Number of columns

//! @cond INTERNAL
    void _check_sizes(const basic_fmatrix& fm) const throw(cvmexception)
    {
        _check_ne(CVM_SIZESMISMATCH, fm.msize(), this->msize());
        _check_ne(CVM_SIZESMISMATCH, fm.nsize(), this->nsize());
    }
    void _check_sizes() const throw(cvmexception)
    {
        static const size_t ZERO(0);
        _check_le(CVM_WRONGSIZE_LE, this->msize(), ZERO);
        _check_le(CVM_WRONGSIZE_LE, this->nsize(), ZERO);
        _check_ne(CVM_SIZESMISMATCH, mM * mN, this->size());
    }
//! @endcond

public:
/**
@brief Default constructor

Creates empty matrix of functions. No memory gets allocated.
*/
    basic_fmatrix()
      : BaseFArray(),
        mM(0),
        mN(0)
    {
    }

/**
@brief Constructor

Creates <tt>m</tt>-by-<tt>n</tt> matrix of zero functions.
\par Example:
\code
using namespace cvm;
rfmatrix rfm(2,3);
std::cout << rfm;
cfmatrix cfm(2,3);
std::cout << cfm;
\endcode
prints
\code
0 0 0
0 0 0
(0,0) (0,0) (0,0)
(0,0) (0,0) (0,0)
\endcode
@param[in] m Number of rows.
@param[in] n Number of columns.
*/
    basic_fmatrix(size_t m, size_t n)
      : BaseFArray(m * n),
        mM(m),
        mN(n)
    {
        this->_check_sizes();
    }

/**
@brief Constructor

Creates <tt>m</tt>-by-<tt>n</tt> matrix of functions.
Each function element is ininialized using appropriate string as
parameterless input in Wolfram's Mathemaca syntax <tt>{var1[,var2,...]} expr</tt>.
For expression syntax look at \ref basic_function description.
%Matrix elements are initialized column by column.
Constructor throws \ref cvmexception in case of inappropriate sizes passed.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
funcs.push_back("{x} x^4");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
std::cout << rfm << std::endl;
std::cout << cfm << std::endl;
\endcode
prints
\code
{x} x {x} x^3
{x} x^2 {x} x^4

{x} x {x} x^(3,0)
{x} x^(2,0) {x} x^(4,0)
\endcode
@param[in] m Number of rows.
@param[in] n Number of columns.
@param[in] saInput Array of strings with functions' expressions.
*/
    basic_fmatrix(size_t m, size_t n, const string_array& saInput)
      : BaseFArray(saInput),
        mM(m),
        mN(n)
    {
        this->_check_sizes();
    }

/**
@brief Constructor

Creates <tt>m</tt>-by-<tt>n</tt> matrix of parameterized functions.
Each function element is ininialized using appropriate string as
parameterless input in Wolfram's Mathemaca syntax <tt>{var1[,var2,...]} expr</tt>.
For expression syntax look at \ref basic_function description.
%Matrix elements are initialized column by column.
Constructor throws \ref cvmexception in case of inappropriate sizes passed.
\par Example:
\code
using namespace cvm;
try{
    string_array vars, bodies, params, meanings;
    vars.push_back("x");
    vars.push_back("y");
    params.push_back("p");
    meanings.push_back("sqrt(x) + sqrt(y)");
    bodies.push_back("p*x");
    bodies.push_back("p+1");
    bodies.push_back("p*y");
    bodies.push_back("p-1");

    rfmatrix rm(2, 2, vars, bodies, params, meanings);
    std::cout << rm;
    cfmatrix cm(2, 2, vars, bodies, params, meanings);
    std::cout << cm;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} (sqrt(x)+sqrt(y))*x {x,y} (sqrt(x)+sqrt(y))*y
{x,y} sqrt(x)+sqrt(y)+1 {x,y} sqrt(x)+sqrt(y)-1
{x,y} (sqrt(x)+sqrt(y))*x {x,y} (sqrt(x)+sqrt(y))*y
{x,y} sqrt(x)+sqrt(y)+(1,0) {x,y} sqrt(x)+sqrt(y)-(1,0)
\endcode
@param[in] m Number of rows.
@param[in] n Number of columns.
@param[in] saVars String array with variables (may be empty).
@param[in] saBodies String array with functions' expressions. For expression syntax look at \ref basic_function description.
@param[in] saParameters String array with parameters (may be empty).
@param[in] saMeanings String array with parameters' meanings (may be empty, must have the same size as saParameters).
*/
    basic_fmatrix(size_t m, size_t n, const string_array& saVars, const string_array& saBodies,
                  const string_array& saParameters, const string_array& saMeanings)
      : BaseFArray(saVars, saBodies, saParameters, saMeanings), mM(m), mN(n)
    {
        this->_check_sizes();
    }

/**
@brief Copy Constructor

Creates copy of matrix of functions referred by <tt>fm</tt>.
@param[in] fm %Matrix of functions (\ref rfmatrix or \ref cfmatrix) to copy from.
*/
    basic_fmatrix(const basic_fmatrix& fm)
      : BaseFArray(fm), mM(fm.msize()), mN(fm.nsize())
    {
    }

/**
@brief Move Constructor
*/
    basic_fmatrix(basic_fmatrix&& fm)
      : BaseFArray(std::move(fm)), mM(fm.msize()), mN(fm.nsize())
    {
    }

/**
@brief Number of rows

Returns number of rows of calling matrix.
\par Example:
\code
using namespace cvm;
rfmatrix rfm(3,4);
std::cout << rfm.msize() << std::endl;
cfmatrix cfm(3,4);
std::cout << cfm.msize() << std::endl;
\endcode
prints
\code
3
3
\endcode
@return Number of columns of calling matrix of functions.
*/
    size_t msize() const {
        return mM;
    }

/**
@brief Number of columns

Returns number of columns of calling matrix.
\par Example:
\code
using namespace cvm;
rfmatrix rfm(3,4);
std::cout << rfm.nsize() << std::endl;
cfmatrix cfm(3,4);
std::cout << cfm.nsize() << std::endl;
\endcode
prints
\code
4
4
\endcode
@return Number of rows of calling matrix of functions.
*/
    size_t nsize() const {
        return mN;
    }

    // no destructor here because non-virtual std::vector::~vector does the job

/**
@brief Assignment operator

Assigns basic_fmatrix object to calling one or throws \ref cvmexception
if objects have different sizes.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
funcs.push_back("{x} x^4");
rfmatrix rfm(2,2,funcs), rfm2(2,2);
cfmatrix cfm(2,2,funcs), cfm2(2,2);
rfm2 = rfm;
cfm2 = cfm;
std::cout << rfm2 << std::endl;
std::cout << cfm2 << std::endl;
\endcode
prints
\code
{x} x {x} x^3
{x} x^2 {x} x^4

{x} x {x} x^(3,0)
{x} x^(2,0) {x} x^(4,0)
\endcode
@param[in] fm Const reference to matrix of functions to assign.
@return Reference to changed calling matrix.
*/
    basic_fmatrix& operator = (const basic_fmatrix& fm) throw(cvmexception)
    {
        this->_check_sizes(fm);
        this->_assign(fm);
        return *this;
    }

/**
@brief Assignment operator
*/
    basic_fmatrix& operator = (basic_fmatrix&& fm) throw(cvmexception)
    {
        this->_check_sizes(fm);
        this->mp = std::move(fm.mv);
        return *this;
    }

/**
@brief Reference to element (l-value)

Operator provides access to a particular element of calling matrix of functions by its row and column index.
<strong>Indexes passed are 0-based.
This is different from core classes indexing like \ref basic_rmatrix indexing.</strong>
That's because function arrays are based on std::vector implementation having 0-based indexing.
Operator returns \e l-value in order to make possible write access to an element.
Operator throws \ref cvmexception if \c nRow or \c nCol is outside of boundaries.
\par Example:
\code
using namespace cvm;
rfmatrix rfm(3,2);
cfmatrix cfm(3,2);
rfm.at(0,1) = rfunction("{x} x+1");
cfm.at(0,1) = cfunction("{x} x+1");

std::cout << rfm << std::endl;
rmatrix rm(3,2);
rfm.value(1., rm);
std::cout << rm << std::endl;

std::cout << cfm << std::endl;
cmatrix cm(3,2);
cfm.value(tcomplex(1., 1.), cm);
std::cout << cm << std::endl;
\endcode
prints
\code
0 {x} x+1
0 0
0 0

0 2
0 0
0 0

(0,0) {x} x+(1,0)
(0,0) (0,0)
(0,0) (0,0)

(0,0) (2,1)
(0,0) (0,0)
(0,0) (0,0)
\endcode
@see http://cvmlib.com/faq.htm
@param[in] nRow Row index (0-based).
@param[in] nCol Column index (0-based).
@return Reference to \ref basic_function element (l-value).
*/
    BaseFunction& at(size_t nRow, size_t nCol) {
        return this->mv.at(mM * nCol + nRow);
    }

/**
@brief Value of element (\e not l-value)

Operator returns value of a particular element of calling matrix of functions by its row and column index.
<strong>Indexes passed are 0-based.
This is different from core classes indexing like \ref basic_rmatrix indexing.</strong>
That's because function arrays are based on std::vector implementation having 0-based indexing.
Operator throws \ref cvmexception if \c nRow or \c nCol is outside of boundaries.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
funcs.push_back("{x} x^4");
const rfmatrix rfm(2,2,funcs);
const cfmatrix cfm(2,2,funcs);
std::cout << rfm.at(1,0) << std::endl;
std::cout << cfm.at(1,0) << std::endl;
\endcode
prints
\code
{x} x^2
{x} x^(2,0)
\endcode
@see http://cvmlib.com/faq.htm
@param[in] nRow Row index (0-based).
@param[in] nCol Column index (0-based).
@return Constant reference to \ref basic_function element (i.e. not l-value).
*/
    const BaseFunction& at(size_t nRow, size_t nCol) const {
        return this->mv.at(mM * nCol + nRow);
    }

/**
@brief Matrices of functions comparison

Returns true if matrices of functions are equal and false otherwise.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
funcs.push_back("{x} x^4");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
rfmatrix rfm2(2,2);
cfmatrix cfm2(2,2);
std::cout << (rfm2 == rfm) << std::endl;
std::cout << (cfm2 == cfm) << std::endl;
rfm2 = rfm;
cfm2 = cfm;
std::cout << (rfm2 == rfm) << std::endl;
std::cout << (cfm2 == cfm) << std::endl;
\endcode
prints
\code
0
0
1
1
\endcode
@param[in] fm Const reference to matrix of functions to compare with.
@return boolean value.
*/
    bool operator == (const basic_fmatrix& fm) const {
        return this->msize() == fm.msize() && this->nsize() == fm.nsize() && this->_equals(fm);
    }

/**
@brief Matrices of functions comparison

Returns true if matrices of functions are not equal and false otherwise.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
funcs.push_back("{x} x^4");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
rfmatrix rfm2(2,2);
cfmatrix cfm2(2,2);
std::cout << (rfm2 != rfm) << std::endl;
std::cout << (cfm2 != cfm) << std::endl;
rfm2 = rfm;
cfm2 = cfm;
std::cout << (rfm2 != rfm) << std::endl;
std::cout << (cfm2 != cfm) << std::endl;
\endcode
prints
\code
1
1
0
0
\endcode
@param[in] fm Const reference to matrix of functions to compare with.
@return boolean value.
*/
    bool operator != (const basic_fmatrix& fm) const {
        return !this->operator == (fm);
    }

/**
@brief Replacement operator

Assigns basic_fmatrix object to calling one without checking anything.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
funcs.push_back("{x} x^4");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
rfmatrix rfm2(2,2);
cfmatrix cfm2(2,2);

rfm2 << rfm;
cfm2 << cfm;
std::cout << rfm2 << cfm2;
\endcode
prints
\code
{x} x {x} x^3
{x} x^2 {x} x^4
{x} x {x} x^(3,0)
{x} x^(2,0) {x} x^(4,0)
\endcode
@param[in] fm Const reference to matrix of functions to replace by.
@return Reference to changed calling matrix.
*/
    basic_fmatrix& operator<<(const basic_fmatrix& fm) {
        this->mv.resize(fm.size());
        this->_assign(fm);
        this->mM = fm.msize();
        this->mN = fm.nsize();
        return *this;
    }

/**
@brief Partial derivative

Creates basic_fmatrix object as a partial derivative of calling
matrix. Variables set remains the same even if the derivative
doesn't depend on some of them. Partial derivative is computed
by <tt>nVarNum</tt>'s variable (0-based, 0 by default).
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y} x + log(x) + x^y");
funcs.push_back("{x,y} sin(x)*cos(y) + sin(y)*cos(x) + x*y");
funcs.push_back("{x,y} log10(x*y)");
funcs.push_back("{x,y} x/(x+y)");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
std::cout << rfm.drv(0) << std::endl << rfm.drv(1) << std::endl;
std::cout << cfm.drv(0) << std::endl << cfm.drv(1) << std::endl;
\endcode
prints
\code
{x,y} 1+1/x+y*x^(y-1) {x,y} 0.434294/x
{x,y} cos(x)*cos(y)-sin(x)*sin(y)+y {x,y} y/(x+y)^2

{x,y} x^y*log(x) {x,y} 0.434294/y
{x,y} cos(y)*cos(x)-sin(y)*sin(x)+x {x,y} -x/(x+y)^2

{x,y} (1,0)+(1,0)/x+y*x^(y-(1,-0)) {x,y} (0.434294,0)/x
{x,y} cos(x)*cos(y)-sin(x)*sin(y)+y {x,y} y/(x+y)^(2,0)

{x,y} x^y*log(x) {x,y} (0.434294,0)/y
{x,y} cos(y)*cos(x)-sin(y)*sin(x)+x {x,y} -x/(x+y)^(2,0)
\endcode
@param[in] nVarNum Variable's index (0-based, 0 by default).
@return %Matrix of functions object.
*/
    basic_fmatrix drv(size_t nVarNum) const
    {
        basic_fmatrix fmRet(this->mM, this->mN);
        fmRet._drv(*this, nVarNum);
        return fmRet;
    }

/**
@brief Simplifier

Simplifies basic_fmatrix for fatsest numerical computation
possible and returns reference to the object changed.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y} x*2 + 3*x");
funcs.push_back("{x,y} x + y + x + y + x");
funcs.push_back("{x,y} 1 + 2 + 3 + 4");
funcs.push_back("{x,y} 2*(x + x + x)");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
std::cout << rfm.simp() << std::endl << cfm.simp();
\endcode
prints
\code
{x,y} x*5 {x,y} 10
{x,y} 2*(y+x)+x {x,y} 6*x

{x,y} x*(5,0) {x,y} (10,0)
{x,y} (2,0)*(y+x)+x {x,y} (6,0)*x
\endcode
@return Reference to changed calling matrix.
*/
    basic_fmatrix& simp()
    {
        this->_simp();
        return *this;
    }

/**
@brief Addition operator

Creates basic_fmatrix object as sum of calling matrix and
matrix referred by <tt>fm</tt>. Operator throws \ref
cvmexception if sizes of matrices or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} y");
    funcs2.push_back("{x,y} x * y");
    rfmatrix rfm1(2,1,funcs1);
    cfmatrix cfm1(2,1,funcs1);
    rfmatrix rfm2(2,1,funcs2);
    cfmatrix cfm2(2,1,funcs2);
    std::cout << (rfm1 + rfm2) << std::endl;
    std::cout << (cfm1 + cfm2) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x+y
{x,y} x+y+x*y

{x,y} x+y
{x,y} x+y+x*y
\endcode
@param[in] fm Const reference to matrix to add.
@return %Matrix of functions object.
*/
    basic_fmatrix operator + (const basic_fmatrix& fm) const throw(cvmexception)
    {
        this->_check_sizes(fm);
        basic_fmatrix fmRet(this->mM, this->mN);
        fmRet._add(*this, fm);
        return fmRet;
    }

/**
@brief Subtraction operator

Creates basic_fmatrix object as difference of calling matrix and
matrix referred by <tt>fm</tt>. Operator throws \ref
cvmexception if sizes of matrices or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} y");
    funcs2.push_back("{x,y} x * y");
    rfmatrix rfm1(2,1,funcs1);
    cfmatrix cfm1(2,1,funcs1);
    rfmatrix rfm2(2,1,funcs2);
    cfmatrix cfm2(2,1,funcs2);
    std::cout << (rfm1 - rfm2) << std::endl;
    std::cout << (cfm1 - cfm2) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x-y
{x,y} x+y-x*y

{x,y} x-y
{x,y} x+y-x*y
\endcode
@param[in] fm Const reference to matrix to subtract.
@return %Matrix of functions object.
*/
    basic_fmatrix operator - (const basic_fmatrix& fm) const throw(cvmexception)
    {
        this->_check_sizes(fm);
        basic_fmatrix fmRet(this->mM, this->mN);
        fmRet._subtract(*this, fm);
        return fmRet;
    }

/**
@brief Increment operator

Adds to calling basic_fmatrix
matrix of functions referred by <tt>fm</tt> and returns
reference to the object changed. Operator throws \ref
cvmexception if sizes of matrices or lists of variables don't
match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} y");
    funcs2.push_back("{x,y} x * y");
    rfmatrix rfm1(2,1,funcs1);
    cfmatrix cfm1(2,1,funcs1);
    rfmatrix rfm2(2,1,funcs2);
    cfmatrix cfm2(2,1,funcs2);
    rfm2 += rfm1;
    cfm2 += cfm1;
    std::cout << rfm2 << std::endl;
    std::cout << cfm2 << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} y+x
{x,y} x*y+x+y

{x,y} y+x
{x,y} x*y+x+y
\endcode
@param[in] fm Const reference to matrix to add.
@return Reference to changed calling matrix.
*/
    basic_fmatrix& operator += (const basic_fmatrix& fm) throw(cvmexception)
    {
        this->_check_sizes(fm);
        this->_add(fm);
        return *this;
    }

/**
@brief Decrement operator

Subtracts from calling basic_fmatrix
matrix of functions referred by <tt>fm</tt> and returns
reference to the object changed. Operator throws \ref
cvmexception if sizes of matrices or lists of variables don't
match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} y");
    funcs2.push_back("{x,y} x * y");
    rfmatrix rfm1(2,1,funcs1);
    cfmatrix cfm1(2,1,funcs1);
    rfmatrix rfm2(2,1,funcs2);
    cfmatrix cfm2(2,1,funcs2);
    rfm2 -= rfm1;
    cfm2 -= cfm1;
    std::cout << rfm2 << std::endl;
    std::cout << cfm2 << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} y-x
{x,y} x*y-x+y

{x,y} y-x
{x,y} x*y-x+y
\endcode
@param[in] fm Const reference to matrix to subtract.
@return Reference to changed calling matrix.
*/
    basic_fmatrix& operator -= (const basic_fmatrix& fm) throw(cvmexception)
    {
        this->_check_sizes(fm);
        this->_subtract(fm);
        return *this;
    }

/**
@brief Multiplication operator

Creates basic_fmatrix object as product of calling matrix
and function referred by <tt>f</tt>. Operator throws \ref
cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    funcs.push_back("{x} x / 2");
    funcs.push_back("{x} x - x^2");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);
    rfunction rf("{x} x/2");
    cfunction cf("{x} x/2");
    std::cout << rfm * rf << std::endl << (rfm * rf).simp() << std::endl;
    std::cout << cfm * cf << std::endl << (cfm * cf).simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)*x/2 {x} x/2*x/2
{x} 2*x*x/2 {x} (x-x^2)*x/2

{x} (x+x^2)*x/2 {x} (x/2)^2
{x} x^2 {x} (x-x^2)*x/2

{x} (x+x^(2,0))*x/(2,0) {x} x/(2,0)*x/(2,0)
{x} (2,0)*x*x/(2,0) {x} (x-x^(2,0))*x/(2,0)

{x} (x+x^(2,0))*x/(2,0) {x} (x/(2,0))^(2,0)
{x} x^(2,0) {x} (x-x^(2,0))*x/(2,0)
\endcode
@param[in] f Const reference to function to multiply by.
@return %Matrix of functions object.
*/
    basic_fmatrix operator *(const BaseFunction& f) const
    {
        basic_fmatrix fmRet(this->mM, this->mN);
        fmRet.BaseFArray::_mult(*this, f);
        return fmRet;
    }

/**
@brief Division operator

Creates basic_fmatrix object as division of calling matrix
by function referred by <tt>f</tt>. Operator throws \ref
cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    funcs.push_back("{x} x / 2");
    funcs.push_back("{x} x - x^2");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);
    rfunction rf("{x} x/2");
    cfunction cf("{x} x/2");
    std::cout << rfm / rf << std::endl << (rfm / rf).simp() << std::endl;
    std::cout << cfm / cf << std::endl << (cfm / cf).simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)/(x/2) {x} (x/2)/(x/2)
{x} 2*x/(x/2) {x} (x-x^2)/(x/2)

{x} (x+x^2)*2/x {x} 1
{x} 4 {x} (x-x^2)*2/x

{x} (x+x^(2,0))/(x/(2,0)) {x} (x/(2,0))/(x/(2,0))
{x} (2,0)*x/(x/(2,0)) {x} (x-x^(2,0))/(x/(2,0))

{x} (x+x^(2,0))*(2,0)/x {x} (1,0)
{x} (4,0) {x} (x-x^(2,0))*(2,0)/x
\endcode
@param[in] f Const reference to function to divide by.
@return %Matrix of functions object.
*/
    basic_fmatrix operator / (const BaseFunction& f) const
    {
        basic_fmatrix fmRet(this->mM, this->mN);
        fmRet._div(*this, f);
        return fmRet;
    }

/**
@brief Multiplication operator

Creates basic_fmatrix object as product of calling matrix
and real or complex number referred by <tt>d</tt>.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x^2");
    funcs.push_back("{x} 2 * x");
    funcs.push_back("{x} x / 2");
    funcs.push_back("{x} x + 2");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);
    std::cout << rfm * 2. << std::endl << (rfm * 2.).simp() << std::endl;
    std::cout << cfm * 2. << std::endl << (cfm * 2.).simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} x^2*2 {x} x/2*2
{x} 2*x*2 {x} (x+2)*2

{x} x^2*2 {x} x
{x} 4*x {x} (x+2)*2

{x} x^(2,0)*(2,0) {x} x/(2,0)*(2,0)
{x} (2,0)*x*(2,0) {x} (x+(2,0))*(2,0)

{x} x^(2,0)*(2,0) {x} x
{x} (4,0)*x {x} (x+(2,0))*(2,0)
\endcode
@param[in] d Number to multiply by.
@return %Matrix of functions object.
*/
    basic_fmatrix operator *(const T& d) const
    {
        basic_fmatrix fmRet(this->mM, this->mN);
        fmRet.BaseFArray::_mult(*this, d);
        return fmRet;
    }

/**
@brief Division operator

Creates basic_fmatrix object as division of calling matrix
by real or complex number referred by <tt>d</tt>.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x^2");
    funcs.push_back("{x} 2 * x");
    funcs.push_back("{x} x / 2");
    funcs.push_back("{x} x + 2");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);
    std::cout << rfm / 2. << std::endl << (rfm / 2.).simp() << std::endl;
    std::cout << cfm / 2. << std::endl << (cfm / 2.).simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} x^2/2 {x} (x/2)/2
{x} 2*x/2 {x} (x+2)/2

{x} x^2/2 {x} x/4
{x} x {x} (x+2)/2

{x} x^(2,0)/(2,0) {x} (x/(2,0))/(2,0)
{x} (2,0)*x/(2,0) {x} (x+(2,0))/(2,0)

{x} x^(2,0)/(2,0) {x} x/(4,0)
{x} x {x} (x+(2,0))/(2,0)
\endcode
@param[in] d Number to multiply by.
@return %Matrix of functions object.
*/
    basic_fmatrix operator / (const T& d) const
    {
        basic_fmatrix fmRet(this->mM, this->mN);
        fmRet._div(*this, d);
        return fmRet;
    }

/**
@brief Multiply and assign operator

Multiplies calling matrix of functions
by function referred by <tt>f</tt> and returns reference to the
object changed. Operator throws
\ref cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    funcs.push_back("{x} x / 2");
    funcs.push_back("{x} x - x^2");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);
    rfunction rf("{x} x/2");
    cfunction cf("{x} x/2");
    rfm *= rf;
    cfm *= cf;
    std::cout << rfm.simp() << std::endl;
    std::cout << cfm.simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)*x/2 {x} (x/2)^2
{x} x^2 {x} (x-x^2)*x/2

{x} (x+x^(2,0))*x/(2,0) {x} (x/(2,0))^(2,0)
{x} x^(2,0) {x} (x-x^(2,0))*x/(2,0)
\endcode
@param[in] f Const reference to function to multiply by.
@return Reference to changed calling matrix.
*/
    basic_fmatrix& operator *= (const BaseFunction& f) {
        this->BaseFArray::_mult(f);
        return *this;
    }

/**
@brief Divide and assign operator

Divides calling matrix of functions
by function referred by <tt>f</tt> and returns reference to the
object changed. Operator throws
\ref cvmexception if lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    funcs.push_back("{x} x / 2");
    funcs.push_back("{x} x - x^2");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);
    rfunction rf("{x} x/2");
    cfunction cf("{x} x/2");
    rfm /= rf;
    cfm /= cf;
    std::cout << rfm.simp() << std::endl;
    std::cout << cfm.simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)*2/x {x} 1
{x} 4 {x} (x-x^2)*2/x

{x} (x+x^(2,0))*(2,0)/x {x} (1,0)
{x} (4,0) {x} (x-x^(2,0))*(2,0)/x
\endcode
@param[in] f Const reference to function to divide by.
@return Reference to changed calling matrix.
*/
    basic_fmatrix& operator /= (const BaseFunction& f)
    {
        this->BaseFArray::_div(f);
        return *this;
    }

/**
@brief Multiply and assign operator

Multiplies calling matrix of functions
by real or complex number referred by <tt>d</tt> and returns reference to the
object changed.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    funcs.push_back("{x} x / 2");
    funcs.push_back("{x} x - x^2");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);
    rfm *= 2.;
    cfm *= 2.;
    std::cout << rfm.simp() << std::endl;
    std::cout << cfm.simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)*2 {x} x
{x} 4*x {x} (x-x^2)*2

{x} (x+x^(2,0))*(2,0) {x} x
{x} (4,0)*x {x} (x-x^(2,0))*(2,0)
\endcode
@param[in] d Number to multiply by.
@return Reference to changed calling matrix.
*/
    basic_fmatrix& operator *= (const T& d)
    {
        this->BaseFArray::_mult(d);
        return *this;
    }

/**
@brief Divide and assign operator

Divides calling matrix of functions
by real or complex number referred by <tt>d</tt> and returns reference to the
object changed.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs;
    funcs.push_back("{x} x + x^2");
    funcs.push_back("{x} 2 * x");
    funcs.push_back("{x} x / 2");
    funcs.push_back("{x} x - x^2");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);
    rfm /= 2.;
    cfm /= 2.;
    std::cout << rfm.simp() << std::endl;
    std::cout << cfm.simp() << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} (x+x^2)/2 {x} x/4
{x} x {x} (x-x^2)/2

{x} (x+x^(2,0))/(2,0) {x} x/(4,0)
{x} x {x} (x-x^(2,0))/(2,0)
\endcode
@param[in] d Number to divide by.
@return Reference to changed calling matrix.
*/
    basic_fmatrix& operator /= (const T& d)
    {
        this->BaseFArray::_div(d);
        return *this;
    }

/**
@brief Row as \em not l-value

Operator creates \ref basic_fvector object as a copy of \c
nRow-th row (0-based) of calling matrix.
<strong>Note: index passed is 0-based.
This is different from core classes indexing like
\ref basic_rmatrix indexing.</strong>
Operator throws
\ref cvmexception if \c nRow is outside of boundaries.
\par Example:
\code
try{
    string_array funcs;
    funcs.push_back("{x} 1");
    funcs.push_back("{x} 2");
    funcs.push_back("{x} 3");
    funcs.push_back("{x} 4");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);
    std::cout << rfm.get_row(0) << std::endl;
    std::cout << rfm.get_row(1) << std::endl;
    std::cout << cfm.get_row(0) << std::endl;
    std::cout << cfm.get_row(1) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} 1 {x} 3

{x} 2 {x} 4

{x} (1,0) {x} (3,0)

{x} (2,0) {x} (4,0)
\endcode
@param[in] nRow Index of row (0-based).
@see set_row(size_t,const basic_fvector<T>&)
@return \ref basic_fvector Row value.
*/
    basic_fvector<T> get_row(size_t nRow) const throw(cvmexception)
    {
        _check_ge(CVM_INDEX_GE, nRow, this->mM);
        basic_fvector<T> fvRet(this->mN);
        _copy(this->mN, &this->mv.at(nRow), this->mM, &fvRet.mv.at(0), 1);
        return fvRet;
    }

/**
@brief Column as \em not l-value

Operator creates \ref basic_fvector object as a copy of \c
nCol-th column (0-based) of calling matrix.
<strong>Note: index passed is 0-based.
This is different from core classes indexing like
\ref basic_rmatrix indexing.</strong>
Operator throws
\ref cvmexception if \c nCol is outside of boundaries.
\par Example:
\code
try{
    string_array funcs;
    funcs.push_back("{x} 1");
    funcs.push_back("{x} 2");
    funcs.push_back("{x} 3");
    funcs.push_back("{x} 4");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);
    std::cout << rfm.get_col(0) << std::endl;
    std::cout << rfm.get_col(1) << std::endl;
    std::cout << cfm.get_col(0) << std::endl;
    std::cout << cfm.get_col(1) << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} 1 {x} 2

{x} 3 {x} 4

{x} (1,0) {x} (2,0)

{x} (3,0) {x} (4,0)
\endcode
@param[in] nCol Index of column (0-based).
@see set_col(size_t,const basic_fvector<T>&)
@return \ref basic_fvector Column value.
*/
    basic_fvector<T> get_col(size_t nCol) const throw(cvmexception)
    {
        _check_ge(CVM_INDEX_GE, nCol, this->mN);
        basic_fvector<T> fvRet(this->mM);
        _copy(this->mM, &this->mv.at(this->mM * nCol), 1, &fvRet.mv.at(0), 1);
        return fvRet;
    }

/**
@brief Row assignment

Function assigns \ref basic_fvector object referred by
<tt>fv</tt> to nRow-th row (0-based) of calling matrix and
   returns reference to the object changed.
<strong>Note: index passed is 0-based.
This is different from core classes indexing like
\ref basic_rmatrix indexing.</strong>
Operator throws
\ref cvmexception if \c nRow is outside of boundaries.
\par Example:
\code
try{
    string_array funcs;
    funcs.push_back("{x} 1");
    funcs.push_back("{x} 2");
    funcs.push_back("{x} 3");
    funcs.push_back("{x} 4");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);

    rfvector r1 = rfm.get_row(1);
    rfm.set_row(1, r1 * 2).simp();
    std::cout << rfm << std::endl;

    cfvector c1 = cfm.get_row(1);
    cfm.set_row(1, c1 * 2).simp();
    std::cout << cfm << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} 1 {x} 3
{x} 4 {x} 8

{x} (1,0) {x} (3,0)
{x} (4,0) {x} (8,0)
\endcode
@param[in] nRow Index of row (0-based).
@param[in] fv %Vector of functions to assign.
@see get_row(size_t)
@return Reference to changed calling matrix.
*/
    basic_fmatrix& set_row(size_t nRow, const basic_fvector<T>& fv) throw(cvmexception)
    {
        _check_ge(CVM_INDEX_GE, nRow, this->mM);
        _check_ne(CVM_SIZESMISMATCH, fv.size(), this->nsize());
        _copy(this->mN, &fv.mv.at(0), 1, &this->mv.at(nRow), this->mM);
        return *this;
    }

/**
@brief Column assignment

Function assigns \ref basic_fvector object referred by
<tt>fv</tt> to nCol-th column (0-based) of calling matrix and
   returns reference to the object changed.
<strong>Note: index passed is 0-based.
This is different from core classes indexing like
\ref basic_rmatrix indexing.</strong>
Operator throws
\ref cvmexception if \c nCol is outside of boundaries.
\par Example:
\code
try{
    string_array funcs;
    funcs.push_back("{x} 1");
    funcs.push_back("{x} 2");
    funcs.push_back("{x} 3");
    funcs.push_back("{x} 4");
    rfmatrix rfm(2,2,funcs);
    cfmatrix cfm(2,2,funcs);

    rfvector r1 = rfm.get_col(1);
    rfm.set_col(1, r1 * 2).simp();
    std::cout << rfm << std::endl;

    cfvector c1 = cfm.get_col(1);
    cfm.set_col(1, c1 * 2).simp();
    std::cout << cfm << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} 1 {x} 6
{x} 2 {x} 8

{x} (1,0) {x} (6,0)
{x} (2,0) {x} (8,0)
\endcode
@param[in] nCol Index of column (0-based).
@param[in] fv %Vector of functions to assign.
@see get_col(size_t)
@return Reference to changed calling matrix.
*/
    basic_fmatrix& set_col(size_t nCol, const basic_fvector<T>& fv) throw(cvmexception)
    {
        _check_ge(CVM_INDEX_GE, nCol, this->nsize());
        _check_ne(CVM_SIZESMISMATCH, fv.size(), this->msize());
        _copy(this->mM, &fv.mv.at(0), 1, &this->mv.at(this->mM * nCol), 1);
        return *this;
    }

/**
@brief %Matrix by vector product

Creates basic_fvector object as product of calling matrix of
functions
and vector of functions referred by <tt>fv</tt>.
Operator throws
\ref cvmexception if sizes of objects or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x,y} x");
    funcs1.push_back("{x,y} x + y");
    funcs2.push_back("{x,y} x");
    funcs2.push_back("{x,y} 1");
    funcs2.push_back("{x,y} 2");
    funcs2.push_back("{x,y} y");
    rfvector rfv1(funcs1);
    cfvector cfv1(funcs1);
    rfmatrix rfm(2,2,funcs2);
    cfmatrix cfm(2,2,funcs2);
    rfvector rfv2 = rfm * rfv1;
    cfvector cfv2 = cfm * cfv1;
    std::cout << rfm << std::endl;
    std::cout << rfv2 << std::endl;
    std::cout << cfm << std::endl;
    std::cout << cfv2 << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} x {x,y} 2
{x,y} 1 {x,y} y

{x,y} x^2+2*(x+y) {x,y} x+y*(x+y)

{x,y} x {x,y} (2,0)
{x,y} (1,0) {x,y} y

{x,y} x^(2,0)+(2,0)*(x+y) {x,y} x+y*(x+y)
\endcode
@param[in] fv Vector of functions to multiply by.
@return %Vector of functions object.
*/
    basic_fvector<T> operator *(const basic_fvector<T>& fv) const throw(cvmexception)
    {
        _check_ne(CVM_SIZESMISMATCH, fv.size(), this->nsize());
        basic_fvector<T> vRet(this->msize());
        vRet._mult(*this, fv);
        return vRet;
    }

/**
@brief %Matrix by matrix product

Creates basic_fmatrix object as product of calling matrix of
functions
and matrix of functions referred by <tt>fm</tt>.
Operator throws
\ref cvmexception if sizes of objects or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x} x");
    funcs1.push_back("{x} x^2");
    funcs1.push_back("{x} x^3");
    funcs1.push_back("{x} x^4");
    funcs2.push_back("{x} 1");
    funcs2.push_back("{x} 2");
    funcs2.push_back("{x} 3");
    funcs2.push_back("{x} 4");
    rfmatrix rfm1(2,2,funcs1);
    cfmatrix cfm1(2,2,funcs1);
    rfmatrix rfm2(2,2,funcs2);
    cfmatrix cfm2(2,2,funcs2);
    std::cout << rfm1 << std::endl;
    std::cout << rfm2 << std::endl;
    std::cout << rfm1 * rfm2 << std::endl;
    std::cout << cfm1 << std::endl;
    std::cout << cfm2 << std::endl;
    std::cout << cfm1 * cfm2 << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} x {x} x^3
{x} x^2 {x} x^4

{x} 1 {x} 3
{x} 2 {x} 4

{x} x+x^3*2 {x} x*3+x^3*4
{x} x^2+x^4*2 {x} x^2*3+x^4*4

{x} x {x} x^(3,0)
{x} x^(2,0) {x} x^(4,0)

{x} (1,0) {x} (3,0)
{x} (2,0) {x} (4,0)

{x} x+x^(3,0)*(2,0) {x} x*(3,0)+x^(3,0)*(4,0)
{x} x^(2,0)+x^(4,0)*(2,0) {x} x^(2,0)*(3,0)+x^(4,0)*(4,0)
\endcode
@param[in] fm %Matrix of functions to multiply by.
@return %Matrix of functions object.
*/
    basic_fmatrix operator *(const basic_fmatrix& fm) const throw(cvmexception)
    {
        _check_ne(CVM_SIZESMISMATCH, fm.msize(), this->nsize());
        basic_fmatrix mRet(this->msize(), fm.nsize());
        mRet._mult(*this, fm);
        return mRet;
    }

/**
@brief %Matrix by matrix product

Sets calling basic_fmatrix object to be product of matrices referred by <tt>fmA</tt>
and <tt>fmB</tt> and returns reference to the object changed. Function throws
\ref cvmexception if sizes of objects or lists of variables don't match.
\par Example:
\code
using namespace cvm;
try{
    string_array funcs1, funcs2;
    funcs1.push_back("{x} x");
    funcs1.push_back("{x} x^2");
    funcs1.push_back("{x} x^3");
    funcs1.push_back("{x} x^4");
    funcs2.push_back("{x} 1");
    funcs2.push_back("{x} 2");
    funcs2.push_back("{x} 3");
    funcs2.push_back("{x} 4");
    rfmatrix rfm1(2,2,funcs1);
    cfmatrix cfm1(2,2,funcs1);
    rfmatrix rfm2(2,2,funcs2);
    cfmatrix cfm2(2,2,funcs2);
    rfmatrix rfm(2,2);
    cfmatrix cfm(2,2);
    rfm.mult(rfm1, rfm2);
    cfm.mult(cfm1, cfm2);
    std::cout << rfm1 << std::endl;
    std::cout << rfm2 << std::endl;
    std::cout << rfm << std::endl;
    std::cout << cfm1 << std::endl;
    std::cout << cfm2 << std::endl;
    std::cout << cfm << std::endl;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x} x {x} x^3
{x} x^2 {x} x^4

{x} 1 {x} 3
{x} 2 {x} 4

{x} x+x^3*2 {x} x*3+x^3*4
{x} x^2+x^4*2 {x} x^2*3+x^4*4

{x} x {x} x^(3,0)
{x} x^(2,0) {x} x^(4,0)

{x} (1,0) {x} (3,0)
{x} (2,0) {x} (4,0)

{x} x+x^(3,0)*(2,0) {x} x*(3,0)+x^(3,0)*(4,0)
{x} x^(2,0)+x^(4,0)*(2,0) {x} x^(2,0)*(3,0)+x^(4,0)*(4,0)
\endcode
@param[in] fmA %Matrix of functions to multiply.
@param[in] fmB %Matrix of functions to multiply by.
@return Reference to changed calling matrix.
*/
    basic_fmatrix& mult(const basic_fmatrix& fmA, const basic_fmatrix& fmB) throw(cvmexception)
    {
        _check_ne(CVM_SIZESMISMATCH, fmA.msize(), this->msize());
        _check_ne(CVM_SIZESMISMATCH, fmB.nsize(), this->nsize());
        _check_ne(CVM_SIZESMISMATCH, fmB.msize(), fmA.nsize());
        return this->_mult(fmA, fmB);
    }

/**
@brief Output operator

Prints matrix of functions referred by <tt>fm</tt>
to output stream referred by <tt>os</tt>
using Wolfram's Mathemaca syntax
<tt>{var1[,var2,...]} expr</tt> for each element.
\par Example:
\code
string_array funcs;
funcs.push_back("{x,y} x");
funcs.push_back("{x,y} x^2");
funcs.push_back("{x,y} y");
funcs.push_back("{x,y} y^2");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
std::cout << rfm << std::endl;
std::cout << cfm << std::endl;
\endcode
prints
\code
{x,y} x {x,y} y
{x,y} x^2 {x,y} y^2

{x,y} x {x,y} y
{x,y} x^(2,0) {x,y} y^(2,0)
\endcode
@param[in] os Reference to output stream.
@param[in] fm Const reference to matrix of functions to print.
@return Reference to output stream.
*/
    friend std::ostream& operator <<(std::ostream& os, const basic_fmatrix<T>& fm)
    {
        for (size_t i = 0; i < fm.msize(); ++i) {
            for (size_t j = 0; j < fm.nsize(); ++j) {
                os << fm.mv.at(fm.msize() * j + i) << CFUN_SSPACE;
            }
            os << std::endl;
        }
        return os;
    }

//! @cond INTERNAL
protected:
    // internal version, it doesn't verify anything
    basic_fmatrix& _mult(const basic_fmatrix& fmA, const basic_fmatrix& fmB) throw(cvmexception)
    {
        for (size_t i = 0; i < this->mM; ++i) {
            for (size_t j = 0; j < this->mN; ++j) {
                this->mv.at(this->mM * j + i)._scalar_product(fmA.nsize(), &fmA.mv.at(i), this->mM, &fmB.mv.at(fmB.msize() * j), 1);
            }
        }
        return *this;
    }
//! @endcond
};


/**
@brief %Matrix of real functions 

Fortran style storage is used here, i.e. elements are stored by columns (the same way as in \ref basic_rmatrix etc.).
@see basic_fmatrix
@see cfmatrix
*/
template <typename TR>
class basic_rfmatrix : public basic_fmatrix<TR>
{
protected:
    using BaseFMatrix = basic_fmatrix<TR>; //!< Base class

public:
/**
@brief Default constructor

Creates empty matrix of functions. No memory gets allocated.
*/
    basic_rfmatrix()
      : BaseFMatrix()
    {
    }

/**
@brief Constructor

Creates <tt>m</tt>-by-<tt>n</tt> matrix of zero functions.
\par Example:
\code
using namespace cvm;
rfmatrix rfm(2,3);
std::cout << rfm;
cfmatrix cfm(2,3);
std::cout << cfm;
\endcode
prints
\code
0 0 0
0 0 0
(0,0) (0,0) (0,0)
(0,0) (0,0) (0,0)
\endcode
@param[in] m Number of rows.
@param[in] n Number of columns.
*/
    basic_rfmatrix(size_t m, size_t n)
      : BaseFMatrix(m, n)
    {
    }

/**
@brief Constructor

Creates <tt>m</tt>-by-<tt>n</tt> matrix of functions.
Each function element is ininialized using appropriate string as
parameterless input in Wolfram's Mathemaca syntax <tt>{var1[,var2,...]} expr</tt>.
For expression syntax look at \ref basic_function description.
%Matrix elements are initialized column by column.
Constructor throws \ref cvmexception in case of inappropriate sizes passed.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
funcs.push_back("{x} x^4");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
std::cout << rfm << std::endl;
std::cout << cfm << std::endl;
\endcode
prints
\code
{x} x {x} x^3
{x} x^2 {x} x^4

{x} x {x} x^(3,0)
{x} x^(2,0) {x} x^(4,0)
\endcode
@param[in] m Number of rows.
@param[in] n Number of columns.
@param[in] saInput Array of strings with functions' expressions.
*/
    basic_rfmatrix(size_t m, size_t n, const string_array& saInput)
      : BaseFMatrix(m, n, saInput)
    {
    }

/**
@brief Constructor

Creates <tt>m</tt>-by-<tt>n</tt> matrix of parameterized functions.
Each function element is initialized using appropriate string as
parameterless input in Wolfram's Mathematica syntax <tt>{var1[,var2,...]} expr</tt>.
For expression syntax look at \ref basic_function description.
%Matrix elements are initialized column by column.
Constructor throws \ref cvmexception in case of inappropriate sizes passed.
\par Example:
\code
using namespace cvm;
try{
    string_array vars, bodies, params, meanings;
    vars.push_back("x");
    vars.push_back("y");
    params.push_back("p");
    meanings.push_back("sqrt(x) + sqrt(y)");
    bodies.push_back("p*x");
    bodies.push_back("p+1");
    bodies.push_back("p*y");
    bodies.push_back("p-1");

    rfmatrix rm(2, 2, vars, bodies, params, meanings);
    std::cout << rm;
    cfmatrix cm(2, 2, vars, bodies, params, meanings);
    std::cout << cm;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} (sqrt(x)+sqrt(y))*x {x,y} (sqrt(x)+sqrt(y))*y
{x,y} sqrt(x)+sqrt(y)+1 {x,y} sqrt(x)+sqrt(y)-1
{x,y} (sqrt(x)+sqrt(y))*x {x,y} (sqrt(x)+sqrt(y))*y
{x,y} sqrt(x)+sqrt(y)+(1,0) {x,y} sqrt(x)+sqrt(y)-(1,0)
\endcode
@param[in] m Number of rows.
@param[in] n Number of columns.
@param[in] saVars String array with variables (may be empty).
@param[in] saBodies String array with functions' expressions. For expression syntax look at \ref basic_function description.
@param[in] saParameters String array with parameters (may be empty).
@param[in] saMeanings String array with parameters' meanings (may be empty, must have the same size as saParameters).
*/
    basic_rfmatrix(size_t m, size_t n, const string_array& saVars, const string_array& saBodies,
             const string_array& saParameters, const string_array& saMeanings)
      : BaseFMatrix(m, n, saVars, saBodies, saParameters, saMeanings)
    {
    }

/**
@brief Copy Constructor

Creates copy of matrix of functions referred by <tt>fm</tt>.
@param[in] fm %Matrix of functions (or \ref rfmatrix) to copy from.
*/
    basic_rfmatrix(const BaseFMatrix& fm)
      : BaseFMatrix(fm)
    {
    }

/**
@brief Move Constructor
*/
    basic_rfmatrix(BaseFMatrix&& fm)
      : BaseFMatrix(std::move(fm))
    {
    }

/**
@brief Numerical value

Returns numerical value of matrix of functions of no variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("1*1");
funcs.push_back("2*2");
funcs.push_back("3*3");
funcs.push_back("4*4");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);

rmatrix ry = rfm();
cmatrix cy = cfm();
std::cout << rfm << ry << std::endl;
std::cout << cfm << cy << std::endl;
\endcode
prints
\code
1*1 3*3
2*2 4*4
1 9
4 16

(1,0)*(1,0) (3,0)*(3,0)
(2,0)*(2,0) (4,0)*(4,0)
(1,0) (9,0)
(4,0) (16,0)
\endcode
@return vector of functions' values.
*/
    basic_rmatrix<TR> operator () () const {
        basic_rmatrix<TR> ret((tint)this->msize(), (tint)this->nsize());
        this->value(ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of matrix of functions of one variable.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
funcs.push_back("{x} x^4");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);

rmatrix ry = rfm(2.);
cmatrix cy = cfm(tcomplex(2.,0.));
std::cout << rfm << ry << std::endl;
std::cout << cfm << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^3
{x} x^2 {x} x^4
2 8
4 16

{x} x {x} x^(3,0)
{x} x^(2,0) {x} x^(4,0)
(2,0) (8,0)
(4,0) (16,0)
\endcode
@param[in] d variable's value.
@return matrix of functions' values.
*/
    basic_rmatrix<TR> operator () (TR d) const {
        basic_rmatrix<TR> ret((tint)this->msize(), (tint)this->nsize());
        this->value(d, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of matrix of functions of two variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y} x+y");
funcs.push_back("{x,y} x-y");
funcs.push_back("{x,y} x*y");
funcs.push_back("{x,y} x^y");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);

rmatrix ry = rfm(2., 3.);
cmatrix cy = cfm(tcomplex(2.,0.), tcomplex(3.,0.));
std::cout << rfm << ry << std::endl;
std::cout << cfm << cy << std::endl;
\endcode
prints
\code
{x,y} x+y {x,y} x*y
{x,y} x-y {x,y} x^y
5 6
-1 8

{x,y} x+y {x,y} x*y
{x,y} x-y {x,y} x^y
(5,0) (6,0)
(-1,0) (8,0)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@return matrix of functions' values.
*/
    basic_rmatrix<TR> operator () (TR d1, TR d2) const {
        basic_rmatrix<TR> ret((tint)this->msize(), (tint)this->nsize());
        this->value(d1, d2, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of matrix of functions of three variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y,z} x+y+z");
funcs.push_back("{x,y,z} x-y-z");
funcs.push_back("{x,y,z} x*y*z");
funcs.push_back("{x,y,z} x^y^z");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);

rmatrix ry = rfm(3., 2., 2.);
cmatrix cy = cfm(tcomplex(3.,0.), tcomplex(2.,0.), tcomplex(2.,0.));
std::cout << rfm << ry << std::endl;
std::cout << cfm << cy << std::endl;
\endcode
prints
\code
{x,y,z} x+y+z {x,y,z} x*y*z
{x,y,z} x-y-z {x,y,z} x^(y^z)
7 12
-1 81

{x,y,z} x+y+z {x,y,z} x*y*z
{x,y,z} x-y-z {x,y,z} x^(y^z)
(7,0) (12,0)
(-1,0) (81,0)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@param[in] d3 Third variable's value.
@return matrix of functions' values.
*/
    basic_rmatrix<TR> operator () (TR d1, TR d2, TR d3) const {
        basic_rmatrix<TR> ret((tint)this->msize(), (tint)this->nsize());
        this->value(d1, d2, d3, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of matrix of functions for given values of
variables. Array's dimension is not verified.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y,z} x+y+z");
funcs.push_back("{x,y,z} x-y-z");
funcs.push_back("{x,y,z} x*y*z");
funcs.push_back("{x,y,z} x^y^z");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
rvector rx(3);
cvector cx(3);
rx[1] = 3.; rx[2] = 2.; rx[3] = 2.; 
cx[1] = tcomplex(3.,0.); cx[2] = tcomplex(2.,0.); cx[3] = tcomplex(2.,0.); 

rmatrix ry = rfm(rx);
cmatrix cy = cfm(cx);
std::cout << rfm << ry << std::endl;
std::cout << cfm << cy << std::endl;
\endcode
prints
\code
{x,y,z} x+y+z {x,y,z} x*y*z
{x,y,z} x-y-z {x,y,z} x^(y^z)
7 12
-1 81

{x,y,z} x+y+z {x,y,z} x*y*z
{x,y,z} x-y-z {x,y,z} x^(y^z)
(7,0) (12,0)
(-1,0) (81,0)
\endcode
@param[in] pd array of variables' values.
@return vector of functions' values.
*/
    basic_rmatrix<TR> operator () (const TR* pd) const {
        basic_rmatrix<TR> ret((tint)this->msize(), (tint)this->nsize());
        this->value(pd, ret);
        return ret;
    }

};



/**
@brief %Matrix of complex functions

Fortran style storage is used here, i.e. elements are stored by columns (the same way as in \ref basic_rmatrix etc.).
@see basic_fmatrix
@see rfmatrix
*/
template <typename TR, typename TC>
class basic_cfmatrix : public basic_fmatrix<TC>
{
protected:
    using BaseFMatrix = basic_fmatrix<TC>; //!< Base class

public:
/**
@brief Default constructor

Creates empty matrix of functions. No memory gets allocated.
*/
    basic_cfmatrix()
      : BaseFMatrix()
    {
    }

/**
@brief Constructor

Creates <tt>m</tt>-by-<tt>n</tt> matrix of zero functions.
\par Example:
\code
using namespace cvm;
rfmatrix rfm(2,3);
std::cout << rfm;
cfmatrix cfm(2,3);
std::cout << cfm;
\endcode
prints
\code
0 0 0
0 0 0
(0,0) (0,0) (0,0)
(0,0) (0,0) (0,0)
\endcode
@param[in] m Number of rows.
@param[in] n Number of columns.
*/
    basic_cfmatrix(size_t m, size_t n)
      : BaseFMatrix(m, n)
    {
    }

/**
@brief Constructor

Creates <tt>m</tt>-by-<tt>n</tt> matrix of functions.
Each function element is ininialized using appropriate string as
parameterless input in Wolfram's Mathemaca syntax <tt>{var1[,var2,...]} expr</tt>.
For expression syntax look at \ref basic_function description.
%Matrix elements are initialized column by column.
Constructor throws \ref cvmexception in case of inappropriate sizes passed.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
funcs.push_back("{x} x^4");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
std::cout << rfm << std::endl;
std::cout << cfm << std::endl;
\endcode
prints
\code
{x} x {x} x^3
{x} x^2 {x} x^4

{x} x {x} x^(3,0)
{x} x^(2,0) {x} x^(4,0)
\endcode
@param[in] m Number of rows.
@param[in] n Number of columns.
@param[in] saInput Array of strings with functions' expressions.
*/
    basic_cfmatrix(size_t m, size_t n, const string_array& saInput)
      : BaseFMatrix(m, n, saInput)
    {
    }

/**
@brief Constructor

Creates <tt>m</tt>-by-<tt>n</tt> matrix of parameterized functions.
Each function element is ininialized using appropriate string as
parameterless input in Wolfram's Mathemaca syntax <tt>{var1[,var2,...]} expr</tt>.
For expression syntax look at \ref basic_function description.
%Matrix elements are initialized column by column.
Constructor throws \ref cvmexception in case of inappropriate sizes passed.
\par Example:
\code
using namespace cvm;
try{
    string_array vars, bodies, params, meanings;
    vars.push_back("x");
    vars.push_back("y");
    params.push_back("p");
    meanings.push_back("sqrt(x) + sqrt(y)");
    bodies.push_back("p*x");
    bodies.push_back("p+1");
    bodies.push_back("p*y");
    bodies.push_back("p-1");

    rfmatrix rm(2, 2, vars, bodies, params, meanings);
    std::cout << rm;
    cfmatrix cm(2, 2, vars, bodies, params, meanings);
    std::cout << cm;
}
catch (std::exception& e) {
    std::cout << "Exception " << e.what () << std::endl;
}
\endcode
prints
\code
{x,y} (sqrt(x)+sqrt(y))*x {x,y} (sqrt(x)+sqrt(y))*y
{x,y} sqrt(x)+sqrt(y)+1 {x,y} sqrt(x)+sqrt(y)-1
{x,y} (sqrt(x)+sqrt(y))*x {x,y} (sqrt(x)+sqrt(y))*y
{x,y} sqrt(x)+sqrt(y)+(1,0) {x,y} sqrt(x)+sqrt(y)-(1,0)
\endcode
@param[in] m Number of rows.
@param[in] n Number of columns.
@param[in] saVars String array with variables (may be empty).
@param[in] saBodies String array with functions' expressions. For expression syntax look at \ref basic_function description.
@param[in] saParameters String array with parameters (may be empty).
@param[in] saMeanings String array with parameters' meanings (may be empty, must have the same size as saParameters).
*/
    basic_cfmatrix(size_t m, size_t n, const string_array& saVars, const string_array& saBodies,
             const string_array& saParameters, const string_array& saMeanings)
      : BaseFMatrix(m, n, saVars, saBodies, saParameters, saMeanings)
    {
    }

/**
@brief Copy Constructor

Creates copy of matrix of functions referred by <tt>fm</tt>.
@param[in] fm %Matrix of functions (or \ref cfmatrix) to copy from.
*/
    basic_cfmatrix(const BaseFMatrix& fm)
      : BaseFMatrix(fm)
    {
    }

/**
@brief Move Constructor
*/
    basic_cfmatrix(BaseFMatrix&& fm)
      : BaseFMatrix(std::move(fm))
    {
    }

/**
@brief Numerical value

Returns numerical value of matrix of functions of no variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("1*1");
funcs.push_back("2*2");
funcs.push_back("3*3");
funcs.push_back("4*4");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);

rmatrix ry = rfm();
cmatrix cy = cfm();
std::cout << rfm << ry << std::endl;
std::cout << cfm << cy << std::endl;
\endcode
prints
\code
1*1 3*3
2*2 4*4
1 9
4 16

(1,0)*(1,0) (3,0)*(3,0)
(2,0)*(2,0) (4,0)*(4,0)
(1,0) (9,0)
(4,0) (16,0)
\endcode
@return vector of functions' values.
*/
    basic_cmatrix<TR,TC> operator () () const {
        basic_cmatrix<TR,TC> ret((tint)this->msize(), (tint)this->nsize());
        this->value(ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of matrix of functions of one variable.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x} x");
funcs.push_back("{x} x^2");
funcs.push_back("{x} x^3");
funcs.push_back("{x} x^4");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);

rmatrix ry = rfm(2.);
cmatrix cy = cfm(tcomplex(2.,0.));
std::cout << rfm << ry << std::endl;
std::cout << cfm << cy << std::endl;
\endcode
prints
\code
{x} x {x} x^3
{x} x^2 {x} x^4
2 8
4 16

{x} x {x} x^(3,0)
{x} x^(2,0) {x} x^(4,0)
(2,0) (8,0)
(4,0) (16,0)
\endcode
@param[in] d variable's value.
@return matrix of functions' values.
*/
    basic_cmatrix<TR,TC> operator () (TC d) const {
        basic_cmatrix<TR,TC> ret((tint)this->msize(), (tint)this->nsize());
        this->value(d, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of matrix of functions of two variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y} x+y");
funcs.push_back("{x,y} x-y");
funcs.push_back("{x,y} x*y");
funcs.push_back("{x,y} x^y");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);

rmatrix ry = rfm(2., 3.);
cmatrix cy = cfm(tcomplex(2.,0.), tcomplex(3.,0.));
std::cout << rfm << ry << std::endl;
std::cout << cfm << cy << std::endl;
\endcode
prints
\code
{x,y} x+y {x,y} x*y
{x,y} x-y {x,y} x^y
5 6
-1 8

{x,y} x+y {x,y} x*y
{x,y} x-y {x,y} x^y
(5,0) (6,0)
(-1,0) (8,0)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@return matrix of functions' values.
*/
    basic_cmatrix<TR,TC> operator () (TC d1, TC d2) const {
        basic_cmatrix<TR,TC> ret((tint)this->msize(), (tint)this->nsize());
        this->value(d1, d2, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of matrix of functions of three variables.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y,z} x+y+z");
funcs.push_back("{x,y,z} x-y-z");
funcs.push_back("{x,y,z} x*y*z");
funcs.push_back("{x,y,z} x^y^z");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);

rmatrix ry = rfm(3., 2., 2.);
cmatrix cy = cfm(tcomplex(3.,0.), tcomplex(2.,0.), tcomplex(2.,0.));
std::cout << rfm << ry << std::endl;
std::cout << cfm << cy << std::endl;
\endcode
prints
\code
{x,y,z} x+y+z {x,y,z} x*y*z
{x,y,z} x-y-z {x,y,z} x^(y^z)
7 12
-1 81

{x,y,z} x+y+z {x,y,z} x*y*z
{x,y,z} x-y-z {x,y,z} x^(y^z)
(7,0) (12,0)
(-1,0) (81,0)
\endcode
@param[in] d1 First variable's value.
@param[in] d2 Second variable's value.
@param[in] d3 Third variable's value.
@return matrix of functions' values.
*/
    basic_cmatrix<TR,TC> operator () (TC d1, TC d2, TC d3) const {
        basic_cmatrix<TR,TC> ret((tint)this->msize(), (tint)this->nsize());
        this->value(d1, d2, d3, ret);
        return ret;
    }

/**
@brief Numerical value

Returns numerical value of matrix of functions for given values of
variables. Array's dimension is not verified.
\par Example:
\code
using namespace cvm;
string_array funcs;
funcs.push_back("{x,y,z} x+y+z");
funcs.push_back("{x,y,z} x-y-z");
funcs.push_back("{x,y,z} x*y*z");
funcs.push_back("{x,y,z} x^y^z");
rfmatrix rfm(2,2,funcs);
cfmatrix cfm(2,2,funcs);
rvector rx(3);
cvector cx(3);
rx[1] = 3.; rx[2] = 2.; rx[3] = 2.; 
cx[1] = tcomplex(3.,0.); cx[2] = tcomplex(2.,0.); cx[3] = tcomplex(2.,0.); 

rmatrix ry = rfm(rx);
cmatrix cy = cfm(cx);
std::cout << rfm << ry << std::endl;
std::cout << cfm << cy << std::endl;
\endcode
prints
\code
{x,y,z} x+y+z {x,y,z} x*y*z
{x,y,z} x-y-z {x,y,z} x^(y^z)
7 12
-1 81

{x,y,z} x+y+z {x,y,z} x*y*z
{x,y,z} x-y-z {x,y,z} x^(y^z)
(7,0) (12,0)
(-1,0) (81,0)
\endcode
@param[in] pd array of variables' values.
@return vector of functions' values.
*/
    basic_cmatrix<TR,TC> operator () (const TC* pd) const {
        basic_cmatrix<TR,TC> ret((tint)this->msize(), (tint)this->nsize());
        this->value(pd, ret);
        return ret;
    }

};




//! @cond INTERNAL

// specializations:
template<typename T>
class Comparator<T,T>
{
public:
    static bool lt(const T& l, const T& r)
    {
        return l < r;
    }
    static bool le(const T& l, const T& r)
    {
        return l <= r;
    }
    static bool gt(const T& l, const T& r)
    {
        return l > r;
    }
    static bool ge(const T& l, const T& r)
    {
        return l >= r;
    }
    static bool eq(const T& l, const T& r)
    {
        return cvm::_abs(l - r) <= cvm::basic_cvmMachMin<T>();
    }
};

template<typename T>
class Comparator<std::complex<T>, std::complex<T> >
{
public:
    static bool lt(const std::complex<T>& l, const std::complex<T>& r)
    {
        return l.real() < r.real();
    }
    static bool le(const std::complex<T>& l, const std::complex<T>& r)
    {
        return l.real() <= r.real();
    }
    static bool gt(const std::complex<T>& l, const std::complex<T>& r)
    {
        return l.real() > r.real();
    }
    static bool ge(const std::complex<T>& l, const std::complex<T>& r)
    {
        return l.real() >= r.real();
    }
    static bool eq(const std::complex<T>& l, const std::complex<T>& r)
    {
        return cvm::_abs(l.real() - r.real()) <= cvm::basic_cvmMachMin<T>() &&
               cvm::_abs(l.imag() - r.imag()) <= cvm::basic_cvmMachMin<T>();
    }
};

template<typename T>
class Comparator<T,std::complex<T> >
{
public:
    static bool lt(const T& l, const std::complex<T>& r)
    {
        return l < r.real();
    }
    static bool le(const T& l, const std::complex<T>& r)
    {
        return l <= r.real();
    }
    static bool gt(const T& l, const std::complex<T>& r)
    {
        return l > r.real();
    }
    static bool ge(const T& l, const std::complex<T>& r)
    {
        return l >= r.real();
    }
    static bool eq(const T& l, const std::complex<T>& r)
    {
        return cvm::_abs(l - r.real()) <= cvm::basic_cvmMachMin<T>() &&
               cvm::_abs(r.imag()) <= cvm::basic_cvmMachMin<T>();
    }
};

template<typename T>
class Comparator<std::complex<T>, T>
{
public:
    static bool lt(const std::complex<T>& l, const T& r)
    {
        return l.real() < r;
    }
    static bool le(const std::complex<T>& l, const T& r)
    {
        return l.real() <= r;
    }
    static bool gt(const std::complex<T>& l, const T& r)
    {
        return l.real() > r;
    }
    static bool ge(const std::complex<T>& l, const T& r)
    {
        return l.real() >= r;
    }
    static bool eq(const std::complex<T>& l, const T& r)
    {
        return cvm::_abs(l.real() - r) <= cvm::basic_cvmMachMin<T>() &&
               cvm::_abs(l.imag()) <= cvm::basic_cvmMachMin<T>();
    }
};
//! @endcond

// end-user classes
using rfunction = basic_function<treal>; //!< End-user class: function of real variables, see \ref basic_function
using cfunction = basic_function<tcomplex>; //!< End-user class: function of complex variables, see \ref basic_function

using rfvector  = basic_rfvector<treal>; //!< End-user class: function vector of real variables, see \ref basic_rfvector
using cfvector  = basic_cfvector<treal,tcomplex>; //!< End-user class: function vector of complex variables, see \ref basic_cfvector
using rfmatrix  = basic_rfmatrix<treal>; //!< End-user class: function matrix of real variables, see \ref basic_rfmatrix
using cfmatrix  = basic_cfmatrix<treal,tcomplex>; //!< End-user class: function matrix of complex variables, see \ref basic_cfmatrix

using rfunction32 = basic_function<float>; //!< End-user class: function of float variables, see \ref basic_function
using cfunction32 = basic_function<std::complex<float>>; //!< End-user class: function of complex float variables, see \ref basic_function
using rfunction64 = basic_function<double>; //!< End-user class: function of double variables, see \ref basic_function
using cfunction64 = basic_function<std::complex<double>>; //!< End-user class: function of complex double variables, see \ref basic_function

using rfvector32  = basic_rfvector<float>; //!< End-user class: function vector of float variables, see \ref basic_rfvector
using cfvector32  = basic_cfvector<float,std::complex<float>>; //!< End-user class: function vector of complex float variables, see \ref basic_cfvector
using rfmatrix32  = basic_rfmatrix<float>; //!< End-user class: function matrix of float variables, see \ref basic_rfmatrix
using cfmatrix32  = basic_cfmatrix<float,std::complex<float>>; //!< End-user class: function matrix of complex float variables, see \ref basic_cfmatrix

using rfvector64  = basic_rfvector<double>; //!< End-user class: function vector of double variables, see \ref basic_rfvector
using cfvector64  = basic_cfvector<double,std::complex<double>>; //!< End-user class: function vector of complex double variables, see \ref basic_cfvector
using rfmatrix64  = basic_rfmatrix<double>; //!< End-user class: function matrix of double variables, see \ref basic_rfmatrix
using cfmatrix64  = basic_cfmatrix<double,std::complex<double>>; //!< End-user class: function matrix of complex double variables, see \ref basic_cfmatrix

CVM_NAMESPACE_END

#endif  // _CFUN_H

