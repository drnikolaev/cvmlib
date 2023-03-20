cvmlib
======

CVM Class Library.

This C++ class library encapsulates concepts of vector and different matrices including square, band, symmetric and hermitian ones in Euclidean space of real and complex numbers. It utilizes BLAS and LAPACK Fortran libraries in order to achieve the best numerical performance possible. Along with basic vector and matrix arithmetic it contains different algorithms including norm computations, elementary transformations, solving of linear systems of kind Ax=b and AX=B, singular value decomposition, matrix rank and determinant computation, non-symmetric and symmetric eigenvalue problem (including Cholesky and Bunch-Kaufman factorization), LU factorization, QR, RQ, LQ and QL factorizations, different linear least square problems solutions, square matrix polynomials, square matrix inversion, pseudo (generalized) inversion and square matrix exponent. All these algorithms are implemented for real and complex numbers. Functional classes, vectors and matrices are later additions to the library (since ver. 7.0).
The library is distributed under the Boost Software License, Version 1.0.
See http://cvmlib.com for binaries and more details.

### Building source code
Intel toolkit is recommended (see [here](https://www.intel.com/content/www/us/en/develop/documentation/installation-guide-for-intel-oneapi-toolkits-linux/top/installation/install-using-package-managers/apt.html)):
```
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list
sudo apt update
sudo apt install intel-basekit
source /opt/intel/oneapi/compiler/latest/env/vars.sh
mkdir build
cmake -DCMAKE_Fortran_COMPILER=ifx -DMKL=ON -DILP64=ON -DBUILD_TESTS=ON -DCMAKE_BUILD_TYPE=Release -S . -B build
cd build
make -j
../lib/cvm_test
```

### Examples

#### [Sine integral](https://en.wikipedia.org/wiki/Trigonometric_integral) derivative:
```
    cvm::rfunction f("{x} sinint(x)");
    cvm::rfunction fd = f.drv();
    std::cout << fd << std::endl;
    std::cout << fd(1.) << std::endl;
```
prints:
```
{x} sin(x)/x
0.841471
```

#### [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant) and its value:
```
  std::vector<std::string> sa;
  sa.emplace_back("{x, y} log(x)");
  sa.emplace_back("{x, y} log(y)");
  sa.emplace_back("{x, y} log(x)+y");
  cvm::rfvector fv(sa);
  cvm::rfmatrix fm = fv.jacobian();
  std::cout << fm << std::endl;
  std::cout << fm(2., 2.) << std::endl;
```
prints
```
{x,y} 1/x {x,y} 0 
{x,y} 0 {x,y} 1/y 
{x,y} 1/x {x,y} 1 

0.5 0 
0 0.5 
0.5 1
```

#### [Polynomial regression](https://en.wikipedia.org/wiki/Polynomial_regression):
```
#include <cvm.h>

cvm::rvector poly_regression (int size, const double *px, const double *py, int deg)
{
  cvm::rmatrix X(size, deg + 1);
  for (int i = 0; i < size; ++i) {
    double x = 1.;
    for (int j = 0;;++j) {
      X(i,j) = x;
      if (j == deg) break;
      x *= px[i];
    }
  }
  cvm::rmatrix Xt(~X);
  cvm::srsmatrix XtX(Xt * X);
  cvm::rvector av(py, size);
  return (XtX.inv() * Xt) * av;
}

int main (int argc, char* argv[]) {
  cvm::rvector x = {3., 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4.};
  cvm::rvector y = {74.6297, 39.317, -105.173, -26.6447, -155.447, -136.955, -151.887, -113.365, -102.947, -50.3158, -40.1129};
  cvm::rvector p = poly_regression (x.size(), x, y, 2);
  std::cout << p;
}
```
prints:
```
8258.98 -4697.31 656.817 
```
