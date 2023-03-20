
#include "../src/cvm.h"

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

//    cvm::rfunction f("{x} sinint(x)");
//    cvm::rfunction fd = f.drv();
//    std::cout << fd << std::endl;
//    std::cout << fd(1.) << std::endl;
//
//
//
//  std::vector<std::string> sa;
//  sa.emplace_back("{x, y} log(x)");
//  sa.emplace_back("{x, y} log(y)");
//  sa.emplace_back("{x, y} log(x)+y");
//  cvm::rfvector fv(sa);
//  cvm::rfmatrix fm = fv.jacobian();
//  std::cout << fm << std::endl;
//  std::cout << fm(2., 2.) << std::endl;
}
