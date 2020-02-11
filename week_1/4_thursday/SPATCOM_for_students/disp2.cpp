#include <Rcpp.h>
using namespace Rcpp;

// Export dispersal function build in C++ to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

NumericVector destinyC(double x,double y,NumericVector angle,NumericVector distance, int gridside) {
  int n = angle.size();
  static const double pi = 3.14159265;
  NumericVector result(n);
  for (int i = 0; i < n; i++) {
    double xi = x + distance[i] * sin(angle[i]*(pi/180));
    double yi = y + distance[i] * cos(angle[i]*(pi/180));
    result(i) = (round (xi) + round (yi) * gridside)+1;
  }
  return result;
}

// [[Rcpp::export]]
NumericVector disp(String kerntype, int z, double peak, double sd, int gridside, NumericMatrix pos) {
  int m = pos.nrow();
  NumericVector posd(m*z);
  for (int i = 0; i < m; i++) {
    NumericVector mdisp(z);
    if(kerntype=="lnorm") {
      NumericVector dispz = Rcpp::rlnorm(z, peak, sd);
      NumericVector dispzz = dispz[dispz>0.5 & dispz<=sqrt(2*gridside*gridside)];
      int dsize = dispzz.size();
      if(dsize > 0)
        mdisp[Range(0,dsize-1)] = dispzz;
      while(dsize < z){
        NumericVector dispzn = Rcpp::rlnorm(1, peak, sd);
        double dispzzn = dispzn[0];
        if(dispzzn>0.5 && dispzzn<=sqrt(2*gridside*gridside)){
          mdisp[dsize] = dispzzn;
          dsize = dsize+1;
        }
      }
    }
    if(kerntype=="exp") {
      NumericVector dispz = Rcpp::rexp(z, sd);
      NumericVector dispzz = dispz[dispz>0.5 & dispz<=sqrt(2*gridside*gridside)];
      int dsize = dispzz.size();
      if(dsize > 0)
        mdisp[Range(0,dsize-1)] = dispzz;
      while(dsize < z){
        NumericVector dispzn = Rcpp::rexp(1, sd);
        double dispzzn = dispzn[0];
        if(dispzzn>0.5 && dispzzn<=sqrt(2*gridside*gridside)){
          mdisp[dsize] = dispzzn;
          dsize = dsize+1;
        }
      }
    }
    if(kerntype=="unif") 
      mdisp = Rcpp::runif(z, 1, sqrt(2*gridside*gridside));
    NumericVector mdir = Rcpp::runif(z,0,359.999);
    NumericVector destm = destinyC(pos(i,0),pos(i,1), mdir, mdisp, gridside);
    for (int j = 0; j < z; j++) {
      posd(i*z+j) = destm(j);
    }
  }
  return posd;
}
