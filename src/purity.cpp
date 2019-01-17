#include <Rcpp.h>
using namespace Rcpp;

double binPurity(NumericVector mz, NumericVector intensity, double dp = 2){
	
	double purity;
	
	intensity = ceiling(intensity);
	
	List mzs;
	
	if (sum(intensity > 1)){
		for (int i = 0; i < mz.size() ;++i){
			mzs[i] = rep(mz[i],intensity[i]);
		}
		
		NumericVector mzvec;
		
		for (int i = 0; i < mzs.size() ;++i){
			mzvec = wrap(mzvec + mzs[i]);
		}
		
		double width = pow(10,-dp);
		
		purity = 1 - (sd(mzvec) / width);
		
	} else {
		purity = 1;
	}
	return(purity);
}