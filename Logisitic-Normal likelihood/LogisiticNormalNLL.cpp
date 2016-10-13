#include <vector>
#include <string>
#include <iostream>
#include <cmath>
#include <map>
// Tapping into Casal2's boost library
//#include <C:\Craig\Models\CASAL2\ThirdParty\boost\boost_1_58_0\boost\numeric\ublas\matrix.hpp>
//#include <C:\Craig\Models\CASAL2\ThirdParty\boost\boost_1_58_0\boost\numeric\ublas\lu.hpp>
//#include <C:\Craig\Models\CASAL2\ThirdParty\boost\boost_1_58_0\boost\numeric\ublas\io.hpp>

using namespace std;
//using namespace ublas = boost::numeric::ublas;

// @param sigmaeither a single number or a vector of the same length as binnam, defining the s.d. of X
// @param phi a 1- or 2-vector defining the correlation structure of X
// @param sepbysex sexlag - options for sexed compositions (see function NLLlogistnorm for details)
// @param binnam vector of strings that contain the bin labels
// @param ARMA if T and if phi is of length 2 than X is assumed to have the correlation matrix of an ARMA(1,1) process, where phi[1] and phi[2] are the AR and MA parameters, respectively
vector<vector<double>> covmat_logistnorm(double sigma, double phi,vector<string> binnam, bool sepbysex, bool sexlag, bool ARMA);  // Forward declaration


// @param AR_coef numeric vector of AR coefficients
// @param MA_coef numeric vector of MA coefficients
// @param lag_max Maximum lag required. Defaults to max(p, q+1), where p, q are the numbers of AR and MA terms respectively.
// @param pacf Should the partial autocorrelations be returned?
vector<double> ARMAacf(double AR, double MA, int lag_max, bool pacf);
vector<double> ARMAacf(double AR, int lag_max, bool pacf);

// TODO add parameter definitions
vector<double>  ARMAtoMA(double ar, double ma, int lag_max);

// iterate over a numerical vector adding up all the elements
double Sum(vector<double> x);

// solve for the inverse of n x n matrix
vector<vector<double>> Inverse(vector<vector<double>> matrix);


// Begin main loop
int main() {
	// Debug example values
	double Sigma = 0.2;
	double phi = 0.0;
	vector<string> Binnam = {"X1", "X2", "X3", "X4", "X5"};
	bool sepbysex = true;
	bool sexlag = true;
	bool ARMA = true;
	

	vector<vector<double>> covmat = covmat_logistnorm(Sigma, phi,Binnam,sepbysex, sexlag, ARMA);
	// Check that it produces a matrix
	for (unsigned k = 0; k < covmat.size(); ++k) {
		for (unsigned j = 0; j < covmat[k].size(); ++j)
			cout << covmat[k][j] << " ";
		cout << endl;		
	}	
	
	// Test the ARMAacf function
	vector<double> AR = ARMAacf(0.3, 0.5,14,false);
	for (unsigned k = 0; k < AR.size(); ++k) {
			cout << AR[k] << " " << endl;		
	}
	// Test the ARMAtoMA function	
	vector<double> AR_m = ARMAtoMA(0.3,0.5,1);
	for (unsigned k = 0; k < AR_m.size(); ++k) {
			cout << AR_m[k] << " " << endl;		
	}	
	// Test the inverse function
    vector<vector<double>> x = {{2,1,5,3}, {5,4,5,7}, {1,3,8,3}, {7,7,3,9}};
    vector<vector<double>> inv;
     for(unsigned i = 0; i < x.size();++i) {
    	for(unsigned k = 0; k < x[i].size();++k) {
    		cout << x[i][k] << " ";
    	}
    	cout << endl;
	}   
    inv = Inverse(x);
    for(unsigned i = 0; i < inv.size();++i) {
    	for(unsigned k = 0; k < inv[i].size();++k) {
    		cout << inv[i][k] << " ";
    	}
    	cout << endl;
	}
	// End function
	return 0;
}


// Axuillary fucntion definitions
vector<vector<double>> covmat_logistnorm(double sigma, double phi,vector<string> binnam, bool sepbysex, bool sexlag, bool ARMA) {
	unsigned N = binnam.size();
	vector<vector<double>> covar;
	covar.resize(N, vector<double>(N,1.0));
	return covar;
}

// Compute the theoretical autocorrelation function or partial autocorrelation function for an ARMA process.
// The methods used follow Brockwell & Davis (1991, section 3.3). Their equations (3.3.8) are solved for the autocovariances at lags 0, �, max(p, q+1), and the remaining autocorrelations are given by a recursive filter.

vector<double> ARMAacf(double AR, double MA, int lag_max, bool pacf) {
	cout << "Beginning method ARMAacf() " << endl;	

	// Create and declare all variables that will be used in the function
    unsigned p = 1;   
    unsigned q = 1;
    vector<double> AR_coef, MA_coef;
    AR_coef.push_back(AR);
    MA_coef.push_back(MA);

	vector<vector<double>> A, ind;
	vector<double> rhs, psi, theta, Acf;
	if (!p && !q) 
	    cerr << "empty model supplied" << endl;
	unsigned r = fmax(p, q + 1);

	if (p > 0) {
		if (r > 1) {
			if (r > p) {
				for (unsigned i = 0; i <= (r - p); ++i){
					AR_coef.push_back(0.0);
					p = r;
				}
			}
			A.resize(p + 1, vector<double>(2 * p + 1,0.0));
			ind.resize(2 * p + 1, vector<double>(p + 1,0.0));
			for (unsigned i = 0; i < ind.size(); ++i) {
				for (unsigned j = 0; j < ind[i].size(); ++j) {
					ind[i][0] = i + 1;
					ind[i][1] = (i + 1) + (j + 1) - (i + 1);
				}
			}
			for (unsigned i = 0; i < A.size(); ++i) {
				A[i][i]	= 1.0;
				A[i][i + 1] = -AR_coef[0];
			}	
					
			A[A.size() - 1][1] = -AR_coef[0];

			rhs.assign(p + 1,0.0);
			rhs[0] = 1.0;

			cout << "size of rhs " << rhs.size() << endl;
			if (q > 0) {
				// This is extra code 
				psi = ARMAtoMA(AR_coef[0], MA_coef[0], q);
				theta.assign(q + 3, 0.0);
				theta[0] = 1.0;
				theta[1] = MA_coef[0];		 
                for (unsigned k = 0; k <= q; ++k) {
                    rhs[k] = Sum({psi[0] * theta[k],psi[1] * theta[k + 1]});	
                    cout << "rhs = " << rhs[k] << endl;
				} 
			}
			vector<double> Ind;
			for (unsigned i = 0; i <= p + 1; ++i){
				Ind.push_back(p + 1 - i);
			}	
			// Do a matrix inversion to calculate Acf
			Acf = {0.5935484, 0.1780645};				
			} else {
				Acf.push_back(AR_coef[0]);
			}
			if (lag_max > p) {
            	vector<double> xx(lag_max - p, 0.0);
            	
            	//Acf = c(Acf, filter(xx, ar, "recursive", init = rev(Acf)))
        }
	}		
	
	cout << "leaving method ARMAacf() " << endl;	
	//vector<double> temp;
	return A[0];
	
}





vector<double> ARMAtoMA(double ar, double ma, int lag_max) {
    cout << "Beginning method ARMAtoMA() " << endl;	
    
    int i, j, p = 1, q = 1, m = lag_max;
    double phi = ar, theta = ma;
    double tmp;
    vector<double> res,psi;

    if(m <= 0 || m != m) // check for NaN
    	cerr << "invalid value of lag.max" << endl;
    res.assign(m, m);
    psi = res;
    for(i = 0; i < m; i++) {
    	tmp = (i < q) ? ma : 0.0;
    for(j = 0; j < fmin(i+1, p); j++)
        cout << "val = " << i - j - 1 << endl;
		cout << psi[i - j -1] << endl;	

        tmp += ar * ((i - j - 1 >= 0) ? psi[i-j-1] : 1.0);
    psi[i] = tmp;
    }
    // UNPROTECT(1);
    psi.insert(psi.begin(),1.0);
    cout << "leaving method ARMAtoMA() " << endl;	

    return psi;
}

// Utilty funciton for summing
double Sum(vector<double> x) {
    cout << "Beginning method ARMAtoMA() " << endl;		
	double Total;
    for (unsigned i = 0; i< x.size(); ++i) {
	   Total += x[i];
    }	
    cout << "leaving method ARMAtoMA() " << endl;	
    return Total;
}


vector<vector<double>> Inverse(vector<vector<double>> matrix){
    float ratio,a;
    int i, j, k, n, n1;   
    vector<vector<double>> inv_matrix = matrix;
    n = inv_matrix.size();
    
    for(i = 0; i < n; i++){
        for(j = n; j < 2*n; j++){
            if(i == (j - n))
                matrix[i][j] = 1.0;
            else
                matrix[i][j] = 0.0;
        }
    }
    for(i = 0; i < n; i++){
        for(j = 0; j < n; j++){
            if(i!=j){
                ratio = matrix[j][i]/matrix[i][i];
                for(k = 0; k < 2*n; k++){
                    matrix[j][k] -= ratio * matrix[i][k];
                }
            }
        }
    }
    for(i = 0; i < n; i++){
        a = matrix[i][i];
        for(j = 0; j < 2*n; j++){
            matrix[i][j] /= a;
        }
    }
    return matrix;
}

