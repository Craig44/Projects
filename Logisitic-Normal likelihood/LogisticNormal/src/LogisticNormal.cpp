#include <vector>
#include <string>
#include <iostream>
#include <cmath>
#include <map>
#include <stdlib.h>
#include <fstream>

// Tapping into Casal2's boost library
#include <boost\numeric\ublas\matrix.hpp>
#include <boost\numeric\ublas\lu.hpp>
#include <boost\numeric\ublas\io.hpp>
#include <boost\numeric\ublas\storage.hpp>
#include <Eigen/Dense>

using namespace std;
//using namespace ublas = boost::numeric::ublas;

// Evaluate the likelihood contribution for the logistic normal, with the following parameters.
/*
 *@param obs Nbin-vector, or a Nyear x Nbin matrix of Nyear observed Nbin-vectors of proportions
 *@param N a Nyear vector of effective sample sizes
 *@param sigma an estimable standard deviation parameter
 *@param phi  auto correlation parameter
 *@param covmat a user defined covariance matrix with the same dimensions as obs
 *@param sepbysex ignore the rest of the bool switches they are for dealing with sexed populations
 *
 *@return a negative log likelihood score for D the data given parameters, sigma and phi
 */
double NLLlogistnorm(vector<vector<double>> obs,vector<double> N, double sigma, vector<double> phi ,bool sepbysex,bool sexlag, bool robust,bool ARMA);

// Generate a set of simulated observations that follow a logistic-normal distribution that
vector<double> rlogistnorm(vector<double> expprop, double sigma, vector<double> phi, vector<vector<double>> covmat, bool ARMA);


// @param sigmaeither a single number or a vector of the same length as binnam, defining the s.d. of X
// @param phi a 1- or 2-vector defining the correlation structure of X
// @param sepbysex sexlag - options for sexed compositions (see function NLLlogistnorm for details)
// @param binnam vector of strings that contain the bin labels
// @param ARMA if T and if phi is of length 2 than X is assumed to have the correlation matrix of an ARMA(1,1) process, where phi[1] and phi[2] are the AR and MA parameters, respectively
vector<vector<double> > covmat_logistnorm(double sigma, vector<double> phi,vector<string> binnam, bool sepbysex, bool sexlag, bool ARMA);  // Forward declaration


// @param AR_coef numeric vector of AR coefficients
// @param MA_coef numeric vector of MA coefficients
// @param lag_max Maximum lag required. Defaults to max(p, q+1), where p, q are the numbers of AR and MA terms respectively.
// @param pacf Should the partial autocorrelations be returned?
// Thwo version of this funciton, one with a two AR and another with a AR and MA coeffecient
// For information on this algorithm, check the ARMAacf() function in R that was used as the basis of testing the algorithm.
vector<double> ARMAacf(double AR, double MA, int nBin);
vector<double> ARMAacf(vector<double> AR, int nBin);
vector<double> GetRho(double& Phi1, int nBin);
vector<double>  RecursiveFilter(vector<double> ar_coef, int nBins, vector<double> initial_vals);

// Forward declarations for utility funcitons
double Sum(vector<double> x);
vector<double> elem_prod(vector<double> x,vector<double> y);
bool InvertMatrix(const boost::numeric::ublas::matrix<double>& input, boost::numeric::ublas::matrix<double>& inverse);

////////////////////
// Begin main function
////////////////////
int main() {
	// Debug example values
	double Sigma = 0.4;
	vector<double> phi = {0.2};
	vector<string> Binnam = {"1", "2", "3", "4", "5","6","7","8", "9", "10", "11", "12","15","16","17","18", "19",};
	vector<vector<double> > obs_mat = {{0.0019, 0.0215, 0.0275, 0.0158, 0.0568, 0.0499, 0.1001, 0.2629, 0.1331, 0.1034, 0.1294, 0.0231, 0.0272, 0.0002, 0.0132, 0.0110, 0.0115, 0.0002, 0.0115},
	    {0.0213, 0.0350, 0.0420, 0.0101, 0.0255, 0.0449, 0.0950, 0.1354, 0.0619, 0.2855, 0.0864, 0.0746, 0.0508, 0.0180, 0.0129, 0.0002, 0.0002, 0.0002, 0.0002},
	    {0.0104, 0.0435, 0.0641, 0.0625, 0.0576, 0.0398, 0.0943, 0.0586, 0.0479, 0.0858, 0.1897, 0.0945, 0.0736, 0.0319, 0.0157, 0.0002, 0.0063, 0.0064, 0.0171},
	    {0.0310, 0.1306, 0.1041, 0.0186, 0.0106, 0.0459, 0.0345, 0.0644, 0.0518, 0.0281, 0.0358, 0.1525, 0.0992, 0.0670, 0.0502, 0.0397, 0.0255, 0.0052, 0.0052},
	    {0.0785, 0.1866, 0.1408, 0.1011, 0.0845, 0.0440, 0.0274, 0.0741, 0.0488, 0.0475, 0.0238, 0.0115, 0.0170, 0.0248, 0.0034, 0.0130, 0.0099, 0.0015, 0.0617},
	    {0.0451, 0.2763, 0.1077, 0.0613, 0.0739, 0.0454, 0.0698, 0.0293, 0.0488, 0.0517, 0.0454, 0.0305, 0.0260, 0.0183, 0.0040, 0.0221, 0.0137, 0.0002, 0.0304},
	    {0.0172, 0.2318, 0.1036, 0.0976, 0.0880, 0.0648, 0.0678, 0.0740, 0.0429, 0.0434, 0.0316, 0.0309, 0.0321, 0.0072, 0.0173, 0.0036, 0.0055, 0.0165, 0.0242},
	    {0.0319, 0.1448, 0.1475, 0.1015, 0.1384, 0.1486, 0.0737, 0.0343, 0.0426, 0.0312, 0.0414, 0.0153, 0.0060, 0.0107, 0.0002, 0.0002, 0.0093, 0.0080, 0.0147},
	    {0.0083, 0.0885, 0.0926, 0.0979, 0.1544, 0.1528, 0.0725, 0.1093, 0.0293, 0.0224, 0.0347, 0.0257, 0.0281, 0.0161, 0.0149, 0.0086, 0.0078, 0.0002, 0.0360},
	    {0.0263, 0.1047, 0.1482, 0.1406, 0.0966, 0.1083, 0.0926, 0.0896, 0.0230, 0.0781, 0.0389, 0.0104, 0.0159, 0.0047, 0.0020, 0.0023, 0.0010, 0.0010, 0.0157},
	    {0.1060, 0.1153, 0.0800, 0.0675, 0.0758, 0.1056, 0.0984, 0.1125, 0.0508, 0.0681, 0.0557, 0.0131, 0.0246, 0.0002, 0.0091, 0.0096, 0.0059, 0.0002, 0.0016},
	    {0.0993, 0.1142, 0.1035, 0.0734, 0.0911, 0.0912, 0.0900, 0.0774, 0.0545, 0.0644, 0.0352, 0.0429, 0.0205, 0.0126, 0.0160, 0.0026, 0.0059, 0.0002, 0.0050},
	    {0.1408, 0.2712, 0.1572, 0.0636, 0.0537, 0.0767, 0.0348, 0.0312, 0.0243, 0.0243, 0.0162, 0.0367, 0.0127, 0.0085, 0.0157, 0.0091, 0.0002, 0.0013, 0.0220},
	    {0.2373, 0.3437, 0.1528, 0.0439, 0.0199, 0.0287, 0.0344, 0.0199, 0.0266, 0.0180, 0.0111, 0.0216, 0.0077, 0.0234, 0.0002, 0.0035, 0.0002, 0.0002, 0.0070},
	    {0.0139, 0.1752, 0.1619, 0.1173, 0.0827, 0.0573, 0.0735, 0.0820, 0.0387, 0.0323, 0.0351, 0.0228, 0.0177, 0.0141, 0.0263, 0.0188, 0.0045, 0.0049, 0.0208},
	    {0.0347, 0.2180, 0.1783, 0.0999, 0.0856, 0.0776, 0.0659, 0.0656, 0.0432, 0.0180, 0.0177, 0.0203, 0.0173, 0.0053, 0.0043, 0.0179, 0.0095, 0.0129, 0.0081}};
	vector<double> N = {19, 21, 30, 36, 58, 46, 52, 38, 30, 40, 51, 49, 59, 45, 49, 60};
	bool sepbysex = false;
	bool sexlag = false;
	bool ARMA = false;
	bool robust = false;
	double score = NLLlogistnorm(obs_mat,N,Sigma, phi,sepbysex, sexlag,  robust, ARMA);



	covmat = covmat_logistnorm(Sigma, phi,Binnam,sepbysex, sexlag, ARMA);
	// Check that it produces a matrix
	cout << "printing covar = " << endl;
	for (unsigned k = 0; k < covmat.size(); ++k) {
		for (unsigned j = 0; j < covmat[k].size(); ++j)
			cout << covmat[k][j] << " ";
		cout << endl;
	}

	// Test the ARMAacf function with two ar coefs
	vector<double> AR;// = ARMAacf({0.3, 0.5},14);
	// Test the ARMAacf function with a ar and ma coefs
	AR = ARMAacf(0.3,0.5,14);

	for (unsigned k = 0; k < AR.size(); ++k) {
			cout << AR[k] << " " << endl;
	}
	// Test the ARMAtoMA function
	//vector<double> AR_m = ARMAtoMA(0.3,0.5,1);
	//for (unsigned k = 0; k < AR_m.size(); ++k) {
	//		cout << AR_m[k] << " " << endl;
	//}
	// Test the inverse function
	vector<double> vals = {2,1,5,3,5,4,5,7,1,3,8,3,7,7,3,9};
	boost::numeric::ublas::matrix<double> x(4,4);
  for(size_t i=0; i<x.size1(); i++) {
     for(size_t j=0; j<x.size2(); j++) {
        x(i,j) = vals[i+j*x.size1()];
     }
  }
  for(unsigned i = 0; i < x.size1();++i) {
    for(unsigned k = 0; k < x.size1();++k) {
      cout << x(i,k) << " ";
    }
    cout << endl;
  }

	//x = boost::numeric::ublas::matrix(4,4,);

     boost::numeric::ublas::matrix<double> inv(4,4);

    bool inverted = InvertMatrix(x, inv);
    if (!inverted) {
      cout << "Failed to invert matrix, exiting app" << endl;
      return 0;
    }
    for(unsigned i = 0; i < inv.size1();++i) {
    	for(unsigned k = 0; k < inv.size1();++k) {
    		cout << inv(i,k) << " ";
    	}
    	cout << endl;
    }
    // Test the recursive filter
    vector<double> ar_coef(2,0.2);
    ar_coef[1] = 0.43;
    vector<double> output;
    //output = RecursiveFilter(ar_coef, 15);
    //for (auto num : output)
      //cout << num << " ";

	// End function
	return 0;
}

// Define the Negative loglikeihood score for a logistic normal distributed comp data.
double NLLlogistnorm(vector<vector<double>> obs,vector<double> N,  double sigma, vector<double> phi ,bool sepbysex,bool sexlag, bool robust,bool ARMA) {
  double score = 0.0;
  // Calculate a scaled N

  return score;
}

vector<double> rlogistnorm(vector<double> expprop, double sigma, vector<double> phi, vector<vector<double>> covmat, bool ARMA) {
  vector<double> sim_data;
  sim_data.push_back(0.0);
  return sim_data;
}


// Axuillary fucntion definitions
vector<vector<double> > covmat_logistnorm(double sigma, vector<double> phi,vector<string> binnam, bool sepbysex, bool sexlag, bool ARMA) {
  unsigned n_phi = phi.size();
  unsigned N = binnam.size();
  vector<vector<double>> covar;
  covar.resize(N, vector<double>(N,0.0));
  if (phi[0] == 0.0) {
    for (unsigned diag = 0; diag < N; ++ diag)
      covar[diag][diag] =sigma * sigma;
  } else {
    vector<double> rho;
    // Get Rho
    cout << "about to enter getRho(), n_phi = " << n_phi << " " << phi[0]<<endl;
    if (n_phi == 1) {
      rho = GetRho(phi[0],N);
    } else if (n_phi == 2 && ARMA) {
      rho = ARMAacf(phi[0],phi[1],N);
    } else {
      rho = ARMAacf(phi,N);
    }

    cout << "Check out rho vector";
    for ( auto num : rho)
      cout << num << " ";
    cout << endl;

    // Simple unsexed example currently
    // Create an identy matrix.
    for (unsigned diag = 0; diag < N; ++ diag) {
      covar[diag][diag] = 1.0 * sigma * sigma;
    }
    for (unsigned diag = 0; diag < N; ++ diag) {
      for (int row = 0; row < N; ++ row) {
        for (int col = 0; col < N; ++ col) {
          if (fabs(row - col) == diag + 1)
            covar[row][col] = rho[diag] * sigma * sigma;
        }
      }
    }
  }
	return covar;
}

// Compute the theoretical autocorrelation function or partial autocorrelation function for an ARMA process.
// The methods used follow Brockwell & Davis (1991, section 3.3). Their equations (3.3.8) are solved for the autocovariances at lags 0, �, max(p, q+1), and the remaining autocorrelations are given by a recursive filter.

vector<double> ARMAacf(double AR, double MA, int nBin) {
  cout << "Beginning method ARMAacf() " << endl;

  // Create and declare all variables that will be used in the function
  unsigned p = 1;
  unsigned q = 1;
  vector<double> AR_coef, MA_coef,final_acf,Cor;
  AR_coef.push_back(AR);
  MA_coef.push_back(MA);

  vector<vector<double> > A, ind;
  vector<double> psi, theta, Acf;
  if (!p && !q)
    cerr << "empty model supplied" << endl;
  unsigned r = fmax(p, q + 1);

  for (unsigned i = 0; i <= (r - p); ++i) {
    AR_coef.push_back(0.0);
    p = r;
  }
  A.resize(p + 1, vector<double>(2 * p + 1, 0.0));
  ind.resize(2 * p + 1, vector<double>(p + 1, 0.0));
  for (unsigned i = 0; i < ind.size(); ++i) {
    for (unsigned j = 0; j < ind[i].size(); ++j) {
      ind[i][0] = i + 1;
      ind[i][1] = (i + 1) + (j + 1) - (i + 1);
    }
  }
  for (unsigned i = 0; i < A.size(); ++i) {
    A[i][i] = 1.0;
    A[i][i + 1] = -AR_coef[0];

  }
  A[0][A.size() - 1] = -AR_coef[1];
  A[A.size() - 1][0] = -AR_coef[1];
  A[A.size() - 1][1] = -AR_coef[0];


  //cout << "size of rhs " << rhs.size() << endl;
  psi.push_back(1.0);
  psi.push_back(AR + MA);
  theta.push_back(1.0);
  theta.push_back(MA);
  for (int i = 0; i <= q; ++i)
    theta.push_back(0.0);

  // Declare Eigen variables
  Eigen::Matrix3d A_eig; // 3f means 3x3 elements of type double
  Eigen::Vector3d rhs;
  // Calculate rhs
  for (unsigned i = 0; i <= q; ++i) {
    double x1 ,x2,tot = 0;
    x1 = psi[0]*theta[i];
    x2 = psi[1]*theta[i + q];
    rhs(i) = Sum({x1,x2});
  }
  rhs(2) = 0.0;

  // Use the eigen library yo solve the inverse of for A with known vector B
  //vector<double> Ind;
  vector<unsigned> seq;
  for (unsigned i = 0; i <= p; ++i) {
    seq.push_back(p - i);
  }


  for (unsigned i = 0; i <= p; ++i) {
    for (unsigned j = 0; j <= p; ++j) {
      cout << ": i = " << i << " j = " << j << " i index = " << seq[i] << " j index = " << seq[j] << " mat value = " << A[seq[i]][seq[j]] << endl;
      A_eig(i,j) = A[seq[i]][seq[j]];
    }
  }

  cout << "Check A mat that we are inverting\n" << A_eig << "\n: rhs = " << rhs << endl;

  // Solve for A_eig given rhs
  Eigen::Vector3d x = A_eig.colPivHouseholderQr().solve(rhs);
  cout << "solution = " << x;

  // Divide the last two elements by the first element.
  cout << "Find the crash" << endl;

  for (unsigned i = 1; i <= 2; ++i) {
    final_acf.push_back(x(i) / x(0));
  }

  cout << "Final Acf" << endl;
  for (auto num : final_acf)
    cout << num << " ";
  cout << endl;

  Cor = RecursiveFilter(AR_coef, nBin, final_acf);

  // Add the initial coeffiecients to the beginning of the series.
  Cor.insert(Cor.begin(), final_acf[1]);
  Cor.insert(Cor.begin(), final_acf[0]);
  // Print results to screen
  vector<double>::const_iterator first = Cor.begin();
  vector<double>::const_iterator last = Cor.begin() + nBin;
  vector<double> newVec(first, last);
  for (auto num : newVec)
    cout << num << " ";
  cout << endl;

  return newVec;

}

vector<double> ARMAacf(vector<double> AR, int nBin) {
  cout << "Beginning method ARMAacf() " << endl;
  // Create and declare all variables that will be used in the function
  unsigned p = AR.size();
  if (p > 2) {
    cerr << "This function has not been coded for more the two AR coeffs." << endl;
  }
  vector<vector<double> > A;
  vector<double> rhs, psi, theta, Acf;
  unsigned r = p;
  A.resize(p + 1, vector<double>(2 * p + 1, 0.0));

  for (unsigned i = 0; i < A.size(); ++i) {
    A[i][i] = 1.0;
    A[i][i + 1] = -AR[0];
    A[i][i + 2] = -AR[1];
  }

  rhs.assign(p + 1, 0.0);
  rhs[0] = 1.0;


  cout << "size of rhs " << rhs.size() << endl;
  //vector<double> Ind;
  vector<unsigned> seq;
  for (unsigned i = 0; i <= p; ++i) {
    seq.push_back(p - i);
  }
  // Create a ublas matrix object for the inversion
  boost::numeric::ublas::matrix<double> A_inv(3,3);
  boost::numeric::ublas::matrix<double> A_ublas(3,3);

  for (unsigned i = 0; i <= p; ++i) {
    for (unsigned j = 0; j <= p ; ++j) {
      if (j == 2)
        A_ublas(i,j) = A[i][j];
      else
        A_ublas(i,j) = A[i][j] + A[i][2 * p  - j];
    }
  }

  for (unsigned i = 0; i <= p; ++i) {
    for (unsigned j = 0; j <= p ; ++j) {
        A_ublas(i,j) =  A_ublas(seq[i],seq[j]);
    }
  }
  // the bodge
  A_ublas(1,2) = 0.0;

  cout << "Check A mat that we are inverti"
      "ng" << endl;
  for(unsigned i = 0; i < A_ublas.size1();++i) {
    for(unsigned k = 0; k < A_ublas.size1();++k) {
      cout << A_ublas(i,k) << " ";
    }
    cout << endl;
  }
  bool inverted = InvertMatrix(A_ublas,A_inv);
  if (!inverted) {
    cerr << "could not invert the matrix" << endl;
  }
  cout << "Check inverted matrix" << endl;
  for(unsigned i = 0; i < A_inv.size1();++i) {
    for(unsigned k = 0; k < A_inv.size1();++k) {
      cout << A_inv(i,k) << " ";
    }
    cout << endl;
  }

  // Take the first column of the inverted matrix
  vector<double> final_acf,xx,Cor, final_Cor;
  for (unsigned i = 0; i < p + 1; ++i) {
    Acf.push_back(A_inv(i,0));
  }

  // Divide the last two elements by the first element.
  for (unsigned i = 1; i <= 2; ++i) {
    final_acf.push_back(Acf[i] / Acf[0]);
  }
  cout << "Final Acf" << endl;
  for (auto num : final_acf)
    cout << num << " ";
  cout << endl;

  // Call the recurisive filter
  xx.assign(nBin - p, 0.0);
  Cor = RecursiveFilter(AR,nBin, final_acf);
  // Add the initial coeffiecients to the beginning of the series.
  Cor.insert(Cor.begin(),final_acf[1]);
  Cor.insert(Cor.begin(),final_acf[0]);
  // Print results to screen
  vector<double>::const_iterator first = Cor.begin();
  vector<double>::const_iterator last = Cor.begin() + nBin;
  vector<double> newVec(first, last);
  for (auto num : newVec)
    cout << num << " ";
  cout << endl;
  return newVec;
}

/**
 * @param ar_coef an AR(2) coefficient
 * @param nBin number of ages
 * @param initial_vals initial vals
 */
vector<double>  RecursiveFilter(vector<double> ar_coef, int nBins, vector<double> initial_vals) {
  vector<double> store_vec(nBins + 1,0.0);
  if (ar_coef.size() > 2) {
    cerr <<  "RecursiveFilter(): has not been coded for more than 2 AR coeffiecients" << endl;
  }

  store_vec[0] = initial_vals[1];
  store_vec[1] = initial_vals[0];
  for (unsigned i = 1; i < nBins + 1; ++i) {
    if (i == 1) {
      store_vec[i] =   store_vec[i - 1] *ar_coef[0]  + store_vec[i] *  ar_coef[1];
    } else {
      store_vec[i] = store_vec[i - 1] *  ar_coef[0] + store_vec[i - 2] * ar_coef[1];
    }
    cout << "value = " << store_vec[i];
  }
  // remove the first value
  store_vec.erase(store_vec.begin());
  return store_vec;
}

/**
 * This method is called at in the CalculateCovarianceLogisiticNormal() method to calculate the auto-regressive vector Rho
 * @param Phi1 an AR(1) coefficient
 * @param nBin number of ages
 */
vector<double> GetRho(double& Phi1, int nBin) {
  cout <<  "entering GetRho()" << endl;

  //calculation of AR(1) acf for  LN2
  vector<double> rho(nBin - 1, 0.0);

  for(int i = 1; i <= nBin - 1; i++) {
    rho[i - 1]= pow(Phi1,(double)i);
    cout << rho[i - 1] << " ";
  }
  cout <<  "\n\n\n";

  return rho;
}


//////////////////////
// Utility Functions
//////////////////////

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

vector<double> elem_prod(vector<double> x,vector<double> y) {
  vector<double> result;
  if (x.size() != y.size()) {
    cerr << "cannot apply this function with uneven sized vectors" << endl;
  }
  for (int i = 0; i < x.size(); ++i) {
    result.push_back(x[i] * y[i]);
  }
  return result;
}


// solve for the inverse of n x n matrix
bool InvertMatrix(const boost::numeric::ublas::matrix<double>& input, boost::numeric::ublas::matrix<double>& inverse) {
  typedef boost::numeric::ublas::permutation_matrix<std::size_t> pmatrix;
  // create a working copy of the input
  boost::numeric::ublas::matrix<double> A(input);

  // create a permutation matrix for the LU-factorization
  pmatrix pm(A.size1());

  // perform LU-factorization
  int res = boost::numeric::ublas::lu_factorize(A, pm);
  if (res != 0)
    return false;

  // create identity matrix of "inverse"
  inverse.assign(boost::numeric::ublas::identity_matrix<double> (A.size1()));

  // backsubstitute to get the inverse
  boost::numeric::ublas::lu_substitute(A, pm, inverse);

  return true;
}

