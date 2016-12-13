#include <vector>
#include <string>
#include <iostream>
#include <cmath>
#include <map>
#include <stdlib.h>
#include <fstream>
#include <time.h>


// Tapping into Casal2's boost library
#include <boost\numeric\ublas\matrix.hpp>
#include <boost\numeric\ublas\lu.hpp>
#include <boost\numeric\ublas\io.hpp>
#include <boost\numeric\ublas\storage.hpp>
#include <boost\random.hpp>
#include <boost\random\normal_distribution.hpp>
#include <Eigen/Dense>

#define PI 3.14159265359
using namespace std;

struct Comparison {
  unsigned  age_ = 0;
  double    expected_ = 0;
  double    observed_ = 0;
};

//using namespace ublas = boost::numeric::ublas;

// Evaluate the likelihood contribution for the logistic normal, with the following parameters.
/*
 *@param obs Nbin-vector, or a Nyear x Nbin matrix of Nyear observed Nbin-vectors of proportions
 *@param exp Nbin-vector, or a Nyear x Nbin matrix of Nyear expected Nbin-vectors of proportions
 *@param N a Nyear vector of effective sample sizes
 *@param sigma an estimable standard deviation parameter
 *@param phi  auto correlation parameter
 *@param covmat a user defined covariance matrix with the same dimensions as obs
 *@param sepbysex ignore the rest of the bool switches they are for dealing with sexed populations
 *
 *@return a negative log likelihood score for exp given observations obs and parameters parameters, sigma and phi
 */
double NLLlogistnorm(vector<vector<double>> obs,vector<vector<double>> Exp ,vector<double> N, double sigma, vector<double> phi ,bool sepbysex,bool sexlag, bool robust,bool ARMA,bool sexed);

// Generate a set of simulated observations that follow a logistic-normal distribution that
void rlogistnorm(map<unsigned,vector<Comparison>>& comparisons, unsigned N, double sigma, vector<double> phi, vector<vector<double>> covmat, bool ARMA, bool sexed);


// @param sigmaeither a single number or a vector of the same length as binnam, defining the s.d. of X
// @param phi a 1- or 2-vector defining the correlation structure of X
// @param sepbysex sexlag - options for sexed compositions (see function NLLlogistnorm for details)
// @param binnam vector of strings that contain the bin labels
// @param ARMA if T and if phi is of length 2 than X is assumed to have the correlation matrix of an ARMA(1,1) process, where phi[1] and phi[2] are the AR and MA parameters, respectively
vector<vector<double> > covmat_logistnorm(double sigma, vector<double> phi,unsigned N_ages , bool sepbysex, bool sexlag, bool ARMA, bool sexed);  // Forward declaration


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
double Mean(vector<double> x);
bool all_ones(vector<double> x);
vector<vector<double> > mat_multiply(vector<vector<double> >& x,vector<vector<double> >& y);
vector<vector<double> > mat_multiply(vector<double> & x,vector<vector<double> >& y);
vector<vector<double> > mat_multiply(vector<vector<double> > & x,vector<double>& y);
// given a vector index return a row and col index for a matrix
vector<int> get_mat_index(int rows, int cols, double index);

// transpose matrix x
vector<vector<double> > t(vector<vector<double> >& x);
vector<vector<double>> log_mat(vector<vector<double> >& x);
double Sum_mat(vector<vector<double> >& x);


vector<double> elem_prod(vector<double> x,vector<double> y);
bool InvertMatrix(const boost::numeric::ublas::matrix<double>& input, boost::numeric::ublas::matrix<double>& inverse);
double det_fast(const boost::numeric::ublas::matrix<double>& matrix);
////////////////////
// Begin main function
////////////////////
int main() {
	// Debug example values
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

  vector<vector<double> > exp_mat = {
{0.03014, 0.04666, 0.04960, 0.05579, 0.07213, 0.05041, 0.10041, 0.17720, 0.12866, 0.08687, 0.06243, 0.03883, 0.02502, 0.01627, 0.01287, 0.01139, 0.00950, 0.00740, 0.01840},
{0.04074, 0.07713, 0.06410, 0.04238, 0.04247, 0.05165, 0.06134, 0.05454, 0.09144, 0.13744, 0.11152, 0.07336, 0.05044, 0.03180, 0.01967, 0.01266, 0.00918, 0.00751, 0.02063},
{0.05752, 0.09241, 0.07116, 0.05653, 0.03997, 0.03988, 0.04823, 0.05518, 0.05298, 0.08222, 0.11611, 0.09747, 0.06459, 0.04320, 0.02732, 0.01664, 0.01055, 0.00737, 0.02067},
{0.06083, 0.12746, 0.08396, 0.06245, 0.05016, 0.03722, 0.03653, 0.04322, 0.04814, 0.04879, 0.07072, 0.09453, 0.08128, 0.05458, 0.03561, 0.02246, 0.01351, 0.00840, 0.02015},
{0.07894, 0.15535, 0.12990, 0.11899, 0.09094, 0.07396, 0.06944, 0.05721, 0.04365, 0.03168, 0.02272, 0.01694, 0.01306, 0.01120, 0.01081, 0.01089, 0.01116, 0.01183, 0.04132},
{0.06990, 0.16744, 0.13393, 0.10943, 0.10010, 0.07856, 0.06359, 0.05771, 0.04743, 0.03581, 0.02585, 0.01836, 0.01346, 0.01027, 0.00862, 0.00807, 0.00795, 0.00796, 0.03555},
{0.05178, 0.15175, 0.14689, 0.11589, 0.09694, 0.08782, 0.06960, 0.05570, 0.04910, 0.04015, 0.03008, 0.02155, 0.01515, 0.01093, 0.00822, 0.00674, 0.00612, 0.00587, 0.02971},
{0.05546, 0.11506, 0.13669, 0.12861, 0.10482, 0.08821, 0.07807, 0.06189, 0.04899, 0.04208, 0.03415, 0.02541, 0.01805, 0.01256, 0.00892, 0.00660, 0.00528, 0.00464, 0.02452},
{0.05716, 0.12394, 0.10599, 0.11833, 0.11370, 0.09422, 0.07845, 0.06761, 0.05352, 0.04200, 0.03521, 0.02827, 0.02091, 0.01471, 0.01013, 0.00707, 0.00513, 0.00400, 0.01964},
{0.07900, 0.12371, 0.10817, 0.09195, 0.10159, 0.09845, 0.08183, 0.06718, 0.05666, 0.04476, 0.03481, 0.02850, 0.02260, 0.01660, 0.01157, 0.00787, 0.00540, 0.00384, 0.01550},
{0.10140, 0.16201, 0.10433, 0.08622, 0.07639, 0.08293, 0.07992, 0.06620, 0.05368, 0.04444, 0.03493, 0.02692, 0.02154, 0.01683, 0.01227, 0.00846, 0.00568, 0.00383, 0.01203},
{0.10106, 0.20025, 0.13008, 0.08257, 0.06809, 0.06184, 0.06533, 0.06227, 0.05146, 0.04130, 0.03358, 0.02621, 0.01999, 0.01565, 0.01203, 0.00869, 0.00593, 0.00393, 0.00975},
{0.09751, 0.20093, 0.16020, 0.10266, 0.06709, 0.05474, 0.04976, 0.05085, 0.04788, 0.03948, 0.03138, 0.02507, 0.01939, 0.01463, 0.01121, 0.00847, 0.00606, 0.00409, 0.00861},
{0.08993, 0.19600, 0.16367, 0.12598, 0.08360, 0.05555, 0.04439, 0.03998, 0.03967, 0.03688, 0.03033, 0.02386, 0.01875, 0.01433, 0.01069, 0.00802, 0.00595, 0.00420, 0.00822},
{0.07011, 0.15630, 0.15514, 0.13439, 0.11251, 0.08908, 0.06132, 0.04084, 0.03132, 0.02747, 0.02589, 0.02336, 0.01904, 0.01467, 0.01114, 0.00828, 0.00600, 0.00432, 0.00882},
{0.03833, 0.15238, 0.13923, 0.13306, 0.11866, 0.10056, 0.07909, 0.05446, 0.03615, 0.02724, 0.02345, 0.02156, 0.01914, 0.01549, 0.01181, 0.00881, 0.00645, 0.00460, 0.00951}};
	vector<double> N = {19, 21, 30, 36, 58, 46, 52, 38, 30, 40, 51, 49, 59, 45, 49, 60};
  double Sigma = 0.2;
  vector<double> phi = {0.0};
	bool sepbysex = false;
	bool sexlag = false;
	bool ARMA = false;
	bool robust = false;
	bool sexed = false;
	// Test 1, This follows the Test 1 in the R script test_C_code.r
	double score = NLLlogistnorm(obs_mat,exp_mat,N,Sigma, phi,sepbysex, sexlag,  robust, ARMA, sexed);
  // Test 2
  phi = {0.4};
  double score1 = NLLlogistnorm(obs_mat,exp_mat,N,Sigma, phi,sepbysex, sexlag,  robust, ARMA, sexed);
  // Test 3
  phi = {0.235,-0.284};
  double score2 = NLLlogistnorm(obs_mat,exp_mat,N,Sigma, phi,sepbysex, sexlag,  robust, ARMA, sexed);
  // Test 4
  ARMA = true;
  double score3 = NLLlogistnorm(obs_mat,exp_mat,N,Sigma, phi,sepbysex, sexlag,  robust, ARMA, sexed);
  // Test 5
  robust = true;
  double score4 = NLLlogistnorm(obs_mat,exp_mat,N,Sigma, phi,sepbysex, sexlag,  robust, ARMA, sexed);

	vector<vector<double>> covmat = covmat_logistnorm(Sigma, phi,19,sepbysex, sexlag, ARMA, sexed);

	// Now deal with the sexed versions
	obs_mat =
	  {{0.0238, 0.0205, 0.0334, 0.0492, 0.0433, 0.0338, 0.0501, 0.0508, 0.0504, 0.0576, 0.0254, 0.0200, 0.0322, 0.0145, 0.0154, 0.0016, 0.0032, 0.0084, 0.0196, 0.0175, 0.0334, 0.0387, 0.0447, 0.0402, 0.0292, 0.0277, 0.0225, 0.0271, 0.0251, 0.0222, 0.0324, 0.0189, 0.0159, 0.0114, 0.0062, 0.0120, 0.0022, 0.0195},
	  {0.0401, 0.0373, 0.0319, 0.0473, 0.0672, 0.0479, 0.0228, 0.0188, 0.0466, 0.0483, 0.0224, 0.0301, 0.0194, 0.0074, 0.0057, 0.0019, 0.0033, 0.0032, 0.0128, 0.0458, 0.0295, 0.0356, 0.0255, 0.0365, 0.0445, 0.0435, 0.0250, 0.0065, 0.0170, 0.0263, 0.0342, 0.0182, 0.0258, 0.0168, 0.0076, 0.0125, 0.0039, 0.0309},
	  {0.0108, 0.0807, 0.0680, 0.0437, 0.0296, 0.0352, 0.0396, 0.0234, 0.0227, 0.0221, 0.0274, 0.0261, 0.0253, 0.0219, 0.0278, 0.0176, 0.0082, 0.0047, 0.0147, 0.0124, 0.0802, 0.0351, 0.0330, 0.0119, 0.0199, 0.0280, 0.0259, 0.0138, 0.0259, 0.0230, 0.0319, 0.0113, 0.0217, 0.0169, 0.0176, 0.0105, 0.0070, 0.0243},
	  {0.0178, 0.0627, 0.0938, 0.0569, 0.0378, 0.0288, 0.0377, 0.0322, 0.0260, 0.0279, 0.0100, 0.0179, 0.0155, 0.0134, 0.0219, 0.0108, 0.0165, 0.0106, 0.0224, 0.0111, 0.0614, 0.0592, 0.0533, 0.0381, 0.0273, 0.0182, 0.0223, 0.0137, 0.0186, 0.0125, 0.0196, 0.0144, 0.0112, 0.0159, 0.0048, 0.0103, 0.0091, 0.0185},
	  {0.0153, 0.0169, 0.0422, 0.0596, 0.0503, 0.0472, 0.0444, 0.0251, 0.0188, 0.0228, 0.0166, 0.0215, 0.0246, 0.0163, 0.0109, 0.0116, 0.0162, 0.0113, 0.0265, 0.0120, 0.0540, 0.0282, 0.0929, 0.0441, 0.0191, 0.0336, 0.0256, 0.0310, 0.0375, 0.0023, 0.0175, 0.0134, 0.0111, 0.0121, 0.0097, 0.0088, 0.0121, 0.0370},
	  {0.0037, 0.0399, 0.0869, 0.0444, 0.0599, 0.0498, 0.0340, 0.0169, 0.0186, 0.0155, 0.0175, 0.0154, 0.0144, 0.0093, 0.0153, 0.0150, 0.0250, 0.0086, 0.0237, 0.0099, 0.0329, 0.0802, 0.0577, 0.0569, 0.0580, 0.0345, 0.0283, 0.0111, 0.0176, 0.0099, 0.0067, 0.0096, 0.0120, 0.0133, 0.0146, 0.0078, 0.0010, 0.0244},
	  {0.0149, 0.0442, 0.0686, 0.0578, 0.0577, 0.0456, 0.0622, 0.0418, 0.0430, 0.0193, 0.0258, 0.0089, 0.0078, 0.0139, 0.0153, 0.0199, 0.0075, 0.0064, 0.0288, 0.0186, 0.0356, 0.0272, 0.0491, 0.0622, 0.0513, 0.0353, 0.0208, 0.0124, 0.0184, 0.0094, 0.0120, 0.0094, 0.0008, 0.0038, 0.0082, 0.0074, 0.0047, 0.0240},
	  {0.0342, 0.0605, 0.0677, 0.0641, 0.0675, 0.0480, 0.0505, 0.0301, 0.0394, 0.0227, 0.0183, 0.0161, 0.0072, 0.0089, 0.0055, 0.0080, 0.0092, 0.0060, 0.0277, 0.0307, 0.0410, 0.0490, 0.0399, 0.0479, 0.0399, 0.0227, 0.0286, 0.0298, 0.0070, 0.0130, 0.0130, 0.0098, 0.0039, 0.0025, 0.0065, 0.0035, 0.0032, 0.0166},
	  {0.0682, 0.0810, 0.0766, 0.0301, 0.0259, 0.0350, 0.0285, 0.0260, 0.0346, 0.0215, 0.0161, 0.0184, 0.0135, 0.0234, 0.0131, 0.0070, 0.0068, 0.0006, 0.0336, 0.0554, 0.0771, 0.0408, 0.0320, 0.0334, 0.0352, 0.0269, 0.0352, 0.0247, 0.0195, 0.0125, 0.0077, 0.0066, 0.0047, 0.0042, 0.0063, 0.0038, 0.0041, 0.0100},
	  {0.0250, 0.1413, 0.1054, 0.0576, 0.0324, 0.0169, 0.0272, 0.0273, 0.0286, 0.0168, 0.0100, 0.0086, 0.0067, 0.0049, 0.0091, 0.0040, 0.0024, 0.0043, 0.0254, 0.0413, 0.0808, 0.1042, 0.0481, 0.0263, 0.0369, 0.0155, 0.0192, 0.0077, 0.0149, 0.0108, 0.0081, 0.0055, 0.0074, 0.0036, 0.0011, 0.0033, 0.0012, 0.0100},
	  {0.0491, 0.0622, 0.1275, 0.0780, 0.0447, 0.0330, 0.0330, 0.0248, 0.0173, 0.0142, 0.0155, 0.0020, 0.0037, 0.0061, 0.0076, 0.0048, 0.0013, 0.0023, 0.0105, 0.0496, 0.0657, 0.1183, 0.0529, 0.0357, 0.0323, 0.0207, 0.0158, 0.0149, 0.0165, 0.0115, 0.0072, 0.0039, 0.0042, 0.0014, 0.0016, 0.0025, 0.0006, 0.0072},
	  {0.0320, 0.0583, 0.0686, 0.0911, 0.0466, 0.0281, 0.0282, 0.0230, 0.0206, 0.0183, 0.0138, 0.0124, 0.0142, 0.0026, 0.0045, 0.0087, 0.0061, 0.0012, 0.0103, 0.0290, 0.0695, 0.0778, 0.0893, 0.0608, 0.0360, 0.0310, 0.0244, 0.0144, 0.0192, 0.0121, 0.0121, 0.0067, 0.0072, 0.0046, 0.0042, 0.0012, 0.0040, 0.0076},
	  {0.0397, 0.0521, 0.0713, 0.0724, 0.0660, 0.0669, 0.0274, 0.0257, 0.0217, 0.0145, 0.0156, 0.0135, 0.0107, 0.0047, 0.0027, 0.0036, 0.0056, 0.0026, 0.0133, 0.0232, 0.0645, 0.0667, 0.0558, 0.0725, 0.0532, 0.0378, 0.0156, 0.0124, 0.0187, 0.0073, 0.0082, 0.0087, 0.0038, 0.0019, 0.0044, 0.0046, 0.0004, 0.0103},
	  {0.0194, 0.1050, 0.0733, 0.0482, 0.0615, 0.0692, 0.0459, 0.0253, 0.0140, 0.0229, 0.0169, 0.0115, 0.0096, 0.0076, 0.0050, 0.0019, 0.0001, 0.0036, 0.0143, 0.0172, 0.0723, 0.0440, 0.0560, 0.0544, 0.0450, 0.0397, 0.0335, 0.0214, 0.0123, 0.0099, 0.0130, 0.0074, 0.0038, 0.0029, 0.0028, 0.0018, 0.0010, 0.0065},
	  {0.0253, 0.0546, 0.0703, 0.0620, 0.0574, 0.0490, 0.0450, 0.0323, 0.0276, 0.0135, 0.0157, 0.0109, 0.0166, 0.0126, 0.0036, 0.0080, 0.0038, 0.0057, 0.0136, 0.0135, 0.0619, 0.0662, 0.0520, 0.0580, 0.0445, 0.0504, 0.0306, 0.0281, 0.0134, 0.0209, 0.0068, 0.0052, 0.0057, 0.0078, 0.0031, 0.0001, 0.0006, 0.0037},
	  {0.0247, 0.0587, 0.0660, 0.0911, 0.0831, 0.0278, 0.0264, 0.0407, 0.0384, 0.0238, 0.0110, 0.0117, 0.0102, 0.0089, 0.0101, 0.0064, 0.0024, 0.0051, 0.0174, 0.0255, 0.0450, 0.0729, 0.0561, 0.0402, 0.0318, 0.0368, 0.0377, 0.0261, 0.0160, 0.0090, 0.0075, 0.0142, 0.0067, 0.0013, 0.0042, 0.0012, 0.0017, 0.0022},
	  {0.0299, 0.0844, 0.0642, 0.0491, 0.0570, 0.0456, 0.0389, 0.0264, 0.0334, 0.0190, 0.0131, 0.0105, 0.0091, 0.0095, 0.0098, 0.0051, 0.0041, 0.0007, 0.0145, 0.0321, 0.0567, 0.0544, 0.0477, 0.0704, 0.0415, 0.0380, 0.0351, 0.0163, 0.0252, 0.0158, 0.0139, 0.0073, 0.0100, 0.0029, 0.0014, 0.0019, 0.0001, 0.0052},
	  {0.0229, 0.0708, 0.0363, 0.0500, 0.0756, 0.0562, 0.0264, 0.0395, 0.0429, 0.0230, 0.0121, 0.0216, 0.0043, 0.0078, 0.0084, 0.0047, 0.0026, 0.0036, 0.0124, 0.0297, 0.0523, 0.0636, 0.0495, 0.0560, 0.0493, 0.0333, 0.0273, 0.0252, 0.0262, 0.0107, 0.0119, 0.0101, 0.0050, 0.0088, 0.0039, 0.0047, 0.0001, 0.0113},
	  {0.0390, 0.0334, 0.0612, 0.0445, 0.0469, 0.0675, 0.0494, 0.0329, 0.0386, 0.0512, 0.0243, 0.0241, 0.0070, 0.0184, 0.0062, 0.0033, 0.0043, 0.0038, 0.0088, 0.0357, 0.0408, 0.0360, 0.0500, 0.0541, 0.0477, 0.0302, 0.0323, 0.0173, 0.0155, 0.0113, 0.0184, 0.0124, 0.0072, 0.0041, 0.0085, 0.0024, 0.0013, 0.0099},
	  {0.0260, 0.0646, 0.0435, 0.0456, 0.0516, 0.0440, 0.0234, 0.0331, 0.0231, 0.0158, 0.0242, 0.0302, 0.0209, 0.0123, 0.0033, 0.0016, 0.0114, 0.0043, 0.0127, 0.0316, 0.0658, 0.0643, 0.0378, 0.0479, 0.0340, 0.0306, 0.0414, 0.0367, 0.0238, 0.0133, 0.0267, 0.0136, 0.0094, 0.0025, 0.0092, 0.0067, 0.0049, 0.0084},
	  {0.0437, 0.0579, 0.0596, 0.0511, 0.0223, 0.0383, 0.0371, 0.0225, 0.0423, 0.0395, 0.0136, 0.0134, 0.0257, 0.0145, 0.0130, 0.0050, 0.0060, 0.0071, 0.0200, 0.0424, 0.0412, 0.0649, 0.0656, 0.0304, 0.0375, 0.0261, 0.0266, 0.0149, 0.0238, 0.0161, 0.0173, 0.0125, 0.0118, 0.0089, 0.0094, 0.0021, 0.0043, 0.0117},
	  {0.0438, 0.0760, 0.0420, 0.0772, 0.0489, 0.0275, 0.0351, 0.0259, 0.0243, 0.0209, 0.0051, 0.0125, 0.0187, 0.0114, 0.0130, 0.0108, 0.0043, 0.0037, 0.0139, 0.0367, 0.0669, 0.0625, 0.0425, 0.0395, 0.0417, 0.0206, 0.0298, 0.0181, 0.0263, 0.0186, 0.0178, 0.0105, 0.0048, 0.0168, 0.0121, 0.0037, 0.0044, 0.0117},
	  {0.0421, 0.0388, 0.0853, 0.0455, 0.0430, 0.0565, 0.0224, 0.0377, 0.0264, 0.0210, 0.0177, 0.0238, 0.0131, 0.0042, 0.0125, 0.0072, 0.0066, 0.0025, 0.0216, 0.0273, 0.0549, 0.0755, 0.0324, 0.0364, 0.0496, 0.0399, 0.0130, 0.0256, 0.0143, 0.0119, 0.0151, 0.0127, 0.0149, 0.0113, 0.0100, 0.0096, 0.0014, 0.0166},
	  {0.0465, 0.0590, 0.0450, 0.0779, 0.0397, 0.0392, 0.0311, 0.0259, 0.0261, 0.0235, 0.0217, 0.0198, 0.0055, 0.0162, 0.0141, 0.0146, 0.0027, 0.0026, 0.0332, 0.0473, 0.0501, 0.0394, 0.0455, 0.0358, 0.0479, 0.0233, 0.0406, 0.0178, 0.0158, 0.0156, 0.0131, 0.0137, 0.0071, 0.0129, 0.0102, 0.0007, 0.0038, 0.0152}};


	exp_mat =
{{0.02314, 0.03704, 0.05336, 0.05646, 0.04551, 0.03345, 0.03507, 0.03846, 0.03650, 0.03565, 0.02885, 0.02394, 0.01870, 0.01341, 0.01059, 0.00898, 0.00754, 0.00627, 0.02555, 0.02164, 0.03454, 0.04681, 0.04884, 0.03931, 0.02878, 0.02999, 0.03271, 0.03086, 0.02997, 0.02411, 0.01988, 0.01545, 0.01103, 0.00866, 0.00729, 0.00607, 0.00502, 0.02057},
{0.03665, 0.05541, 0.05714, 0.04059, 0.04052, 0.04127, 0.03360, 0.02541, 0.02577, 0.02761, 0.02644, 0.02510, 0.02070, 0.01692, 0.01323, 0.00964, 0.00755, 0.00635, 0.02801, 0.03428, 0.05167, 0.05013, 0.03512, 0.03496, 0.03539, 0.02860, 0.02148, 0.02164, 0.02305, 0.02195, 0.02073, 0.01701, 0.01383, 0.01077, 0.00782, 0.00610, 0.00510, 0.02250},
{0.02623, 0.07295, 0.06758, 0.04950, 0.03485, 0.03435, 0.03462, 0.02823, 0.02145, 0.02127, 0.02245, 0.02150, 0.02017, 0.01675, 0.01363, 0.01066, 0.00782, 0.00611, 0.02779, 0.02453, 0.06800, 0.05925, 0.04281, 0.03008, 0.02939, 0.02933, 0.02372, 0.01790, 0.01763, 0.01851, 0.01764, 0.01648, 0.01363, 0.01104, 0.00860, 0.00630, 0.00490, 0.02234},
{0.02972, 0.05274, 0.08872, 0.05899, 0.04223, 0.03008, 0.02921, 0.02904, 0.02362, 0.01796, 0.01742, 0.01816, 0.01741, 0.01619, 0.01353, 0.01097, 0.00858, 0.00634, 0.02742, 0.02780, 0.04913, 0.07775, 0.05099, 0.03645, 0.02580, 0.02476, 0.02436, 0.01965, 0.01483, 0.01430, 0.01483, 0.01415, 0.01312, 0.01092, 0.00882, 0.00688, 0.00508, 0.02203},
{0.02459, 0.06026, 0.06596, 0.07762, 0.05152, 0.03669, 0.02636, 0.02508, 0.02448, 0.01979, 0.01503, 0.01432, 0.01478, 0.01419, 0.01311, 0.01101, 0.00891, 0.00697, 0.02759, 0.02301, 0.05620, 0.05782, 0.06706, 0.04443, 0.03149, 0.02242, 0.02107, 0.02035, 0.01632, 0.01230, 0.01165, 0.01197, 0.01144, 0.01054, 0.00883, 0.00712, 0.00556, 0.02215},
{0.02693, 0.05061, 0.07532, 0.05956, 0.06676, 0.04526, 0.03196, 0.02297, 0.02126, 0.02026, 0.01624, 0.01232, 0.01157, 0.01184, 0.01137, 0.01046, 0.00882, 0.00713, 0.02782, 0.02519, 0.04716, 0.06605, 0.05149, 0.05755, 0.03877, 0.02716, 0.01933, 0.01767, 0.01666, 0.01325, 0.00999, 0.00932, 0.00950, 0.00910, 0.00835, 0.00703, 0.00567, 0.02231},
{0.03415, 0.05455, 0.06285, 0.06575, 0.05191, 0.05634, 0.03900, 0.02719, 0.01942, 0.01745, 0.01629, 0.01300, 0.00991, 0.00920, 0.00935, 0.00899, 0.00824, 0.00697, 0.02778, 0.03194, 0.05083, 0.05509, 0.05684, 0.04475, 0.04815, 0.03301, 0.02280, 0.01612, 0.01431, 0.01322, 0.01048, 0.00793, 0.00733, 0.00743, 0.00713, 0.00653, 0.00552, 0.02225},
{0.04622, 0.06612, 0.06456, 0.05307, 0.05337, 0.04302, 0.04517, 0.03178, 0.02179, 0.01543, 0.01352, 0.01243, 0.00993, 0.00762, 0.00701, 0.00708, 0.00681, 0.00623, 0.02651, 0.04322, 0.06161, 0.05657, 0.04587, 0.04598, 0.03667, 0.03800, 0.02647, 0.01798, 0.01261, 0.01092, 0.00994, 0.00789, 0.00602, 0.00553, 0.00557, 0.00536, 0.00490, 0.02122},
{0.05482, 0.08524, 0.07440, 0.05147, 0.04128, 0.04067, 0.03341, 0.03386, 0.02408, 0.01622, 0.01141, 0.00981, 0.00892, 0.00715, 0.00553, 0.00506, 0.00507, 0.00488, 0.02362, 0.05127, 0.07935, 0.06512, 0.04448, 0.03555, 0.03454, 0.02786, 0.02784, 0.01960, 0.01308, 0.00912, 0.00776, 0.00700, 0.00558, 0.00431, 0.00394, 0.00395, 0.00381, 0.01891},
{0.03154, 0.10215, 0.09682, 0.06017, 0.04070, 0.03263, 0.03151, 0.02618, 0.02553, 0.01823, 0.01207, 0.00845, 0.00716, 0.00645, 0.00519, 0.00405, 0.00368, 0.00367, 0.02071, 0.02949, 0.09514, 0.08470, 0.05194, 0.03506, 0.02777, 0.02630, 0.02144, 0.02061, 0.01457, 0.00957, 0.00664, 0.00557, 0.00498, 0.00400, 0.00312, 0.00284, 0.00284, 0.01655},
{0.03115, 0.06189, 0.12027, 0.08089, 0.04934, 0.03331, 0.02663, 0.02515, 0.02102, 0.01977, 0.01421, 0.00933, 0.00653, 0.00547, 0.00489, 0.00396, 0.00311, 0.00281, 0.01863, 0.02913, 0.05755, 0.10524, 0.06979, 0.04246, 0.02834, 0.02223, 0.02058, 0.01687, 0.01565, 0.01114, 0.00726, 0.00504, 0.00419, 0.00373, 0.00301, 0.00237, 0.00216, 0.01487},
{0.02850, 0.06251, 0.07635, 0.10265, 0.06832, 0.04191, 0.02818, 0.02234, 0.02058, 0.01720, 0.01568, 0.01135, 0.00743, 0.00521, 0.00432, 0.00384, 0.00312, 0.00247, 0.01696, 0.02665, 0.05820, 0.06677, 0.08857, 0.05879, 0.03566, 0.02357, 0.01831, 0.01653, 0.01356, 0.01219, 0.00875, 0.00569, 0.00396, 0.00327, 0.00289, 0.00235, 0.00187, 0.01351},
{0.03474, 0.05678, 0.07574, 0.06662, 0.08458, 0.05741, 0.03536, 0.02358, 0.01843, 0.01654, 0.01381, 0.01228, 0.00896, 0.00587, 0.00412, 0.00340, 0.00299, 0.00244, 0.01530, 0.03249, 0.05287, 0.06630, 0.05747, 0.07278, 0.04890, 0.02965, 0.01940, 0.01486, 0.01307, 0.01073, 0.00940, 0.00681, 0.00444, 0.00310, 0.00255, 0.00224, 0.00183, 0.01216},
{0.02772, 0.06916, 0.06928, 0.06516, 0.05693, 0.06962, 0.04830, 0.02975, 0.01961, 0.01511, 0.01328, 0.01112, 0.00970, 0.00714, 0.00471, 0.00331, 0.00271, 0.00237, 0.01418, 0.02592, 0.06442, 0.06065, 0.05624, 0.04896, 0.05927, 0.04052, 0.02454, 0.01587, 0.01198, 0.01033, 0.00850, 0.00733, 0.00536, 0.00351, 0.00246, 0.00201, 0.00176, 0.01122},
{0.02560, 0.05587, 0.08452, 0.06082, 0.05537, 0.04920, 0.05800, 0.04098, 0.02513, 0.01635, 0.01243, 0.01075, 0.00903, 0.00777, 0.00576, 0.00382, 0.00269, 0.00219, 0.01339, 0.02394, 0.05204, 0.07401, 0.05250, 0.04769, 0.04196, 0.04878, 0.03396, 0.02048, 0.01307, 0.00974, 0.00827, 0.00683, 0.00581, 0.00428, 0.00283, 0.00199, 0.00162, 0.01053},
{0.02610, 0.05197, 0.06951, 0.07398, 0.05312, 0.04773, 0.04302, 0.04882, 0.03496, 0.02129, 0.01368, 0.01028, 0.00879, 0.00740, 0.00630, 0.00470, 0.00314, 0.00222, 0.01276, 0.02441, 0.04844, 0.06090, 0.06386, 0.04576, 0.04085, 0.03635, 0.04066, 0.02869, 0.01718, 0.01083, 0.00798, 0.00670, 0.00555, 0.00467, 0.00347, 0.00231, 0.00163, 0.00996},
{0.02413, 0.05306, 0.06472, 0.06189, 0.06359, 0.04662, 0.04130, 0.03758, 0.04110, 0.02978, 0.01808, 0.01155, 0.00862, 0.00730, 0.00617, 0.00520, 0.00390, 0.00263, 0.01252, 0.02257, 0.04947, 0.05675, 0.05346, 0.05478, 0.03993, 0.03505, 0.03146, 0.03392, 0.02423, 0.01447, 0.00907, 0.00664, 0.00552, 0.00459, 0.00384, 0.00286, 0.00193, 0.00969},
{0.02726, 0.04926, 0.06605, 0.05746, 0.05389, 0.05446, 0.04075, 0.03552, 0.03244, 0.03426, 0.02510, 0.01525, 0.00971, 0.00721, 0.00606, 0.00513, 0.00430, 0.00324, 0.01266, 0.02550, 0.04591, 0.05791, 0.04968, 0.04644, 0.04657, 0.03452, 0.02979, 0.02684, 0.02794, 0.02020, 0.01207, 0.00755, 0.00550, 0.00455, 0.00379, 0.00315, 0.00236, 0.00973},
{0.03108, 0.05477, 0.06072, 0.05769, 0.04936, 0.04631, 0.04604, 0.03502, 0.03000, 0.02739, 0.02805, 0.02078, 0.01268, 0.00807, 0.00597, 0.00498, 0.00421, 0.00351, 0.01314, 0.02908, 0.05106, 0.05321, 0.04987, 0.04259, 0.03962, 0.03897, 0.02935, 0.02488, 0.02241, 0.02263, 0.01655, 0.00994, 0.00622, 0.00451, 0.00371, 0.00310, 0.00256, 0.01000},
{0.02534, 0.06216, 0.06711, 0.05335, 0.04936, 0.04246, 0.03977, 0.03883, 0.02985, 0.02511, 0.02286, 0.02280, 0.01707, 0.01049, 0.00668, 0.00492, 0.00408, 0.00345, 0.01368, 0.02370, 0.05797, 0.05884, 0.04610, 0.04259, 0.03644, 0.03377, 0.03261, 0.02482, 0.02065, 0.01856, 0.01826, 0.01349, 0.00817, 0.00512, 0.00370, 0.00302, 0.00252, 0.01031},
{0.03749, 0.05005, 0.07470, 0.05771, 0.04526, 0.04149, 0.03584, 0.03340, 0.03198, 0.02476, 0.02051, 0.01865, 0.01820, 0.01375, 0.00854, 0.00544, 0.00399, 0.00328, 0.01383, 0.03507, 0.04665, 0.06552, 0.04988, 0.03904, 0.03563, 0.03055, 0.02815, 0.02665, 0.02043, 0.01674, 0.01502, 0.01447, 0.01080, 0.00661, 0.00414, 0.00298, 0.00242, 0.01036},
{0.02721, 0.07336, 0.06063, 0.06380, 0.04880, 0.03846, 0.03489, 0.03018, 0.02787, 0.02619, 0.02043, 0.01675, 0.01521, 0.01458, 0.01112, 0.00698, 0.00446, 0.00325, 0.01389, 0.02545, 0.06846, 0.05319, 0.05516, 0.04211, 0.03303, 0.02977, 0.02555, 0.02333, 0.02168, 0.01674, 0.01358, 0.01217, 0.01152, 0.00868, 0.00537, 0.00338, 0.00242, 0.01036},
{0.03065, 0.05391, 0.08796, 0.05305, 0.05344, 0.04132, 0.03264, 0.02921, 0.02520, 0.02302, 0.02126, 0.01670, 0.01362, 0.01234, 0.01165, 0.00895, 0.00568, 0.00364, 0.01389, 0.02867, 0.05025, 0.07718, 0.04588, 0.04613, 0.03550, 0.02784, 0.02475, 0.02117, 0.01912, 0.01747, 0.01359, 0.01096, 0.00981, 0.00915, 0.00695, 0.00435, 0.00275, 0.01035},
{0.03891, 0.05953, 0.06464, 0.07382, 0.04527, 0.04425, 0.03458, 0.02725, 0.02399, 0.02058, 0.01858, 0.01693, 0.01340, 0.01089, 0.00985, 0.00917, 0.00709, 0.00456, 0.01403, 0.03640, 0.05554, 0.05670, 0.06384, 0.03908, 0.03804, 0.02953, 0.02310, 0.02019, 0.01718, 0.01534, 0.01382, 0.01083, 0.00872, 0.00778, 0.00716, 0.00548, 0.00348, 0.01047}};

  N = {50, 66, 78, 74, 44, 52, 57, 51, 54, 77, 89, 77, 69, 71, 63, 75, 63, 56, 56, 58, 44, 56, 57, 49};

  // Test 6
  sexed = true;
  sepbysex = true;
  robust = false;
  double score5 = NLLlogistnorm(obs_mat,exp_mat,N,Sigma, phi,sepbysex, sexlag,  robust, ARMA, sexed);
  // Test 7
  sepbysex = false;
  sexlag = true;
  robust = false;
  double score6 = NLLlogistnorm(obs_mat,exp_mat,N,Sigma, phi,sepbysex, sexlag,  robust, ARMA, sexed);
  // Test 8
  sexed = true;
  sexlag = false;
  sepbysex = true;
  robust = true;

  double score7 = NLLlogistnorm(obs_mat,exp_mat,N,Sigma, phi,sepbysex, sexlag,  robust, ARMA, sexed);
  // Test 9
  sepbysex = false;
  sexlag = true;
  double score8 = NLLlogistnorm(obs_mat,exp_mat,N,Sigma, phi,sepbysex, sexlag,  robust, ARMA, sexed);

  // Test simulating data now
  map<unsigned,vector<Comparison>> Comparisons;
  Comparison new_comparison;
  vector<double> exp =  {0.0213, 0.0350, 0.0420, 0.0101, 0.0255, 0.0449, 0.0950, 0.1354, 0.0619, 0.2855, 0.0864, 0.0746, 0.0508, 0.0180, 0.0129, 0.0002, 0.0002, 0.0002, 0.0002};
  unsigned age = 3;
  for (auto num : exp) {
    new_comparison.age_ = 3;
    new_comparison.expected_ = num;
    ++age;
    Comparisons[1990].push_back(new_comparison);
  }
  exp =  {0.09751, 0.20093, 0.16020, 0.10266, 0.06709, 0.05474, 0.04976, 0.05085, 0.04788, 0.03948, 0.03138, 0.02507, 0.01939, 0.01463, 0.01121, 0.00847, 0.00606, 0.00409, 0.00861};
  age = 3;
  for (auto num : exp) {
    new_comparison.age_ = 3;
    new_comparison.expected_ = num;
    ++age;
    Comparisons[1991].push_back(new_comparison);
  }

  cout << "\n Check out randomly generated vals \n ";
  int N_ages = 19;
  double sigma = 0.2;
  covmat = covmat_logistnorm(sigma, phi, N_ages,sepbysex, sexlag, ARMA, sexed);
  phi = {0.235,-0.284};
  rlogistnorm(Comparisons,19,0.3, phi,covmat, ARMA, sexed);
	// check out what happened
  for (auto year_iterator = Comparisons.begin(); year_iterator != Comparisons.end(); ++year_iterator) {
    for (Comparison& comparison : year_iterator->second) {
      cout << "simulated  = " <<  comparison.observed_<< " expected = " << comparison.expected_ << endl;
    }
  }


	return 0;
}

// Define the Negative loglikeihood score for a logistic normal distributed comp data.
double NLLlogistnorm(vector<vector<double>> obs,vector<vector<double>> Exp,vector<double> N,  double sigma, vector<double> phi ,bool sepbysex,bool sexlag, bool robust,bool ARMA,bool sexed) {
  double score, mean_N, N_years, N_ages, Nbin;
  unsigned i,j,k;
  vector<double> weights;
  vector<vector<double>> covmat, Kmat, Vmat,t_kmat,log_obs,ww,V_invert;
  N_years = obs.size();
  if (sexed)
    N_ages = (double)obs[0].size() / 2;
  else
    N_ages = obs[0].size();

  Nbin = obs[0].size();
  cerr << "number of ages = " << N_ages << " number of years " << N_years << endl;
  // initialise Kmat and Vmat;

  Kmat.resize(Nbin, vector<double>(Nbin,0.0));
  Vmat.resize(Nbin, vector<double>(Nbin,0.0));

  score = 0.0;
  mean_N = Mean(N);
  cerr << "mean N = " << mean_N << ", no years = " << N_years << ", no ages = " << N_ages << endl;
  // Calculate a weights that are essentially scaled N
  for (auto this_N : N) {
    double temp = sqrt(mean_N/this_N);
    cerr << "weight = " << temp << endl;
    weights.push_back(temp);
  }
  // Get the covariance matrix with given parameters Sigma, phi and ARMA
  covmat = covmat_logistnorm(sigma, phi, N_ages,sepbysex, sexlag, ARMA, sexed);

  cerr << "printing covar = " << endl;
  for (k = 0; k < covmat.size(); ++k) {
    for (j = 0; j < covmat[k].size(); ++j)
      cout << covmat[k][j] << " ";
    cout << endl;
  }


  for (i = 0; i < (Nbin - 1); ++i) {
    Kmat[i][i] = 1.0;
    Kmat[i][Nbin - 1] = -1.0;
  }


  // Calculate Vmat
  t_kmat = t(Kmat);
  cout << "Check the transpose " << Kmat.size() << " " << Kmat[0].size() << endl;
  for (k = 0; k < Kmat.size(); ++k) {
    for (j = 0; j < Kmat[k].size() ; ++j) {
      cout << Kmat[k][j]<< " ";
    }
    cout << endl;
  }

  t_kmat= mat_multiply(covmat,t_kmat);
  cout << "row vmat = " << t_kmat.size() << " col vmat= " << t_kmat[0].size() << endl;
  Vmat = mat_multiply(Kmat, t_kmat);

  boost::numeric::ublas::matrix<double> ublasV_mat(Nbin - 1, Nbin - 1);
  boost::numeric::ublas::matrix<double> inv_Vmat(Nbin - 1, Nbin - 1);

  cout << "row vmat = " << Vmat.size() << " col vmat= " << Vmat[0].size() << " in temp mat = " << ublasV_mat.size1() << " cols = " << ublasV_mat.size2() << endl;

  for (k = 0; k < Vmat.size() - 1; ++k) {
    for (j = 0; j < Vmat[k].size() - 1; ++j) {
      ublasV_mat(k,j) = Vmat[k][j];
      cout << ublasV_mat(k,j) << " ";
    }
    cout << endl;
  }

  bool inverted = InvertMatrix(ublasV_mat,inv_Vmat);
  if (!inverted) {
    cerr << "could not invert matrix, exiting program";
    return 0;
  }
  // Need to convert inverse back into a function that will be handled by all my shitty functions... how fustrating, this is where templating will be useful
  V_invert.resize(inv_Vmat.size1(), vector<double>(inv_Vmat.size2()));
  cout << inv_Vmat.size1() << " " << inv_Vmat.size2() << endl;
  for (i = 0; i < inv_Vmat.size1(); ++i) {
    for (j = 0; j < inv_Vmat.size2(); ++j) {
      V_invert[i][j] = inv_Vmat(i,j);
      cout <<  inv_Vmat(i,j) << " ";
    }
    cout << endl;
  }

  log_obs = log_mat(obs);

  double tot_log_obs = Sum_mat(log_obs);
  score = 0.5 * N_years * (Nbin - 1) * log(2 * PI) + tot_log_obs + 0.5 * N_years * log(det_fast(ublasV_mat));
  cout << "total log obs = " << tot_log_obs << " determinant of inverted mat = " << det_fast(ublasV_mat) << " initial score = " <<  score << endl;

  // check if any weights deviate form 1
  if (!all_ones(weights)) {
    vector<double> log_weights;
    for (auto ww: weights)
      log_weights.push_back(log(ww));
    score += (Nbin - 1) * Sum(log_weights);
  }

  // Sweep over the the obs and create this ww object.
  ww.resize(N_years, vector<double>(Nbin - 1));
  for ( i = 0; i < N_years; ++i) {
    for ( j = 0; j < Nbin - 1; ++j) {
      double l_obs = obs[i][j] / obs[i][Nbin - 1];
      double l_exp = Exp[i][j] / Exp[i][Nbin - 1];
      ww[i][j] = log(l_obs) - log(l_exp);
      cout << ww[i][j] << " ";
    }
    cout << endl;
  }
  // Now finish with a robustification
  if (robust) {
    // get the diaganol components of the inverted matrix
    double temp3,temp4,value;
    vector<double> diag_inv;
    vector<vector<double>> temp1,temp2;
    for (i = 0; i < inv_Vmat.size1(); ++i)
      diag_inv.push_back(inv_Vmat(i,i));
    for (unsigned year = 0; year < N_years; ++year) {
      vector<double>  temp_holder, robust_temp;
      temp_holder = elem_prod(ww[year],ww[year]);
      temp_holder = elem_prod(temp_holder,diag_inv);

       for (i = 0; i < temp_holder.size(); ++i) {
         double exp_val = exp(-temp_holder[i]) + 0.01;
         robust_temp.push_back(log(exp_val));
       }
       cout << endl;
       temp1 = mat_multiply(ww[year], V_invert);
       temp2 = mat_multiply(temp1,ww[year]);
       temp3 = 0.5 / (weights[year] * weights[year]);
       score += temp3 * (temp2[0][0] - Sum(temp_holder) - Sum(robust_temp));
       cout << "score after  = " << score << " = " << (temp3 * temp2[0][0]) << " temp2 = " << Sum(temp_holder)  << " temp3 = " << Sum(robust_temp) << endl;


    }


  } else {
    vector<vector<double>> temp1,temp2;
    double temp3;
    for (unsigned year = 0; year < N_years; ++year) {
      temp1 = mat_multiply(ww[year], V_invert);
      temp2 = mat_multiply(temp1,ww[year]);
      temp3 = 0.5 / (weights[year] * weights[year]);
      cout << "temp3 = " << temp3 << " temp2 " <<  temp2[0][0] << " current NLL " << score << endl;
      score += (temp3 * temp2[0][0]);
    }
  }



  cout << "--------------------------------------"<< endl;
  cout << "Negative loglikelihood----------------"<< endl;
  cout << "--------------------------------------"<< endl;
  cout << score << endl;



  return score;
}

void rlogistnorm(map<unsigned,vector<Comparison>>& comparisons, unsigned N, double sigma, vector<double> phi, vector<vector<double>> covmat, bool ARMA, bool sexed){
   vector<double> age_totals(N, 0.0);
   boost::mt19937 generator_;
   generator_.seed(234);
   // define it is a standard normal dist
   boost::normal_distribution<> normal(0, 1);
   boost::variate_generator<boost::mt19937&, boost::normal_distribution<> > generator(generator_, normal);
   double val = generator();

   for (auto year_iterator = comparisons.begin(); year_iterator != comparisons.end(); ++year_iterator) {
     unsigned vec_iter = 0;
     for (Comparison& comparison : year_iterator->second) {
       comparison.observed_ = exp(generator() * sigma + log(comparison.expected_));
       age_totals[vec_iter] += comparison.observed_;
       ++vec_iter;
     }
   }

   for (auto year_iterator = comparisons.begin(); year_iterator != comparisons.end(); ++year_iterator) {
     unsigned vec_iter = 0;
     for (Comparison& comparison : year_iterator->second) {
       comparison.observed_ /= age_totals[vec_iter];
       ++vec_iter;
     }
   }


}


// Axuillary fucntion definitions
vector<vector<double> > covmat_logistnorm(double sigma, vector<double> phi,unsigned N_ages , bool sepbysex, bool sexlag, bool ARMA, bool sexed) {
  unsigned n_phi = phi.size();
  vector<double> rho;
  unsigned Nbins;
  if (sexed)
    Nbins = N_ages * 2;
  else
    Nbins = N_ages;
  vector<vector<double>> covar;

  covar.resize(Nbins, vector<double>(Nbins, 0.0));


  // initialise covar as the identity matrix
  for (unsigned diag = 0; diag < covar.size(); ++ diag)
    covar[diag][diag] = 1.0 *sigma * sigma;

  if (phi[0] == 0.0) {
    for (unsigned diag = 0; diag < N_ages; ++ diag)
      covar[diag][diag] *= sigma * sigma;
  } else if (sexed && sepbysex) {
    cout << "Calculating covar for sexed patrition with sepbysex = true, covar dims = " << covar.size()  << " " << covar[0].size() << endl;
    for (int i = 0; i < 1; ++i) {
      // Get rho
      if (n_phi == 1) {
        rho = GetRho(phi[0],N_ages);
      } else if (n_phi == 2 && ARMA) {
        rho = ARMAacf(phi[0],phi[1],N_ages);
      } else {
        rho = ARMAacf(phi,N_ages);
      }
      for (auto num:rho)
        cout << num << " ";
      cout << endl;
      // Store in the Covariance matrix
      cout << "sigma = " << sigma << endl;
      for (i = 0; i <= 1; ++i) {
        for (unsigned diag = 0; diag < N_ages; ++diag) {
          for (int row = 0; row < N_ages; ++row) {
            for (int col = 0; col < N_ages; ++col) {
              if (fabs(row - col) == diag + 1)
                covar[row + (i * N_ages)][col + (i * N_ages)] = rho[diag] * sigma * sigma;
            }
          }
        }
      }
    }
  } else if (sexed && sexlag) {
    vector<int> binlab;
    int ages = 1;
    for (int i = 1; i <= Nbins; ++i, ++ages) {
      if (ages > N_ages)
        ages = 1;
      binlab.push_back(ages);
      cout << ages << " ";
    }

      // Get rho
      if (n_phi == 1) {
        rho = GetRho(phi[0],N_ages + 1);
      } else if (n_phi == 2 && ARMA) {
        rho = ARMAacf(phi[0],phi[1],N_ages + 1);
      } else {
        rho = ARMAacf(phi,N_ages + 1);
      }
      // Calculate lag off set vector
      vector<int> col_vec, row_vec, offset_vec;

      for (int row = 0; row < Nbins; ++row) {
        for (int col = 0; col < Nbins; ++col) {
          col_vec.push_back(binlab[col]);
          row_vec.push_back(binlab[row]);
        }
      }
      cout << endl;
      // now calculate an offset vector;
      for (int index = 0; index < col_vec.size(); ++index) {
        offset_vec.push_back(fabs(row_vec[index] - col_vec[index]) + 1.0);
        cout << offset_vec[index] << " ";
      }
      for (int index = 1; index <= N_ages; ++index) {
        for (int index2 = 0; index2 < offset_vec.size(); ++index2) {
          if (index == offset_vec[index2]) {
            vector<int> indexes = get_mat_index(covar.size(), covar[0].size(), index2);
            covar[indexes[0]][indexes[1]] = rho[index - 1];
            //cout << "row = " << indexes[0] << " col = " << indexes[1] << " index = " << index2 << endl;
          }
        }
      }


      // Add the identity mat
    for (int row = 0; row < Nbins; ++row)
      covar[row][row] = 1.0;
    // Store in the Covariance matrix
    for (int row = 0; row < Nbins; ++row) {
      for (int col = 0; col < Nbins; ++col) {
        covar[row][col] *= sigma * sigma;
      }
    }

  } else {
    // Unisex
    cout << "about to enter getRho(), n_phi = " << n_phi << " " << phi[0]<<endl;
    if (n_phi == 1) {
      rho = GetRho(phi[0],N_ages);
    } else if (n_phi == 2 && ARMA) {
      rho = ARMAacf(phi[0],phi[1],N_ages);
    } else {
      rho = ARMAacf(phi,N_ages);
    }

    cout << "Check out rho vector";
    for ( auto num : rho)
      cout << num << " ";
    cout << endl;

    for (unsigned diag = 0; diag < N_ages; ++ diag) {
      for (int row = 0; row < N_ages; ++ row) {
        for (int col = 0; col < N_ages; ++ col) {
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
  cerr << "Beginning method ARMAacf() " << endl;

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

  cerr << "Check A mat that we are inverting\n" << A_eig << "\n: rhs = " << rhs << endl;

  // Solve for A_eig given rhs
  Eigen::Vector3d x = A_eig.colPivHouseholderQr().solve(rhs);
  cerr << "solution = " << x;

  // Divide the last two elements by the first element.
  cerr << "Find the crash" << endl;

  for (unsigned i = 1; i <= 2; ++i) {
    final_acf.push_back(x(i) / x(0));
  }

  cerr << "Final Acf" << endl;
  for (auto num : final_acf)
    cout << num << " ";
  cerr << endl;

  Cor = RecursiveFilter(AR_coef, nBin, final_acf);

  // Add the initial coeffiecients to the beginning of the series.
  Cor.insert(Cor.begin(), final_acf[1]);
  Cor.insert(Cor.begin(), final_acf[0]);
  // Print results to screen
  vector<double>::const_iterator first = Cor.begin();
  vector<double>::const_iterator last = Cor.begin() + nBin;
  vector<double> newVec(first, last);
  for (auto num : newVec)
    cerr << num << " ";
  cerr << endl;

  return newVec;

}

vector<double> ARMAacf(vector<double> AR, int nBin) {
  cerr << "Beginning method ARMAacf() " << endl;
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


  cerr << "size of rhs " << rhs.size() << endl;
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

  cerr << "Check A mat that we are inverti"
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
  cerr << "Check inverted matrix" << endl;
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
  cerr << "Final Acf" << endl;
  for (auto num : final_acf)
    cerr << num << " ";
  cerr << endl;

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
    cerr << num << " ";
  cerr << endl;
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
    cerr << "value = " << store_vec[i];
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
  cerr <<  "entering GetRho()" << endl;

  //calculation of AR(1) acf for  LN2
  vector<double> rho(nBin - 1, 0.0);

  for(int i = 1; i <= nBin - 1; i++) {
    rho[i - 1]= pow(Phi1,(double)i);
    cout << rho[i - 1] << " ";
  }
  cerr <<  "\n\n\n";

  return rho;
}


//////////////////////
// Utility Functions
//////////////////////

// Utilty funciton for summing
double Sum(vector<double> x) {
  double Total;
    for (unsigned i = 0; i< x.size(); ++i) {
     Total += x[i];
    }
    return Total;
}

double Mean(vector<double> x) {
  double Total;
    for (unsigned i = 0; i< x.size(); ++i) {
     Total += x[i];
    }
    return Total / (double)x.size();
}

bool all_ones(vector<double> x) {
  for(auto num : x) {
    if (num != 1.0)
      return false;
  }
  return true;
}

vector<int> get_mat_index(int rows, int cols, double index) {
  int row_index, col_index;
  vector<int> result;
  row_index = floor((double)index / (double)rows);
  col_index = index - (row_index * (double)cols);
  result.push_back(row_index);
  result.push_back(col_index);
  return result;
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

// A method for matrix multiplication
vector<vector<double> > mat_multiply(vector<vector<double> >& x,vector<vector<double> >& y) {
  cerr << "Entering mat_multiply() method" <<endl;
  vector<vector<double>> result;
  result.resize(x.size(), vector<double>(x.size()));
  cerr << "rows of x = " <<  x.size() << " rows of y = " <<  y.size() << endl;
   for (int i = 0; i < x.size(); i++) {
    for (int k = 0; k < x[i].size(); k++) {
     for (int j = 0; j < y.size(); j++) { // swapped order
       result[i][j] += x[i][k] * y[k][j];
     }
    }
   }
   return result;
}

// A method for matrix multiplication
vector<vector<double> > mat_multiply(vector<double> & x,vector<vector<double> >& y) {
  vector<vector<double>> result;
  result.resize(1, vector<double>(x.size()));
   for (int i = 0; i < y.size(); i++) {
    for (int k = 0; k < y[i].size(); k++) {
       result[0][k] += x[i] * y[i][k];
    }
   }
   return result;
}
// A method for matrix multiplication
vector<vector<double> > mat_multiply(vector<vector<double> >& x,vector<double> & y) {
  vector<vector<double>> result;
  result.resize(x.size(), vector<double>(1));
   for (int i = 0; i < x.size(); i++) {
    for (int k = 0; k < x[i].size(); k++) {
       result[i][0] += y[k] * x[i][k];
    }
   }
   return result;
}
vector<vector<double> > t(vector<vector<double>>& x) {
  vector<vector<double>> result;
  result.resize(x[0].size(), vector<double>(x.size()));
  for (int i = 0; i < x.size(); i++) {
    for (int j = 0; j < x[i].size(); j++) {
      result[j][i] = x[i][j];
    }
  }
  return result;
}

vector<vector<double>> log_mat(vector<vector<double> >& x) {
  vector<vector<double>> result;
  result.resize(x.size(), vector<double>(x[0].size()));
  for (unsigned i = 0; i < x.size(); ++i) {
    for (unsigned j = 0; j < x[i].size(); ++j)
      result[i][j] = log(x[i][j]);
  }
  return result;
}

double Sum_mat(vector<vector<double> >& x) {
  double Tot = 0.0;
  for (unsigned i = 0; i < x.size(); ++i) {
    for (unsigned j = 0; j < x[i].size(); ++j)
      Tot += x[i][j];
  }
  return Tot;
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

// Calculate the determinant
double det_fast(const boost::numeric::ublas::matrix<double>& matrix) {
    // create a working copy of the input
  boost::numeric::ublas::matrix<double> mLu(matrix);
  boost::numeric::ublas::permutation_matrix<std::size_t> pivots(matrix.size1());

    auto isSingular = boost::numeric::ublas::lu_factorize(mLu, pivots);
    if (isSingular)
        return static_cast<double>(0);

    double det = static_cast<double>(1);
    for (size_t i = 0; i < pivots.size(); ++i)
    {
        if (pivots(i) != i)
            det *= static_cast<double>(-1);

        det *= mLu(i, i);
    }

    return det;
}

