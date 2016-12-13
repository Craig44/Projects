## testing double normal functionality

mu = 124;
Sl = 6;
Sr = 59;

Length = seq(50,180,by = 1);

## Utilty function
pow = function(x,exponent) {x^exponent}; ## power function
square = function(x) {x^2};	## square power function

## CASAL's version
d_norm = function(X,mu,Sl,Sr) {
store_info = vector();
	for(i in 1:length(X)) {
		if (X[i] <= mu) {
			store_info[i] = pow(2.0, -((X[i] - mu) / Sl * (X[i] - mu) / Sl));
		} else {
			store_info[i] = pow(2.0, -((X[i] - mu) / Sr * (X[i] - mu) / Sr));
		}
	}
	return(store_info);
}

## Test
casal_sel = d_norm(Length,mu,Sl,Sr)
plot(Length,casal_sel, type = "l", xlab = "Length", ylab = "Selectivity")



## Try Darcies STAN code
double_normal = function(X, mu,Sl,Sr, delta = 5) { ## not sure what the delta value is for?
	store_info = vector();
	tmp = log(0.5);
	for(i in 1:length(X)) {
		stmp = 1.0 / (1.0 + exp(-delta * (X[i] - mu)));
		store_info[i] = stmp * exp(tmp * square((X[i] - mu) / Sr)) + (1 - stmp) * exp(tmp * square((X[i] - mu) / Sl));
	}
	return(store_info);
}
# test
darcy_sel = double_normal(Length,mu,Sl,Sr)
lines(Length,darcy_sel,col = "red",lty = 2)


## The question that would be nice to answer with the simualtion study is what difference does it make on inference
## if you don't assume process error or, check error structure.