# FFFT
A fast failure forecast tool written in R.  

Classical approaches to failure forecasting using the Weibull or the Log-normal distribution utilize median rank regression (MRR) or maximum likelihood estimation (MLE) to estimate distribution parameters. These methods only allow for calculation of confidence bounds, which, in cases where a large proportion of data are censored, are extremely broad. No distribution information about the parameters themselves can be obtained in this way, which renders predictions based on MRR or MLE shallow at best and strongly biased at worst. In this article, we discuss the advantages and results of incorporating prior engineering knowledge, and the lack thereof, into models for the prediction of engine failures in a Bayesian context. We also make the argument, that many different sources of uncertainty are equivalent to prior Information about the distribution parameters for Bayesian Inference. The goal of this article is to unify classical Weibull forecasting with modern Bayesian inference and present a robust framework, to automate these procedures. Arguments on how to remove undocumented and subjective engineering decisions from forecasting are presented along the way. 
The currently incomplete paper that explains the theory is available here: 
https://github.com/benearnthof/bayesff
