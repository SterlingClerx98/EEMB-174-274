# Shiny app for symptom inferrence
This app performs a simple Bayesian calculation to infer
the probability that an individual has covid given that 
they are experiencing cold-like symptoms (i.e. a runny nose.)
Covid patients don't often have a runny nose, but sometimes 
do. We'll set the probability of cold symptoms given covid
at 25% for convenience. 

People who do not have covid sometimes have colds, and if 
they have a cold then they have cold symptoms. This app 
plots the inferred probabiilty of covid given observed
cold symptoms as a function of the frequency of colds
in the population as a whole. 
