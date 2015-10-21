trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

plot(x,y, main="Test Data")

# We need to derive the posterior distribution at the first step, but before that, both likelihood function and prior distribution are required given the reason that the posterior function is the product of both of them.

# Derive the likelihood function
likelihood <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")


# Prior distribution
prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=0, max=10, log = T)
  bprior = dnorm(b, sd = 5, log = T)
  sdprior = dunif(sd, min=0, max=30, log = T)
  return(aprior+bprior+sdprior)
}

# The product of likelihood function and prior distribution is the posterior distribution, while in logarithm, the sum is.
posterior <- function(param){
  return (likelihood(param) + prior(param))
}


######## Metropolis algorithm ################
# Choose a probability distribution as the proposal function, and in this case, we choose a normal distribution, with the number of random generation to be 3.
proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

run_metropolis_MCMC <- function(startvalue, iterations){
  # We choose chain to be a matrix of (iterations+1)*3
  chain = array(dim = c(iterations+1,3))
  # Set the first row of chain to be the initial value
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    # Acceptance probability is named as 'probab'
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    # Now we set a random number from uniform distribution. If it is less than acceptance probability, we accept it as the new 'proposal',
    if (runif(1) < probab){
      chain[i+1,] = proposal
      # otherwise, we reject the new value and still set the old value as the one we are going to use.
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}
# set the initial value, and with the expected iterations, we can have the matrix 'chain'. 
startvalue = c(4,0,10)
chain = run_metropolis_MCMC(startvalue, 10000)

burnIn = 5000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))

### Summary: #######################
# Posterior estimate for slope(a)
par(mfrow = c(2,3))
hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior of a", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]))
abline(v = trueA, col="red" )
# Posterior estimate for slope(b)
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]))
abline(v = trueB, col="red" )
# Posterior estimate for standard deviation of the error
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),3]) )
abline(v = trueSd, col="red" )
# Markov Chain of parameter value of (a)
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a", )
abline(h = trueA, col="red" )
# Markov Chain of parameter value of (b)
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", )
abline(h = trueB, col="red" )
# Markov Chain of parameter value of standard deviation of the error
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd", )
abline(h = trueSd, col="red" )

# for comparison:
summary(lm(y~x))

