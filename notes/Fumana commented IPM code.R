##
### Commented R code for IPM analyses done in Dahlgren et al., submitted to Ecology May 2015
##
#
# Analyses were run in R v. 3.0.0, and using packages gamm4 v. 0.1-6, lme4 v. 0.999999-2 and mgcv v. 1.7-22.

###
## load gamm package 
library(gamm4)

###vital rate gamms:
##k=3 puts a cap on the maximum wiggliness of the (default) "thin-plate splines" (lowest possible number of 'knots')
##sum.precX are summer precipitation in years t and t+1 (for simplicitly/readability, the temperature variables are not included here)
##prop.dens.t is plot density per year (as proportional to maximum observed density)
##random factors are included to account for that climatic variables vary only among years, and density across plots
##individual id was not included as a grouping variable to reduce model complexity
##all variables are contained in a data frame object named 'dat'

frutmod <-
  gamm4(
    frut1 ~ s(log.sizet, k = 3) + s(sum.prec.t1, k = 3) + s(prop.dens.t, k = 3),
    random = ~ (1 | yrt) + (1 | Plot),
    family = poisson,
    data = dat
  )

survmod <-
  gamm4(
    survt.t1 ~ s(log.sizet, k = 3) + s(sum.prec.t, k = 3) + s(sum.prec.t1, k = 3) + s(prop.dens.t, k = 3),
    random = ~ (1 | yrt) + (1 | Plot),
    family = binomial,
    data = dat
  )

sizemod <-
  gamm4(
    log.sizet1 ~ s(log.sizet, k = 3) + s(sum.prec.t, k = 3) + s(sum.prec.t1, k = 3) + s(prop.dens.t, k = 3),
    random = ~ (1 | yrt) + (1 | Plot),
    family = gaussian,
    data = dat
  )

flowmod <-
  gamm4(
    flowt1 ~ s(log.sizet, k = 3) + s(sum.prec.t, k = 3) + s (sum.prec.t1, k = 3) + s(prop.dens.t, k = 3),
    random = ~ (1 | yrt) + (1 | Plot),
    family = binomial,
    data = dat
  )



## IPM ----

########Vital rate functions-------------------------------------------------------
## the generic predict() function is used to get vital rate components for the kernel

## the plogis() function is used to back transform predictions from logit in the logistic regressions
## exp() to back-transform the log-transformed seed numbers in the poisson regression of seed numbers

###### Probability of survival
sx <- function(x, prect, prect1, dens) {
  plogis(predict(
    survmod$gam,
    data.frame(
      log.sizet = x,
      sum.prec.t = prect,
      sum.prec.t1 = prect1,
      prop.dens.t = dens
    )
  ))
}

###### Growth function
gyx <- function(y, x, prect, prect1, dens) {
  M <-
    predict(
      sizemod$gam,
      data.frame(
        log.sizet = x,
        sum.prec.t = prect,
        sum.prec.t1 = prect1,
        prop.dens.t = dens
      )
    )
  return(dnorm(y, mean = M, sd = 0.359812))
}

###### Fertility function 
fx <- function(x, prect, prect1, dens) {
  #probability of flowering:
  p.flow <-
    plogis(predict(
      flowmod$gam,
      data.frame(
        log.sizet = x,
        sum.prec.t = prect,
        sum.prec.t1 = prect1,
        prop.dens.t = dens
      )
    ))
  
  #number of fruits per flowering ind:
  n.fruits <-
    exp(predict(
      frutmod$gam,
      data.frame(
        log.sizet = x,
        sum.prec.t = prect,
        sum.prec.t1 = prect1,
        prop.dens.t = dens
      )
    ))
  
  return(p.flow * n.fruits)
}

###### Seedling establishment function
## This could also be dependent on density and environment
sey <- function(x) {
  n.seedlings <- 0.3
  seedl.sizes <-
    dnorm(y, -2.309, 0.9294483) / (1 - pnorm(-5.121157, -2.309, 0.9294483))
  return(n.seedlings * seedl.sizes)
}

################# kernel specification ##################
##as in Ellner and Rees 2006

# number of classes, or points for midpoint rule approximation  
n.size = 200

# minimum and maximum sizes 
minsize = log(0.9) + min(dat$log.sizet, na.rm = TRUE)
maxsize = log(1.1) + max(dat$log.sizet, na.rm = TRUE) 

L <- minsize #<- 3
U <- maxsize #<- 25
n <- n.size 
b <- L + c(0:n) * (U - L) / n
y <- 0.5 * (b[1:n] + b[2:(n+1)]) #meshpoints
h <- y[2] - y[1]


###
## Function to get different matrices/kernels depending on environment and density
#

K.fnc <- function(y, prect, prect1, dens) {
  #The growth kernel
  G <- h * outer(y,
                 y,
                 gyx,
                 prect = prect,
                 prect1 = prect1,
                 dens = dens)
  ####the following to make G sum to 1.0 in the event that there is some 'eviction' of individuals
  for (i in 1:(n.size / 2))
    G[1, i] <- G[1, i] + 1 - sum(G[, i])
  for (i in (n.size / 2 + 1):n.size)
    G[n.size, i] <- G[n.size, i] + 1 - sum(G[, i])
  
  
  #The survival-growth kernel
  S <- sx(y,
          prect = prect,
          prect1 = prect1,
          dens = dens)
  Ps <- sweep(G, MARGIN = 2, S, '*')
  P <- matrix(Ps, nrow = n.size)
  
  #Fecundity
  F <- fx(y,
          prect = prect,
          prect1 = prect1,
          dens = dens) * S
  
  #Seedling establishment
  SE <- h * sey(y)
  
  #Putting it all together in the complete IPM kernel, including transitions to and from a discrete seed class
  M <- matrix(0, nrow = n.size + 1, ncol = n.size + 1)
  M[2:201, 2:201] <- P
  M[1, 2:201] <- F
  M[2:201, 1] <- SE
  
  return(M)
}



## Using the IPM ----												   


## Getting lambda and elasticities of the kernel, e.g.:
K <- K.fnc(y, prect = 50, prect1 = 50, dens = 0.5)

lambda <- as.double(eigen(K)$values[1])
stable.dist <-
  as.double(eigen(K)$vectors[, 1] / sum(eigen(K)$vectors[, 1]))
V <- Conj(solve(eigen(K)$vectors))
repro.val <- abs(V[1, ] / V[1, 1])
sens = outer(as.vector(repro.val), as.vector(stable.dist)) / sum(repro.val *
                                                                   stable.dist)
elas = sens * K / lambda


###Projecting environmentally-dependent population growth over the observed years:----

# the observed amounts of precipitation per study year:
precip <-
  c(61.80, 50.00, 68.15, 67.10, 38.75, 40.90, 70.65, 17.65,
    46.45, 22.35, 49.85, 61.05, 28.90, 26.25, 98.30, 28.25,
    62.25, 74.25, 93.75, 56.50, 59.95, 42.95, 53.00)				

## the resulting population dynamics according to the model:
# define a population vector:
Nt <- rep(0, n.size + 1)

# start with population distribution in 1985
Nt[2:200] <- hist(dat$log.sizet[dat$yrt == 1985], breaks = y, plot = F)$counts

# adding the number of seeds to start with (equal to number of individuals)
Nt[1] <- sum(Nt)

# store this as the year 0 vector
Nt0 <- Nt

# for storing projection kernels
K.list <- list()

##the year with the highest pop dens, used to calculate proportional density during simulations
max.pop.dens <- sum(exp(dat$log.sizet)[dat$yrt == 2009]) 		
pop.size <- pop.dens <- rep(NA, 22) 				##for storing results

for(yr in 1:22) {
  pop.size[yr] <- sum(Nt[2:(n.size + 1)])
  # Density is calculated from population size (n.size) and physical size (exp(y))
  pop.dens[yr] <- sum(Nt[2:(n.size + 1)] * exp(y)) / max.pop.dens
  K <-
    K.fnc(
      y = y,
      prect = precip[yr],
      prect1 = precip[yr + 1],
      dens = pop.dens[yr]
    )
  Nt1 <- K %*% Nt
  Nt <- Nt1
  K.list[[yr]] <- K
} 

## Plotting the results

win.graph()
par(mfrow = c(1, 2))
plot(1:22, pop.size) ## Population size (number of inds)
lines(1:22, pop.size)
plot(1:22, pop.dens) ## Total (proportional) density (based on summed individual lengths)
lines(1:22, pop.dens)


######### Comparing "realistic" density-explicit and "unrealistic" density-implicit projections----

n.runs <- 10000							## number of model iterations
n.yrs <- 30							## number of modeled years
Nt <- Nt0							## we start with the observed population distribution

env.probs <- rep(1, 22) #for randomly sampling environmental states
env.probs2 <- env.probs #for doubled probability of encountering environmental state 3
env.probs2[3] <- 2
env.probs <- env.probs / sum(env.probs)
env.probs2 <- env.probs2 / sum(env.probs2)

probs <- env.probs # change to env.probs2 to double the risk of drawing the drought-year

env <- matrix(NA, nrow = n.runs, ncol = years)
for (run in 1:n.runs)
  for (yr in 1:years)
    env[run, yr] <- sample(1:22, 1, prob = probs)



### Density-implicit kernels for each environment are gotten from K.list----
##  Sampling randomly from these kernels and calculate to calculate extinction risk (e.g. Morris and Doak 2002):
#

ext.yn <- matrix(1,nrow=n.yrs,ncol=n.runs)			## for storing extinction events

for(run in 1:n.runs) {
	Nt <- Nt0
	for(yr in 1:n.yrs) { 
		ext.yn[yr, run] <- 0    
		K <- K.list[[env[run,yr]]] 			## one of the randomly ordered environmental states 
		Nt <- K %*% Nt 	## model iteration

		if(sum(Nt[2:n.size+1]) < 10) {	## check whether extinction has occurred, and leave the for loop if so
		ext.yn[yr, run] <- 1 # i.e. if non-seed population size is lower than the "quasi-extinction threshold" of 10 
		break
		}							
	}						
}

## Estimate and plot extinction probability over time

cum.prob <- 1:n.yrs
for (i in 1:n.yrs) {
  cum.prob[i] <- mean(ext.yn[i,])
}
win.graph()
plot(1:n.yrs,
     cum.prob,
     type = "l",
     xlab = "Year",
     ylab = "Cumulative probability of quasi-extinction")


##
##### More realistic "density-explicit" simulation (this loop runs very slow due to calls to the K.fnc function):
##

ext.yn2 <- matrix(1, nrow = n.yrs, ncol = n.runs)			

for(run in 1:n.runs) {
  Nt <- Nt0
  for (yr in 1:n.yrs) {
    ext.yn2[yr, run] <- 0
    current.density <-
      sum(Nt[2:(n.size + 1)] * exp(y)) / max.pop.dens
    K <-
      K.fnc(
        y = y,
        prect = precip[env[run, yr]],
        prect1 = precip[env[run, yr] + 1],
        dens = current.density
      )
    Nt <- K %*% Nt
    if (sum(Nt[2:n.size + 1]) < 10) {
      ext.yn2[yr, run] <- 1
      break
    }
  }
}


## Adding the density-explicit curve to the plot
cum.prob <- 1:n.yrs
for (i in 1:n.yrs) {
  cum.prob[i] <- mean(ext.yn2[i, ])
}
lines(1:n.yrs, cum.prob)

