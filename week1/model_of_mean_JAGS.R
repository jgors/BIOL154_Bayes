###############################################################
#        Model of the mean in JAGS from R                  #
###############################################################



### 5.2. Data generation
# Generate two samples of body mass measurements of male peregrines
y10 <- rnorm(n = 10, mean = 600, sd = 30) # Sample of 10 birds
y1000 <- rnorm(n = 1000, mean = 600, sd = 30) # Sample of 1000 birds

# Plot data
xlim = c(450, 750)
par(mfrow = c(2,1))
hist(y10, col = 'grey ', xlim = xlim, main = 'Body mass (g) of 10 male peregrines')
hist(y1000, col = 'grey', xlim = xlim, main = ' Body mass (g) of 1000 male peregrines')



### 5.3. Analysis using R
summary(lm(y1000 ~ 1))



### 5.4. Analysis using WinBUGS
library(coda); library(rjags); library(R2jags) #load the required libraries

rm(list=ls()) #resets your working directory
setwd("/Users/nina/Documents/BIOL154_Bayes/week1") # May have to adapt that
if(basename(getwd())!="week1"){cat("Plz change your working directory. It should be 'week1' in the BIOL154_Bayes github repo")}

# Save BUGS/JAGS description of the model to working directory
sink("model.txt")
cat("
model {

# Priors
 population.mean ~ dunif(0,5000)		# Normal parameterized by precision
 precision <- 1 / population.variance	# Precision = 1/variance
 population.variance <- population.sd * population.sd
 population.sd ~ dunif(0,100)

# Likelihood
 for(i in 1:nobs){
    mass[i] ~ dnorm(population.mean, precision)
 }
}
",fill=TRUE)
sink()

# Package all the stuff to be handed over to WinBUGS
# Bundle data
jags.data <- list(mass = y1000, nobs = length(y1000))

# Function to generate starting values
inits <- function()
  list (population.mean = rnorm(1,600), population.sd = runif(1, 1, 30))

# Parameters to be monitored (= to estimate)
params <- c("population.mean", "population.sd", "population.variance")

# MCMC settings
nc <- 3					# Number of chains
ni <- 1000				# Number of draws from posterior (for each chain)
nb <- 1					# Number of draws to discard as burn-in
nt <- 1					# Thinning rate

# Start Gibbs sampler: Run model in WinBUGS and save results in object called out
out <- jags(data = jags.data, inits = inits, parameters.to.save = params, model.file = "model.txt", 
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, DIC = TRUE, working.directory = getwd())

ls()

out					# Produces a summary of the object

names(out)

str(out)

hist(out$summary[,8])			# Rhat values in the eighth column of the summary
which(out$summary[,8] > 1.1)		# None in this case

par(mfrow = c(3,1))
matplot(out$sims.array[1:999,1:3,1], type = "l")
matplot(out$sims.array[,,2] , type = "l")
matplot(out$sims.array[,,3] , type = "l")

par(mfrow = c(3,1))
matplot(out$sims.array[1:20,1:3,1], type = "l")
matplot(out$sims.array[1:20,,2] , type = "l")
matplot(out$sims.array[1:20,,3] , type = "l")

par(mfrow = c(3,1))
hist(out$sims.list$population.mean, col = "grey")
hist(out$sims.list$population.sd, col = "blue")
hist(out$sims.list$population.variance, col = "green")

par(mfrow = c(1,1))
plot(out$sims.list$population.mean, out$sims.list$population.sd)

pairs(cbind(out$sims.list$population.mean, out$sims.list$population.sd, out$sims.list$population.variance))

summary(out$sims.list$population.mean)
summary(out$sims.list$population.sd)
sd(out$sims.list$population.mean)
sd(out$sims.list$population.sd)

summary(lm(y1000 ~ 1))




model1.mcmc <-as.mcmc(out)
xyplot(model1.mcmc)
densityplot(model1.mcmc)
print(out)