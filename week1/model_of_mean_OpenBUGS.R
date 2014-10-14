###############################################################
#        Model of the mean in JAGS from R                  #
###############################################################

rm(list=ls()) #clear your workspace
#set your working directory to the week1 folder in your github folder for this seminar.
setwd("C:\\Users\\Carissa2\\BayesCourse\\week1") 

if(basename(getwd())!="week1"){cat("Plz change your working directory. It should be 'week1'")}

### 5.2. Data generation
# Generate a sample of body mass measurements of male peregrines
y1000 <- rnorm(n = 1000, mean = 600, sd = 30) # Sample of 1000 birds

# Plot data
xlim = c(450, 750)
hist(y1000, col = 'grey', xlim = xlim, main = ' Body mass (g) of 1000 male peregrines')

### 5.3. Analysis using R
summary(lm(y1000 ~ 1))

### 5.4. Analysis using OpenBUGS
library(R2OpenBUGS) 

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

# Package all the stuff to be handed over to BUGS
# Bundle data
open.data <- list(mass = y1000, nobs = length(y1000))

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

# Start Gibbs sampler: Run model in OpenBUGS and save results in object called 'model1'
model1 <- bugs(data = open.data, inits = inits, parameters.to.save = params, model.file = "model.txt", 
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, DIC = TRUE, codaPkg=TRUE, 
working.directory = getwd())

ls()
model1					
names(model1)
str(model1)

model1 = read.bugs(model1) #Creates an mcmc list from a BUGS object
par(mfrow=c(3,1))
traceplot(model1)
densityplot(model1)
print(model1)

summary(lm(y1000 ~ 1)) #compare to least squares estimation

#Change MCMC settings - increase number of draws and burn-in
# MCMC settings
nc <- 3					# Number of chains
ni <- 10000				# Number of draws from posterior (for each chain)
nb <- 1000				# Number of draws to discard as burn-in
nt <- 1					# Thinning rate

model1 <- jags(data = jags.data, inits = inits, parameters.to.save = params, model.file = "model.txt", n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, DIC = TRUE, working.directory = getwd())

model1 = read.bugs(model1) 
traceplot(model1)
densityplot(model1)
print(model1)

#Highly informative priors
# Save a new BUGS/JAGS description of the model, this time with informative priors, to working directory
sink("model2.txt")
cat("
model {

# Priors
 population.mean ~ dnorm(500, 50)		# Normal parameterized by precision
 precision <- 1 / population.variance	# Precision = 1/variance
 population.variance <- population.sd * population.sd
 population.sd ~ dnorm(20,20)

# Likelihood
 for(i in 1:nobs){
    mass[i] ~ dnorm(population.mean, precision)
 }
}
",fill=TRUE)
sink()

ls()

#MCMC settings
nc <- 3					# Number of chains
ni <- 10000				# Number of draws from posterior (for each chain)
nb <- 1000				# Number of draws to discard as burn-in
nt <- 1					# Thinning rate

# Use same MCMCsettings as above and start Gibbs sampler: Run model in JAGS and save results in object called model2
model2 <- jags(data = jags.data, inits = inits, parameters.to.save = params, model.file = "model2.txt", n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, DIC = TRUE, working.directory = getwd())

model2 = read.bugs(model2) 
traceplot(model2)
densityplot(model2)
print(model2)

