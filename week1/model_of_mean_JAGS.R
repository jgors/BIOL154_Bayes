###############################################################
#        Model of the mean in JAGS from R                  #
###############################################################

rm(list=ls()) #clear your workspace
#set your working directory to the week1 folder in your github folder for this seminar.
setwd("/Users/nina/Documents/BIOL154_Bayes/week1") #You'll have to adapt that

if(basename(getwd())!="week1"){cat("Plz change your working directory. It should be 'week1'")}


### 5.2. Data generation
# Generate a sample of body mass measurements of male peregrines
y1000 <- rnorm(n = 1000, mean = 600, sd = 30) # Sample of 1000 birds

# Plot data
xlim = c(450, 750)
hist(y1000, col = 'grey', xlim = xlim, main = ' Body mass (g) of 1000 male peregrines')

### 5.3. Analysis using R
summary(lm(y1000 ~ 1))


### 5.4. Analysis using JAGS
library(coda); library(rjags); library(R2jags) #load the required libraries

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

# Package all the stuff to be handed over to JAGS
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

# Start Gibbs sampler: Run model in JAGS and save results in object called 'model1'
model1 <- jags(data = jags.data, inits = inits, parameters.to.save = params, model.file = "model.txt", 
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, DIC = TRUE, working.directory = getwd())

ls()
model1					
names(model1)
str(model1)

model1.mcmc <-as.mcmc(out)
xyplot(model1.mcmc)
densityplot(model1.mcmc)
print(model1)

summary(lm(y1000 ~ 1)) #compare to least squares estimation