gluc = scan("glucose.dat")

# Hyperparameter values
a = 1; b = 1
mu0 = 120
tau2.0 = 200
sigma2.0 = 1000
nu0 = 10
n.iter = 15000

#x = rep(NA,n.iter)
theta.1 = rep(0,n.iter)
theta.2 = rep(0,n.iter)
sigma2.1 = rep(0,n.iter)
sigma2.2 = rep(0,n.iter)
p = rep(0,n.iter)
y.tilde = rep(0,n.iter)

theta.1[1] = mu0
theta.2[1] = mu0
sigma2.1[1] = sigma2.0
sigma2.2[1] = sigma2.0
p[1] = .5
x[1] = 1
y.tilde[1] = rnorm(1,theta.1[1],sqrt(sigma2.1[1]))

for(i in 2:n.iter){
  fc.p.x.1 = (p[i-1]*dnorm(gluc,theta.1[i-1],sqrt(sigma2.1[i-1]))) / 
    (p[i-1]*dnorm(gluc,theta.1[i-1],sqrt(sigma2.1[i-1])) + (1-p[i-1])*dnorm(gluc,theta.2[i-1],sqrt(sigma2.2[i-1])))
    #Generating actual draws from of x
  x.1 = 2-rbinom(length(fc.p.x.1),1,fc.p.x.1)
  
  i.1 = (x.1 == 1)
  n.1 = sum(i.1)
  y.1 = gluc[i.1]
  y.bar.1 = mean(y.1)
  s1 = var(y.1)
  
  i.2 = (x.1 == 2)
  n.2 = sum(i.2)
  y.2 = gluc[i.2]
  y.bar.2 = mean(y.2)
  s2 = var(y.2)
  # Posterior for p
  p[i] = rbeta(1,a+n.1,b+n.2)

  # Code breaks during these updates
  # From theta
  tau2.n.1 = 1 /((1/tau2.0) + n.1/sigma2.1[i-1])
  mu.n.1 = ((mu0/tau2.0) + (n.1*y.bar.1) / sigma2.1[i-1]) * tau2.n.1
  theta.1[i] = rnorm(1,mu.n.1,sqrt(tau2.n.1))
  
  tau2.n.2 = 1 /((1/tau2.0) + n.2/sigma2.2[i-1])
  mu.n.2 = ((mu0/tau2.0) + (n.2*y.bar.2) / sigma2.2[i-1]) * tau2.n.2
  theta.2[i] = rnorm(1,mu.n.2,sqrt(tau2.n.2))
  
  # From sigma
  nu.n.1 = nu0 + n.1
  sigma2.n.1 = (1/nu.n.1)*(nu0 * sigma2.0 + (n.1 - 1) * s1 + n.1*(y.bar.1 - theta.1[i-1])^2)
  sigma2.1[i] = 1 / rgamma(1,nu.n.1/2,nu.n.1*sigma2.n.1 / 2)
  
  nu.n.2 = nu0 + n.2
  sigma2.n.2 = (1/nu.n.2)*(nu0 * sigma2.0 + (n.2 - 1) * s2 + n.2*(y.bar.2 - theta.2[i-1])^2) 
  sigma2.1[i] = 1 / rgamma(1,nu.n.2/2,nu.n.2*sigma2.n.2 / 2)
  
  #x[i] = 2-rbinom(1,1,p[i])
  #if(x[i] == 1){
  #  y.tilde[i] = rnorm(1,theta.1[i],sqrt(sigma2.1[i]))
  #}else{
  #  y.tilde[i] = rnorm(1,theta.2[i],sqrt(sigma2.2[i]))
  #}
}

gluc = scan("glucose.dat")

# Hyperparameter values
a = 1; b = 1
mu0 = 120
tau2.0 = 200
sigma2.0 = 1000
nu0 = 10
n.iter = 15000

x = rep(NA,n.iter)
theta.1 = rep(NA,n.iter)
theta.2 = rep(NA,n.iter)
sigma2.1 = rep(NA,n.iter)
sigma2.2 = rep(NA,n.iter)
p = rep(NA,n.iter)
y.tilde = rep(NA,n.iter)

theta.1[1] = mu0
theta.2[1] = mu0
sigma2.1[1] = sigma2.0
sigma2.2[1] = sigma2.0
p[1] = .5
x[1] = 1
y.tilde[1] = rnorm(1,theta.1[1],sqrt(sigma2.1[1]))

samples = cbind(x,p,theta.1,theta.2,sigma2.1,sigma2.2,y.tilde)
colnames(samples) = c("")

## Shai's Code, sort of 
a = 1; b = 1
mu0 = 120
tau2.0 = 200
sigma2.0 = 1000
nu0 = 10
S = 15000

p = .5

samples =data.frame(matrix(0,ncol = 7, nrow = S))
colnames(samples) = c("x","p","theta.1","theta.2","sigma2.1","sigma2.2","ytilde")
samples[1,2:6] = c(p,mu0,mu0,sigma2.0,sigma2.0)

for(i in 1:(S-1)){
  FC.p.x.1 = (samples$p[i] * dnorm(gluc,samples$theta.1[i],sqrt(samples$sigma2.1[i]))) /
    (samples$p[i] * dnorm(gluc,samples$theta.1[i],sqrt(samples$sigma2.1[i])) + 
       (samples$p[i]) * dnorm(gluc,samples$theta.2[i],sqrt(samples$sigma2.2[i])))
  x = 2-rbinom(length(FC.p.x.1),1,FC.p.x.1)
  
  I.1 = (x==1)
  n.1 = sum(I.1)
  y.1 = gluc[I.1]
  y.bar.1 = mean(y.1)
  s.1 = var(y.1)
  
  I.2 = (x==2)
  n.2 = sum(I.2)
  y.2 = gluc[I.2]
  y.bar.2 = mean(y.2)
  s.2 = var(y.2)
  
  samples$p[i+1] = rbeta(1,a+n.1,b+n.2)
  
  tau2.n.1 = 1/(1/tau2.0 + n.1/samples$sigma2.1[i])
  mu.n.1 = tau2.n.1 * (mu0/tau2.0 + n.1*y.bar.1/samples$sigma2.1[i])
  samples$theta.1[i+1] = rnorm(1,mu.n.1,tau2.n.1)
  
  tau2.n.2 = 1/(1/tau2.0 + n.2/samples$sigma2.2[i])
  mu.n.2 = tau2.n.2 * (mu0/tau2.0 + n.2*y.bar.2/samples$sigma2.2[i])
  samples$theta.2[i+1] = rnorm(1,mu.n.2,tau2.n.2)
  
  nu.n.1 = nu0 + n.1
  sigma2.n.1 = (nu0 * sigma2.0 + (n.1-1) * s.1 + n.1*(y.bar.1-samples$theta.1[i])^2) / nu.n.1
  samples$sigma2.1[i+1] = 1 / rgamma(1,nu.n.1/2,nu.n.1*sigma2.n.1/2/2)
  
  nu.n.2 = nu0 + n.2
  sigma2.n.2 = (nu0 * sigma2.0 + (n.2-1) * s.2 + n.2*(y.bar.2-samples$theta.2[i])^2) / nu.n.2
  samples$sigma2.2[i+1] = 1 / rgamma(1,nu.n.2/2,nu.n.2*sigma2.n.2/2/2)
  
  samples$x[i] = 2-rbinom(1,1,samples$p[i])
  if(samples$x[i] == 1){
    samples$y.tilde[i] = rnorm(1,samples$theta.1[i],sqrt(samples$sigma2.1[i]))
  }else{
    samples$y.tilde[i] = rnorm(1,samples$theta.2[i],sqrt(samples$sigma2.2[i]))
  }
}