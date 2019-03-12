 
#Première stratégie
n0 = 50
theta = 1000

#1.1
#On simule un échantillon pour n poissons pêchés
n = 10000
x = rbinom(n, 1, n0/theta)
x_n = mean(x)

var_n = var(x)
s = sum(x)

#1.4
alpha <- c(0.01, 0.05, 0.1, 0.2)
binf_exacte = n0*(1+(n-s)/(s+1)*qf(alpha/2, 2*(n-s), 2*(s+1)))
bins_exacte = n0*(1+(n-s+1)/s*qf(1-alpha/2, 2*(n-s+1), 2*s))
binf_asym = n0*1/((s/n)+qnorm(1-alpha/2)*sqrt((s/n)*(1-s/n)/n))
bins_asym = n0*1/((s/n)-qnorm(1-alpha/2)*sqrt((s/n)*(1-s/n)/n))

sprintf("Un intervalle exact de seuil %f pour theta est [%f; %f]", alpha, binf_exacte, bins_exacte)
sprintf("Un intervalle asymptotique de seuil %f pour theta est [%f; %f]", alpha, binf_asym, bins_asym)
