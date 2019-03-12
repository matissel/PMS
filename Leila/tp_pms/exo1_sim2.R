theta <- 1000
n0 <- 50
p <- integer(20)
for(n in 1:20){
  for(i in 1:1000){
    t <- sum(rbinom(n,1,n0/theta))
    if(t == 0){
      p[n] <- p[n] + 1/1000
    }
  }
}
plot(p, xlab="n", ylab="P(theta_n = inf)")
abline(h=0.5)
abline(v=13.5)