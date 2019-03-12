m <- 300
n <- 1000
p <- .3
moy <- integer(m)
for (i in 1:m) {
    x <- rbinom(n, 1, p)
    moy[i] <- sum(x)/n
}
#verification loi faible des grands nombres
eps <-.02
c<-0
for(i in 1:m){
  if(abs(moy[i]-p) > eps){
    c <- c + 1
  }
}
sprintf("pour epsilon = %f la proportion de fois ou la moyenne empirique s'eloigne de l'esperance est : %f (n = %d)", eps, c/m, n)
#verification centrale limite 
k <- as.integer(1 + log(m))
moy <- sort(moy)
a <- integer(k+1)
a[1] <- moy[1] - 0.025 * (moy[m] - moy[1])
a[k+1] <- moy[m] + 0.025 * (moy[m] - moy[1])
h <- (a[k+1] - a[1])/k
for (i in 2:k) {
    a[i] = a[i-1] + h
}
hist(moy, prob = T, breaks = a, main = "histogramme de la simulation")
plot(sort(moy), qnorm(seq(1:m)/m))