library(actuar)

#1.naloga
#a)
vzorec <- scan("vzorec3.txt")

histogram <- hist(vzorec, main="Histogram", xlab = "Višina odškodnine", ylab= "Frekvenca")

#b)

Y = mde(vzorec, pweibull, start =list(shape = 1, scale = 1), measure = "CvM")

k = Y$estimate[1]
lambda = Y$estimate[2]

#c)
hist(vzorec, main="Histogram", xlab = "Višina odškodnine", ylab= "Gostota", probability = TRUE)
curve(dweibull(x, k, lambda), add = TRUE, col = "green")

#d)

upanje_Y <- lambda*gamma(1 + 1/k)
upanje_N <- 25/2
disperzija_Y <- lambda^2*gamma(1 + 2/k) - upanje_Y^2
disperzija_N <- 25 * 1/2 * 1/2
upanje_YY <- upanje_Y^2 + disperzija_Y

upanje_S <- upanje_Y * upanje_N
disperzija_S <- disperzija_Y * upanje_N + disperzija_N * upanje_YY

#2.naloga
#a in b)
h <- 0.25
n <- 35

diskretno <- discretize(pweibull(x, k, lambda), 0, h*n, step = h)
plot(diffinv(diskretno))
plot(stepfun(seq(0, (n-1)*h, by = h), diffinv(diskretno)), main = "Weibullova porazdelitev", xlab = "x", ylab = "Porazdelitvena funkcija")
curve(pweibull(x, k, lambda), add = TRUE, col = 'red')
legend("bottomright", legend=c('diskretizacija', 'Weibullova porazdelitev'), col=c('black','red'),lty=1:1, cex=0.8)

#c)
porazdelitvena_S <- aggregateDist(method = 'recursive',
                                  model.freq = 'binom',
                                  model.sev = diskretno,
                                  size = 25,
                                  prob = 1/2,
                                  x.scale = h,
                                  maxit = 1000000,
                                  tol = 0.000025)
plot(porazdelitvena_S)

#d)

upanje_S_1 <- mean(porazdelitvena_S)
disperzija_S_1 <- sum(diff(porazdelitvena_S) * knots(porazdelitvena_S)^2) - upanje_S_1^2

#3.naloga
#a)

simulacija_N <- rbinom(10000, 25, 1/2)
simulacija_S <- c()

for (i in 1: 10000){
  simulacija_S[i] <- sum(rweibull(simulacija_N[i], k, lambda))
  }

#b)

upanje_S_2 <- mean(simulacija_S)
disperzija_S_2 <- var(simulacija_S)

#c)
plot(ecdf(simulacija_S),
     col = 'red',
     add = TRUE)
legend('bottomright',
       legend = c("Panjarjev algoritem","Monte Carlo simulacija"),
       col = c('black', 'red'),
       lty=1:1, 
       cex=0.9)
