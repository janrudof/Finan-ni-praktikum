library(Rlab)
library(combinat)

S_0 <- 50
u <- 1.05
d <- 0.95
U <- 5
T <- 3
R <- 0.03

#1.naloga
#a)

pot_1 <- c(50.00, 52.50, 49.88, 52.37, 49.75, 52.24)
pot_2 <- c(50.00, 52.50, 55.12, 57.88, 60.78, 63.81)
pot_3 <- c(50.00, 47.50, 49.88, 47.38, 45.01, 42.76)
pot_4 <- c(50.00, 47.50, 45.12, 47.38, 49.75, 52.24)
pot_5 <- c(50.00, 52.50, 49.88, 52.37, 54.99, 57.74)

#b)
izplacilo <- function(vrsta, T, type){
  if (type == 'call'){
    m <- - max(vrsta[1:T]) + max(vrsta[(T+1):length(vrsta)])
  } else if (type == 'put'){
    m <- - min(vrsta[1:T]) + min(vrsta[(T+1):length(vrsta)])
  }
  if (m > 0){
    return(m)
  } else {
    return(0)
  }
}

#dodatek k nalogi a)

izplacilo_X1 <- izplacilo(pot_1, T, "call")
izplacilo_X2 <- izplacilo(pot_2, T, "call")
izplacilo_X3 <- izplacilo(pot_3, T, "call")
izplacilo_X4 <- izplacilo(pot_4, T, "call")
izplacilo_X5 <- izplacilo(pot_5, T, "call")

izplacilo_Y1 <- izplacilo(pot_1, T, "put")
izplacilo_Y2 <- izplacilo(pot_2, T, "put")
izplacilo_Y3 <- izplacilo(pot_3, T, "put")
izplacilo_Y4 <- izplacilo(pot_4, T, "put")
izplacilo_Y5 <- izplacilo(pot_5, T, "put")

#2.naloga
#a)
binomski <- function(S_0, u, d, U, R, T, type){
  q <- (1+R-d)/(u-d)
  E <- 0
  vrsta <- c(S_0)
  kocka <- hcube(c(rep(2,U)), translation = -1)
  for (i in 1:(2^U)) {
    for (j in 1:U){
      vrsta[j+1] <- vrsta[j]*(u*kocka[i,j] + d*(1-kocka[i,j]))
    }
    gor <- rowSums(kocka)[i]
    izplacilo <- izplacilo(vrsta, T, type)
    E <- E + izplacilo*q^(gor)*(1-q)^(U-gor)
  }
  c <- E/(1+R)^(U)
  return(c)
}

binom_X <- binomski(S_0, u, d, U, R, T, "call")
binom_Y <- binomski(S_0, u, d, U, R, T, "put")


#b)
monte <- function(S0, u, d, U, R, T, type, N){
  q <- (1+R-d)/(u-d)
  vrednosti <- c(0)
  vrsta <- c(S0)
  for (i in 1:N){
    kombinacija <- rbinom(U,1,q)
    for (j in 1:U) {
      vrsta[j+1] <- vrsta[j]*(u*kombinacija[j] + d*(1-kombinacija[j]))
    }
    vrednosti[i] <- izplacilo(vrsta, T, type)
  }
  c <- mean(vrednosti)/(1+R)^U
  return(c)
}

N1 <- 10
N2 <- 100
N3 <- 1000 

vrednost_N1 <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N1)
vrednost_N2 <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N2)
vrednost_N3 <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N3)

#3.naloga
#a)

M <- 100
ocene_N1 <-  c(0)
ocene_N2 <-  c(0)
ocene_N3 <-  c(0)

for (i in 1:M){
  ocene_N1[i] <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N1)
  ocene_N2[i] <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N2)
  ocene_N3[i] <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N3)
}

odklon1 <- var(ocene_N1)^0.5
odklon2 <- var(ocene_N2)^0.5
odklon3 <- var(ocene_N3)^0.5

histogram1 <- hist(ocene_N1, 
                   main = 'Monte Carlo, N = 10',
                   xlim = c(0,11),
                   col = 'orange',
                   xlab = 'Premija')
abline(v = mean(ocene_N1), col = 'purple', add = TRUE, lwd = 2)
abline(v = bin_pov, col = 'green', add = TRUE)
legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('purple', 'green'),
       lwd = 1:1,
       bty = 'n')
arrows(mean(ocene_N1), 0, mean(ocene_N1) + odklon1, 0, add = TRUE)
arrows(mean(ocene_N1), 0, mean(ocene_N1) - odklon1, 0, add = TRUE)

histogram2 <- hist(ocene_N2, 
                   main = 'Monte Carlo, N = 100',
                   xlim = c(0,11),
                   col = 'orange',
                   xlab = 'Premija')
abline(v = mean(ocene_N2), col = 'purple', add = TRUE, lwd = 2)
abline(v = bin_pov, col = 'green', add = TRUE)
legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('purple', 'green'),
       lwd = 1:1,
       bty = 'n')
arrows(mean(ocene_N2), 0, mean(ocene_N2) + odklon2, 0, add = TRUE)
arrows(mean(ocene_N2), 0, mean(ocene_N2) - odklon2, 0, add = TRUE)

histogram3 <- hist(ocene_N3, 
                   main = 'Monte Carlo, N = 1000',
                   xlim = c(0,11),
                   col = 'orange',
                   xlab = 'Premija')
abline(v = mean(ocene_N3), col = 'purple', add = TRUE)
abline(v = bin_pov, col = 'green', add = TRUE)
legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('purple', 'green'),
       lwd = 1:1,
       bty = 'n')
arrows(mean(ocene_N3), 0, mean(ocene_N3) + odklon3, 0, add = TRUE)
arrows(mean(ocene_N3), 0, mean(ocene_N3) - odklon3, 0, add = TRUE)
