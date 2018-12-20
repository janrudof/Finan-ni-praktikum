library(readr)

#1.naloga 
#a)

platina <- read.csv("Platina.csv")
platina1 <- platina[c(127:1), 5]
platina1 <- data.frame(platina1)
colnames(platina1) <- c("Close")

#b)

casovna_vrsta <- ts(platina1)
graf_platina <- ts.plot(casovna_vrsta, xlab = "Leto", ylab = "EUR", main = "Graf vrednosti platine")

#2.naloga
#a)

G <- function(vrsta, k){
  dolzina <- length(vrsta)
  glajenje_vrednosti <- c()
  for (i in 1:(dolzina-k)) {
    glajenje_vrednosti[i] <- sum(vrsta[i:(k+i-1)])/k
  }
  zglajena_vrsta <- ts(glajenje_vrednosti)
  return(zglajena_vrsta)
}

#b)

zglajena_vrsta_8 <- G(casovna_vrsta, 8)
dolzina <- length(casovna_vrsta)
napoved_8 <- sum(casovna_vrsta[(dolzina - 8 + 1):dolzina])/8

#c)

graf2 <- ts.plot(casovna_vrsta, zglajena_vrsta_8, xlab = "Leto", ylab = "EUR", main = "Drsece povprecje", lwd = 2:1, col = 1:10)

#d)

SKN <- function(vrsta, zglajena_vrsta, k) {
  T <- length(vrsta)
  delna_vsota <- 0
  for (i in (k+1): T) {
    delna_vsota <- delna_vsota + (vrsta[i] - zglajena_vrsta[i-k])^2
  }
  napaka <- (1/(T-k))*delna_vsota
  return(napaka)
}

napaka_8 <- SKN(casovna_vrsta, zglajena_vrsta_8, 8)

#e)

zglajena_vrsta_16 <- G(casovna_vrsta, 16)
napoved_16 <- sum(casovna_vrsta[(dolzina-16+1):dolzina])/16
napaka_16 <- SKN(casovna_vrsta, zglajena_vrsta_16, 16)

zglajena_vrsta_24 <- G(casovna_vrsta, 24)
napoved_24 <- sum(casovna_vrsta[(dolzina-24+1):dolzina])/24
napaka_24 <- SKN(casovna_vrsta, zglajena_vrsta_24, 24)

par(mfrow = (c(2,2)))
graf3 <- ts.plot(casovna_vrsta, zglajena_vrsta_8, xlab = "Leto", ylab = "EUR", main = "Drsece povprecje reda 8",lwd = 2:1, col = 1:10)
graf4 <- ts.plot(casovna_vrsta, zglajena_vrsta_16,xlab = "Leto", ylab = "EUR", main = "Drsece povprecje reda 16", lwd = 2:1, col = 1:10)
graf5 <- ts.plot(casovna_vrsta, zglajena_vrsta_24,xlab = "Leto", ylab = "EUR", main = "Drsece povprecje reda 24", lwd = 2:1, col = 1:10)
par(mfrow = (c(1,1)))

#3,naloga
#a)

EG <- function(vrsta, alpha){
  glajena_vrsta <- c(vrsta[1])
  dolzina <- length(vrsta)
  for (i in 2:dolzina){
    glajena_vrsta[i] <- alpha*vrsta[i] + (1-alpha)*glajena_vrsta[i-1]
  }
  return(glajena_vrsta)
}

#b)

alpha <- 0.3
zglajena_vrsta_alpha <- EG(casovna_vrsta, alpha)
naslednji_dan <- zglajena_vrsta_alpha[1]

graf6 <- ts.plot(casovna_vrsta, ts(zglajena_vrsta_alpha), xlab = "Leto", ylab = "EUR", main = "Eksponentno glajenje", lwd = 2:1, col = 1:10)

#c)

SKN <- function(vrsta, alpha) {
  dolzina <- length(vrsta)
  napaka <- 0
  glajena <- EG(vrsta, alpha)
  for (i in 1:(dolzina-1)) {
    napaka <- napaka + (vrsta[i+1] - glajena[i+1])^2
  }
  return(napaka/(dolzina -1))
}

optimalni_alpha <- optimize(SKN, c(0, 1), vrsta = casovna_vrsta)

#d)

zglajena_vrsta_optimalni_alpha <- EG(casovna_vrsta, optimalni_alpha$minimum)

graf7 <- ts.plot(casovna_vrsta, ts(zglajena_vrsta_optimalni_alpha), ylab = "EUR", main = "Eksponentsno glajenje, minimalen MSE",lwd = 3:1, col = 10:1)


