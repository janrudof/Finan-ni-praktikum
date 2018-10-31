library(readxl)
library(tidyr)
library(dplyr)
library(gsubfn)
library(rvest)
library(knitr)
library(readr)

#Prva naloga
#1.a) uvoz podatkov

podatki_2012 <- read.csv("hist_EURIBOR_2012.csv")
podatki_2013 <- read.csv("hist_EURIBOR_2013.csv")
podatki_2014 <- read.csv("hist_EURIBOR_2014.csv")

#1.b) tabela podatkov

podatki_2012_pravilna <- podatki_2012 %>% select(X, X02.01.2012, X01.02.2012, X01.03.2012, X02.04.2012, X02.05.2012, X01.06.2012, X02.07.2012, X01.08.2012, X03.09.2012, X01.10.2012, X01.11.2012, X03.12.2012)
podatki_2012t <- t(podatki_2012_pravilna)
colnames(podatki_2012t) <- podatki_2012t[1, ]
podatki_2012t <- podatki_2012t[-1, ]
podatki_2012t <- podatki_2012t[ ,c(1, 2, 4, 5, 6, 9, 12, 15)]

podatki_2013_pravilna <- podatki_2013 %>% select(X, X02.01.2013, X01.02.2013, X01.03.2013, X02.04.2013, X02.05.2013, X03.06.2013, X01.07.2013, X01.08.2013, X02.09.2013, X01.10.2013, X01.11.2013, X02.12.2013)
podatki_2013t <- t(podatki_2013_pravilna)
colnames(podatki_2013t) <- podatki_2013t[1, ]
podatki_2013t <- podatki_2013t[-1, ]
podatki_2013t <- podatki_2013t[ ,c(1, 2, 4, 5, 6, 9, 12, 15)]

podatki_2014_pravilna <- podatki_2014 %>% select(X, X2.01.2014, X3.02.2014, X3.03.2014, X1.04.2014, X2.05.2014, X2.06.2014, X1.07.2014, X1.08.2014, X1.09.2014, X1.10.2014, X3.11.2014, X1.12.2014)
podatki_2014t<- t(podatki_2014_pravilna)
colnames(podatki_2014t) <- podatki_2014t[1, ]
podatki_2014t <- podatki_2014t[-1, ]

vsi_podatki<- rbind(podatki_2012t, podatki_2013t, podatki_2014t)

#1.c) graf 

vrsta1 <- ts(vsi_podatki[ ,6], start = c(2012,1), frequency = 12)
vrsta2 <- ts(vsi_podatki[ ,8], start = c(2012,1), frequency = 12)



graf1 <- ts.plot(vrsta1, vrsta2, main="EURIBOR", xlab = "Čas", ylab="%", col = c("red", "blue"))
legend("topright", legend= c("6m", "12m"), col = c("red", "blue"), lty =1)

#Druga naloga
#2.a) Izbrani datumi: 2.7.2012, 2.9.2013, 2.5.2014

#2.b) graf časovne strukture obrestnih mer

podatki_2 <- vsi_podatki[c(7, 21, 29), ]
cas = c(0.25, 0.5, 1, 2, 3, 6, 9, 12)
colnames(podatki_2) <- cas

graf2 <- plot(x = cas,
              y = podatki_2[1,],
              ylim = c(min(0), max(1.3)),
              xlab="Dospetje [mesec]", 
              ylab="%", 
              col="red", 
              main="Casovna struktura Euribor"
              )
lines(podatki_2[c(1),], x=cas, col="gold", type="o", text(11,1.25, "2.7.2012", col = "gold"))
lines(podatki_2[c(2),], x=cas, col="red", type="o", text(11,0.42, "2.9.2013", col = "red"))
lines(podatki_2[c(3),], x=cas, col="blue", type="o", text(11,0.63, "2.5.2014", col = "blue"))

#Krivulje pričakovano naraščajo z večjo dospelostjo.

#3.a) Terminske obrestne mere

terminske_obrestne_mere <- c(0)
obrestne_mere <- vsi_podatki[,c(6,8)]
podatki_3 <- cbind(obrestne_mere, terminske_obrestne_mere)
colnames(podatki_3) <- c("6m", "12m", "6m x 12m")

for (i in 1:36){
  podatki_3[i,3] <- 2*((1 + as.numeric(podatki_3[i, 2])/100)/(1 + (1/2) * as.numeric(podatki_3[i, 1])/100) - 1)*100
  }

#3.b)

Euribor6m <- vsi_podatki[,6]
Euribor12m <- vsi_podatki[,8]
podatki_3 <- cbind(podatki_3, Euribor6m, Euribor12m)
primerjava <- podatki_3[,c(4,5,3)]
napoved <- c(c(NA,NA,NA,NA,NA,NA),primerjava[-c(1:6),3])
primerjava[,3] <- napoved

#pomožne tabele za zadnje grafe 

primerjava_za_grafe <- primerjava[c(7:36), c(1,3)]
primerjava_za_grafe <- as.data.frame(primerjava_za_grafe)
primerjava_za_grafe[,1] <- as.numeric(as.character(primerjava_za_grafe[,1]))
primerjava_za_grafe[,2] <- as.numeric(as.character(primerjava_za_grafe[,2]))
primerjava_za_grafe[,3] <- as.numeric(as.character(primerjava_za_grafe[,3]))

leto2012 <- primerjava_za_grafe[c(1:6),]
leto2013 <- primerjava_za_grafe[c(7:18),]
leto2014 <- primerjava_za_grafe[c(19:30),]

#3.c) 
graf3 <- plot(primerjava_za_grafe, 
              type = "n",
              xlab= "Napoved",
              ylab= "Opazovano",
              ylim=c(0,1.5),
              xlim=c(0,1.5), 
              main="6m Euribor 2012-2014")
points(x=leto2012[,2], y = leto2012[,1], type = "p", col="red",pch = 16)
points(x=leto2013[,2], y = leto2013[,1], type = "p", col="blue",pch = 16)
points(x=leto2014[,2], y = leto2014[,1], type = "p", col="green",pch = 16)
abline(a=0,b=1, lty=2) 
abline(lm(primerjava_za_grafe[,1]~primerjava_graf[,2]),lwd = 2, col = "black")
legend("topleft",bty = "n", c("2012","2013","2014"), pch=16, col =c("red","blue","green"))

#3.d) Graf za leto 2012

graf4 <- plot(leto2012,
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano", 
              ylim=c(0,1),
              xlim=c(0,1),
              main="6m Euribor 2012")
points(x=leto2012[,2], y = leto2012[,1], type = "p", col="red",pch = 16)
abline(a=0,b=1,lty=2)
abline(lm(leto2012[,1]~leto2012[,2]), lwd = 2, col ="black")
legend("topleft",bty = "n",c("2012"), pch = 16, col =c("red"))

#3.d) Graf za leto 2013
graf5 <- plot(leto2013,
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano",
              ylim=c(0.2,1), 
              xlim=c(0.2,1),
              main="6m Euribor 2013")
points(x=leto2013[,2], y = leto2013[,1], type = "p", col="blue",pch = 16)
abline(a=0,b=1,lty=2)
abline(lm(leto2013[,1]~leto2013[,2]), lwd = 2, col ="black")
legend("topleft",bty = "n",c("2013"), pch = 16, col =c("blue"))

#3.d) Graf za leto 2014
graf6 <- plot(leto2014,
              type = "n",
              xlab= "Napoved",
              ylab = "Opazovano",
              ylim=c(0,1),
              xlim=c(0,1),
              main="6m Euribor 2014")
points(x=leto2014[,2], y = leto2014[,1], type = "p", col="green",pch = 16)
abline(a=0,b=1,lty=2)
abline(lm(leto2014[,1]~leto2014[,2]), lwd = 2, col ="black")
legend("topleft",bty = "n",c("2014"), pch = 16, col =c("green"))

#3.e) Da bi hipoteza veljala, bi morali biti napovedana in opazovana obrestna mera enaki. To bi pomenilo, da bi vse točke ležale na simetrali lihih kvadrantov. 
#     Iz mojih grafov, pa je jasno razvidno, da točke ne ležijo na simetrali oziroma niti v njeni neposredni bližini. Torej lahko sklepam, da hipoteza ne drži.

View(vsi_podatki)
View(primerjava)
graf1
graf2
graf3
graf4
graf5
graf6