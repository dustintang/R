data1 <- read.csv("e:/11.csv")
head(data1)
summary(data1)
#  install.packages(c("dplyr","mice","lattice","Rcpp")) #
library(dplyr)
data2 <- filter(data1,money>0)
head(data2,n=20L)
attach(data2)
id <- paste(id,date)
newid <- as.factor(newid)
money <- as.numeric(money)
data2 <- data.frame(newid,money)
d <- tapply(data2$newid,data2$money,sum)


data2 <- data2[c(-8130,-8649,-28804,-46906,-54698,-57330),]
f <- e$money
g <- paste(e$id,e$date)
data3 <- data.frame(g,f)
head(data3,n=20L)
d <- tapply(data3$g,data3$f,sum)
data2[c(8127,8128,8129,8130,8131),]




a1<-seq(0,2,0.01)
b1<-seq(0,1,0.01)
f<-function(a1,b1) ifelse(a1<=1, 11*a1*log10(a1)*b1*(b1-1)+exp(-((25*a1-25/exp(1))^2+(25*b1-25/2)^2)^3)/25, 11*(a1-1)*log10(abs(a1-1))*b1*(b1-1)+exp(-((25*(a1-1)-25/exp(1))^2+(25*b1-25/2)^2)^3)/25)
c1<-outer(a1,b1,f)
persp(a1,b1,c1,theta=30,phi=30,expand=0.5,col="yellow")


