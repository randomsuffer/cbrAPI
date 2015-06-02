## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----message=FALSE-------------------------------------------------------
library(cbrAPI)

## ----eval=FALSE----------------------------------------------------------
#  data101 <- download.dbf.all(form = 101)

## ----include=FALSE, echo=FALSE, warning=FALSE----------------------------
load("~/Documents/R packages/cbrAPI/big data.RData")
load("~/Documents/R packages/cbrAPI/big data 102.RData")

## ------------------------------------------------------------------------
data101 <- subset(data101, NUM_SC %in% c(10200:10999))

## ------------------------------------------------------------------------
data101 <- data101[ , which(names(data101) %in% c('REGN','NUM_SC','ITOGO','IITG','A_P','DT'))]
x <- subset(data101, DT < '2007-01-01')
y <- subset(data101, DT >= '2007-01-01')
data101$ITOGO <- c(x$ITOGO, y$IITG)
data101 <- data101[ , -which(names(data101) %in% c('IITG'))]
rm(x,y)

## ------------------------------------------------------------------------
data101$NUM_SC <- as.integer(as.character(data101$NUM_SC))
data101$ITOGO <- as.integer(as.character(data101$ITOGO))
x <- subset(data101, subset=A_P==1)
y <- subset(data101, subset=A_P==2)
x$ITOGO <- x$ITOGO * (-1)
data101 <- rbind(x,y)
data101 <- data101[with(data101, order(DT, REGN, NUM_SC)), ]
rm(x,y)

## ----message=FALSE-------------------------------------------------------
data101.sub <- matrix(nrow=0,ncol=3)
colnames(data101.sub) <- c("REGN", "DT", "Equity")
regnums1 <- levels(as.factor(data101$REGN))
dates1 <- levels(as.factor(data101$DT))
for (i in regnums1) {
  sub <- subset(data101, REGN == i)
  for (j in dates1) {
    equity <- subset(sub, DT == j)
    equity <- sum(equity$ITOGO)
    data101.sub <- rbind(data101.sub, c(i, j, equity))
  }
  message(i, ' / ', regnums1[length(regnums1)])
}
data101.sub <- as.data.frame(data101.sub)
rm(sub, equity, i, j)

## ----eval=FALSE----------------------------------------------------------
#  data102 <- download.dbf.all(form = 102)

## ------------------------------------------------------------------------
data102 <- subset(data102, CODE %in% c(11100:11999))

## ------------------------------------------------------------------------
x <- subset(data102, DT < '2007-01-01')
y <- subset(data102, DT >= '2007-01-01')
data102$ITOGO <- c(x$ITOGO, y$SIM_ITOGO)
data102 <- data102[ , -which(names(data102) %in% c('SIM_R','SIM_V','SIM_ITOGO'))]
rm(x,y)

## ----message=FALSE-------------------------------------------------------
data102.sub <- matrix(nrow=0,ncol=3)
colnames(data102.sub) <- c("REGN", "DT", "Income")
regnums2 <- levels(as.factor(data102$REGN))
dates2 <- levels(as.factor(data102$DT))
for (i in regnums2) {
  sub <- subset(data102, REGN == i)
  for (j in dates2) {
    income <- subset(sub, DT == j)
    income <- sum(income$ITOGO)
    data102.sub <- rbind(data102.sub, c(i, j, income))
  }
  message(i, ' / ', regnums2[length(regnums2)])
}
data102.sub <- as.data.frame(data102.sub)
rm(sub, income, i, j)

## ------------------------------------------------------------------------
regnums <- subset(regnums1, regnums1 %in% regnums2)
rm(regnums1, regnums2)

## ----message=FALSE-------------------------------------------------------
data.plot <- matrix(nrow=0,ncol=5)
colnames(data.plot) <- c("REGN", "DT", "Equity", "Income", "Coef")
for (i in regnums) {
  sub <- subset(data101.sub, REGN == i)
  sub$DT <- as.character(sub$DT)
  for (j in 2:length(dates2)) {
    equity <- subset(sub, DT <= dates2[j])
    equity <- subset(equity, DT > dates2[j-1])
    equity$Equity <- as.numeric(as.character(equity$Equity))
    equity <- sum(equity$Equity)
    income <- subset(data102.sub, REGN == i & DT == dates2[j])
    income$Income <- as.numeric(as.character(income$Income))
    income <- sum(income$Income)
    data.plot <- rbind(data.plot, c(i, dates2[j], equity, income, income/equity))
  }
  message(i, ' / ', regnums[length(regnums)])
}
data.plot <- as.data.frame(data.plot)
rm(sub, equity, income, i, j)

## ------------------------------------------------------------------------
df <- data.plot
df$Coef <- as.numeric(as.character(df$Coef))
df$Coef <- log(df$Coef)
df$Coef <- round(df$Coef, 4)
# уберем все нулевые строки
df <- df[apply(df, 1, function(row) all(row !=0 )),]
df$DT <- as.Date(df$DT, format='%Y-%m-%d')

## ----fig.show='hold', fig.width=7, fig.height=4--------------------------
library("ggplot2")
labels <- rep('', length(dates2))
labels[seq(4,length(dates2),4)] <- 2005:2015
plot <- ggplot(data=df, aes(factor(DT), Coef))
plot + ggtitle('Box plot') +
  scale_x_discrete(name='Coefficient', labels=labels) +
  scale_y_discrete(name='Date') +
  geom_boxplot()

