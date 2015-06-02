## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----message=FALSE-------------------------------------------------------
library(cbrAPI)

## ----eval=FALSE----------------------------------------------------------
#  data101 <- download.dbf.all(form = 101)

## ----include=FALSE, echo=FALSE, warning=FALSE----------------------------
load("~/Documents/R packages/cbrAPI/big data.RData")

## ------------------------------------------------------------------------
data <- data101[ , which(names(data101) %in% c('REGN','NUM_SC','ITOGO','IITG','A_P','DT'))]
x <- subset(data, DT < '2007-01-01')
y <- subset(data, DT >= '2007-01-01')
data$ITOGO <- c(x$ITOGO, y$IITG)
data <- data[ , -which(names(data) %in% c('IITG'))]
rm(x,y)

## ------------------------------------------------------------------------
data.small <- subset(data, REGN == 1481)

## ------------------------------------------------------------------------
data.small$NUM_SC <- as.integer(as.character(data.small$NUM_SC))
data.small$ITOGO <- as.integer(as.character(data.small$ITOGO))
x <- subset(data.small, subset=A_P==1)
y <- subset(data.small, subset=A_P==2)
x$ITOGO <- x$ITOGO * (-1)
data.small <- rbind(x,y)
data.small <- data.small[with(data.small, order(DT, REGN, NUM_SC)), ]
rm(x,y)

## ------------------------------------------------------------------------
data.small <- subset(data.small, subset=A_P==2)
data.small <- subset(data.small, subset=(NUM_SC %in% c(30000:32902)))
data.sub <- matrix(nrow=0,ncol=2)
colnames(data.sub) <- c("DT", "Corporate")
for (i in levels(as.factor(data.small$DT))) {
  corporate <- subset(data.small, subset=(DT==i))
  data.sub <- rbind(data.sub, c(i, sum(corporate$ITOGO)))
}
data.sub <- as.data.frame(data.sub)
rm(corporate)

## ----fig.show='hold', fig.width=7, fig.height=4--------------------------
barplot(as.numeric(as.character(data.sub$Corporate))/1000000,
        main=paste("Bar Chart of", getBankInfo(data.small$REGN[1])$name),
        xlab="Date, months", ylab="Volume, millions of rubles",
        names.arg=data.sub$DT)


