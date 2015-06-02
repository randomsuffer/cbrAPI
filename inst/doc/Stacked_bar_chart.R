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
data.small <- subset(data, REGN == 1623)

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
sub1 <- subset(data.small, subset=(NUM_SC %in% c(42300:42399, 42600:42699)))
sub2 <- subset(data.small, subset=(NUM_SC %in% c(42000:42299, 42500:42599)))
sub3 <- subset(data.small, subset=A_P==2)
sub3 <- subset(data.small, subset=(NUM_SC %in% c(10200:10999)))

## ------------------------------------------------------------------------
data.sub <- matrix(nrow=0,ncol=3)
colnames(data.sub) <- c("DT", 'Type', "Volume")
for (i in levels(as.factor(data.small$DT))) {
  individuals <- subset(sub1, subset=(DT==i))
  data.sub <- rbind(data.sub, c(i, 'individuals', sum(individuals$ITOGO)))
  corporate <- subset(sub2, subset=(DT==i))
  data.sub <- rbind(data.sub, c(i, 'corporate', sum(corporate$ITOGO)))
  equity <- subset(sub3, subset=(DT==i))
  data.sub <- rbind(data.sub, c(i, 'equity', sum(equity$ITOGO)))
}
data.sub <- as.data.frame(data.sub)
rm(sub1, sub2, sub3, individuals, corporate, equity)

## ------------------------------------------------------------------------
data.plot <- data.sub
data.plot$DT <- as.character(data.plot$DT)
data.plot$Type <- as.character(data.plot$Type)
data.plot$Volume <- as.numeric(as.character(data.plot$Volume))/1000000
data.plot <- subset(data.plot, subset=(DT >= '2014-01-01' & DT < '2015-01-01'))

## ----fig.show='hold', fig.width=7, fig.height=4--------------------------
library("ggplot2")
ggplot(data=data.plot, aes(x=DT, y=Volume, fill=Type)) +
  geom_bar(stat="identity") +
  scale_y_discrete(breaks=seq(0, 5000, 300), 'Volume, millions of rubles') +
  scale_x_discrete(breaks=levels(as.factor(data.plot$DT))[seq(1,12,3)], 'Date') +
  ggtitle(paste("Stacked bar chart of", getBankInfo(data.small$REGN[1])$name))

