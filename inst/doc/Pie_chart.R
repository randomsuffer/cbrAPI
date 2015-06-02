## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----message=FALSE-------------------------------------------------------
library(cbrAPI)

## ------------------------------------------------------------------------
data <- download.dbf("2004-06")

## ------------------------------------------------------------------------
data <- data[ , which(names(data) %in% c('REGN','NUM_SC','ITOGO','A_P','DT'))]
head(data)

## ------------------------------------------------------------------------
data$NUM_SC <- as.integer(as.character(data$NUM_SC))
data$ITOGO <- as.integer(as.character(data$ITOGO))
x <- subset(data, subset=A_P==1)
y <- subset(data, subset=A_P==2)
x$ITOGO <- x$ITOGO * (-1)
data <- rbind(x,y)
data <- data[with(data, order(DT, REGN, NUM_SC)), ]
rm(x,y)

## ------------------------------------------------------------------------
sub <- subset(data, subset=(NUM_SC %in% c(70100:70499, 70600:70699)))
data.sub <- matrix(nrow=0,ncol=2)
colnames(data.sub) <- c("REGN", "Profit")
for (i in levels(as.factor(data$REGN))) {
  profit <- subset(sub, subset=(REGN==i))
  data.sub <- rbind(data.sub, c(i, sum(profit$ITOGO)))
}
data.sub <- as.data.frame(data.sub)
rm(sub, profit)

## ------------------------------------------------------------------------
data.sub$REGN <- as.numeric(as.character(data.sub$REGN))
data.sub$Profit <- as.numeric(as.character(data.sub$Profit))
data.plot <- data.sub[with(data.sub, order(-Profit)), ]
data.plot <- rbind(data.plot[1:5,], c(NA, sum(data.plot[6:nrow(data.plot),])))

## ----message=FALSE-------------------------------------------------------
for (i in 2:nrow(data.plot)-1) {
  message(i)
  data.plot$REGN[i] <- as.character(getBankInfo(data.plot[i,1])$name)
}
data.plot[nrow(data.plot),1] <- 'остальные'
colnames(data.plot)[1] <- 'Name'
rownames(data.plot) <- NULL

## ----fig.show='hold', fig.width=7, fig.height=4--------------------------
library("ggplot2")
ggplot(data.plot, aes(x='', y=Profit, fill=Name))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_x_discrete('') +
  ggtitle(paste("Stacked bar chart", data[1,5])) +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

