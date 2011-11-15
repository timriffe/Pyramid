\name{ESP2000_5}
\alias{ESP2000_5}
\docType{data}
\title{
Year 2000 5-year-age Spanish population estimates from the Human Mortality Database
}
\description{two columns, males and females in 5-year abridged ages. Age zero is of width 1, follwed by 1-4 (4 years), follwed by 5 year widths until age 110+.}
\usage{data(ESP2000_5)}
\format{
  The format is:
 num [1:24, 1:2] 195176 750879 1004855 1114086 1368065 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:2] "males" "females"
}

\source{Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded on 10 May, 2010).}


\examples{
data(ESP2000_5)
data(ESP2000)
plotvecm <- c(ESP2000_5[1,1],rep(ESP2000_5[2,1],4)/4)
plotvecf <- c(ESP2000_5[1,2],rep(ESP2000_5[2,2],4)/4)
for (i in 3:(nrow(ESP2000_5)-1)){
	plotvecm <- c(plotvecm,rep(ESP2000_5[i,1],5)/5)
	plotvecf <- c(plotvecf,rep(ESP2000_5[i,2],5)/5)
}
plotvecm <- c(plotvecm,ESP2000_5[nrow(ESP2000_5),1])
plotvecf <- c(plotvecf,ESP2000_5[nrow(ESP2000_5),2])

plot(0:110,plotvecm,col="blue",type='l',main="Spain 2000, HMD")
lines(0:110,plotvecf,col="red",type='l')
lines(0:110,ESP2000[,1],col="blue",lty=2)
lines(0:110,ESP2000[,2],col="red",lty=2)
legend("topright",legend=c("females 5","males 5", "females 1","males 1"),col=c("red","blue","red","blue"),lty=c(1,1,2,2))
}
\keyword{datasets}
