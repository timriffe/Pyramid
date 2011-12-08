\name{ESextr}
\alias{ESextr}
\docType{data}
\title{
Spanish foreign population by continent of origin, 2010. 
}
\description{
Four continents, 5-year age groups, males and females.
}
\usage{data(ESextr)}
\format{
  The format is:
 chr [1:36, 1:5] "males" "males" "males" "males" "males" ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:36] "0-4" "5-9" "10-14" "15-19" ...
  ..$ : chr [1:5] "" "PAISES.EUROPEOS" "PAISES.AFRICANOS" "PAISES.AMERICANOS" ...
}

\source{
Instituto Nacional de Estadistica (INE). Cifras oficiales de población: Revisión del Padrón municipal. Población a 1 de enero de 2010 (23 diciembre 2010). Available at: www.ine.es
}

\examples{
data(ESextr)

head(ESextr)
is.matrix(ESextr) # TRUE
is.character(ESextr) # TRUE
# sorry for the slop: need numeric data: 
males <- matrix(as.numeric(ESextr[1:18,2:5]),ncol=4)
females <- matrix(as.numeric(ESextr[19:36,2:5]),ncol=4)

widths <- rep(5,nrow(ESextr/2))

# composition of total registered foreign population by continent of origin:
Pyramid(males,
		females,
		widths=widths,
		main = "Foreign population in Spain  (registered) by continent of bith, 2010",
		fill.males=gray(c(.8,.6,.4,.2)),
		fill.females=gray(c(.8,.6,.4,.2))
)
rect(1,70,2,90,col="white",border="black")
legend(1,90,legend=c("Europe","Africa","Americas","Asia"),fill=gray(c(.8,.6,.4,.2)),xpd=TRUE,bty="n")

# or separately, easier comparison of structure by age and sex. Fix x limits!
countries <- c("Europe","Africa","Americas","Asia")
dev.new(height=8,width=8)
par(mfrow=c(2,2))
for (i in 1:4){
	Pyramid(males[,i],
			females[,i],
			widths=widths,
			main = countries[i],
			fill.males=gray(.4),
			fill.females=gray(.6),
			xlim=c(-3,3),
			mar=c(2,2,2,2)
	)
}
}
\keyword{datasets}
