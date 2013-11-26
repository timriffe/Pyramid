#' @title MyersN a Myers-like index for periods of a given length
#'
#' @description This function returns the same answer as \code{MyersI()} under the default settings. If you're looking at month-preference, or some other range where heaping is possible (e.g., Fire-Horse), then change the period accordingly. One can toggle \codeP{standardize} to change the interpretation of the result. \code{standardize = FALSE} gives the interpretation as Myers meant it: what percentage would need to move to another digit in order to make the digit distribution uniform? \code{standardize = TRUE} gives the interpretation: how does this digit distribution compare to its maximum value given the interval steps implied by its period? See examples for clarification on that.
#' 
#' @param x vector of population counts (age-classified observations in general).
#' @param ages integer vector of the ordered classifying variable. If not specified, this is assumed to count up in single integers starting from 0 along the length of \code{x}.
#' @param period digit period. In the most common case of age-heaping, digit preference is on 0 and 5, and so the period is 10 (but you could compare with 5 here).
#' @param standardize logical. Does the max value equal the percent that would need to shift for uniformity (\code{FALSE}) or the percent degree of approximation to perfect concentration for the given degree of discreteness(\code{TRUE})?
#' 
#' @return The index value, ranging from 0 to 100 (a percent).
#' 
#' @export
#'
#' @examples
#' 
#' #' library(Pyramid)
#' data(PTpop)
#' MyersI(rowSums(PTpop[,1:2]))
#' MyersN(rowSums(PTpop[,1:2]))
#' MyersN(rowSums(PTpop[,1:2]), period = 5) # we conclude that
#' MyersN(rowSums(PTpop[,1:2]), period = 5, standardize = TRUE)
#' MyersN(rowSums(PTpop[,1:2]), period = 10, standardize = TRUE)
#' # we conclude that heaping is more on 0 than on 5
#' 
#' # full uniformity: makes no difference whether you standardize:
#' MyersN(rep(1, 100), period = 15, standardize = TRUE)
#' MyersN(rep(1, 100), period = 15, standardize = FALSE)
#' 
#' # absolute concentration.
#' # standardize = TRUE will give 100 if it can't be any more concentrated
#' # standardize = FALSE could only give 100 in the limit, i.e. in all 
#' #              values are in a single digit, and the age interval is 1/Inf
#' MyersN(x=rep(c(1,0,0,0),30),period=4, standardize = TRUE)  # how concentrated is it relative to the theoretical max?
#' MyersN(x=rep(c(1,0,0,0),30),period=4, standardize = FALSE) # what prop would need to move for uniformity
#' 
MyersN <- function(x, ages = 0:(length(x) - 1), period = 10, standardize = FALSE){
   
# ul = upper limit = largest age evenly divisible by 10 up to 100
# i.e if highest age is 85, then it spits back 80
    ul   <- max(ages[ages %% period == 0])
# indices to pick out 2 tabulations
    ind1 <- ages >= period & ages < (ul - period)
    ind2 <- ages >= (period * 2) & ages < ul
# sum by digits, in this case picked out by modulars (%%)
    tab1 <- tapply(x[ind1],ages[ind1] %% period,sum)
    tab2 <- tapply(x[ind2],ages[ind2] %% period,sum)
# weighted tabulation
    TAB <- tab1 * 1:period + tab2 * c(period:1 - 1)
# interpret as % that would need to be redistributed...
    
    ifelse(standardize,
            {100 * sum(abs(TAB/sum(TAB) - 1 / period)) / (2 * (1 - 1 / period))},
            {sum(abs(TAB/sum(TAB) - 1 / period)) * 50})
}



