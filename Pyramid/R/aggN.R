#'
#' @title aggN aggregate vector of single age (year) data to N-year groups
#' 
#' @description This only makes sense if N is 5 or 10 (or 2 I guess), since it uses modulo to find groups
#'    Ages are assumed to start at 0 and count up in single ages.
#' 
#' @param x the vector of single-age-classified data
#' @param N desired width of interval, e.g., 5 or 10
#' 
#' @export 
#' 
aggN <- function(x,N){
    age <- 0:(length(x) - 1)
    tapply(x,age - age %% N, sum)
}








