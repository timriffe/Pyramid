#'
#' @title PyramidOutline draws a population pyramid as a single polygon
#' 
#' @description This function rescales and positions a population pyramid according to the convention of having males on the left and females on the right, and using discrete age groups. Ages are assumed to be in single-years intervals and to start at age 0. A plot device must already be open.
#' 
#' @param males vector of male population counts or fractions
#' @param females vector of male population counts or fractions
#' @param scale the total population, for purposes of plotting. For proportions in each age group, set to 1.
#' @param x the x position of the middle of the pyramid
#' @param y the y position of the bottom of the pyramid
#' @param ... arguments passed to \code{polygon()}.
#' 
#' @export
#' 
#' @return function called primarily for its graphical side effects, although a list of x and y coordinates is invisibly returned.
#' 

PyramidOutline <- function(males, females, 
        scale = sum(c(males, females)), 
        x = 0, y = 0, ...){
    N       <- length(males)
    Total   <- sum(c(males, females), na.rm = TRUE)
    widths  <- rep(1, N)
    age     <- c(0,cumsum(widths)[-N])
    u.age   <- age[N] + widths[N]
    
    males   <- scale * (males / Total)
    females <- scale * (females / Total)
    
    xout <- c(0, rep(females, each = 2) + 0,0, rev(c(-0, rep(-abs(males), each = 2) - 0, -0))) + x
    yout <- c(rep(c(age, u.age), each = 2), rev(c(rep(c(age, u.age), each = 2)))) + y
   
    polygon(x = xout, y = yout, ...)
   
    invisible(list(x = xout, y = yout))
}
