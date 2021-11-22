#' The Euclidean algorithm: Greatest common divisor of two scalars
#' 
#' @param a A natural number.
#' @param b A natural number.
#' @return Returns the greatest common divisor of a and b
#' @examples
#' euclidean(123612, 13892347912) # = 4
#' euclidean(100, 1000) # = 100
#' @source \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export

euclidean <- function(a,b){
  stopifnot(is.numeric(a)|is.integer(a),is.numeric(b)|is.integer(b))
  
  a <- abs(a) #if a negative value is the input, the algorithm uses the absolute value
  b <- abs(b)
  
  while(b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
