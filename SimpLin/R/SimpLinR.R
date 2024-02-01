SimpLinR <- function(Y, X){
  
  if(length(Y) != length(X)){
    stop("X and Y not the same length idiot")
  }
  
  if(is.numeric(Y) == FALSE | is.numeric(X) == FALSE){
    stop("she only works with numbers, try again sweetie")
  }
  
  SimpLinCpp(Y, X)
}