# ***********************************************************************
# Implementation of a changepoint detection
# ***********************************************************************

#' S4 class to detect changepoints
#' 
#' @description Implementation of the CUSUM technique for seqential analysis in R
#' 
#' 
#' @slot a This is the variable that contains the numeric vector
#' 
#' @docType class
#' 
#' @importFrom Rdpack reprompt
#' @references Page, E.S. (1954). "Continuous Inspection Scheme". Biometrika. 41 (1/2): 100-115
#' 
#' @examples
#' require(simplecpt)
#' obj <- simplecpt()
#' obj <- addData(obj, c(1, 8, 3))
#' checkCpt(obj, 0.01)
#' checkCpt(obj, 100)
#' 
#' @export
setClass("simplecpt", representation=representation(a="numeric"))

#' Constructor for the simplecpt Class
#'
#' @description Initializes a new instance of the simplecpt class with an empty data vector.
#' @export
simplecpt <- function(x=numeric(0)) {
  newobj <- new("simplecpt")
  newobj@a <- x
  return(newobj)
}

#' Add more data from a vector
#' 
#' @param obj An object of the class "simplecpt".
#' @param data A numeric input vector.
#' 
#' @return An object of the class "simplecpt".
#' 
#' @importFrom Rdpack reprompt
#' @references https://en.wikipedia.org/wiki/CUSUM
#' 
#' @examples
#' require(simplecpt)
#' obj <- simplecpt()
#' obj <- addData(obj, rnorm(100,mean=1))
#' 
#' @rdname addData-methods
#' @docType methods
#' @export
setGeneric("addData", function(obj, data) standardGeneric("addData"))
#' @rdname addData-methods
setMethod("addData", signature(obj = "simplecpt"),
          function(obj,data) {
            obj@a <- c(obj@a, data)
            return (obj)
          }
)

#' Check if any cusum statistic exceeds the threshold
#' 
#' @param obj An object of the class "simplecpt".
#' @param threshold A numeric value indicating the threshold for the cusum statistic.
#' 
#' @return True if any of the cusum statistic exceeds the threshold.
#' 
#' @importFrom Rdpack reprompt
#' @references https://en.wikipedia.org/wiki/CUSUM
#' 
#' @examples
#' require(simplecpt)
#' obj <- simplecpt()
#' obj <- addData(obj, c(1, 8, 3))
#' checkCpt(obj, 0.01)
#' checkCpt(obj, 100)
#' 
#' @rdname checkCpt-methods
#' @docType methods
#' @export
setGeneric("checkCpt", function(obj, threshold) standardGeneric("checkCpt"))
#' @rdname checkCpt-methods
setMethod("checkCpt", signature(obj = "simplecpt"),
          function(obj, threshold) {
            n <- length(obj@a)
            cusum <- sapply( 1:(n-1), function(i) abs(mean(obj@a[1:i])-mean(obj@a[-(1:i)])) )
            return ( any(cusum > threshold) )
          }
)