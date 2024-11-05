# ***********************************************************************
# Implementation of a hashtable
# ***********************************************************************

# S4 class to represent the hashtable
setClass("hashing", representation=representation(a="list",s="numeric"))

# constructor to start either empty or with given array
hashing <- function(size) {
  newobj <- new("hashing")
  newobj@a <- vector("list", size)
  newobj@s <- length(newobj@a)
  return(newobj)
}

# hashfunction
setGeneric("hashfunction", function(obj, value) standardGeneric("hashfunction"))
setMethod("hashfunction", signature(obj="hashing"),
          function(obj,value) { 
            return (value %% obj@s + 1) # indexing in R starts with 1
          }
)

# store
setGeneric("store", function(obj, value) standardGeneric("store"))
setMethod("store", signature(obj="hashing"),
          function(obj,value) {
            
            index <- hashfunction(obj, value)
            
            ## No collision detected
            if (is.null(obj@a[[index]])) {
              obj@a[[index]] <- list(value) ## initialize an array with that element
            } else {
              obj@a[[index]] <- c(value, obj@a[[index]]) ## merge the value in
            }
            
            return (obj)
            
           }
)

# lookup
setGeneric("lookup", function(obj, value) standardGeneric("lookup"))
setMethod("lookup", signature(obj="hashing"),
          
          function(obj,value) {
            
            index <- hashfunction(obj, value)
            
            ## No element
            if (is.null(obj@a[[index]])) {
              
              return (FALSE)
              
            } else {
              
              for (element in obj@a[[index]]) {
                if (element == value) {
                  return (TRUE)
                }
              }
              
              return (FALSE)
              
            }
            
          }
          
)