## ************************************************************************** ##
##  Creates Cache Matrix to cache to inverse of the matrix                    ##
##  Cachesolve: Computes if the inverse is not calculated. If calculated, it  ##
##              returns cache inverse
## ************************************************************************** ##

## Objective of makeCacheMatrix(x) function : 
## -----------------------------------------
##
## Returns a list of functions which does the following :
## 
## getMat()  : Get the value of the matrix
## setMat(m) : set the value of the matrix
## getInv(m) : Get the value of the inverse of the matrix
## setInv(i) : set the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 setMat <- function(m){
   x <<- m
   inv <<- NULL
 }
 getMat <- function() x
 setInv <- function(i) inv <<- i
 getInv <- function() inv
 list(getMat = getMat, setMat = setMat, getInv = getInv, setInv = setInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("Getting Cached Matrix Inverse")
    return(inv)
  }
  message("Getting Inverse : First Time Computation or if matrix Changed")
  matX <-x$getMat()
  inv <- solve(matX)
  x$setInv(inv)
  inv
}
