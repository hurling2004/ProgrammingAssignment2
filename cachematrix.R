## These functions allow you to create and cache inverse matrix values 


## Create a list of functions to set and get a matrix and it's inverse.
## 

makeCacheMatrix <- function(orig = matrix()) {
  
    invrse <- NULL
    set <- function(y) {
      orig <<- y
      invrse <<- NULL
    }
    get <- function() orig
    setinvr <- function(solve) invrse <<- solve
    getinvr <- function() invrse
    list(set = set, get = get,
         setinvr = setinvr,
         getinvr = getinvr)
  
}


## This function returns the inverse of a matrix, but only performs the
##   calculation if it is not already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrse <- x$getinvr()
  if(!is.null(invrse)) {
    message("getting cached data")
    return(invrse)
  }
  data <- x$get()
  invrse <- solve(data, ...)
  x$setinvr(invrse)
  invrse  
}
