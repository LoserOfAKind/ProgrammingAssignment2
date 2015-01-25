## So basically these main functions

## 1. makeCacheMatrix (gives us a list of functions) which are used to store the matrix on which computations are to be done (subfunction named - set)
##Or to store the resultant inverse matrix (subfunction named - setinverse function)
## Or to get the stored matrix (subfunction named - get function) 
## Or to get the cache (stored) computed inverse matrix (subfunction named - getinv)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function () x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Calculate the inverse of the "special" matrix created by the above makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse
  if (inv != matrix()) {
    message("Getting cached data (inverse matrix)")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
