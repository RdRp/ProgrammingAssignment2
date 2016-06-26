## Some values could be stored in a cache 
##to prevent wasting of computational time for recomputing them.
##This script is consisted of two functions. The first creates object
##that stores matrix and caches its inverse. The second computes the inverse
##matrix, if the inverse is not in the cache.

## This function creates 'matrix' object in a cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse matrix was calculated.
## If so, it gets the inverse matrix from the cache and skpis the computation.
##However, it calculates the iverse matrix, if the one has not been calculated before.
cacheSolve <- function(x, ...) {
        
  m <- x$getinverse() ## Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

