##Matrix inversion is usually a costly computation 
##and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
##Your assignment is to write a pair of functions that cache the inverse of a matrix.


##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x  <<-  y
    xinv  <<-  NULL
  }
  get <- function() x
  setinv <- function(m) xinv <<- m
  getinv <- function() xinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...) 
  x$setinv(xinv)
  xinv
}