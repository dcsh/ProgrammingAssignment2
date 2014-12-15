## Put comments here that give an overall description of what your
## functions do

## special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL  # this will store/cache the inverted matrix
  set           <- function(y) {
                     x <<- y
                     inverseMatrix <<- NULL
                   }
  get           <- function() { x }
  getInverse    <- function() { inverseMatrix }
  setInverse    <- function(inv) { inverseMatrix <<- inv }
  list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)  
}


## computes inverse of "special matrix". Cached value is
## returned if inverse was already computed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    ## inv already computed
    message("returning cached value ...")
  }
  else {
    ## if inv is NULL, inverse needs to be computed
    message("inverse matrix needs to be computed ...")
    inv <- solve(x$get(), ...)
    x$setInverse(inv)
  }
  inv
}
