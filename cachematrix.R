## Functions for creating and using inverted matrices which caching ability

## Creates cacheable matrix which provides input to cacheSolve() function which sets and gets the cached values
makeCacheMatrix <- function(x = matrix()) {
  if (!is.matrix(original.matrix)) {
    stop("Please give a matrix")
  }
  
  inverted.matrix <- NULL
  
  set <- function(y) {
    original.matrix <<- y
    inverted.matrix <<- NULL
  }
  
  get <- function() original.matrix
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
}

## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted.matrix <- x$get.inverse()
    if(!is.null(inverted.matrix)) {
      message("Getting cached inverse matrix")
      return(inverted.matrix)
    }
    matrix.to.inverse <- x$get()
    inverted.matrix <- solve(matrix.to.inverse)
    x$set.inverse(inverted.matrix)
    inverted.matrix
}