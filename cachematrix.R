## This R script is about a R function caching the inverse of a matrix by 'Orange Cat'


## 'makeCacheMatrix' is a function which makes a input matrix 'Special Matrix' that fit into caching function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_invmtx <- function(invmtx) i <<- invmtx
  get_invmtx <- function() i
  list(set = set, get = get,
       set_invmtx = set_invmtx,
       get_invmtx = get_invmtx)
  
}


## 'cacheSolve' is a cache tracking function

cacheSolve <- function(x, ...) {
  i <- x$get_invmtx()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_invmtx(i)
  i
}