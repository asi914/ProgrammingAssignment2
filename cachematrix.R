## The makeCacheMatrix and cacheSolve functions create a special type of matrix that caches the value
## of its inverse and computes the inverse respectively

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <- y
    inv <- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. If the
## inverse has already been calculated (and the matrix has not changed), then the cachesolve
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinv()
  if (!is.null(i)) {
    message("Getting cached inverse")
    return (i)
  }
  data <- x$get()
  message("Calculating the inverse")
  i <- solve(data, ...)
  x$setinv(i)
  i
}
