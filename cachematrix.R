## these two functions, makeCacheMatrix and cacheSolve, allow us to use
## matrices with cacheable inverses.  makeCacheMatrix builds the cacheable
## matrix object, and cacheSolve returns the inverse.

## creates a vector that is a list of the operations we can do on a cacheable
## matrix.  we can store/retrieve the matrix, and store/retrieve the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## accepts a cacheable matrix object x, and arguments for the r base solve
## function.  returns the inverse of x, from the cache if it exists.

cacheSolve <- function(x, ...) {
  inv <- x$getinv
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
