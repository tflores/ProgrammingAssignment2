## The 2 functions below manage matrices in a way
## that is possible to have cache of it's inverses

## Function 'makeCacheMatrix' creates a cacheable
## matrix which can be used with cacheSolve function
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

## Function cacheSolve works together with 
## 'makeCacheMatrix' function, returning a 
## calculated value of matrix's inverse if
## it already exists.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
