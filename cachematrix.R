##  inverse matrix is calculated or fetched from the cache, the inverse matrix 

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
##  The above function creates a matrix object that can cache the inverse. 
##  get returns the vector x 
##  set changes the main function vector
##  setmean works like set.
##  getmean works like get.

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
##  Above function solves for the inverse of the matrix created in the first function.
##  If matrix is fetched, inverse is computed directly from cache