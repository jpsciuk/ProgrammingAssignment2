# The makeCacheMatrix function creates a matrix that can cache its inverse.  The cacheSolve function computes the 
# inverse of the matrix.  If the inverse has been calculated then you should receive the inverse from the cache. 

# This function creates a matrix that can cache its inverse.  
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the matrix. If the inverse has been calculated then you should receive the 
# inverse from the cache. 
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
