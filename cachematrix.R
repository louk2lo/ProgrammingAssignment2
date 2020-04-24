## R Programming Assignment 2



## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list (set = set, get = get,
        setInverse = setInv,
        getInverse = getInv)

}


## This function caomputes the inverse of the special matrix returned by makeCacheMatrix above.  If the 
## inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if (!is.null(m)) {
          message("getting cached data")
          return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}
