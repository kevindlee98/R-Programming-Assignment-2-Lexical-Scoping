## The functions in this script are used to create a special object
## that stores a matrix and then caches its inverse.

## 'makeCacheMatrix' is a function creating a special matrix object that can cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)  inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 'cacheSolve' is a function computing the inverse of the matrix returned by the
## 'makeCacheMatrix' function. If the inverse has already been computed, the cached
## inverse is received and a message indicating this is shown, else the inverse will be
## set.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
