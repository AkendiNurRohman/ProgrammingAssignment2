## These functions are used to cache the inverse of a matrix.
## This is useful when dealing with large matrices and repeated inverse calculations, because it avoids recalculating the inverse if it has already been computed.

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve:
## This function computes the inverse of the matrix returned by makeCacheMatrix. If the inverse has already been calculated, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
