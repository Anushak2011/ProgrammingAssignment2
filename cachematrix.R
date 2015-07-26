## Matrix conversion is costly, so caching the inverse of a matrix may be beneficial
## rather than repeatedly computing it. The pair of functions below are use to cache the 
## inverse of a matrix

## The first function, makeCacheMatrix, caches the inverse matrix by "setting" the value 
## then "getting" the value of the matrix. Then, it "set"s and "get"s the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL}
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,  ## sets matrix value
    get = get,  ## gets matrix value
    setinverse = setinverse,   ## sets inverse matrix value
    getinverse = getinverse)   ## gets inverse matrix value
}
## This next function returns the matrix inverse. IF the matrix has already been calculated, 
## it gets the result without repeating the calculation. Otherwise, it calculates the inverse, 
## and sets the value in the cache with the "set" function. Again, this function assumes that 
## the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv))   ## a) matrix has already been calculated
    {
      message("retrieving cache data")
      return(inv)}
    data <- x$get() ## b) matrix has not been calculated previously, so it is calculated now
    inv <- solve(data, ...)
    x$setinverse(i) ## value of inverse set in cache
    inv
}
