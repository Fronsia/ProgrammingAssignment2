## makeCacheMatrix creates a special matrix object.
## makeCacheMatix then solves the inverse of the matrix.
## If the matrix inverse has already been solved,
## It will find it in the cache and return it
## It will not be calculated again.

  makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
 

## cacheSolve returns the inverse of a matrix A created with makeCacheMatrix.
## If the cached inverse is available, cacheSolve will retrieve it.
## If the cached inverse is NOT available,cacheSolve will computes, caches, and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}
 
