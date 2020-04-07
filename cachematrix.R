## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function caches the inverse of the matrix given

makeCacheMatrix <- function(X = matrix()) {
    M <- NULL
    set <- function(Y) {
        X <<- Y
        M <<- NULL
    }
    get <- function() X
    setinverse <- function(solve) M <<- solve
    getinverse <- function() M
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function retrieves the inverse of the given matrix.
## It returns it from cache if it already exists, otherwise it calculates it.

cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'X'
    M <- X$getinverse()
    if(!is.null(M)) {
        message("getting cached data")
        return(M)
    }
    data <- X$get()
    M <- solve(data, ...)
    X$setinverse(M)
    M
}
