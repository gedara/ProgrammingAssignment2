## Put comments here that give an overall description of what your
## functions do
## Two functions are created to store a matrix and display inverse.

## Write a short comment describing this function
## This function creates an object that can cache its inverse.

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


## Write a short comment describing this function
## This function displays the inverse of the matrix from cache if the said matrix
## has not changed. If it new, output is calculated.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        
        message("getting cached data")
        return(inv)
        }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}
