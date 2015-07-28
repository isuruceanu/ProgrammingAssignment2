## Functions that caches the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #Check if matrix is squered
    matrixDim <- dim(x)
    
    if (matrixDim[1] != matrixDim[2])
        stop(paste("(", matrixDim[1], "X", matrixDim[2], ") is not a squered matrix"))
    
    set <- function(m){
        x <<- m
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function (inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
## Note: The matrix should be squered matrix and it should be always invertible

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    if (!is.null(inv)) {
        return (inv)
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
