# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse rather than computing it repeatedly. The following pair 
# of functions can cache the inverse of a matrix for subsequent retrieval, 
# assuming that the matrix supplied is always invertible.


# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( matrixInput = matrix()) {
   
    inverseMatrix <- NULL
    
    getMatrix <- function() matrixInput
    setInverse <- function(iM) inverseMatrix <<- iM
    getInverse <- function() inverseMatrix
    
    list(getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
    
}


# This function computes the inverse of the special "matrix" object returned by 
# makeCacheMatrix. If the inverse of the matrix has already been calculated, 
# then this function will retrieve the inverse from the cache.

cacheSolve <- function(matrixObject) {
    
    inverseM <- matrixObject$getInverse()
    
    if(!is.null(inverseM)) {
        message("getting cached data")
        return(inverseM)
    }
    
    matrixData <- matrixObject$getMatrix()
    inverseM <- solve(matrixData) 
    
    matrixObject$setInverse(inverseM)
    inverseM

}
