## These functions allow the user to set a matrix, and solve
## for the inverse of this matrix. If the matrix inverse has
## already been calculated and the inverse hasn't changed,
## the inverse can just be read from the cache instead of 
## recalculated

## function returns a list of functions. Once assigned, 
## the matrix and inverse are stored inside the function,
## allowing us to read it from another function and see if 
## the inverse has been determined

makeCacheMatrix <- function(x = matrix()) {
    matInverse <- NULL
    setMatrix <- function(y){
        x <<- y
        matInverse <<- NULL 
    }
    getMatrix <- function() x
    setInverse <- function(Inverse){
        matInverse <<- Inverse
    }
    getInverse <- function() matInverse
    
    list(setMatrix=setMatrix,getMatrix=getMatrix,
         getInverse=getInverse,setInverse=setInverse)
    
}


## Solves for the inverse if not already solved
## reads inverse from the cache if already solved

cacheSolve <- function(x, ...) {
    Inverse <- x$getInverse()
    if(!is.null(Inverse)) {
      print('loading inverse from cache...')
      return(Inverse)
    }
    Matrix <- x$getMatrix()
    Inverse <- solve(Matrix)
    x$setInverse(Inverse)
    Inverse
}
