## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list containing 4 other functions. These 4 functions 
## either get or set (in cache memory) a matrix or they get (from cache) or set  
## (in cache) the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setMatrix <- function(y) {
          x <<- y
          i <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function either returns the inverse of a matrix if it has already been 
## stored in cache or, if the inverse is not already stored in cache, it 
## calculates the inverse matrix, stores it in cache and returns the value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
