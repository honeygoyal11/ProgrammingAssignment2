## Put comments here that give an overall description of what your
## functions do

## This function provides the inverse matrix of a square matrix via the 'solve' function in R 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) inv <<- solve
    
    getinverse  <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    

}


## cacheSolve uses the makeCacheMatrix function (which in turn uses the matrix) elements of 'setinverse' & 'getinverse',
## where it first checks if 'inv' previously exists, not having been 'set' under a new matrix in makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
