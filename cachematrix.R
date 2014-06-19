## These functions provide a method for solving for the inverse of a matrix 
## once, and then reusing it as needed without the need to refer to
## additional variables or to check if the inverse has been previously
## stored.

## Creates a special "matrix" which stores the inverse once it is calculated
## with the help of an additional function cacheSolve().
## Actually defines 4 functions to enable the creation of a special "matrix",
## then creates a list based on those functions.
makeCacheMatrix <- function(x = matrix()) {
    
        ## Initialize the stored inverse of the matrix to NULL
        ix <- NULL
        
        ## Used to set the value of the matrix.
        ## First sets the stored matrix to the value provided,
        ## then sets the stored inverse to NULL.
        ## Example usage: 
        ## > n$set(matrix(0:3,2,2))
        set <- function(y) {
            x <<- y
            ix <<- NULL
        }
        
        ## Used to retrive the value of the matrix.
        ## Example usage: 
        ## > n$get()
        get <- function() x
        
        ## Function to store the inverse of the matrix
        ## provided by cacheSolve().
        ## Should not be used directly
        ## instead use cacheSolve().
        setinverse <- function(solved) ix <<- solved
        
        ## Function to retrive the stored inverse of the matrix.
        ## Should not be used directly
        ## instead use cacheSolve().
        getinverse <- function() ix
        
        ## list of all functions provided for in the definitions above.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Checks if the inverse has already been stored.
## If it has, simply returns the stored inverse;
## otherwise, calculates, stores, and returns the inverse.
cacheSolve <- function(x, ...) {
    
    ## Get the stored inverse of x.
    ix <- x$getinverse()
    
    ## if the stored inverse is not null,
    if(!is.null(ix)) {
        ## inform that it is the cached solution.
        message("getting cached inverse")
        ## and return the inverted matrix.
        return(ix)
    }
    
    ## otherwise, get the matrix x,
    data <- x$get()
    ## solve for it's inverse,
    ix <- solve(data, ...)
    ## store the inverse,
    x$setinverse(ix)
    
    ## and finally return the solved inverse matrix.
    ix
}
