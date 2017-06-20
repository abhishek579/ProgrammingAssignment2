# This is a program to cache the inverse of a square matrix. It makes use of the '<<-' operator in R language.
# There are 2 functions in this program - makeCacheMatrix function and cacheSolve funciton

# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverse){
        inv <<- inverse
    }
    
    getInverse <- function() inv
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
    
}


# The cacheSolve function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix function above. If the inverse has already been calculated (and the
# matrix has not changed), then the cacheSolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(!is.null(inv)){
        message("This is the cached Inverse Matrix")
        return (inv)
    }
    
    else{
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        return(inv)
    }
}
