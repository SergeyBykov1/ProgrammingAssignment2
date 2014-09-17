## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    
    set <- function(in_x){
        x <<- in_x
        x_inv <<- NULL
    }
    get <- function() x 
    setinv <- function(in_inv){
        x_inv <<- in_inv
    }
    getinv <- function() x_inv
    
    # Return
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinv()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data)
    x$setinv(x_inv)
    x_inv
}
