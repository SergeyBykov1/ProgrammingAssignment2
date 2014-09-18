##
## Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix
## cachematrix.R - Simple matrix wrapper with inverse matrix caching
##

# stopifnot acts as assertion

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mat.x = matrix()) {
    # init inverse matrix holder
    mat.x.inv <- NULL
    
    # original matrix setter
    set <- function(y.in){      
        dims <- dim(y.in)       # check if matrix is invertible
        stopifnot(dims[1]==dims[2])
        
        mat.x <<- y.in
        mat.x.inv <<- NULL
    }
    # original matrix getter
    get <- function() mat.x
    # inverse matrix setter
    setinv <- function(y.inv){
        stopifnot(dim(mat.x) == dim(y.inv))
        mat.id <- diag(dim(mat.x)[1]) # create identity matrix
        # check if A*inv(A)=I (identity matrix)
        stopifnot( (mat.x %*% y.inv - mat.id) < 0.001 )
        
        mat.x.inv <<- y.inv # set if passed all checks
    }
    # inverse matrix getter
    getinv <- function() mat.x.inv  
    
    # Return matrix wrapper
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    x.inv <- x$getinv()
    if(!is.null(x.inv)) {
        message("Getting cached data")
        return(x.inv)
    }
    x.inv <- solve(x$get())
    x$setinv(x.inv)
    x.inv
}
