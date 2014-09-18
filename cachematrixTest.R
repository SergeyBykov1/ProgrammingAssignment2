#
# Programming Assignment 2: Lexical Scoping
# cachematrixTest.R - Some tests (with expected output) for PA2
# (as introduced in 
# class.coursera.org/rprog-007/forum/thread?thread_id=83#post-336 )
#

# stopifnot acts as assertion

source("cachematrix.R")

matrA = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))

# Return original matrix
stopifnot(matrA$get() == matrix(1:4, 2, 2))

# Compute, cache, and return inverse matrix
expInvA <- matrix(c(-2, 1, 1.5, -0.5), 2, 2) # expected inv(A)
stopifnot( cacheSolve(matrA) == expInvA )

# Return inverse matrix
stopifnot( matrA$getinv() == expInvA )

# Returns cached inv matrix (previously computed)
cacheSolve(matrA)

# Modify existing matrix
matrA$set(matrix(c(0,5,99,66), nrow=2, ncol=2))

# Computes, caches, and returns new matrix inverse
expInvA <- matrix(c(-2/15, 1/99, 1/5, 0), 2, 2) # new expected inv(A)
epsilon <- 0.001
stopifnot( abs(cacheSolve(matrA) - expInvA) < epsilon )

# Returns matrix
stopifnot(matrA$get() == matrix(c(0,5,99,66), 2, 2))

cacheSolve(matrA)

# Returns matrix inverse
stopifnot(abs(matrA$getinv() - expInvA) < epsilon)
