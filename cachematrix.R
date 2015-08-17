## Put comments here that give an overall description of what your
## functions do

## Returns a list describing a matrix object capable of caching own inverse.

makeCacheMatrix <- function(mat = matrix()) {
        inverse <- NULL
        set <- function(newMat) {
                mat <<- newMat
                inverse <<- NULL
        }
        get <- function() mat
        setInv <- function(inv) inverse <<- inv
        getInv <- function() inverse
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## Returns inverse of matrix 'cacheMat' and recalculates
## the inverse only if it has not been calculated already.

cacheSolve <- function(cacheMat, ...) {
        ## Return a matrix that is the inverse of 'cacheMat'
        inv <- cacheMat$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }
        mat <- cacheMat$get()
        inv <- solve(mat, ...)
        cacheMat$setInv(inv)
        inv
}


source("cachematrix.R")
myMat <- matrix(c(1,2,3,4,5,4,3,2,1), nrow=3)
solve(myMat)
cacheMat <- makeCacheMatrix(myMat)
cacheMat$get()
cacheSolve(cacheMat)
