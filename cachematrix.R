## Following functions implement a function to create a 'cached matrix object'
## capable to remember an inverse matrix of itself, and a function 'cacheSolve'
## to calculate inverse matrix for this 'cached matrix object'.


## Returns a list describing a matrix object capable of caching
## own inverse matrix.

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


## Returns inverse of matrix 'cacheMat' and recalculates the
## inverse matrix only if it has not been calculated already.

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
