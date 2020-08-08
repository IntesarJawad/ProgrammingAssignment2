## A script that caches the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        verse <- NULL
        set <- function(y) {
                x <<- y
                verse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) verse <<- inverse
        getInverse <- function() verse 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        verse <- x$getInverse()
        if(!is.null(verse)){
                message("getting cached data")
                return(verse)
        }
        mat <- x$get()
        verse <- solve(mat,...)
        x$setInverse(verse)
        verse
}