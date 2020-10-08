## Luca Balzano: week 3 assignment

## In this assignment I've written two function s.t. I can get the inverse of an
## invertible matrix. These are written in such a way that each time the value is
## stored in the cache and so when we ask for it, R just takes it from the cache without
## doing again the computation: this is such an improvement in speed!


## This function creates a special "matrix" object (a list) that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix"  
## returned by makeCacheMatrix above. If the inverse has already  
## been calculated (and the matrix has not changed), then the  
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) #when if stat is satisfied the function stops.
        }
        data <- x$get() #if the value isnt in the cache the fun has to compute it.
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## Example:
x <- makeCacheMatrix(matrix(c(5,7,2,6),nrow=2,byrow=TRUE))
cacheSolve(x)
