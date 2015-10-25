## The combination of the two functions caches the inverse 
## of a matrix

## This below function creates a special "matrix" object that can 
## cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m 
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This funciton computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## check if it is already cached
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}



