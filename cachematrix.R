## This are two functions that create a cached matrix and it inverse to 
## reduce computation time if that object is needed again

## makeCacheMatrix -- creates an object to hold a user defined matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Return a matrix that is the inverse of 'x'
	
	    s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve -- returns the inverse of the matrix created with makeCacheMatrix
## first it checks if the inverse of that matrix already have been cached
## if so, then it returns the inversed cached matrix, if not then it calculates it and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
