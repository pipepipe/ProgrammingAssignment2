## Using cache for quick matrix inverting


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setInvert <- function(inv) m <<- inv
            getInvert <- function() m
            list(set = set, get = get,
                 setInvert = setInvert,
                 getInvert = getInvert)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## Retrieve the inverse from the cache if "matrix" is the same.

cacheSolve <- function(x, ...) {
		
               m <- x$getInvert()
               if(!is.null(m)) {
                           message("getting cached data")
                           return(m)
               }
               data <- x$get()
               m <- solve(data, ...)
               x$setInvert(m)
               m
}		
