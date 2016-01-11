## Put comments here that give an overall description of what your
## functions do

# The function makeCacheMatrix creates a new object out of an input matrix x.
# The function cacheSolve uses this object to cache information.

## Write a short comment describing this function

# The function makeCacheMatrix transforms the input matrix x into a list of functions 
# that can be applied to x to set or return x or to set or return i, the inverse of x

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
              x <<- y
              i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# The function cacheSolve uses the functions in the object created by makeCacheMatrix.
# It first checks if i already exists (cached). 
# If so, it returns i via the getinverse function in the object. 
# If not, it calculates the inverse and sets it via the setinverse function in the object


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}