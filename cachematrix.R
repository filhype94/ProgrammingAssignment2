## Put comments here that give an overall description of what your
## functions do

# The 2 following functions compute and cache matrix inversion so as to avoid repeating matrix inversion
# that is a costly operation.

## Write a short comment describing this function
# makeCacheMatrix returns a list of functions (set, get, setinvert, getinvert):
#     - set/get are setter/getter for the cache matrix
#     - setinvert/getinvert are setter/getter for the cache invert

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y){
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) invert <<- solve
        getinvert <- function() invert
        list( set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}

## Write a short comment describing this function
# cacheSolve checks if invert matrix can be retrieved from the cache (and returns it) or computes
# the invert of the matrix and put it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getinvert()
        if(!is.null(invert)){
                message("Invert matrix retrieved from cache")
                return invert
        }
        data <- x$get()
        invert <- solve(data, ...)
        x$setinvert(invert)
        invert
}
