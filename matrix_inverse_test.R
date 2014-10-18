### The first function creates a matrix that can cache its inverse (assuming a square matrix with an
### inverse), while the second returns the cached value.

## makeCacheMatrix creates a matrix that can cache its inverse. It also provides a way to change the
## value of the cached matrix and cue the second function to recalculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL               # Creates final output variable, stored locally
        set <- function(y) {    # Function called by user. Sets value of matrix and changes cached
                x <<- y         # value of m back to NULL.
                m <<- NULL
        }
        get <- function() x                             # Simply returns current matrix
        setinverse <- function(matrix) m <<- matrix     # Caches calculated inverse
        getinverse <- function() m                      # Retrieves cached inverse or NULL if unavaliable
        list(set = set, get = get,                      # Stores functions in list for cacheSolve's use
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve takes the matrix and functions provided by makeCacheVector and uses them to initially
## calculate and cache the matrix's inverse, then return it. When drawing from cached data, function
## indicates this to the user.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()             # Locally defines m for use within function without caching it
        if(!is.null(m)) {               # Returns cached data if already calculated, with message
                message("getting cached data")
                return(m)
        }
        data <- x$get()                 # Assigns matrix to local variable
        m <- solve(data, ...)           # Assigns matrix's inverse to local variable
        x$setinverse(m)                 # Caches matrix in variable to global variable 
        m                               # Returns newly-cached matrix
}