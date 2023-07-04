## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL                     # Create a variable 'j' and initialize it as NULL
    set <- function(y) {          # Define the 'set' function to set the matrix
        x <<- y                     # Assign the input 'y' to the 'x' variable using the '<<-' operator
        j <<- NULL                  # Clear the previous inverse stored in 'j'
    }
    get <- function() x           # Define the 'get' function to retrieve the matrix
    
    setInverse <- function(inverse) j <<- inverse  # Define the 'setInverse' function to set the inverse
    getInverse <- function() j                     # Define the 'getInverse' function to retrieve the inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    # Return a list of functions: 'set', 'get', 'setInverse', and 'getInverse'
}
cacheSolve <- function(x, ...) {
    j <- x$getInverse()       # Retrieve the cached inverse from 'x' using the 'getInverse' function
    if (!is.null(j)) {        # Check if the inverse is already cached
        message("getting cached data")
        return(j)               # If the inverse is cached, return it directly
    }
    
    mat <- x$get()            # Retrieve the matrix from 'x' using the 'get' function
    j <- solve(mat, ...)      # Calculate the inverse of the matrix using 'solve' function
    
    x$setInverse(j)           # Cache the calculated inverse in 'x' using the 'setInverse' function
    j                         # Return the calculated inverse
}
