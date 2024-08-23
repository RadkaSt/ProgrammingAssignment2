# This script contains two functions: makeCacheMatrix and cacheSolve.
#
# 1. makeCacheMatrix: 
#    - This function creates a special "matrix" object that can store a matrix 
#      and cache its inverse. 
#    - It returns a list of four functions to:
#        - Set the value of the matrix.
#        - Get the value of the matrix.
#        - Set the value of the inverse.
#        - Get the value of the inverse.
#    - If the matrix is modified using the 'set' function, the cached inverse is 
#      cleared and will need to be recalculated when next needed.
#
# 2. cacheSolve:
#    - This function computes the inverse of the "matrix" created by makeCacheMatrix. 
#    - If the inverse has already been calculated and is stored in the cache, it retrieves 
#      the inverse from the cache, avoiding the need for redundant computation.
#    - If the inverse has not been calculated, it computes the inverse using the 
#      `solve` function in R, caches it, and returns the result.
#
# The caching mechanism helps improve performance, especially when working with 
# large matrices, by avoiding repeated calculations of the matrix inverse.

## This function creates a list of four methods (set, get, setInverse, and getInverse) 
##to interact with a matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initialize the null property
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clear the cached inverse when the matrix is reset
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the methods to interact with the matrix and the inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function checks if the inverse has been cached. If so, it returns the cached inverse; 
##otherwise, it computes the inverse, caches it, and then returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()  # Retrieve cached inverse if it exists
  
  # If the inverse has already been calculated, return it
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  # Otherwise, calculate the inverse
  mat <- x$get()  # Get the matrix from the special "matrix" object
  inv <- solve(mat, ...)  # Calculate the inverse using the solve() function
  x$setInverse(inv)  # Cache the calculated inverse
  
  inv  # Return the inverse
}

## Usage example
# Create a matrix
my_matrix <- matrix(c(1, 2, 3, 4), 2, 2)

# Create the special "matrix" using makeCacheMatrix
cached_matrix <- makeCacheMatrix(my_matrix)

# Compute the inverse using cacheSolve
inverse_matrix <- cacheSolve(cached_matrix)

# Get the cached inverse on the next call (no need to compute it again)
inverse_matrix_cached <- cacheSolve(cached_matrix)
print(my_matrix)
print(inverse_matrix)
print(inverse_matrix_cached)
