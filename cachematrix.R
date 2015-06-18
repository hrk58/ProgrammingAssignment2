## The functions in this file are used for calculating the inverse matrix of
## a provided matrix that is invertible. The first function makeCacheMatrix
## creates a list containing the funtions required by the function: cacheSolve.

## =======
## makeCacheMatrix

## This function takes a provided matrix and creates and returns a list of 
## functions that can be used to perform operations on that matrix.  T


makeCacheMatrix <- function(x = matrix()) {
     
     ## x is an invertible matrix.
     
     m <- NULL                ## Initialize the inverse matrix space to NULL
  
     ## ==========
     ## set stores a matrix into the cache storage.
     ## The inverse matrix cache space is reset since a new matrix has been 
     ## specified.
     ## ==========
     
     set <- function (y) {
          
          if (!all.equal(x,y)) {
               
               ## If a new matrix is being set, then save it in cache area and
               ## reset the inverse matrix to NULL
               
               x <<- y
               m <<- NULL
          }
          else {
               
               message("INFO: Matrices are the same. No changes made")
          }
     }
     
     ## ===========
     ## get returns the original matrix
     ## =========== 
     
     get <- function() {
          x
     }
     
     
     ## ===========
     ## setInverse stores the inverse matrix in the cache area NOTE: The
     ## cache area must be NULL before setting the Inverse. This minimizes the
     ## chance that the matrix and inverse matrix do not correspond.
     ## ===========
     
     
     setInverse <- function(mInverse) {
          
          if (is.null(m)) {
               m <<- mInverse
          }
          else {
               message("ERROR: Unable to set inverse. The cache is not NULL.")
          }
          
     }
     

     ## ===========
     ## getInverse returns the inverse matrix from the cache area.
     ## ===========
     
     getInverse <- function () {
          m
     }
     
     
     ## return the list of functions created 
     
     list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## =============
## cacheSolve returns the inverse matrix for an invertible matrix. The inverse
## matrix is returned if it is already stored in cache, or it is computed from
## the matrix, stored in cache, and returned.
## =============


cacheSolve <- function(x, ...) {
        
     ## x   is the list of the functions and caches
     
     m <- x$getInverse()      # Get the inverse matrix if it already exists
     
     if (!is.null(m)) {       # If matrix exists return
          
          message ("getting cached data")
          return (m)
     }
     
     data <- x$get()          # Inverse matrix needs to be computed. Get matrix
     m <- solve(data, ...)    # Compute the inverse matrix

     x$setInverse (m)         # Place the inverse matrix in cache
     
     return(m)                # Return the inverse matrix.
}
