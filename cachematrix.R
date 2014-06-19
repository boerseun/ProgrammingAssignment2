## COURSERA.rprog-004     Johann Beukes     June 18th, 2014     v:1.0.0.0
###############################################################################

makeCacheMatrix <- function( m = matrix() ) {
  ## Creates a special matrix object that can cache its inverse
  # Args:
  #   m: A square matrix
  # Returns:
  #   A list with 4 functions.
  # Example call: 
  #  A <- makeCacheMatrix(matrix(c(0, 2, 2, 0), nrow = 2,  ncol = 2))
  #############################################################################
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Function 1
      # Set the matrix function passing the argument (matrix) passed to 
      # the main function (makeCacheMatrix) into this sub-function.
      set <- function( matrix ) {
        ## Using the the <<- operator which can be used to assign a value to an 
        ## object in an environment that is different from the current environment
        m <<- matrix
        i <<- NULL
      }
  
  ## Function 2
      # Get the matrix function
      get <- function() {
        ## Return the matrix
        m
      }
    
  ## Function 3
      # Function to set the inverse of the matrix
      setInverse <- function(inverse) {
        i <<- inverse
      }
  
  ## Function 4
      # Method to get the inverse of the matrix
      getInverse <- function() {
        ## Return the inverse property
        i
      }
  
  ## Return a list of the functions
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned by 
  # makeCacheMatrix function. If the inverse has already been calculated 
  # (and the matrix has not changed), then the cachesolve should retrieve 
  # the inverse from the cache.
  # Args:
  #   x: A list of functions
  # Returns:
  #   The inverse of a matrix, or the cached version if already set before
  # Example call: 
  #  cacheSolve(A)
  #############################################################################
  
  ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
  ## Just return the inverse if its already set
    if( !is.null(m) ) { ## A is-not-null check is done to see if we have cache
      message("Getting cached data!")
      return(m) ## This will exit the function and stop execution
    }
  
  ## Get the matrix from our object
    data <- x$get()
  
  ## Calculate the inverse 
    m  <- solve(data, ...) 
    ## Note: The solve(a, b, ...) function in R computes the inverse of a square matrix
  
  ## Set the inverse to the object
    x$setInverse(m)
  
  ## Return the matrix
    m
}