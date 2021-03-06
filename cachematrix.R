# Function makeCacheMatrix: construct object of class 'CacheMatrix' from input.
#    First, check to ensure that input x is a square matrix (and error if not).
#    Then, define:
#      * The set method 
#      * The setInverse method
#      * The accessor method get
#      * The accessor method getInverse

makeCacheMatrix <- function(x = matrix()) {
  
  # Ensure that the input is a square matrix.
  if(!is.matrix(x) || nrow(x) != ncol(x)){
    stop("Argument must be a square matrix!")
  }
  x_inv <- NULL
  
  # Set the value of the matrix: assign a matrix to x, reset x_inv
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  
  # Get the matrix x  (i.e. the input to constructor)
  get <- function() x
  
  # Set the inverse of the matrix (assigns a matrix to x_inv)
  setInverse <- function(y) x_inv <<- y
  
  # Get the inverse of the matrix (returns x_inv) 
  getInverse <- function() {
    if(!is.null(x_inv)){ return(x_inv) }
    else{ stop("The inverse hasn't been calculated yet!") }
  }
  cm <- list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
  
  # Give the instance a class name
  class(cm) <- "CacheMatrix"
  return(cm)
}



# Function cacheSolve: returns the inverse of the CacheMatrix.
# - It checks to see if the inverse has already been calculated. 
#    * If so, it returns the inverse from the cache. 
#    * Otherwise, it computes the inverse of the matrix and caches it
#       via the setInverse function:
#        - If solve returns error, matrix is not invertible,
#            so return correctly-sized matrix of NA's.

cacheSolve <- function(x, ...) {

  inv <- try(x$getInverse(), silent=TRUE)
  
  # Check to see whether inverse has already been set
  if(inherits(inv,"try-error") || is.null(inv)){ # inverse not yet set
    message("Calculating inverse...")
    m <- x$get() 
    calc_inv <- try(solve(m)) # try calculating the inverse
    if(inherits(calc_inv,"try-error")){ 
      message("The matrix is not invertible... setting to NA.")
      calc_inv <- matrix(data=NA, nrow=nrow(m), ncol=ncol(m))
    }
    (x$setInverse(calc_inv)) # r shorthand () sets and returns the matrix
  }
  else{ # inverse has already been set
    message("Getting cached inverse...")
    return(inv)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Test the Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Function rUnifSqMatrix:  returns square matrix of uniformly distributed
#    random numbers for testing.  This test matrix is unlikely to be singular
#    (i.e. non-invertible).

rUnifSqMatrix <- function(n){
  matrix(runif(n^2), nrow=n, ncol=n)
}

l <- 2000 # size for an example that runs not too fast, not too slow

# Informal example (not a benchmark) to show that the cache works, saves time:
test_matrix <- rUnifSqMatrix(l)

# First, just solve the base matrix using solve():
system.time(ans_solve <- solve(test_matrix)) # ~0.02s CPU time (at l = 2000)

# Now construct a CacheMatrix:
cm <- makeCacheMatrix(test_matrix)

# Demonstrate that constructor did not set the inverse:
cm$getInverse() # returns error: The inverse hasn't been calculated yet!

# Demonstrate that cacheSolve returns the inverse:
system.time(ans_cachesolve <- cacheSolve(cm)) # ~0.01s CPU time

# Demonstrate that cacheSolve set the inverse
system.time(ans_getcache <- cm$getInverse()) # 0s CPU time

# The example shows that if you might need to calculate the inverse more
#   than once, using cacheSolve will save you time for any repeated
#   calculations.  The tradeoff is that you will have to store the inverse
#   in memory too.  Thus, this function is useful when your matrix is large
#   enough that calculating the inverse is non-negligible for the CPU but not
#   so large that storing another matrix of the same size runs you out
#   of memory, at which point you should explore other options.

# Demonstrate that the results returned by base solve, cacheSolve, and 
#   getInverse are the same:
identical(ans_solve, ans_cachesolve) # TRUE
identical(ans_solve, ans_getcache) # TRUE

# Demonstrate that the calculated inverse is correct (allow "near equality"):
all.equal(test_matrix %*% ans_getcache, diag(l)) # TRUE