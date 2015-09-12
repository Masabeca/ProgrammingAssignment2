## I wrote a function called "speedtest()", which takes in any invertible matrix, 
## calculates its inverse twice using the below functions, and prints out the times it takes for both runs. 
## The first run takes longer than the second run because it actually calculates 
## the inverse, while the second run only does a look-up from the cache.

## The following functions can compute and cache the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
  ## makeCasheMatrix creates a special "matrix" object that can cache its inverse
  ## x: a invertible matrix
  inv = NULL
  set = function(y) {
    ## use "<<-" to assign a value to an object in an environment 
    ## different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x 
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## return: a list containing functions to
##           1. set the matrix
##           2. get the matrix
##           3. set the inverse
##           4. get the inverse
##    this list is used as the input to cacheSolve()


cacheSolve <- function(x, ...) {
  ## x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # If the inverse has already been calculated
  # get it from the cache and skips the computation with message
  
  if (!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  
  x$setinv(inv)
  
  return(inv)
} 


## Test:

speedtest = function(matsq) {
  ## matsq: an a square invertible matrix
  
  temp = makeCacheMatrix(matsq)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

## Test with square matrix of 525 rows and 525 columns filled with normal random numbers.

set.seed(300000)
r = rnorm(275625)
matsq = matrix(r, nrow=525, ncol=525)
speedtest(matsq)


##Results of test:

##Time difference of 0.2988369 secs
##getting cached data
##Time difference of 0.0009739399 secs

#The time difference for second run function is a 99,67% decrease, and this speed test function represents the power of caching.
