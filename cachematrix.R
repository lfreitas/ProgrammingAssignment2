# The functions below are complements. makeCacheMatrix() uses a set of functions
# to store a matrix and its inverse in a separate environment. cacheSolve()
# sets or retrieves the inverse of an object create using makeCacheMatrix()


# function accepts a square (n x n) matrix as input and returns a list of four 
# functions, each of which accesses value stored in a separate environment:
#   set() stores the input matrix and sets the value of the inverse to NULL
#   get() retrieves the matrix stored by set()
#   setinv() stores the value of the input matrix inverse
#   getinv() retrieves the value of the input matrix inverse
rm(list=ls())

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# accepts an object created by makeCacheMatrix() as input. If the object already
# has a solved inverse stored in its environment, cacheSolve() returns that 
# value. Otherwise, cacheSolve() uses solve() to obtain the inverse of the 
# square matrix and stores the result using the setinv() function that is 
# part of the input object
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## testing parameters -- meant to illustrate how functions work together
## fix seed and create a relatively large matrix
# set.seed(90210)
# dim <- 1000
# mat <- matrix(runif(dim*dim), dim, dim)
#
# rm(obj)
# obj <- makeCacheMatrix(mat)
# 
## will likely take a couple of seconds to compute
# ptm <- proc.time()
# invisible(cacheSolve(obj))
# proc.time() - ptm
# 
## note much faster time on the second run, since the value is retrieved rather
## than calculated
# ptm <- proc.time()
# invisible(cacheSolve(obj))
# proc.time() - ptm