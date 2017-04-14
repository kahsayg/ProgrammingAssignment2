## This code  caches the inverse of the matrix assuming that the given matrix is invertible.
## I have written Two functions 

## The first function, makeCachematrix is like a class in c progroramming which creates a special matrix and contains functions within it that can  
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  matrxInv <- NULL
  set <- function(y) {
    x <<- y
    matrxInv <<- NULL
  }
  ## get the value of the matrix.
  get <- function() x
  ## set the inverse of the matrix.
  setInv <- function(i) matrxInv <<- i
  getInv <- function() matrxInv
  ## get the inverse of the matrix.
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## get the inverse of the matrix.
  matrxInv <- x$getInv()
  ## check if there is the matrix, if yes: print the message.
  if(!is.null(matrxInv)) {
    print("getting cached data")
    return(matrxInv)
  }
  ## if not: get the inverse of the matrix.
  data <- x$get()
  matrxInv <- solve(data, ...)
  ## set the inverse of the matrix.
  x$setInv(matrxInv)
  ##return the inverse of a matrix
  matrxInv
}
