## These functions allow one to calculate and cache a matrix inverse,
## avoiding the need to recalculate the inverse if it is needed multiple times.

### Example:
### > source("./cachematrix.R")
### > mymat <- makeCacheMatrix(matrix(c(1,2,3,3,2,1,4,5,2),ncol=3,nrow=3))
### > cacheSolve(mymat)
###        [,1]   [,2]    [,3]
### [1,] -0.0625 -0.125  0.4375
### [2,]  0.6875 -0.625  0.1875
### [3,] -0.2500  0.500 -0.2500
###
### > cacheSolve(mymat)
### getting cached data
###        [,1]   [,2]    [,3]
### [1,] -0.0625 -0.125  0.4375
### [2,]  0.6875 -0.625  0.1875
### [3,] -0.2500  0.500 -0.2500


## This creates the matrix object that will store the cached inverse, and a method to calculate the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i

  return( list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)  )
}


## This function will return the inverse of a matrix, via the cached value if possible
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i) #if we already cached inverse then return and stop here
  }
  #otherwise calculate and cache inverse
  data <- x$get()
  i <- solve(data, ...) #solve for inverse - R function
  x$setinv(i) #cache inverse
  return(i)
}
