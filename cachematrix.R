## This function takes as input @x: a square invertible matrix
## It returns: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL # this initializes inv, the cached matrix inverse, to be null.
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y #since we now have a new matrix in x, 
    inv <<- NULL #we clear the previously cached inverse
  }
  #these are the other three functions that will be returned in the list, so cacheSolve can use them.
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  #these are the four functions that will be returned in the list, so cacheSolve can use them to calculate the inverse of x.
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse (inv) has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # if the inverse has already been calculated
  if(!is.null(inv)) {
    # get it from the cache and skips the computation. 
    message("getting cached data.")
    return(inv)
  }
  # otherwise, calculates the inverse 
  
  data <- x$get()
  inv <- solve(data)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(inv)
  return(inv)
}

#Test code to see if this works:
## At the command line enter the following: 
## > x = rbind(c(1, -1/2), c(-5, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    1 -0.5
## [2,]   -5  1.0

## No cache in the first run
## > cacheSolve(m)
## [,1]       [,2]
## [1,] -0.6666667 -0.3333333
## [2,] -3.3333333 -0.6666667

## > cacheSolve(m)
## getting cached data.
##  [,1]       [,2]
##  [1,] -0.6666667 -0.3333333
##  [2,] -3.3333333 -0.6666667
##  > 