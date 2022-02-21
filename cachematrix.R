## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function takes a numeric vector as an input and transforms it internally
# into a matrix via the matrix() function and feeding in the square root of 
# vector length as parameters. Once a matrix is created, we run it through
# our makeCacheMatrix() function prior to feeding it into our final cacheSolve()
# as variable 'x'. Lastly, our function will determine whether or not the result
# is already in cache, if not - a result will be found via Solve. Our final
# output ends up being the inverse matrix for the Original Matrix generated 
# by our function using the numeric vector as input.



CalcInverseMatrix <- function(numvec){
  
  ## Here we transform our numeric vector into a matrix.

  matx <- matrix(numvec,nrow = sqrt(length(numvec)),ncol = sqrt(length(numvec)))
  print("Input Matrix (Original):")
  print(matx)
  
  
  ## Here we create the cache matrix for a given numeric vector 'x'.
  makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get, setinverse=setinverse,getinverse=getinverse)
  }
  
  x <- makeCacheMatrix(matx)
  
  # We feed our 'x' variable into cacheSolve.
  
  cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
     ## Return a matrix that is the inverse of 'x'.
  }
  result <- cacheSolve(x)
  result
  print("Output Matrix (Inverse):")
  print(result)
}