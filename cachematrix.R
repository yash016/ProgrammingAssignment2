## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                 ## Initialize variable i as NULL
  set <- function(y) {      ## Define set function
    x <<- y                 ## Value of matrix in parent environment
    i <<- NULL              ## reset i to NULL, if there is a new matrix
  }
  get <- function() x       ## define the get function to return the value of matrix
  setinverse <- function(inverse) i <<- inverse  ## assign value of i in parent environment
  getinverse <- function() i    ## gets the value of i when function is called
  list(set = set, get = get,    ## to refer to function with $ operator
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function defined above
## If inverse has already been calculated then the cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inverse(data, ...)
  x$setinverse(i)
  i
}
