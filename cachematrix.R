## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { # this will store the matrix to be inverted into the unction
    x <<- y
    i <<- NULL
  }
  get <- function() x # this will "extract" the matrix from inide the function
  setinv <- function(inv) i <<- inv # this will allow us to "save" the inversed matrix
  getinv <- function() i # this will allow us to get the previously saved matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv() # we load the inverse matrix from the cachematrix vector
  if(!is.null(i)) { 
    message("getting cached data")
    return(i) # we return the cached result
  }# if the inverse has not yet been calculated we'll get a null value, and 
   # it will be calculated
  data <- x$get() # we get the "actual" matrix frommthe cachematrix
  i <- solve(data, ...) # we compute the inverted matrix
  x$setinv(i) # we store the result for future uses
  i # we return the result for this particular use of cacheSolve
}
