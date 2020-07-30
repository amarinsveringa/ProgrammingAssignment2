## these functions cache the inverse of a matrix rather than computing it repreatedly

## creates a special matrix object (that can cache its inverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) {                    ## define the set function to assign new 
    x <<- y                             ## value of matrix in parent environment
    inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                     ## define the get fucntion - returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
  ## to the functions with the $ operator
}


## computes or returns the inverse of the special matrix that above function returned

cacheSolve <- function(x,list, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- setinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  }

