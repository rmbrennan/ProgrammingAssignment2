## This function is designed in two parts. The first part creates a special "matrix" object that can
## cache its inverse. The second function computes the inverse of the special "matrix" returned by the
## function above. If the inverse has already been calculated, then the second function retrieves the
## inverse from the cache

## This function creates a matrix object "x" and can cache its inverse "i"

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the matrix defined in the above function
## If inverse has not been calculated, then inverse is computed. If it has been calculated, then inverse
## is pulled from cache

## Return a matrix that is the inverse of 'x'
## Calculates and displays inverse if not stored

cacheSolve <- function(x, ...) {

  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

## This code worked when checking on the test matrices linked in the forum
