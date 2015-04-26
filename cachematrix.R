## The following functions allow for the caching of matrix inverse
## calculations, given an input matrix that is assumed to be
## invertible

## Create a list of modifier functions to allow
## for cachable calculations of the matrix inverse of the function
## parameter x
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return cached matrix inverse if available,
## otherwise calculate inverse, cache it, and return it
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
