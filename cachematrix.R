makeCacheMatrix <- function(x = matrix()) {
  ## x is a special vector and defined, implicitly
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  ## i is the variable where we'll keep the matrix
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ## This notes the data structure of the matrix
}

cacheSolve <- function(x, ...) {
## Function returns in the inverse
  i <- x$getinverse()
  if(!is.null(i)) {
## If i is not null, we've already cached the data so it doesn't get recomputed
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
## "solve" function performs an inverse (if not already cached)
  x$setinverse(i)
  i
}
