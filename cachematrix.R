## returns cached matrix inverse if already calculated,
## otherwise calculates the matrix inverse

## return list containing functions to set and get 
## value of matrix and set and get value of inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## compute inverse of special "matrix" or return
## cached inverse if already computed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
