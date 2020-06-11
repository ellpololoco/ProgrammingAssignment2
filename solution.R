## Function cache inverse of matrix.
## This function makes "matrix" object caching the inverse.

makeCacheMatrix <- function(new = matrix()) {
  inverse <- NULL
  set <- function(y){
    new <<- y
    inverse <<- NULL
  }
  get <- function() new
  setInv <- function(soln) inv <<- soln
  getInv <- function() inverse
  list(set1 = set1, get1 = get1, setInv = setInv, getInv = getInv)
}


## Function to calculate inverse of matrix returned by makeCacheMatrix function defined above.
cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("fetching cahed data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setInverse(inver)
  inver      
}
