## R programming ass. 2, cache matrix inversion functions 
## lexical scoping

## Creating a matrix with get and set and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ## GET for matrix
  get <- function()x
  ## SET ofr inverse
  setInverse <- function(inverse) m <<- inverse
  ## GET for inverse
  getInverse <- function() m 
  ## Retuning a list for set and get functions
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## cachSolve returns cached inverse if already calculated instead of re calculating

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
