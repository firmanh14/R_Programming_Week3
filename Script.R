
## Creates matrix object that can cache its own inverse

makeCacheMatrix <- function( mat = matrix() ) {
  
  ## Initialize the inverse
  inv <- NULL

  ## Set the matrix
  set <- function( matrix ) {
    mat <<- matrix
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## Return the matrix
    mat
  }
  
  ## Set the matrix inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the matrix inverse
  getInverse <- function() {
    ## Return the inverse
    inv
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix that is returned by "makeCacheMatrix".
cacheSolve <- function(x, ...) {
  
  ## Return an inverse matrix that
  mat <- x$getInverse()
  
  ## Return the inverse if the data already set
  if( !is.null(mat) ) {
    message("cached data is done")
    return(mat)
  }
  
  ## Get the matrix
  dat <- x$get()
  
  ## Calculate the matrix inverse
  mat <- solve(dat) %*% dat
  
  ## Set the inverse
  x$setInverse(mat)
  
  ## Return the matrix
  mat
}