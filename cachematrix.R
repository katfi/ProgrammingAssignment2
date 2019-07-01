## Assigment 2 (week 3): Caching the Inverse of a Matrix


## Following function makes an additional variable m, there m saves as an invertible matrix

makeCacheMatrix <- function( m = matrix() ) {
  
  myCache <- NULL
  
  set <- function( matrix ) {
    m <<- matrix
    myCache <<- NULL
  }
  
  get <- function() {
    m
  }
  
  setInverse <- function(inverse) {
    myCache <<- inverse
  }
  
  getInverse <- function() {
    myCache
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##If the variable is in memory it is displayed directly and calculated otherwise
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    return(m)
  }
  
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
## To check, for example: create matrix m1, write mm <- makeCacheMatrix(m1) and cacheSolve(mm)
