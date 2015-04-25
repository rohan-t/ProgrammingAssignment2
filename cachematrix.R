## z <- makeCacheMatrix(some matrix)
## cacheSolve(z)

makeCacheMatrix <- function(x = matrix()){
  inv<-NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) inv <<- solve
  getmatrix <-function() inv
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...){
  ## function, reusing cached result if it is available
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getmatrix()
  if(!is.null(inv)){
    message("cached data...")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setmatrix(inv)
  inv
}
