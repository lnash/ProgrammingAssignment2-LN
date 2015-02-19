# ProgrammingAssignment2-LN

## These functions cache the inverse of my matrix


## The makeCacheMatrix function creates a special "matrix" object 
## that can cache the input matrix and its inverse 


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) s <<- solve
  getmatrix <- function() s
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## The cacheSolve function calls functions stored in the matrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve retrieves the inverse from the cache. If the input is new
## it calculates the inverse of the data and sets the inverse in the cache 
## via the setinverse function.


cacheSolve <- function(x=matrix(), ...) {
  s <- x$getmatrix()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setmatrix(s)
  s
}
