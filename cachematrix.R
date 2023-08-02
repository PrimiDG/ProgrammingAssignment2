## These functions correspond to the Programming Assignment 2: Lexical Scoping
## for the R programming course

## First function creates a special type of cache matrix, which stores the values of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y){
    x <<-y
    i <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## This second function, performs the inverse if it is necessary or only calls the chave values.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
