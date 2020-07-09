
##This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x  =  matrix ()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setresol <- function(resol) r <<- resol
  getresol <- function() r
  list(set = set, get = get,
       setresol = setresol,
       getresol = getresol)
}

##cacheSolve retrieves the inverse of the cache. 
##If the inverse has already been calculated 
## and the matrix has not changed.

cacheSolve <- function(x, ...) {
  r <- x$getresol()
  if(!is.null(r)) {
    message("Obteniendo cache matriz")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setresol(r)
  r
}

