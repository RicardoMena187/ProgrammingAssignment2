##Su tarea es escribir un par de funciones que almacenan en 
## caché el inverso de una matriz

##Creamos una funcion que llama a la función inversa de una matriz
## y guarda en cache

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

##La funcion busca la inversa de una matriz 
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

