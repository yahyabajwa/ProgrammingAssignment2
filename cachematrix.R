
makeCacheMatrix <- function(x = matrix()) {
  int <- NULL
  set <- function(a) {
    mat <<- a
    int <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverse) int <<- inverse
  getinverse <- function() int
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(mat, ...) {
  int <- int$getinverse()   
  if(!is.null(int)) {
    message("getting cached data")
    return(int)
  }
  else{
  data <- mat$get()
  int <- mean(data, ...)
  mat$setinverse(int)
  int
  }
}
