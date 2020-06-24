## Function used to make matrix

makeCacheMatrix <- function(x = matrix()) {
      invertibleMatrix <- NULL
      
      set <- function(y){
          x <<- y
          invertibleMatrix <<- NULL
      }
  
      get <- function(){x}
      setInverse <- function(inverse) {invertibleMatrix <<- inverse}
      getInverse <- function(){invertibleMatrix}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Cache matrix

cacheSolve <- function(x, ...) {
      invertibleMatrix <- x$getInverse()
      
      if(!is.null(invertibleMatrix)){
            message("Getting cached data")
            return(invertibleMatrix)
      }
      
      mat <- x$get()
      invertibleMatrix <- solve(mat, ...)
      x$setInverse(invertibleMatrix)
      invertibleMatrix
        
}
