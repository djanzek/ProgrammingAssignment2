## "makeCacheMatrix" creates a special matrix that is able to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      
      ## Initilize the variable that will store the inverse cache.
      cachedInv <- NULL
      
      ## Set a new matrix, delete the cache.
      set <- function(newMatrix) {
            x <<- newMatrix
            cachedInv <<- NULL
      }
      
      ## Return the matrix.
      get <- function() {
            x
      }
      
      ## Set the cached inverse.
      setInverse <- function(inverse) {
            cachedInv <<- inverse
      }
      
      ## Return the cached inverse.
      getInverse <- function() {
            cachedInv
      }
      
      ## Return the special matrix.
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## "cacheSolve" takes a special matrix that was created with "makeCacheMatrix",
## checks if its cached inverse exists and solves for its inverse if it has not
## already been cached.
cacheSolve <- function(x, ...) {
      
      ## Check if cached data exsts.
      inverseX <- x$getInverse()
      if (!is.null(inverseX)) {

            ## We already have cached data, just return it.
            return(inverseX)
      }
      
      ## We have no cached data. Solve for the inverse and cache it.
      inverseX <- solve(x$get()
      )
      x$setInverse(inverseX)

      ## Return the inverse.
      inverseX
}
