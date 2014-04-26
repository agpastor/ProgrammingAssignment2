## makeCachematrix creates a 'special object' or list that takes can 
## cache the inverse of a given matrix and overwrite the old cached value when 
## data is updated

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {    # sets matrix to a new matrix
      x <<- y    
      inv <<- NULL          # invalidates cache of inverse
    }
    get <- function() x     #returns current matrix 
    setInverse <- function(inverse) {
      inv <<- inverse #sets new matrix inverse
    }
    getInverse <- function() inv #returns new matrix inverse
    list(set = set, get = get,
         setinverse = setInverse,
         getinverse = getInverse)
  }




## cacheSolve calls on subsetted elements of the  list that was 
## created by makeCacheMatrix to return a new inverse when needed 

cacheSolve <- function(x, ...) {     # x arg is the list that is fed in by makeCacheMatrix
    inv <- x$getinverse()            # assigns currently stored inverse to inv
    if(!is.null(inv)) {              #if the currently stored inverse is still valid 
      message("getting cached data") # a msg is printed
      return(inv)                    # and the cached matrix is returned
    }
    data <- x$get()          # retrieves updated matrix and assigns to data 
    inv <- solve(data, ...)  # finds the inverse of the new matrix and sets it to inv
    x$setinverse(inv)        # caches the new inverse 
    inv 
  }
        ## Returns a matrix that is the inverse of 'x'

