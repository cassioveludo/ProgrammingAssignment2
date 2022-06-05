

  ## Following the template offered on the quiz, we set the value of the vector, but this time we use 
  ## the matrix() function, so we can set a matrix. The command inv <- c() creates an empty (NULL) 
  ## object inv so it can used later to store the inverse matrix. We also use the <<- operator so we can 
  ## assign a value to an object in an environment that is different from the current environment.
  ## The rest of the function sets the inverse matrix and caches the result


makeCacheMatrix <- function(x = matrix()) {
  inv <- c()
  set <- function(y) {
    x <<- y
    n <<- c()
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

  # The following function calculates the inverse matrix of special "vector" created with the above 
  # function. However, it first checks to see if the the inverse matrix has already been calculated. 
  # If so, it gets the result from the cache and skips the computation. Otherwise, it calculates the 
  # inverse matrix of the data and sets this value via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
