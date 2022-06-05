

  ## Following the template offered on the quiz, we set the value of the vector, but this time we use 
  ## the matrix() function, so we can set a matrix. The function i <- c() creates an empty (NULL) 
  ## objetc i so it can used later to store the inverse matrix. We also use the <<- operator so we can 
  ## assign a value to an object in an environment that is different from the current environment.
  ## The rest of the funcion evaluates de matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- c()
  set <- function(y) {
    x <<- y
    i <<- c()
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

  # The following function calculates the inverse matrix of special "vector" created with the above 
  # function. However, it first checks to see if the the inverse matrix has already been calculated. 
  # If so, it gets the result from the cache and skips the computation. Otherwise, it calculates the 
  # inverse matrix of the data and sets this valuein the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#############
## TESTING ##
#############

m <- matrix(c(2,4,6,8),2,2)
m

m_1 <- makeCacheMatrix(m)
cacheSolve(m_1) 



