## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  ## function is only being used if we need to change the matrix 
  set <- function (y){
    x <<- y
    m_inverse <<- NULL
  }
  
  get <- function () x
  setInverse <- function(inverse) m_inverse <<- inverse
  getInverse <- function() m_inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m_inverse <- x$getInverse()
    if (!is.null(m_inverse)) {
      # the inverse has already been calculated, return it
      message ("getting cached inverse of the matrix")
      return (m_inverse)
    }
    # the inverse hasn't been calculated yet.
    # let's calculate and store it 
    data <- x$get()
    m_inverse <- solve(data, ...)
    # set the inverse into parent's environment
    x$setInverse(m_inverse)
    m_inverse
}
