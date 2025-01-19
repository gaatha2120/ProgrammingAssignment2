makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
} 
# Create a sample matrix
matrix_data <- matrix(c(1, 2, 3, 4), 2, 2)

# Create a special "matrix" object
special_matrix <- makeCacheMatrix(matrix_data)

# Compute the inverse for the first time
cacheSolve(special_matrix)

# Retrieve the cached inverse
cacheSolve(special_matrix) 
