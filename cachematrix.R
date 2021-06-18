makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # This assigns NULL to the inv variable 
  set <- function(y) {  # This set the value of the matrix using another function
    x <<- y   # The <<- operator is used to assign a value to an object in an environment different from the current environment
    inv <<- NULL
  }
  get <- function() x   # Outside the set function, this gets the value of the matrix
  setinverse <- function(inverse) (inv <<- inverse)   # This sets the inverse of the matrix 
  getinverse <- function() inv  # This gets the value of the inverse 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {  # This function computes the inverse of the function created with the makeCacheMatrix function 
  inv <- x$getinverse()   # This returns a matrix that is inverse of x and assigns it to inv 
  if(!is.null(inv)) {   # If a inverse has already been calculated it is not computed and simply returned
    message("getting cached data")
    return(inv)
  }
  data <- x$get()   # Otherwise, the inverse of the matrix must be computed and set the value from the cache
  inv <- solve(data, ...)   # solve is the standard R function for computing the inverse 
  x$setinverse(inv)   # This set the inverse of the matrix
  inv
}