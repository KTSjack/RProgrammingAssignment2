makeCacheMatri <- function(x = matrix()) {
  ## This function has four parts.
  ## first, Set to the matrix.
  ## Then, Get the matrix.
  ## And, set up the inverse matrix.
  ## Final，Get the inverse matrix.
  
  i <- NULL                         
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse_matrix <- function(solve) i <<- solve
  getInverse_matrix <- function() i
  list(set = set, get = get,
       setInverse_matrix = setInverse_matrix,
       getInverse_matrix = getInverse_matrix)
} 



cacheSolve <- function(x, ...) {
  ## The function can get the inverse matrix about the previous vector. 
  ## But this one first check out whether previous function get 
  ## the inverse matrix.If previous one already get the inverse matrix,  
  ## then this function will obtain the inverse matrix from the cache 
  ## and skip the count.Otherwise, it will use the function getInverse_matrix
  ## to get the inverse matrix.

  i <- x$getInverse_matrix()
  if(!is.null(i)) {                 ## this step determines skip or not.
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse_matrix(i)
  i
}
