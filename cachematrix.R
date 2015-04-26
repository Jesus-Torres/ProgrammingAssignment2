# Function makeCacheMatrix:
# Matrix Cache Handler with Intrinsic Functions
# to get, set, get inverse matrix and set inverse matrix 

makeCacheMatrix <- function(matrix = numeric()) {
   invMatrix <- NULL
   set <- function(newmatrix) {
	   matrix <<- newmatrix
	   invMatrix <<- NULL
   }
   get <- function() matrix
   setinv <- function(mean) invMatrix <<- mean
   getinv <- function() invMatrix
   list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Function cacheSolve:
# Inverse Matrix Calculation, keeping cached version
# It will not calculate the inverse if the given
# matrix was already calculated

cacheSolve <- function(matrix, ...) {
  invMatrix <- matrix$getinv()
  if(!is.null(invMatrix)) {
    message("getting cached inverted matrix")
    return(invMatrix)
  }
  data <- matrix$get()
  invMatrix <- solve(data, ...)
  matrix$setinv(invMatrix)
  invMatrix
}