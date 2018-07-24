# First of all sorry everyone, I've been into too much work load and didn;t have time to study on time!
# This method creates a Matrix that can be inverse and cached
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
+    inv <<- NULL
+  }
+  get <- function() x
+  setinv <- function(inverse) inv <<- inverse
+  getinv <- function() inv
+  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



# Calculate the inverse of a matrix or return the cached version if the calculation was executed previously.
# If the calculation is executed, the result is cached for fututr reference
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("get cache")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}