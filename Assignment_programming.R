makeCacheMatrix <- function(x) {
  ## Initialize the cached inverse as NULL
  cached_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    ## When setting a new matrix, invalidate the cached inverse
    cached_inverse <<- NULL
  }
  
  get <- function() x
  
  ## Return a list of functions
  list(set = set, get = get)
}

cacheSolve <- function(x) {
  ## Check if a cached inverse exists
  cached_inverse <- x$cached_inverse
  if (!is.null(cached_inverse)) {
    message("Getting cached inverse")
    return(cached_inverse)
  }
  
  ## If no cached inverse, compute and cache it
  mat <- x$get()
  inverse <- solve(mat)
  x$cached_inverse <- inverse
  inverse
}


## checking the code

mat <- makeCacheMatrix(matrix(c(2, 1, 1, 3), nrow = 2))

# Set a new matrix
new_mat <- matrix(c(4, 5, 6, 7), nrow = 2)
mat$set(new_mat)

# Get the matrix using the get function
current_mat <- mat$get()

# Print the current matrix
print("Current Matrix:")
print(current_mat)

# Compute and cache the inverse using cacheSolve
inverse <- cacheSolve(mat)

# Print the computed inverse
print("Inverse Matrix:")
print(inverse)

# Get the cached inverse
cached_inverse <- mat$cached_inverse

# Print the cached inverse
print("Cached Inverse Matrix:")
print(cached_inverse)









