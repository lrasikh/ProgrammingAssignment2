## In this assignment, I have written a set of functions that caches a matrix. 
##  makeCacheMatrix: here I created a cacheable matrix and it contains subset functions and inputs that are as follows:
##  x: this is the input cacheable matrix.
##  set: this subset function sets a completely new matrix. Since I make a habit of setting new values every time in R, I assigned this so that it can be used to invalidate the old matrix once new matrix is provided. Z is the matrix to be given.
##  get: I have a habit of always using get function to call the current stored matrix.
##  cacheSolve: it is used for calculating and caching the results of matrix for future references. 
##  cached_inverse: I have created this to hold the cached inverse of the provided matrix. I have assigned it NULL and it stores the already calculated inverse matrix for retrieving upon the need.
##  list(set = set, get = get): here I have listed the set and get functions to allow a variety of operations on the matrices. 
##  To check the existence of the I have done the following conditions:
##  if (!is.null(cached_inverse)): the conditional if checks whether cached inverse exists for the given matrix.
##  mat <- x$get(): it can be used simply to retrieve objects from the matrix.
##  x$cached_inverse <- inverse: I have set this for the storage of inverse matrix for future reference. 

makeCacheMatrix <- function(x) {
    ## here I created a cacheable matrix and it contains subset functions and inputs that are as follows:

    cached_inverse <- NULL
    
    set <- function(z) {
        x <<- z
        ## this subset function sets a completely new matrix. Since I make a habit of setting new values every time in R,
        ## I assigned this so that it can be used to invalidate the old matrix once new matrix is provided.
        ## Z is the matrix to be given.
        ## I have created the line below to hold the cached inverse of the provided matrix.
        ## I have assigned it NULL and it stores the already calculated inverse matrix for retrieving upon the need.
        cached_inverse <<- NULL
    }
    
    get <- function() x
    
    ## I have a habit of always using get function to call the current stored matrix.
    ## set: this subset function sets a completely new matrix. Since I make a habit of setting new values every time in R. 
    ## I assigned this so that it can be used to invalidate the old matrix once new matrix is provided. Z is the matrix to be given.
    ## get: I have a habit of always using get function to call the current stored matrix.
    list(set = set, get = get)
    }

cacheSolve <- function(x) {
    ## it is used for calculating and caching the results of matrix for future references.
    cached_inverse <- x$cached_inverse
    if (!is.null(cached_inverse)) {
    ## the conditional if checks whether cached inverse exists for the given matrix.
        message("Getting cached inverse")
        return(cached_inverse)
    }
    
    ## Computations and storage
    mat <- x$get()
    ## it can be used simply to retrieve objects from the matrix.
    inverse <- solve(mat)
    ## this calculates the inverse of matrix
    x$cached_inverse <- inverse
    ## I have set this for the storage of inverse matrix for future reference
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




