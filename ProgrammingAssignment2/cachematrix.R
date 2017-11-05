# ### Assignment: Caching the Inverse of a Matrix
# 
#
# T. Broekhof
#
#
#
# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly (there are also alternatives to matrix inversion that we will
#             not discuss here). Your assignment is to write a pair of functions that
# cache the inverse of a matrix.
# 
# Write the following functions:
#   
#   1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.
# 2.  `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
# 
# Computing the inverse of a square matrix can be done with the `solve`
# function in R. For example, if `X` is a square invertible matrix, then
# `solve(X)` returns its inverse.

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) { # receives matrix x
      s <- NULL           # initialize inverse matrix object s as a placeholder for future use
    set <- function(y) {  # defines a function to set matrix x, ....
      x <<- y             # to a new matrix y,....
      s <<- NULL          # and resets the inverse matrix object s to NULL, x and y are stored outside this environment via <<- operator
    }
    get <- function() x   # returns matrix x
    setinverse <- function(solve) s <<- solve # get inverse matrix 's' from cache called solve 
    getinverse <- function() s # returns object 's'
    list(set = set, get = get,   # list of arguments
         setinverse = setinverse, # continued ..
         getinverse = getinverse) # end of list
}
  
cacheSolve <- function(x, ...) {  # function receives matrix x and is stored in variable 'cachesolve' 
  s <- x$getinverse()   # call getinverse function on matrix x and store result in s
  if(!is.null(s)) {     # Is a cached version of x available from s ?
    message("getting cached data") # if so return a message
    return(s)           # ....and cached object
  }                     # else: previously not cached - cache empty
  data <- x$get()       # call get functon on matrix 'x' and store it as variable data
  s <- solve(data, ...) # set s as a variable to hold the inverse matrix, resulting from solve function on 'data'
  x$setinverse(s)       # set the inverse matrix of x in s
  s                     # return inverse matrix of x
}

# # Run test
# 
# # Create 2x2 matrix of 4 random numbers
# > # Run test
#   > 
#   > # Create 2x2 matrix of 4 random numbers
#   > my_matrix <- matrix(runif(4),2,2)
# > my_matrix
# [,1]        [,2]
# [1,] 0.5765377 0.003652904
# [2,] 0.6890518 0.797181142


# > CacheMatrix <- makeCacheMatrix(my_matrix)
# > CacheMatrix
# $set
# function (y) 
# {
#   x <<- y
#   s <<- NULL
# }
# <environment: 0x0000000004988b18>
#   
#   $get
# function () 
#   x
# <environment: 0x0000000004988b18>
#   
#   $setinverse
# function (solve) 
#   s <<- solve
# <environment: 0x0000000004988b18>
#   
#   $getinverse
# function () 
#   s
# <environment: 0x0000000004988b18>
#   
#   > 
#   > CacheMatrix$get()
# [,1]        [,2]
# [1,] 0.5765377 0.003652904
# [2,] 0.6890518 0.797181142
# > 
#   > 
#   > CacheMatrix$getinverse()
# NULL
# > 