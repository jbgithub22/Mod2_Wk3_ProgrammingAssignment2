##---------------------------------------
## Resources used to study assignment:
# 1. TIPS: Formal arguments and local variable names in the lexical scoping assignment Part 1
# https://www.coursera.org/learn/r-programming/discussions/forums/_ZerTCj2EeaZ8Apto8QB_w/threads/mowodw4rEemS0Q4U5mc4kg
#
# 2. TIPS: Formal arguments and local variable names in the lexical scoping assignment Part II
# https://www.coursera.org/learn/r-programming/discussions/forums/_ZerTCj2EeaZ8Apto8QB_w/threads/n_UhPw4tEembrRJ2GRXQmA
#
# 3. Advanced R: Environments: The Function Environment
# https://adv-r.hadley.nz/environments.html
# 
# 4. Demystifying makeVector()
# https://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in-r
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
#
# 5. Inverse of Matrix in R
# https://www.geeksforgeeks.org/inverse-of-matrix-in-r/#:~:text=The%20inverse%20of%20a%20matrix,the%20value%20of%20unknown%20variables.
##---------------------------------------

## Start of Assignment
## The following functions 1-Cache the inverse of a matrix and 2-retrieves and computes the inverse of function 1
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = getsolve,
           getsolve = getsolve)
}


## This functions computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache,

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
