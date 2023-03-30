###################################
###################################
## Caching the Inverse of a Matrix
###################################
###################################

## Function 1:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) im <<- solve
  getsolve <- function() im
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Function 2:
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getsolve()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setsolve(im)
  im
}

## Output testing

# Creating an initial matrix

a <- c(4,3)
b <- c(3,2)
mymatrix <- as.matrix(rbind(a,b))
mymatrix
solve(mymatrix)

# Result using both function 1 and 2

myMatrix_object <- makeCacheMatrix(mymatrix)
cacheSolve(myMatrix_object)

# Getting cached data!

cacheSolve(myMatrix_object)

# Creating a second matrix

c <- c(2,1)
d <- c(1,3)
mymatrix2 <- as.matrix(rbind(c,d))
mymatrix2
solve(mymatrix2)

# Resetting input matrix

myMatrix_object$set(mymatrix2)
cacheSolve(myMatrix_object)
