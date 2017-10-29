## This function creates a special matric object that can cache it's inverse
## to use it with cachesolve function create square matrix

## This fuction will create a matrix and cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## this function creates the invese of matrix created by makeCacheMatrix function and checks if there is 
## inverse in cache , it gives that output else calculates inverse of the matrix .This function assumes
##that input is square invertible matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting Cached Data...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
