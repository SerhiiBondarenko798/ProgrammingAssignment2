## Put comments here that give an overall description of what your
## functions do


#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){ #set the value of the vector
    x <<- y
    i <<- NULL
  }
  get <- function()x  #get the value of the vector
  setinverse <- function(inverse) i <<- inverse   #set the value of the inverse
  getinverse <- function() i  #  get the value of the inverse
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()#getting cached data
  if(!is.null(i)){#cheacking cached data
    message("getting cached data")
    return(i)#returning cached data
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
