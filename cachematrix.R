## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    #initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){x} #this is the function to get matrix x
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}  #this is the function to get the inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { #gets cache data
  inv <- x$getinverse() 
  if(!is.null(inv)) { #checking whether inverse is NULL
    message("getting cache data")
    return(inv) #returns the inverse value
  }
  mat <- x$get()
  inv <- solve(mat,...)  #calculates the inverse value
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'  
}
