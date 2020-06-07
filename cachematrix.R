## The function, makeCacheMatrix creates a special "matrix"
## which is really a list containing a function to

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
        }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve will retrieve the inverse from the cache.
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function
cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)){
       message("getting cached data")
       return(inv)
      }
     mat <- x$get()
     inv <- solve(mat, ...)
     x$setInverse(inv)
     inv
}
## To Test type the following code:
## matrix <- matrix(rnorm(100),10, 10)
## specialmat<-makeCacheMatrix(matrix)
## cacheSolve(specialmat)

