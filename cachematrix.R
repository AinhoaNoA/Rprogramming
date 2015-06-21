## This pair of functions cache the inverse of a matrix

makeCacheMatrix <- function(x) {
## Creates a special "matrix" object that can cache its inverse
## Is basicaly a function (makeCacheMatrix) that stores a list of functions (set, get, 
## setinverse, getinverse). To call these subfunctions we use "mainfunction$subfunction"
      
      m <- NULL    #m is de inverse matrix
      set <- function(y) {    #set is a function that changes the matrix stored in the main function
            x <<- y
            m <<- NULL    #if we change the vector, we remove old inverse matrix
      }
      get <- function() x    #get is a function that returns the matrix x stored in the main function
      setinverse <- function(inverse) m <<- inverse    #setinverse stores the value (matrix) of the input variable (inverse) in a variable m into the main function. This funciton doesn't calculate the inverse, it just stores a value (matrix)
      getinverse <- function() m    #getinverse returns the inverse matrix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}




cacheSolve <- function(a, ...) {  # Input of cacheSolve "a" is the object where the returns of makeCacheMatrix are stored
## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
      
      m <- a$getinverse()    #gets the value of "m" stored in the object containing makeCacheMatrix result of getinverse subfunction
      if(!is.null(m)) {    #if stored value is not empty, it returns it
            message("getting cached data")
            return(m)
      }
      data <- a$get()    #if stored value is empty, we will calculate it (get data)
      m <- solve(data, ...)    #calculate inverse
      a$setinverse(m)    #store inverse
      m
}
