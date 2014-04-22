## makeMatrix lists functions that will be used by cacheSolove
## cacheSolve make use of functions inside makeMatrix and its cached data

## makeMatrix caches value by using
## double arrow assignment operator (<<-)
## then it holds/stores 4 other functions: set, get,setinverse,getinverse
## (<<-) makes it possible to look up value in current and upward to parent environment;hence cache its value.

makeMatrix <- function(x = matrix(...)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## first cacheSolve checks to see if inverse of (x) is available
## if so, it prints "getting cached data"" and returns its inverse from cached value
## put x into "data"
## define m as "solve(data,...)",which calculates inverse and assign it to m
## store m into setinverse
## lastly print m

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

x<-matrix(c(1:4),nrow=2)
m.x<-makeMatrix(x)
cacheSolve(m.x)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
cacheSolve(m.x)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5