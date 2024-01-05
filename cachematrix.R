
## Store matrix and function in cache

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y){
    x <<- y
    ix <<- NULL
}
  get <- function() x
  setinverse <- function(solve) ix <<- solve
  getinverse <- function() ix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## return inverse matrix from cached matrix

cacheSolve <- function(x, ...) {
  ix <- x$getinverse()
  if(!is.null(ix)){
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data)
  x$setinverse(ix)
  ix
        ## Return a matrix that is the inverse of 'x'
}
