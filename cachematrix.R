makeCacheMatrix <- function(x = matrix()){
  temp<- NULL
  #Functions within makeCacheMatrix
  #function returns a list of functions to operate on
  set <- function(y){
    x <<- y
    temp<<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inverse){
    temp<<- inverse
  }
  getInverse <- function(){
    inv
     }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  #this function returns a matrix that is the inverse of x
  temp<- x$getInverse()
  if(!is.null(temp)){
    message("getting cached data")
    return(temp)
  }
  matrice <- x$get()
  temp<- solve(matrice, ...)
  x$setInverse(temp)
  temp
}

