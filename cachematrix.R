################################ Function to cache Inverse of a Matrix ###############################################
### Developer: Ramit Das Modified : 03-03-2016 ####################


#################This Function will create a special Matrix Object ##################
makeCacheMatrix <- function(resmatrix)
{
  i <- NULL
  set <- function(y){
    
    resmatrix <<-  y
    i <<- NULL
    
  }
  
  get <- function() resmatrix
  
  setinverse <- function(solve) i <<- solve
  
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}
  
########################This Function will actually inverse the matrix and Invoke the cache ################################
cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
