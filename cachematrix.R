
## A set of functions that are used to get the inverse of a matrix and save the inverse to a cache to be used later (useful since inverse is computational heavy)


#Note, function description from assignment instructions
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x){
  invMatrix <- NULL
  #Used to update the instance with a new matrix (and reset cache)
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  #Used to get the matrix object being inverted
  get <- function() x
  #Used to update the cache with the inverted matrix
  setInverse <- function(inverseMatrix) invMatrix <<- inverseMatrix
  #Used to get the inverted matrix from cache
  getInverse <- function() invMatrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Note, function description from assignment instructions
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x){
 
  invMatrix <- x$getInverse()
  
  #Check if inverse is cached (will not be NULL)
  if(!is.null(invMatrix)) {
    #print for demo purpose when data is from cache
    message("getting cached data")
    #return the inverted matrix of x from cache
    return(invMatrix)
  }
  #If inverse has not been cached, get matrix to be inverted
  tmp <- x$get()
  #Calculate inverse of the matrix using solve
  invMatrix = solve(tmp)
  #Update cache with the inverse
  x$setInverse(invMatrix)
  #Return the inverted matrix of x
  invMatrix
}