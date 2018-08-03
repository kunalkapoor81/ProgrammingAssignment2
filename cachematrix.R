  ##The following two fucntions are used to create a special object that stores a 
  ##matrix and cache's its mean.
  
  
  
  ##This function creates a special "matrix" object that can cache its inverse
  
  makeCacheMatrix <- function(x =  matrix() ){
    i <- NULL
    
    #Setter function for data
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    #Getter function for data
    get <- function() x
    
    #Setter function for the inverse
    setinv <- function(inv) i <<- inv
    
    #Getter fucntion for the inverse
    getinv <- function() i
    
    #Create a list containing, getter and setter functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
  
  
  
  ##This function computes the inverse of the special matrix returned by
  ##makeCacheMAtrix, If the inverse has already been calculated (and the 
  ##matrix has not changed), then the cachesolve should retrieve the inverse 
  ##from the cache.
  
  cacheSolve <- function(x, ...) {
    #Get the value of inverse
    i <- x$getinv()
    
    #If inverse is found in cache, display it
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    
    #If inverse i not found in the cache, then compute inverse and 
    #store it in cache for later use.
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
  }
  
  
    
    
    
  
