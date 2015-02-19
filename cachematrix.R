## Create a matrix that can chache its inverse, then return the chache if calculated, if not, calculate the inverse and chache it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #Sets the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #Gets the value of the matrix
  setmatrix <- function(solve) m <<- solve #Sets the value of the inverse matrix
  getmatrix <- function() m #Gets the value of the inverse matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix) #Calls functions to set & get the inverse matrix value
}

## Checks if inverse matrix is chached. If it is not cached, calculate and return the inverse.

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)) { #Check if inverse is already calculated
            message("getting cached matrix")
            return(m) #If it is, return chached matrix
        }
        matrix <- x$get()
        m <- solve(matrix, ...) #If inverse is not chached, calculate inverse
        x$setmatrix(m) #Set value of cache to inverse matrix
        m        ## Return a matrix that is the inverse of 'x'
    }


