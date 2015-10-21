## There are two functions in these R file
## makeCacheMatrix function basically is to create a new matrix and provide inner function to set and get inverse of the matrix
## cacheSolve function is to compute the inverse of matrix using inner function of the makeCacheMatrix

## Function to get and set a matrix, also get and set the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL									## inverse of matrix, NULL when function first called
        set <- function(y) {
                x <<- y								## set new matrix
                i <<- NULL
        }
        get <- function() x							## get the matrix
        setinverse <- function(solve) i <<- solve	## function to set the inverse of matrix
        getinverse <- function() i					## function to get the inverse of matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)				## return a list of inner function
}


## Function to compute inverse of matrix, parameter x here is makeCacheMatrix function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()							## calling getinverse function 
        if(!is.null(i)) {
                message("getting cached data")
                return(i)							## return cached data when the matrix has not changed
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
