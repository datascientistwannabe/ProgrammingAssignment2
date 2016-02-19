## These two functions take a matrix as an input and returns the inverse. In 
## the process, the first function, makeCacheMatrix, creates a special "matrix" 
## object that can cache the inverse of the inputted matrix. The second function 
## responsible for either computing the inverse of the matrix or retrieving the
## inverse from the cache if the inverse has already been calculated.

## This function takes a matrix as an argument, and returns a list containing 
## four functions: 1) one that sets the value of the matrx 2) one that retrives
## the value of the matrix 3) one that sets the value of the inverse matrix
## 4) one that retreives the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                        x <<- y
                        im <<- NULL
        }
        get <- function() {
                        x
        }
        setinverse <- function(inverse) {
                        im <<- inverse
        }
        getinverse <- function() {
                im
        }
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function takes the output of the first function as an argument and 
## outputs the actual inverse matrix of the original matrix, x. This function 
## obtains the inverse matrix by calculating it or retrieiving it from the 
## cache, if it has already been calculated, using the functions that are 
## returned in the list from the previous function.

cacheSolve <- function(x, ...) {
       
         ## Return a matrix that is the inverse of 'x'
        
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached matrix")
                return(im)
        }
        data <- x$get()
        im <- solve(data)
        x$setinverse(im)
        im
}
