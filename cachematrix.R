## Put comments here that give an overall description of what your
## functions do

## This function constructs a list of functions (getters and setters) that serves as an input for the cacheSolve function

makeCacheMatrix <- function(x = matrix()) { #initialize the empty matrix x as a function argument
            m <- NULL #initialize an object where save the inverse of my matrix
            set <- function(y) { #define a setter
                    x <<- y #initialize the object x but in parent environment (globalenv in this case), assigning the input value 
                    m <<- NULL #initialize the object m but in parent environment (globalenv in this case), this will clear any prior cached value
            }
            get <- function() x #define a getter, retrieve the x value in parent environment and assign to the function get()
            setinverse <- function(inversa) m <<- inversa #define the setter, save the input in the object m in the parent environment, in the runtime "inversa" will be the newly inverse matrix calculated by cacheSolve
            getinverse <- function() m #define a getter, retrieve the m value in parent environment and assign to the function getinverse()
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # generate a list that save the function environment values in the parent environment as available data
}


## This function is the one that actually calculate de inverse matrix and stores it in the cache

cacheSolve <- function(x, ...) { # define the function cacheSolve with x as an argument
                m <- x$getinverse() #stores in the object m (locally) the getinverse function of th input 
            if(!is.null(m)) { # if m is not NULL (that is a previous value was cached, and makeCacheMatrix was not run again since then), the function returns the cached value with the message "getting cached data"
                    message("getting cached data")
                    return(m)
            }
            data <- x$get() #this is executed if m is NULL (no values previous cached, or makeCacheMatrix was rerun),retrieves the original matrix (makeCacheMatrix input) and stores it in data
            m <- solve(data, ...) #calculate the inverse of the matrix in data and stores it locally in m
            x$setinverse(m) #uses the calculated matrix as input of the setinverse function, thus storing it in the object m of the parent environment (cache)
            m # Return a matrix that is the inverse of 'x'
}
