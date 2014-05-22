makeCacheMatrix <- function(x = matrix()) {    
        ##takes matrix as input, calculates inverse and saves in cache
        i <- NULL                       ##initialize cache
        
        set <- function(y) {            ##create set fuction for initializing matrix
                x <<- y                 ##initializes x variable (matrix) in parent environment
                i <<- NULL              ##clears cache
        }
        
        get <- function() x             ##returns matrix stored in x
        setcache <- function(solve) i <<- solve ##takes inverse matrix
        getcache <- function() i        ##passes cached matrix
        list(set = set, get = get, setcache = setcache, getcache = getcache)
        
}

cacheSolve <- function(x, ...) {
        i <- x$getcache()               ##returns the contents of previous cache
        
        if(!is.null(i)) {               ##if cache exists, 
                message("getting cached data")
                return(i)               ##return previously calculated inverse matrix and exit
        }
        
        data <- x$get()                 ##if cache of inverse matrix doesn't exist,                                                                           
                                        ##get matrix and store in local variable, data
        
        i <- solve(data, ...)           ##solve inverse of matrix (data) in i
        x$setcache(i)                   ##store inverse in cache
        i                               ##return inverse
}