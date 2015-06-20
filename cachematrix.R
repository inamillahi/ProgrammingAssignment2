# R Programming Week 2 Assignment 2 inamillahi
# makeCacheMatrix receives a matrix variable, and this function creates a special 
#"matrix" object that can cache its inverse.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                        
        set <- function(y) {                                   
                x <<- y                                  
                m <<- NULL                               
        }
        get <- function() x                              
        setInvrs<- function(inv) m <<- inv   
        getInvrs <- function() m                       
        list(set = set, get = get,
             setInvrs = setInvrs,
             getInvrs = getInvrs)
}

#m <- makeCacheMatrix() an invertible matrix using the m$set() function that is nested 
# in makeCacheMatrix(). In this syntax, the variable "m" can be any letter. 

# cacheSolve returns the inverted form of input matrix
 
# When cacheSolve is called its checks to see if there already exists a non-NULL value for m. 

# If yes it returns that value.  

# If cacheSolve does not find an any values its  gets the commandline values for m, inverts the matrix 
# in m, and sets the value of m.


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'                    
        m<- x$getInvrs()               
                                        
        if(!is.null(m)) {                 ## Check to see if m is NULL.  
                message("obtaining cache")  ## If not NULL, return the value.
                return(m)
        }                                       
        start <- x$get()               ## Calling the nested function x$get in makeCacheMatrix which gives the un -inverted matrix.                         
        end <- solve(start)   ## solve() fucntion invert the start
        x$setInvrs(end)             
        end                            
}

#
# Test Results:
# 
# > m <- makeCacheMatrix()
# > m$set(matrix(c(0,2,2,0),2,2))
# > m$get()
# [,1] [,2]
# [1,]    0    2
# [2,]    2    0
# > cacheSolve(m)
#      [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > cacheSolve(m)
# Obtaining cache
#      [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0


