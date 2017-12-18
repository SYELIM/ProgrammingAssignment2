## Coursera R Programming Week 3 Assignment 2; Github ID: SYELIM
## Put comments here that give an overall description of what your
## functions do:
## 1) makeCacheMatrix: create a special object that stores a matrix and cache's its inverse matrix 
## 2) cacheSolve: return the inverse of the special matrix returned by makeCacheMatrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                Mat.Inv <- NULL
                set <-function(y) { #Set the value of the matrix
                        x <<- y
                        Mat.Inv <<- NULL                                                        
                }
                get <- function () x #Get the value of the matrix
                setmatrixinv <- function(solvematrix) Mat.Inv <<- solvematrix # Set the inverse matrix
                getmatrixinv <- function()Mat.Inv #Get the inverse matrix
                list(set=set, get=get, setmatrixinv=setmatrixinv, getmatrixinv=getmatrixinv)
        }



## Write a short comment describing this function
# It takes the output from the makeCacheMatrix (function above) as an input. Then it checks the inverse matrix from makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                Mat.Inv <- x$getmatrixinv() 
                
                #If makeCahceMatrix:
                # a) has a value, return a message "getting cached data" and the cached object.
                if(!is.null(Mat.Inv)){
                message("getting cached data")  
                return(Mat.Inv)
                }
                
                # b) is empty, get the original matrix data, set the inverse of matrix by using the solve() function.  
                data <-x$get()
                Mat.Inv <- solve(data,...)
                x$setmatrixinv(Mat.Inv)
                Mat.Inv
        }

