#  cachematrix.R
#  Assignment 2 for Coursera's R Programming MOOC
#  By Juan Paredes, 2015.

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## These functions attempt to calculate the inverse of a matrix,
## which could be a costly operation, and cache the results
## for later reuse, taking advantage of R lexical scoping / closures

## makeCacheMatrix creates a special data structure for a matrix
## that is able to cache its inverse. The structure contains
## as functions to get/set both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    
    # set: store new data matrix, and invalidate inverted matrix cache
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    
    # get: return stored data matrix
    get <- function() x
    
    # setinverse: cache inverted matrix
    setinverse <- function(inverse) cachedInverse <<- inverse
    
    # getinverse: return cached inverted matrix
    getinverse <- function() cachedInverse
    
    cm = list(set = set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)
    class(cm) <- 'cachematrix' # assign class name for later validation
    cm
}


## cacheSolve attempts to use an already cached inverse for
## a given matrix, if it's available, or calculate and cache the
## matrix inverse otherwise, using the "solve" function.
## The first argument (x) should be the result of calling makeCacheMatrix
## on the desired data matrix to be inverted

cacheSolve <- function(x, ...) {
    # first, validate if input data structure is appropiate
    if (class(x) != 'cachematrix') {
        stop("input parameter is not valid. Call makeCacheMatrix first")
    }
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inverse <- solve( x$get(), ... )
    x$setinverse(inverse)
    inverse
}
