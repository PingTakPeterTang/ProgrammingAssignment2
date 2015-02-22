## This file consists of two functions that together provide a basic matrix method
## for computing and caching the matrix's inverse
##
## The first function, makeCacheMatrix is basically an object or class definition.
## This function creates a matrix object and attach to it the numeric values of the matrix
## and four "methods", as a list of four functions.
## Whenever makeCacheMatrix is called, it always "clear" the cached inverse by setting it to NULL
## The list of four functions are
## (1) set    takes a numeric matrix and set as the numeric content of the matrix object
##            Note that whenever "set" is called, the cache is always reset to NULL. This is
##            because we assume calling "set" is changing the matrix's numeric content
##            and any previously cached inverse is no longer correct.
## (2) get    this function retrieves the numeric content of the matrix from the matrix object
## (3) setInv takes a numeric matrix and put it in the cache. It is assumed that the user
##            will invoke this with the correct inverse of the matrix object. There is no
##            checking to ensure that this is the case.
## (4) getInv retrievs the numeric matrix that is cached as the inverse
##
## The second function, cacheSolve(x), takes a matrix object x returns the numeric inverse,
## not as a matrix object, but as the R native numeric matrix type.
## The inverse may not need to be computed as it may already exist and cached. In this
## case, cacheSolve simply retrieves it (using the x$getInv function attached to the matrix
## object x). In case the inverse does not exist, cacheSolve (1) obtain the numeric content
## of the matrix, using x$get(), (2) compute the numeric inverse, (3) store this inverse
## in cache by calling x$setInv, and (4) return the numeric inverse
##


## makeCacheMatrix creates a matrix object that has four "methods". See explanation above 

makeCacheMatrix <- function(x = matrix()) {

        xInv <- NULL
	set <- function(y) {
	        x <<- y
		xInv <<- NULL
        }
	get <- function() x
	setInv <- function(z) xInv <<- z
        getInv <- function() xInv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve returns the numeric inverse of the matrix object x. It either
## just retrieve the inverse that is already cached, or computes as well as caching the
## inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInv <- x$getInv()
	if (!is.null(xInv)){
	        message("getting the cached inverse")
		return(xInv)
        }
	x_numeric <- x$get()
	xInv <- solve(x_numeric)
	x$setInv( xInv )
	xInv
}
