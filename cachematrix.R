##################################################################################
#
#	Author: Kalyan
#	Platform: R(3.4.3) x86_64-w64-mingw32/x64 (64-bit)
#	Date: 3 Feb 2018
#
#	Description: 
#               Defines two function- 
# 	        makeCacheMatrix: creates a special list of 4 function to handle a matrix and cache its inverse. Takes a matrix as input. Computes cache on initialisation
# 	        cacheInverse: 	 computes the cache of the special "Matrix" and stores it. If cache already exists, it just retreives the inverse.

makeCacheMatrix <- function(x = matrix())
{
	
	#inistialise inverse as NULL
	inv <- NULL

	#basic functions to operate on the matrix
	set <- function(y)
	{
		x <<- y
		inv <<- NULL
	}

	get <- function()
	{
		x
	}

	setInv <- function(inver)
	{
		inv <<- inver
	}

	getInv <- function()
	{
		inv
	}

	# Added a small feauture where the cache of the matirx is directly computed upon the initialisation
	thisList <- list(set = set, get = get, setInv = setInv, getInv = getInv)
	#computes the inverse of this matrix 'x' and sets it automatically on initialisation
	setInv(cacheInverse(thisList))
	#return a list for further operations
	thisList
}

cacheInverse <- function(x)
{
	#retrieves the inverse of matrix
	inv <- x$getInv()
	
	#if inverse does not exist, compute the inverse
	if(is.null(inv))
	{
		inv <- solve(x$get())
		x$setInv(inv)
		return(inv)
	}
	
	#else print the cached inverse
	print('Cached Inv: ')
	inv
}

myMat = matrix(c(2, 3, 5, 1, 6, 1, 5, 3, 3), 3, 3)
myBetterMat <- makeCacheMatrix(myMat)

# Cached inverse already exists
myBetterMat$getInv()
# Cached inverse already exists
cacheInverse(myBetterMat)

newMat = matrix(c(1, 3, 6, 2, 3, 1, 2, 8, 3), 3, 3)
myBetterMat$set(newMat)

# Matrix updated therefore cache needs to be recalculated
myBetterMat$getInv()

cacheInverse(myBetterMat)


