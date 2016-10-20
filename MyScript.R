##################################################################################
#                                                                                #
#                                                                                #
#       Programming Assignment 2: Lexical Scoping                                #
#                                                                                #
#                                                                                #
#################################################################################


# makeCacheMatrix: create a special matrix that can cache

# The inverse of an invertible sqaure matrix in our case

# Note: double operator (<<-) make it possible to maintain state 

# Across function call together with the parent environment

makeCacheMatrix      <-function(X=matrix()){ ### Parent environment
        InV<-NULL 
        set<-function(Y){ 
                X<<- Y    ### Double arrow keep looking for matching name
                ### in parent environment
                InV<<- NULL
        }
        get<-function() X ### Get values of the matrix and set inverse 
        setinverse<- function(inverse) InV <<- inverse 
        getinverse <- function () InV ### Get inverse values and store in a list
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


###################CacheSolve ############################

# CacheSolve: This function compute the inverse of a special matrix (matkeCacheMatrix)

# Aim of function cacheSolve is to retreive inverse from cache 

# If the inverse is already calculated otherwise it uses the data to setinverse via cache

cacheSolve <- function(X, ...) {
        ### Cache inverse of X if already computed and store in InV or skip computation
        InV <- X$getinverse()
        if(!is.null(InV)) { ### Otherwise we set condition to compute inverse of X
                message("getting cached data")
                return(InV)
        }
        data <- X$get() 
        InV <- solve(data, ...) ### Solve return inverse of sqaure invertible matrix
        X$setinverse(InV) ### Set inverse of matrix via the cache
        InV
}

set.seed(12345)
# create a square matrix from rnorm
my.mat<-makeCacheMatrix(matrix(sample(rnorm(100),4), nrow = 2, ncol = 2, byrow = T))
my.mat$get()

my.mat$getinverse()
cacheSolve(my.mat)
