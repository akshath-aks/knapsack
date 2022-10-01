#' Title
#'
#' @param x 
#' @param W 
#'
#' @return
#' @export
#'
#' @examples
dynamic_knapsack<- function(x,W){
  
  #checking the input arguments
  if(!is.data.frame(x)) stop('First argument must be data frame')
  if(W<=0) stop('second argument must be positive number')
  
  n<-nrow(x)
  m<-matrix(0,n+1,W+1)
  
  for (i in 1:nrow(x)) {
    for(j in 1:W){
      if(x$w[i]>j){
        m[i+1,j+1]<-m[i,j+1]
      }else{
        # formula to fill the matrix for dynamic programming method
        m[i+1,j+1]<-max(m[i,j+1],m[i,j+1-x$w[i]]+x$v[i])
      }
      
    }
  }
  
  # getting the index of maximum value
  j<-j+1
  i <- which.max(m[,j])
  ele<-length(nrow(x))
  p <- 1
  ele[p] <- i-1
  
  # looping through to check previous row contains same value
  while(m[i,j]!=0 && j!=1 && i!=0){
    p<-p+1
    j<-(j-x$w[i-1])
    i<-which(m[,j] == m[i-1,j])[1]
    ele[p]<-i-1
  }
  
  #returning as named list
  return(list(value=round(m[nrow(x)+1,W+1]),elements=sort(ele[which(ele>0)])))
    
}
RNGversion(min(as.character(getRversion()),"3.5.3"))
##Warning in RNGkind("Mersenne-Twister", "Inversion", "Rounding"): non-uniform 'Rounding'
##sampler used
##old sampler used for backward compatibility
## suppressWarnings() can be used so that the above warning is not displayed
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


dynamic_knapsack(x = knapsack_objects[1:12,], W = 2000)

system.time(
  abc<-dynamic_knapsack(x = knapsack_objects[1:500,], W = 3500)
  )
