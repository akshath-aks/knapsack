#' Greedy Heuristic method to solve knapsack problem
#'
#' @param x Data Frama having weights(w) and values(v) columns
#' @param W Positive number weight of knapsack
#'
#' @return The maximum value and elements added to Knapsack
#' @export


greedy_knapsack <- function(x, W){
  
  #checking the input arguments
  if(!is.data.frame(x)) stop('First argument must be data frame')
  if(W<=0) stop('second argument must be positive number')
  
  #creating d column in x data frame
  x$d <- x$v / x$w
  
  #ordering based on v/w in data frame
  x<-x[order(x$d,decreasing=T),]
  
  #assigning initial values for weight(w), value(v), elements
  val <- 0
  wei <- 0
  ele <- c()
  i=1
  
  #looping to get the values and elements which has high v/w in ordered data frame
  while(wei < W){
    wei <- wei + x$w[i]
    if(wei < W){
      val <- val + x$v[i]
      ele <- c(ele, rownames(x)[i])
    }
    i=i+1
  }
  
  # returning as list
  return(list(value = round(val), elements = as.numeric(ele)))
}

suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)



greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

system.time(
  abc<-greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500)
)
