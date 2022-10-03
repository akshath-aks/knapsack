#' Brute Force method to solve knapsack problem
#'
#' @param x Data Frama having weights(w) and values(v) columns
#' @param W Positive number weight of knapsack
#' @param parallel Argument to parallelize brute force
#'
#' @return The maximum value and elements added to Knapsack
#' @import foreach
#' @import parallel
#' @import doParallel
#' @export


brute_force_knapsack<-function(x, W,parallel=FALSE){
  #registerDoParallel(4)  # use multicore, set to the number of our cores
  cores<-parallel::detectCores()
  cl <- parallel::makeCluster(cores,type='PSOCK') #not to overload your computer
  doParallel::registerDoParallel(cl)
  
  if(is.data.frame(x)==FALSE){stop("inputs are incorrect")}
  if(W<=0) stop('second argument must be positive number')
  if(ncol(x)!=2){stop("columns are incorrect")}
  else{
    value<-c()
    weight<-c()
    elements_binary<-c(1:(2^nrow(x)-1))
    if(parallel==TRUE){
      each_element <- c()
      
      "%dopar%" <- foreach::"%dopar%"
      out <- foreach::foreach(i=1:(2^nrow(x)-1), .combine='cbind',
                              .export=c("brute_force_knapsack_element")) %dopar% {
                                brute_force_knapsack_element(i, nrow(x), x$w, x$v)
                              }
      
      out_t <- as.data.frame(out)
      value<- unname(unlist(out_t[2,]))
      weight<- unname(unlist(out_t[1,]))
    }
    
    else{
      for(i in 1:(2^nrow(x)-1)){   
        out <- brute_force_knapsack_element(i, nrow(x), x$w, x$v)
        #out_t <- as.data.frame(out)
        value<-c(value, unname(unlist(out[2])))
        weight<-c(weight, unname(unlist(out[1])))
      }
      
    }
    result<-data.frame(value,weight,elements_binary)
    result_valid<-result[result$weight<=W,]
    value<-max(result_valid$value)
    options(digits = 1)
    position<-c(intToBits(result_valid$elements_binary[which(result_valid$value==max(result_valid$value))]))
    elements<-which(position==01)
    result_output<-list(value=value,elements=elements)
    
    parallel::stopCluster(cl)
    
    return(result_output)
    
  }
}
brute_force_knapsack_element<-function(i, total_row, x_w, x_v){
  combis_weight<-0
  combis_value<-0
  combis<-intToBits(i)
  for(j in 1:total_row) {
    if(combis[j]==01){
      combis_weight<-combis_weight + x_w[j]
      combis_value<-combis_value + x_v[j]
    }
  }
  each_element <- list(combis_weight=combis_weight, combis_value=combis_value)
  
  return (each_element)
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
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

system.time(abc<-brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))
