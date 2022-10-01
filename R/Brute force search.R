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

brute_force_knapsack<-function(x, W){
  if(is.data.frame(x)==FALSE){stop(print("inputs are incorrect"))}
  if(ncol(x)!=2){stop(print("columns are incorrect"))}
  else{
    value<-c()
    weight<-c()
    elements_binary<-c(1:(2^nrow(x)-1))
    for(i in 1:(2^nrow(x)-1)){   
    combis_weight<-0
    combis_value<-0
    combis<-intToBits(elements_binary[i])
    for(j in 1:nrow(x)){
      if(combis[j]==01){
        combis_weight<-combis_weight+x$w[j]
        combis_value<-combis_value+x$v[j]
      }
    }
    value<-c(value,combis_value)
    weight<-c(weight,combis_weight)
    }
    result<-data.frame(value,weight,elements_binary)
    result_xxx<-result[result$weight<=W,]
    value<-max(result_xxx$value)
    options(digits = 1)
    position<-c(intToBits(result_xxx$elements_binary[which(result_xxx$value==max(result_xxx$value))]))
    elements<-which(position==01)
    result_output<-list(value=value,elements=elements)
    print(result_output)
  }
}
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)