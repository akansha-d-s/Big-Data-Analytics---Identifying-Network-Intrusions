# misclassification functions

misclassification <- function(pool){
  count <- sum(pool$count)
  if(count==0){
    return(0)
  }else{
    return( misclassificationCalc(pool$count[pool$attack_type == poolClass(pool)], count) )
  }
}

misclassification2 <- function(pool){
  count <- sum(pool$count)
  if(count==0){
    return(0)
  }else{
    return( misclassificationCalc(sum(pool$count[attackLookup(pool$attack_type) == attackLookup(poolClass(pool))]), count) )
  }
}

misclassificationCalc <- function(correct, count){
  return( (count-correct)/count )
}