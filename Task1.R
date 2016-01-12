##Entropy is log2(N) - (1/N)*sum( ni * log2(ni) ) where N is total, ni is number of type i
## http://www.johndcook.com/blog/2013/08/17/calculating-entropy/


entropy <- function(pool){
  N <- sum(pool$count)
  if(N == 0){
    return(0)
  }else{
    attacks <- unique(pool$attack_type)
    summation <- 0
    for( type in attacks ){
      ni <- pool$count[pool$attack_type == type]
      summation <- summation + ni*log2(ni) 
    }
    return( log2(N) - (1/N)*summation )
  }
}

entropy2 <- function(pool){
  N <- sum(pool$count)
  if(N == 0){
    return(0)
  }else{
    attacks <- unique(attackLookup(pool$attack_type))
    summation <- 0
    for( type in attacks ){
      ni <- sum(pool$count[attackLookup(pool$attack_type) == type])
      summation <- summation + ni*log2(ni) 
    }
    return( log2(N) - (1/N)*summation )
  }
}


##GINI Coefficient is ( \sum_i^N \sum_j^N abs(ni - nj) )/2*N^2 * mu
##where N is total, ni is number of type i, mu is mean 
## http://mathworld.wolfram.com/GiniCoefficient.html

gini <- function(pool){
  N <- sum(pool$count)
  if(N==0){
    return(0)
  }else{
    return ( 1-sum( (pool$count/N)^2 ) )
  }
}

gini2 <- function(pool){
  N <- sum(pool$count)
  if(N==0){
    return(0)
  }else{
    attacks <- unique(attackLookup(pool$attack_type))
    summation <- 0
    for( type in attacks ){
      ni <- sum(pool$count[attackLookup(pool$attack_type) == type])
      summation <- summation + ((ni/N)^2)
    }
    return ( 1-summation )
  }
}

# gini <- function(pool){
#   N <- sum(pool$count)
#   if(N==0){
#     return(0)
#   }else{
#     attacks <- unique(pool$attack_type)
#     mu = 0
#     summation = 0
#     for(typei in attacks){
#       i <- pool$count[pool$attack_type == typei]
#       mu <- mu + i
#       for(typej in attacks){
#         j <- pool$count[pool$attack_type == typej]
#         summation <- summation + abs(i - j)
#       }
#     }
#     mu <- mu/N
#     return ( summation/(2*N^2*mu) )
#   }
# }