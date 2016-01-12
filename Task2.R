## Returns generic attack type for a specific attack
## Takes a vector of strings.

attack_data <- read.csv("attack_types.csv")

attackLookup <- function(attacklist){
  if(is.na(attacklist)){
    return(NA)
  }
  generic <- attacklist
  for( i in 1:length(attacklist) ){
    g <- toString(attack_data$generic[attack_data$specific == attacklist[i]])
    generic[i] <- g
  }
  if(is.na(generic)){
    print(c(attackList,"->",generic))
  }
  return( generic )
}


#list <- c("back", "land", "pod", "rootkit") 
#attackLookup(list)

# attackLookup <- function(class){return(class)}
