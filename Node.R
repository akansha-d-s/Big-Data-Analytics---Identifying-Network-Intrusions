# Node and comparison structures

# Node: comp, children, pool, class
#   where
#     comp is a Comparison
#     children is a list of Nodes
#     pool is a data frame of (attack_type, count)
#     class is the dominant class in the node

makeNode <- function(comp, children, pool, impurity, class=NA){
  if(is.na(class)){
    class <- poolClass(pool)
  }
  return(list(comp=comp, children=children, pool=pool, impurity=impurity, class=class))
}

# get the class for the given pool of records
poolClass <- function(pool){
  m <- max(pool$count)
  return(as.character(pool$attack_type[pool$count==m][1]))
}



# Comparison: op, val1, val2
#   where op is <, <=, =, >=, >, !=, and, or, lst
# lst op has val1 = attribute, val2 = vector of possible values for the attribute

# make a comparison object
makeComp <- function(op, val1, val2){
  return(list(op=op,val1=val1,val2=val2))
}

compAddVal <- function(comp,val){
  return(makeComp(comp$op,comp$val1,c(comp$val2,val)))
}

getAttribute <- function(comp){
  if(comp$op == "=" | comp$op == "lst"){
    return(comp$val1)
  }else if(comp$op == "or"){
    attribute1 <- getAttribute(comp$val1)
    attribute2 <- getAttribute(comp$val2)
    if(is.na(attribute1) | is.na(attribute2)){
      return(NA)
    }else if(attribute1 == attribute2){
      return(attribute1)
    }else{
      return(NA)
    }
  }else{
    return(NA)
  }
}

getCategories <- function(comp){
  if(comp$op == "=" | (comp$op=="lst" & length(comp$val2)<=1)){
    return(comp$val2)
  }else if(comp$op == "lst" & length(comp$val2)>1){
    categories <- paste(comp$val2[[1]])
    for(cat in comp$val2[2:length(comp$val2)]){
      categories <- paste(categories,", ",cat,sep="")
    }
    return(categories)
  }else{
    return(paste(getCategories(comp$val1),", ",getCategories(comp$val2),sep=""))
  }
}

# create a string to put ino a SQL query
compToStringPretty <- function(comp){
  attribute <- getAttribute(comp)
  if((comp$op == "or" & !is.na(attribute)) | comp$op == "lst"){
    categories <- getCategories(comp)
    return(paste(attribute," = {",categories,"}",sep=""))
  }else if(comp$op == "and" | comp$op == "or"){
    return(paste(compToString(comp$val1),comp$op,compToString(comp$val2)))
  }else if(class(comp$val2)=="character"){
    val2 <- paste("\'",comp$val2,"\'",sep="")
    return(paste(comp$val1,comp$op,val2))
  }else{
    return(paste(comp$val1,comp$op,comp$val2))
  }
}

compToString <- function(comp){
  if(comp$op == "and" | comp$op == "or"){
    return(paste(compToString(comp$val1),comp$op,compToString(comp$val2)))
  }else if(comp$op == "lst"){
    string <- paste(comp$val1," = ","\'",comp$val2[[1]],"\'",sep="")
    if(length(comp$val2) > 1){
      for(i in 2:length(comp$val2)){
        string <- paste(string," or ",comp$val1," = ","\'",comp$val2[[i]],"\'",sep="")
      }
    }
    return(string)
  }else if(class(comp$val2)=="character"){
    val2 <- paste("\'",comp$val2,"\'",sep="")
    return(paste(comp$val1,comp$op,val2))
  }else{
    return(paste(comp$val1,comp$op,comp$val2))
  }
}


compEval <- function(comp){
  if(comp$op == "or"){
    return(compEval(comp$val1) | compEval(comp$val2))
  }else if(comp$op == "and"){
    return(compEval(comp$val1) & compEval(comp$val2))
  }else if(comp$op == "lst"){
    return(comp$val1 %in% comp$val2)
  }else if(comp$op=="<"){
    return(comp$val1 < comp$val2)
  }else if(comp$op=="<="){
    return(comp$val1 <= comp$val2)
  }else if(comp$op=="="){
    return(comp$val1 == comp$val2)
  }else if(comp$op==">="){
    return(comp$val1 >= comp$val2)
  }else if(comp$op==">"){
    return(comp$val1 > comp$val2)
  }else if(comp$op=="!="){
    return(comp$val1 != comp$val2)
  }else{
    return(NA)
  }
}

# expects that if the operator is a comparison, val1 is the attribute name
compEvalNode <- function(record,comp){
  if(comp$op == "or"){
    return(compEvalNode(record,comp$val1) | compEvalNode(record,comp$val2))
  }else if(comp$op == "and"){
    return(compEvalNode(record,comp$val1) & compEvalNode(record,comp$val2))
  }else{
    return(compEval(makeComp(comp$op,as.character(record[comp$val1]),comp$val2)))
  }
}

