#Create the actual decision tree

# split categorical attributes
# attributeSplitCat : Connection * String * Function(Pool -> Number) * String -> List( C ) OR NA
#   where C is List(Comparison comp, Pool pool, String class, Number impurity)
attributeSplitCat <- function(conn,attribute,getImpurity,where_clause,minElements, verbose = FALSE){
  queryString <- paste("select",attribute,"as a, attack_type, count(*) from",tablename,"where",where_clause,"group by",attribute,", attack_type")
  
  masterPool <- fetch(dbSendQuery(conn,queryString))
  
  if(verbose){ print(masterPool) }
  
  possibleVals <- unique(masterPool$a)
  if(length(possibleVals)<=1){
    return(NA)
  }
  
  clusters <- list()
  for(val in possibleVals){
      comp <- makeComp("lst",attribute,val)
      pool <- data.frame(attack_type = masterPool$attack_type[masterPool$a==val], count = masterPool$count[masterPool$a==val])
      impurity <- getImpurity(pool)
      clusters <- c(list(list(comp=comp, pool=pool, class=poolClass(pool), impurity=impurity)), clusters)
  }
  
  if(verbose){
    print("starting clusters:")
    for(cluster in clusters){
      cat(paste(compToStringPretty(cluster$comp),"\n"))
    }
    writeLines('\n')
  }
  clusters <- simplifyCategoricalClusters(clusters,getImpurity,minElements,verbose)
  
  if(verbose){
    print("final clusters:")
    for(cluster in clusters){
      cat(paste(compToStringPretty(cluster$comp),"\n"))
    }
    writeLines('\n')
  }
  
  return(clusters)
}

# simplify categorical clusters so there are only 2 clusters
# simplifyCategoricalClusters : List( C ) -> List( C )
#   where C is List(Comparison comp, Pool pool, String class, Number impurity)
simplifyCategoricalClusters <- function(clusters,getImpurity,minElements, verbose=FALSE){
  
  # if there are only 2 clusters, do not combine them
  # if there are more than 2 clusters, combine them
  if(length(clusters)<2){
    return (NA)
  }else if(length(clusters)>2){
    # make a list of all classes in the set of clusters
    classes <- c()
    for(cluster in clusters){
      classes <- c(classes,cluster$class)
    }
    
    class_num <- length(unique(classes))
    
    if(class_num == 1){
      if(verbose){cat("1\n")}
      if(verbose){
        print("phase 1:")
        for(cluster in clusters){
          cat(paste(compToStringPretty(cluster$comp),"\n"))
        }
        writeLines('\n')
      }
      # cluster disregarding class
      clusters <- clusterSmallClusters(clusters,getImpurity,minElements,verbose = verbose)
      
      if(verbose){
        cat("phase 2:\n")
        for(cluster in clusters){
          cat(paste(compToStringPretty(cluster$comp),"\n"))
        }
        writeLines('\n')
      }
      
      clusters <- clusterIgnoringClass(clusters,getImpurity,verbose = verbose)
      
      if(verbose){
        cat("phase 3:\n")
        for(cluster in clusters){
          cat(paste(compToStringPretty(cluster$comp),"\n"))
        }
        writeLines('\n')
      }
      
    }else if(class_num == 2){
      if(verbose){cat("2\n")}
      if(verbose){
        cat("phase 1:\n")
        for(cluster in clusters){
          cat(paste(compToStringPretty(cluster$comp),"\n"))
        }
        writeLines('\n')
      }
      # cluster just by class
      clusters <- clusterByClass(clusters,classes,getImpurity,verbose = verbose)
      
      if(verbose){
        cat("phase 2:\n")
        for(cluster in clusters){
          cat(paste(compToStringPretty(cluster$comp),"\n"))
        }
        writeLines('\n')
      }
      
      clusters <- clusterSmallClusters(clusters,getImpurity,minElements,verbose = verbose)
      
      if(verbose){
        cat("phase 3:\n")
        for(cluster in clusters){
          cat(paste(compToStringPretty(cluster$comp),"\n"))
        }
        writeLines('\n')
      }

    }else{
      if(verbose){cat("3\n")}
      if(verbose){
        cat("phase 1:\n")
        for(cluster in clusters){
          cat(paste(compToStringPretty(cluster$comp),"\n"))
        }
        writeLines('\n')
      }
      # cluster by class, then combine clusters
      if(class_num < length(clusters)){
        
        clusters <- clusterByClass(clusters,classes,getImpurity,verbose = verbose)
        
        if(verbose){
          cat("phase 1.5:\n")
          for(cluster in clusters){
            cat(paste(compToStringPretty(cluster$comp),"\n"))
          }
          writeLines('\n')
        }
      }

      clusters <- clusterSmallClusters(clusters,getImpurity,minElements,verbose = verbose)
      
      if(verbose){
        cat("phase 2:\n")
        for(cluster in clusters){
          cat(paste(compToStringPretty(cluster$comp),"\n"))
        }
        writeLines('\n')
      }
      
      clusters <- clusterIgnoringClass(clusters,getImpurity,verbose = verbose)
      
      if(verbose){
        cat("phase 3:\n")
        for(cluster in clusters){
          cat(paste(compToStringPretty(cluster$comp),"\n"))
        }
        writeLines('\n')
      }
    }
  }
  return (clusters)
}

# group clusters with the same dominant class together
clusterByClass <- function(clusters,classes,getImpurity,verbose = FALSE){
  newClusters <- list()
  for(class in unique(classes)){
    newCluster <- combineClusters(clusters[classes==class],getImpurity)
    if(length(newClusters)==0){
      newClusters <- list(newCluster)
     }else{
      newClusters <- append(list(newCluster),newClusters)
    }
  }
  return(removeExtraLists(newClusters))
}

# remove extra lists wrapped around clusters
removeExtraLists <- function(clusters){
  for(i in 1:length(clusters)){
    while(is.null(clusters[[i]]$comp)){
      clusters[i] <- clusters [[i]]
    }
  }
  return(clusters)
}

# combine clusters that are too small with the closest cluster
# clusterSmallClusters : List( C ) -> List( C )
clusterSmallClusters <- function(clusters,getImpurity,minElements, verbose = FALSE){
  if(length(clusters)<2){
    return(NA)
  }
  while(clustersTooSmall(clusters, minElements)){
    clusters <- clusterSmallClustersHelper(clusters,minElements,getImpurity)
  }
  if(length(clusters) >= 2){ return(clusters) }
  else{ return(NA) }
}

# true iff there are clusters that are too small
clustersTooSmall <- function(clusters, minElements){
  if(length(clusters)<=2){ return(FALSE) }
  for(cluster in clusters){
    if(sum(cluster$pool$count) < minElements) return(TRUE)
  }
  return(FALSE)
}

# combine the first cluster that is too small with the closest cluster
clusterSmallClustersHelper <- function(clusters, minElements, getImpurity){
  for(i in 1:length(clusters)){
    # if the cluster is too small, find the closest and merge them
    if(sum(clusters[[i]]$pool$count) < minElements){
      # the range to look for the closest cluster in (exclude current cluster)
      if(i==1){ range <- 2:length(clusters)}
      else if(i==length(clusters)){ range <- 1:(length(clusters)-1)}
      else{ range <- c(1:(i-1),(i+1):length(clusters)) }
      
      bestDistance <- Inf
      bestIndices <- NA
      # find the closest clusters
      for(j in range){
        dist <- getDist(clusters[[i]],clusters[[j]])
        
        if(dist < bestDistance){
          bestDistance <- dist
          bestIndices <- c(i,j)
        }
      }
      
      i <- bestIndices[[1]]
      j <- bestIndices[[2]]
      
      # merge the closest clusters
      temp <- list(clusters[[i]],clusters[[j]])
      newCluster <- combineClusters(temp, getImpurity)
      
      include <- rep(TRUE,length(clusters))
      include[c(i,j)] <- FALSE
      
      return(removeExtraLists(append(list(newCluster),clusters[include])))
    }
  }
}

# combine the closest clusters together
# this is called when all clusters belong to the same class, or clusters have already been combined by class
clusterIgnoringClass <- function(clusters,getImpurity,verbose = FALSE){
  
  if(verbose){
    writeLines("input clusters:")
    for(cluster in clusters){
      writeLines(compToStringPretty(cluster$comp))
    }
    cat("\n")
  }
  
  if(length(clusters) < 2){
    return(NA)
  }else if(length(clusters) == 2){
    return(clusters)
  }
  
  # find two closest clusters
  bestDistance <- Inf
  bestIndices <- NA
  for(i in 1:length(clusters)){
    for(j in 1:length(clusters)){
      if(i!=j){
        dist <- getDist(clusters[[i]],clusters[[j]])
        if(dist < bestDistance){
          bestDistance <- dist
          bestIndices <- c(i,j)
        }
      }
    }
  }
  
  i <- bestIndices[1]
  j <- bestIndices[2]
  
  newCluster <- combineClusters(list(clusters[[i]],clusters[[j]]),getImpurity)
  
  if(verbose){
    writeLines("new cluster:")
    writeLines(compToStringPretty(newCluster$comp))
    cat("\n")
  }
  
  include <- rep(TRUE,length(clusters))
  include[c(i,j)] <- FALSE
  
  if(verbose){
    writeLines("included cluster list:")
        for(cluster in clusters){
          writeLines(compToStringPretty(cluster$comp))
          cat("\n")
        }
    cat("\n")
  }
  
  clusters <- append(list(newCluster),clusters[include])
  
  if(verbose){
    writeLines("new cluster list:")

    for(cluster in clusters){
      writeLines(compToStringPretty(cluster$comp))
      cat("\n")
    }
    cat("\n")
  }
  
  return(clusterIgnoringClass(clusters,getImpurity))
}

# get the distance between two clusters
getDist <- function(cluster1,cluster2){
  attacks <- unique(c(as.character(cluster1$pool$attack_type),as.character(cluster2$pool$attack_type)))

  percent1 <- getPercent(attacks,cluster1$pool)
  percent2 <- getPercent(attacks,cluster2$pool)

  dist <- abs(percent1-percent2)
  
  return ( sum(dist) )
}

# get the percent of each attack that occurs in a given set of records
# helper for getDist()
getPercent <- function(attacks,pool){
  percent <- c()
  for(attack in attacks){
    p <- pool$count[pool$attack_type==attack]
    if(length(p)==0){
      percent <- c(percent,0)
    }else{
      percent <- c(percent,pool$count[pool$attack_type==attack])
    }
  }
  
  return( percent / sum(percent) )
}

# combine two clusters together and return the new cluster
# combineClusters : List ( C ) -> C
combineClusters <- function(clusters,getImpurity){
  if(length(clusters)<2){
    return(clusters)
  }
  
  cluster <- clusters[[1]]
  for(i in 2:length(clusters)){
    cluster$comp <- compAddVal(cluster$comp,clusters[[i]]$comp$val2)
    cluster$pool <- setNames(aggregate(c(cluster$pool$count,clusters[[i]]$pool$count),list(c(as.character(cluster$pool$attack_type),as.character(clusters[[i]]$pool$attack_type))),sum), c("attack_type","count"))
  }
  
  cluster$class <- poolClass(cluster$pool)
  cluster$impurity <- getImpurity(cluster$pool)
  
  return(cluster)
}

# split continuous attributes
# attributeSplitCont : Connection * String * Number * Function(Pool -> Number) * String -> List( C ) OR NA
#   where C is List(Comparison comp, Pool pool, String class, Number impurity)
attributeSplitCont <- function(conn,attribute,quantize_factor,getImpurity, where_clause,minElements, verbose = FALSE){
  
  if(quantize_factor > 1){
    bucketCalc <- paste("(",attribute,"/",quantize_factor,")")
  }else{
    bucketCalc <- paste("round(",attribute,"*",1/quantize_factor,")")
  }
  
  queryString2 <- paste("select ",bucketCalc,"as bucket, attack_type, count (*) from",tablename,"where",where_clause,"group by attack_type,",bucketCalc)
  masterPool <- fetch(dbSendQuery(conn,queryString2))
  
  buckets <- unique(masterPool$bucket)
  
#  possibleSplits <- list()
  bestSplitVal <-0
  bestImpurity <- 1
  bestResult <- NA
  for(b in buckets){
    
    val <- b * quantize_factor
    if(length(masterPool$count[masterPool$bucket<b])>0 & length(masterPool$count[masterPool$bucket>=b]>0)){
      
      pool1 <- setNames(aggregate(masterPool$count[masterPool$bucket<b],list(masterPool$attack_type[masterPool$bucket<b]),sum), c("attack_type","count"))
      pool2 <- setNames(aggregate(masterPool$count[masterPool$bucket>=b],list(masterPool$attack_type[masterPool$bucket>=b]),sum), c("attack_type","count"))
      
      
      impurity1 <- getImpurity(pool1)
      impurity2 <- getImpurity(pool2)
      pool1_count <- sum(pool1$count)
      pool2_count <- sum(pool2$count)
      impurity <- ((impurity1 * pool1_count) + (impurity2 * pool2_count))/(pool1_count + pool2_count)
      
      if(impurity < bestImpurity & pool1_count >= minElements & pool2_count >= minElements){
        bestImpurity <- impurity
        bestSplitVal <- val
        
        comp1 <- makeComp("<",attribute,val)
        comp2 <- makeComp(">=",attribute,val)
        bestResult <- list(list(comp=comp1,pool=pool1,class=poolClass(pool1),impurity=impurity1),
                           list(comp=comp2,pool=pool2,class=poolClass(pool2),impurity=impurity2))
      }
    }
  }
  return(bestResult)
}

# calculate the weighted impurity of each cluster (weighted average of impurities)
# getWeightedImpurity : List( C ) -> Number
#   where C is List(Comparison comp, Pool pool, String class, Number impurity)
getWeightedImpurity <- function(clusters, verbose = FALSE){
  totalCount <- 0
  totalImpurity <- 0
  for(cluster in clusters){
    count <- sum(cluster$pool$count)
    
    totalCount <- totalCount + count
    totalImpurity <- totalImpurity + ( count * cluster$impurity )
  }
  if(verbose){ print(c("weighted impurity",totalImpurity,totalCount,totalImpurity / totalCount))
  print(clusters)}
  return( totalImpurity / totalCount )
}

# set up logging file
treeFile <- file("verificationFile2","w")

# actually make the tree, returns the tree
# makeTree : Connection * List(String) * DataFrame(attribute,type,unique,target,categorical,quantize_factor) *
#             (Pool -> Number) * String * String * Number * Number
#             -> List(Node)
makeTree <- function(conn,attributes,metadata,getImpurity,where_clause,indent,stoppingImpurity,minElements,verbose = FALSE){
  if(verbose){
    print(attributes)
  }
  
  # attribute list to pass on to the creation of children nodes
  newAttributes <- attributes
  
  BestClusters <- NA
  BestImpurity <- 1
  BestAttribute <- NA
  
  for(attribute in attributes){
    
    if(metadata[metadata$attribute==attribute,"categorical"]){
      clusters <- attributeSplitCat(conn,attribute,getImpurity,where_clause,minElements,verbose = FALSE)
    }else{
      quantize_factor <- metadata[metadata$attribute==attribute,"quantize_factor"]
      clusters <- attributeSplitCont(conn,attribute,quantize_factor,getImpurity,where_clause,minElements,verbose = FALSE)
    }
    
    # this attribute cannot be split on, so should be thrown out
    if(is.na(clusters)){
      print(paste(attribute,"not splittable"))
      newAttributes <- newAttributes[newAttributes!=attribute]
    }else{
      impurity <- getWeightedImpurity(clusters,verbose)
      cat(paste("(",attribute,impurity,")\n"),file = treeFile)
      print(paste("(",attribute,impurity,")"))
      if(verbose){
        print(paste(attribute,"impurity =",impurity))
      }
      if(impurity < BestImpurity){
        BestClusters <- clusters
        BestImpurity <- impurity
        BestAttribute <- attribute
      }
    }
  }
  
  if(is.na(BestAttribute)){
    return(NA)
  }
  
  # Currently, if we split on a categorical attribute, we can never split on it again
  if(metadata[metadata$attribute==BestAttribute,"categorical"]){
    newAttributes <- newAttributes[newAttributes!=BestAttribute]
  }
  
#   print(c("Best Attribute = ",BestAttribute,"impurity =",BestImpurity))
  
  # make nodes for the children nodes
  children_lst <- list()
  for(cluster in BestClusters){
    
    cat(paste(indent,"if",compToStringPretty(cluster$comp),cluster$impurity,cluster$class, sum(cluster$pool$count),"\n"),file = treeFile)
    flush(treeFile)
    print(paste(indent,"if",compToStringPretty(cluster$comp),cluster$impurity,cluster$class, sum(cluster$pool$count)))
    
    new_where_clause <- paste(where_clause,"and",compToString(cluster$comp))
    
    # if there is still too much impurity and not too few elements, find the children nodes
    if( (cluster$impurity > stoppingImpurity) & (sum(cluster$pool$count) > minElements) ){
      children <- makeTree(conn,newAttributes,metadata,getImpurity,new_where_clause,paste(indent,"~~",sep=""),
                           stoppingImpurity,minElements)
    }else{
#       print( c((cluster$impurity > stoppingImpurity), (sum(cluster$pool$count) > minElements) ) )
      cat(paste(paste(indent,"~~",sep=""),"class =",cluster$class,"impurity =",cluster$impurity,"count =",sum(cluster$pool$count),"\n"),file = treeFile)
      flush(treeFile)
      print(paste(paste(indent,"~~",sep=""),"class =",cluster$class,"impurity =",cluster$impurity,
                  "count =",sum(cluster$pool$count)))
      children <- NA
    }
    
    node <- makeNode(cluster$comp, children, cluster$pool, cluster$impurity, cluster$class)
    
    children_lst[length(children_lst)+1] <- list(node, children_lst)
  }
  return(children_lst)
}

# get the classification of a record from the tree
getClass <- function(record,tree){
  if(is.na(tree)){
    return(NA)
  }
  
  if(compEvalNode(record,tree[[1]]$comp)){
    branch <- tree[[1]]
  }else if(compEvalNode(record,tree[[2]]$comp)){
    branch <- tree[[2]]
  }else{
    return(NA)
  }
  
  if(is.na(branch$children)){
    return(poolClass(branch$pool))
  }else{
    return(getClass(record,branch$children))
  }
}

# print the tree out
printTree <- function(tree, indent = ""){
  for(cluster in tree){
    writeLines(paste(indent,"if",compToStringPretty(cluster$comp)))
    if(is.na(cluster$children)){
      writeLines(paste(indent,"    ",cluster$class," (",cluster$impurity,")",sep=""))
    }else{
      printTree(cluster$children,paste(indent,"    ",sep=""))
    }
  }
}

# get the misclassification percent
verification <- function(conn,tablename,tree, getImpurityCalc, reader = newReader(), increment = 50000){
  count <- 0 # number of classifications performed
  correct <- 0 # number of correct classifications
  
  temp <- 0
  while(TRUE){
    r <- nextRecord(reader,conn,tablename,increment = increment)
    if(is.na(r)){
      return(getImpurityCalc(correct,count))
    }
    
    reader <- r$reader
    record <- r$record[1:(length(r$record)-1)] # record without attack type
    expected_class <- r$record$attack_type
    class <- getClass(record,tree)
    if(!is.na(class) & !is.na(expected_class) & attackLookup(class) == attackLookup(expected_class)){
      correct <- correct + 1
    }else{
#       print(r$record)
      print(c(class,attackLookup(class)))
    }
    count <- count + 1
    
#     if(temp >= increment){
#       print(c(correct,count,getImpurityCalc(correct,count)))
#       cat(c(correct,count,getImpurityCalc(correct,count)),file = treeFile)
#       flush(treeFile)
#       temp <- 0
#     }
    temp <- temp + 1
  }
}