# example run
# loops through different error and min element numbers, creates trees, saves the object as a file,
# gets the impurity, and outputs that to a file
# if there are any errors, if keeps going with the next task

f1 <- function(error,min){
  temp_tree <- makeTree(conn,
                        metadata$attribute[!metadata$unique & !metadata$target],
                        metadata,
                        misclassification,
                        "split<0.9",
                        "",
                        error,
                        min)
  
  dput(temp_tree,paste("tree_set3",error,min,sep="_"))
}

f2 <- function(error,min){
  temp_tree <- makeTree(conn,
                        metadata$attribute[!metadata$unique & !metadata$target],
                        metadata,
                        misclassification2,
                        "split<0.9",
                        "",
                        error,
                        min)
  
  dput(temp_tree,paste("tree_set3b",error,min,sep="_"))
  
  return(temp_tree)
}

f3 <- function(tree,error,min,str){
  impurity <- verification(conn,tablename,tree,misclassificationCalc,reader = newReader("split>=0.9"))
  
  print(paste("impurity =",impurity))
  cat(paste(str,"error =",error,", min =",min,", impurity =",impurity,"\n"),file=file)
  flush(file)
}

file <- file("output3","w")

for(error in c(0.025,0.01,0.005)){
  for(min in c(10000,1000,100)){
    print(Sys.time())
    
    conn <- dbConnect(drv,host="reddwarf.cs.rit.edu",user="p62002b",password="BigData2ndGroup")
    tryCatch( tree <- f1(error,min) ,error = function(e) e)
    print(Sys.time())
    tryCatch( impurity <- f3(tree,error,min,"tree3"), error = function(e) e)
    tryCatch( dbDisconnect(conn) ,error = function(e) e)
    
    print(Sys.time())
    
    conn <- dbConnect(drv,host="reddwarf.cs.rit.edu",user="p62002b",password="BigData2ndGroup")
    tryCatch( f2(error,min) ,error = function(e) e)
    print(Sys.time())
    tryCatch( impurity <- f3(tree,error,min,"tree3 b"), error = function(e) e)
    tryCatch( dbDisconnect(conn) ,error = function(e) e)

    print(Sys.time())
  }
}
close(file)
dbDisconnect(conn)
