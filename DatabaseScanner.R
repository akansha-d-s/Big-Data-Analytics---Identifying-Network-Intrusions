# A buffered reader of sorts that allows us to read a record at a time,
# but collects batches of records ffrom the database to save time

newReader <- function(where=NA,id=0){
  if(is.na(where)){
    return(list(records=data.frame(),id=id,index=1,max=NA,where=""))
  }else{
    return(list(records=data.frame(),id=id,index=1,max=NA,where=paste("and",where)))
  }
}

# returns NA when there are no more rows in the database
# returns the next row in the database, if there is a split column, do not return it
# split was a random number assigned for use in distinguishing testing and training data and isn't used in classification
nextRecord <- function(reader,conn,tablename, increment = 50000){
  
  if(is.na(reader$max)){
    reader$max <- fetch(dbSendQuery(conn,paste("select count(*) from",tablename)))
  }
  if(reader$index > nrow(reader$records) & reader$id >= reader$max){
    return(NA)
  }else if(reader$index > nrow(reader$records)){
    print(c("lookup records:",reader$id,(reader$id+increment)))
    reader$records <- fetch(dbSendQuery(conn,paste("select * from",tablename,"where id >=",reader$id,"and id<",(reader$id+increment),reader$where)),n=increment)
    reader$records <- reader$records[1:43] # remove split column, if it exist
    reader$id <- reader$id + increment
    reader$index <- 1
  }
  record <- reader$records[reader$index,2:length(reader$records)]
  reader$index <- reader$index + 1
  return(list(reader = reader,record = record))
}