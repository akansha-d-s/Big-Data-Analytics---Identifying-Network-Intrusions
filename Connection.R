# create a connection and save the basic data we need for creating the decision tree

library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv,host="reddwarf.cs.rit.edu",user="p62002b",password="BigData2ndGroup")
tablename <-"training_data"
testing_tablename <- "testing_data"
metadata <- read.csv("attributes.csv")

attack_types <- fetch(dbSendQuery(conn,paste("select distinct attack_type from",tablename)))
