getwd()

category_tree <- read.csv("category_tree.csv", header=T)
head(category_tree)

events <- read.csv("events.csv", header=T)
head(events)

item_properties1 <- read.csv("item_properties_part1.csv", header=T)

item_properties2 <- read.csv("item_properties_part2.csv", header=T)
head(item_properties1)

#nrow() for all
#summary() for all

library(recommenderlab)
library(dplyr)

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(events_rating_matrix), size = floor(.75*nrow(events_rating_matrix)), replace = F)
train_data <- events_rating_matrix[sample, ]
test_data  <- events_rating_matrix[-sample, ]

#train_data_matrix <- data.matrix(train_data)
#test_data_matrix <- data.matrix(test_data)

library(reshape2)
library(arules)


events_matrix <- as(events, "matrix")
#similarity matrix
item_similarity <- similarity(events_rating_matrix[300:400, ], method ="cosine", which = "itemid")

image(as.matrix(item_similarity), main = "visitor similarity")

similarity_of_items <- similarity(events_rating_matrix[, 1:2], method ="cosine", which = "itemid") #for first 4 items
as.matrix(similarity_of_items)
#############################model##############################################

rec_model <- Recommender(data = train_data, method = "UBCF",  param=list(normalize = "Z-score",method="Cosine",nn=5))

predict_item <- function(visitorid) 
{
  
  visitor_items <- events$itemid
  visitor_items <- as(visitor_items, "matrix")
  visitor_items_matrix   <- as(visitor_items, "binaryRatingMatrix")
  
  # perform the prediction
  recommend <- predict(rec_model, visitor_items_matrix,n=3)
  
  return(as(recommend,"list"))
}

#############################model##############################################
#subset of data
date_only <- events["timestamp"]
event_only <- events["event"]
events_visitorid <- events["visitorid"]
new_events2 <- events[100:200,c("visitorid","event")]

new_events <- events[1:100,c("visitorid","itemid")]
new_events_matrix <- as(new_events, "matrix")
new_events_rating <- as(new_events, "realRatingMatrix")
slotNames(new_events_rating)  #has a data slot
class(new_events_rating@data)
dim(new_events_rating@data)

#occurrence of various items
item_occurrence <- as.vector(new_events_rating@data)
unique(item_occurrence)
sum(item_occurrence)
occurrence_table <- table(item_occurrence)
occurrence_table


occurrence <- table(new_events$itemid)

occurrence_not1 <- occurrence[occurrence!=1]

#explore which item has been viewed the most
unique(new_events2$event)

Mode <- function(event) 
{
  event_u <- unique(new_events$event)
  event_u[which.max(tabulate(match(event, event_u)))]
}

sk <- apply(new_events2, 1, function(event) sum(event == Mode(event)))

#number of activities
table(event_only)

#ggplot for the command above
qplot(x=event, y=table(event_only), data=event_only)
ggplot(data = event_only, mapping = aes(event_only$event,table(event_only)))

#max activity by this visitor
max(table(events_visitorid))

#exploring date/time
val <- date_only$timestamp
as.POSIXct(val, origin="1970-01-01")
date_modified <- as.Date(as.POSIXct(val, origin="1970-01-01"))
date_original <- date_only$timestamp

date_df <- data.frame(date_original, date_modified)

tail(names(sort(table(date_df$date_original))), 5)

date_occurences<-table(unlist(date_only$timestamp))

date_occurences<-table(unlist(date_df$date_modified))
max(date_occurences)

tail(names(sort(table(date_df$date_modified))), 5)

#do same for visitors.

tail(names(sort(table(events$visitorid))), 5)
visitor_occurences<-table(unlist(events$visitorid))

max(visitor_occurences)
min(visitor_occurences)



#finally make graphs

hist(events$visitorid)
hist(events$itemid)
library(ggplot2)
