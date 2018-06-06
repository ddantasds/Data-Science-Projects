

library(dplyr)
library(ggplot2)

list.files("project/pre_git/goodreads/data")

books<-read.csv("project/pre_git/goodreads/data/books.csv")
head(books)
# 10k
nrow(books)

ratings<-read.csv("project/pre_git/goodreads/data/ratings.csv")


ratings[, N := .N, .(user_id, book_id)]

# Almost 1M
nrow(ratings)

ratings%>%
  group_by(book_id)%>%
  summarise(total=n())%>%
  ggplot(aes(total))+
  geom_histogram()
  

# 53k+ Users
ratings%>%
  group_by(user_id)%>%
  summarise(total=n(), rate = mean(rating))

# Remove duplicated rates - There are user with 2+ reviews for the same book
ratings<-ratings%>%
  group_by(book_id,user_id)%>%
  mutate(total=n())%>%
  filter(total==1)

# Select 100 books random books - Reduce

book_sample <- sample(unique(ratings$book_id),100,replace=FALSE)

ratings_sample<-ratings%>%
  filter(book_id%in%book_sample)

#### Collaborative Filtering Recommendation ####


book_id <- unique(ratings_sample$book_id)
user_id <- unique(ratings_sample$user_id)

book_user<-expand.grid(book_id,user_id)
names(book_user)<-c("book_id","user_id")

###

# Centralize book rating by average

ratings_sample<-merge(x=ratings_sample,
               y=ratings_sample%>%
                 group_by(book_id)%>%
                 summarise(rating_avg = mean(rating)),
               by.x="book_id",
               by.y="book_id",
               all.x=TRUE)%>%
  mutate(c_rating=rating-rating_avg)

head(ratings_sample)



###

book_user<-merge(x=book_user,
                 y=ratings_sample,
                 by.x=c("book_id","user_id"),
                 by.y=c("book_id","user_id"),
                 all.x=TRUE
)

head(book_user)

book_user$c_rating<-ifelse(is.na(book_user$c_rating),0,book_user$c_rating)

book_user%>%
  group_by(book_id)%>%
  summarise(total=n())%>%
  summarise(max(total))

book_user<-book_user%>%
  arrange(book_id,user_id)


### Cosine Similarity ###

cosineSimilarity<-function(x,y){
  sum(x*y)/(sqrt(sum(x*x))*sqrt(sum(y*y)))
}

# cosineSimilarity(c(1,2,3,4),c(1,2,3,4))
# cosineSimilarity(c(1,2,3,4),c(4,3,2,1))

###

books_similar<-data.frame(t(combn(book_id,2)))
names(books_similar)<-c("book_id_1","book_id_2")


### Calculate the similarity for book 159 - The Battle of the Labyrinth (Percy Jackson and the Olympians, #4)

book_159<-books_similar%>%
  filter(book_id_1==495 | book_id_2==495)

book_159$similarity<-apply(book_159,1,function(x) cosineSimilarity(book_user$c_rating[book_user$book_id==x[1]],book_user$c_rating[book_user$book_id==x[2]]))


book_159%>%
  filter(similarity>0)%>%
  arrange(desc(similarity))

books%>%
  filter(id==77)


# Book 6 - 



############ Example with Recommender ###########
library(recommenderlab)

test<-matrix(c(c(5,NA,NA,3,2,NA),c(NA,3,NA,4,NA,4),c(4,2,1,NA,NA,NA),c(NA,NA,5,NA,4,4),
             c(3,3,4,5,1,2),c(NA,NA,NA,3,5,5),c(5,NA,5,5,NA,NA)),
             nrow=7,byrow=TRUE)

dimnames(test)<-list(user_id=1:7,item_id=1:6)

test[is.na(test)]<-0

str(test)

test<-as(test,"sparseMatrix")

ratings<-new("realRatingMatrix",data=test)

head(ratings@data)

model1<-Recommender(ratings,method="IBCF")

prediction<-predict(model1,ratings[1,],type="ratings")

prediction@data
ratings@data


model_user<-Recommender(ratings,method="UBCF")



ratings_t<-ratings
ratings_t@data<-t(ratings@data)

model_item1<-Recommender(ratings,method="IBCF")
model_item2<-Recommender(ratings_t,method="UBCF",param=list(nn=30))

model_item1@model
model_item2@model

pred_item1<-predict(model_item1,ratings,type="ratings")
pred_item2<-predict(model_item2,ratings_t,type="ratings")

pred_item1@data
pred_item2@data

