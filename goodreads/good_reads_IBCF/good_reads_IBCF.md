
# Good Reads: Item-Based Collaborative Filtering

![title](https://blog.tellwell.ca/wp-content/uploads/2016/12/goodreads.jpg)

## Overview

We cannot escape! If you use online services or buy anything from a e-commerce company, recommendation is part of your online routine. Online services are suggesting you products and services that you might like. It is everywhere. Netflix recommends shows/movies based on what you watched and recent demand. Amazon displays on your website products that you might be interested based on previous purchases, clicks, and user behavior. Youtube recommends videos and channels based on what you and others have watched. We could keep going and the idea would be the same: suggest something that you probably will enjoy.

Books are not different!! The website [goodreads.com](http://www.goodreads.com) could be defined as a social network dedicated for people interested on books. The idea is that you can interact with other readers, authors, and of course books. Between many features, the readers social network provides a members review database to give you more information for the books you are looking for. It works excatly as any other review system. You rate the book you have read (between 1 and 5 stars) and write down your opinion about it, explaining what you like or don't. I know, nothing is new here. The [goodreads.com](http://www.goodreads.com) also have its recommender system for books. It will look for the books you read and the rates you gave to suggest you new books.

In this notebook, I apply a very well known called **Item-Based Collaborative Filtering** (IBCF) technique to estimate the rate of books based on other similar books. The idea is simple, if I like a book (i.e., I rated it 5 stars) it is likely that I will also enjoy a very similar book to that one.

This technique is not new and also there are other methodologies to estimate the rates. Here we are going to focus on the IBCF and see how it works when predicting books rating!!!

## Loading and Cleaning data


```R
# Display plot options
options(repr.plot.width=5, repr.plot.height=3)

# load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
```

The data is available at [Kaggle](https://www.kaggle.com/zygmunt/goodbooks-10k/data). Here we find a dataset with millions of ratings for 10k books. In this page we can also find a very good kernel about **User-Based Collaborative Filtering** (UBCF). The objective is the same, to improve the recommendations system but **UBCF** approach looks for similar users instead of books.


```R
ratings<-read.csv("~/project/pre_git/goodreads/data/ratings.csv")
books<-read.csv("~/project/pre_git/goodreads/data/books.csv")
```

For this application we are going to use 2 datasets:

* **ratings**: Contains information about ratings. It is a simple dataset with only 3 variables: **book_id**, **user_id**, and **rating**. It is pretty straightforward. It represents the rate given from a user for a book.
* **books**: This is dataset has more information. It contains different features for books such as **title**, **author name**, **year of publication**, **number of reviews**, and etc. 


```R
head(books)
```


<table>
<thead><tr><th scope=col>id</th><th scope=col>book_id</th><th scope=col>best_book_id</th><th scope=col>work_id</th><th scope=col>books_count</th><th scope=col>isbn</th><th scope=col>isbn13</th><th scope=col>authors</th><th scope=col>original_publication_year</th><th scope=col>original_title</th><th scope=col>⋯</th><th scope=col>ratings_count</th><th scope=col>work_ratings_count</th><th scope=col>work_text_reviews_count</th><th scope=col>ratings_1</th><th scope=col>ratings_2</th><th scope=col>ratings_3</th><th scope=col>ratings_4</th><th scope=col>ratings_5</th><th scope=col>image_url</th><th scope=col>small_image_url</th></tr></thead>
<tbody>
	<tr><td>1                                                          </td><td> 2767052                                                   </td><td> 2767052                                                   </td><td> 2792775                                                   </td><td> 272                                                       </td><td>439023483                                                  </td><td>9.780439e+12                                               </td><td>Suzanne Collins                                            </td><td>2008                                                       </td><td>The Hunger Games                                           </td><td>⋯                                                          </td><td>4780653                                                    </td><td>4942365                                                    </td><td>155254                                                     </td><td> 66715                                                     </td><td>127936                                                     </td><td>560092                                                     </td><td>1481305                                                    </td><td>2706317                                                    </td><td>https://images.gr-assets.com/books/1447303603m/2767052.jpg </td><td>https://images.gr-assets.com/books/1447303603s/2767052.jpg </td></tr>
	<tr><td>2                                                          </td><td>       3                                                   </td><td>       3                                                   </td><td> 4640799                                                   </td><td> 491                                                       </td><td>439554934                                                  </td><td>9.780440e+12                                               </td><td>J.K. Rowling, Mary GrandPré                                </td><td>1997                                                       </td><td>Harry Potter and the Philosopher's Stone                   </td><td>⋯                                                          </td><td>4602479                                                    </td><td>4800065                                                    </td><td> 75867                                                     </td><td> 75504                                                     </td><td>101676                                                     </td><td>455024                                                     </td><td>1156318                                                    </td><td>3011543                                                    </td><td>https://images.gr-assets.com/books/1474154022m/3.jpg       </td><td>https://images.gr-assets.com/books/1474154022s/3.jpg       </td></tr>
	<tr><td>3                                                          </td><td>   41865                                                   </td><td>   41865                                                   </td><td> 3212258                                                   </td><td> 226                                                       </td><td>316015849                                                  </td><td>9.780316e+12                                               </td><td>Stephenie Meyer                                            </td><td>2005                                                       </td><td>Twilight                                                   </td><td>⋯                                                          </td><td>3866839                                                    </td><td>3916824                                                    </td><td> 95009                                                     </td><td>456191                                                     </td><td>436802                                                     </td><td>793319                                                     </td><td> 875073                                                    </td><td>1355439                                                    </td><td>https://images.gr-assets.com/books/1361039443m/41865.jpg   </td><td>https://images.gr-assets.com/books/1361039443s/41865.jpg   </td></tr>
	<tr><td>4                                                          </td><td>    2657                                                   </td><td>    2657                                                   </td><td> 3275794                                                   </td><td> 487                                                       </td><td>61120081                                                   </td><td>9.780061e+12                                               </td><td>Harper Lee                                                 </td><td>1960                                                       </td><td>To Kill a Mockingbird                                      </td><td>⋯                                                          </td><td>3198671                                                    </td><td>3340896                                                    </td><td> 72586                                                     </td><td> 60427                                                     </td><td>117415                                                     </td><td>446835                                                     </td><td>1001952                                                    </td><td>1714267                                                    </td><td>https://images.gr-assets.com/books/1361975680m/2657.jpg    </td><td>https://images.gr-assets.com/books/1361975680s/2657.jpg    </td></tr>
	<tr><td>5                                                          </td><td>    4671                                                   </td><td>    4671                                                   </td><td>  245494                                                   </td><td>1356                                                       </td><td>743273567                                                  </td><td>9.780743e+12                                               </td><td>F. Scott Fitzgerald                                        </td><td>1925                                                       </td><td>The Great Gatsby                                           </td><td>⋯                                                          </td><td>2683664                                                    </td><td>2773745                                                    </td><td> 51992                                                     </td><td> 86236                                                     </td><td>197621                                                     </td><td>606158                                                     </td><td> 936012                                                    </td><td> 947718                                                    </td><td>https://images.gr-assets.com/books/1490528560m/4671.jpg    </td><td>https://images.gr-assets.com/books/1490528560s/4671.jpg    </td></tr>
	<tr><td>6                                                          </td><td>11870085                                                   </td><td>11870085                                                   </td><td>16827462                                                   </td><td> 226                                                       </td><td>525478817                                                  </td><td>9.780525e+12                                               </td><td>John Green                                                 </td><td>2012                                                       </td><td>The Fault in Our Stars                                     </td><td>⋯                                                          </td><td>2346404                                                    </td><td>2478609                                                    </td><td>140739                                                     </td><td> 47994                                                     </td><td> 92723                                                     </td><td>327550                                                     </td><td> 698471                                                    </td><td>1311871                                                    </td><td>https://images.gr-assets.com/books/1360206420m/11870085.jpg</td><td>https://images.gr-assets.com/books/1360206420s/11870085.jpg</td></tr>
</tbody>
</table>




```R
head(ratings)
```


<table>
<thead><tr><th scope=col>book_id</th><th scope=col>user_id</th><th scope=col>rating</th></tr></thead>
<tbody>
	<tr><td>1   </td><td> 314</td><td>5   </td></tr>
	<tr><td>1   </td><td> 439</td><td>3   </td></tr>
	<tr><td>1   </td><td> 588</td><td>5   </td></tr>
	<tr><td>1   </td><td>1169</td><td>4   </td></tr>
	<tr><td>1   </td><td>1185</td><td>4   </td></tr>
	<tr><td>1   </td><td>2077</td><td>4   </td></tr>
</tbody>
</table>



Let's start to clean our **ratings** dataset. It is possible for a user have more than one rating for the same book. We could assume that the user change its mind and reevalute the book rating. It definitely can happen, but for simplicity we remove these cases and assume that every combination of book and user has only one rating. In this case, we have to eliminate the cases where users gave more than on rating for the same book


```R
ratings<-ratings %>% group_by(user_id, book_id) %>% mutate(total=n())
cat('Number of duplicate ratings: ', format(nrow(ratings[ratings$total > 1,]),big.mark=",",scientific=FALSE))
```

    Number of duplicate ratings:  4,487

Example of duplicated rating: As you can see below, user **3204** rated book **8946** five times. We eliminate these cases.


```R
duplicated_ratings<-ratings%>%
    group_by(book_id,user_id)%>%
    summarise(total=n())%>%
    filter(total>1)%>%
    arrange(desc(total))

head(duplicated_ratings)
```


<table>
<thead><tr><th scope=col>book_id</th><th scope=col>user_id</th><th scope=col>total</th></tr></thead>
<tbody>
	<tr><td>8946 </td><td> 3204</td><td>5    </td></tr>
	<tr><td>2515 </td><td> 4359</td><td>4    </td></tr>
	<tr><td>3996 </td><td>38259</td><td>4    </td></tr>
	<tr><td>6472 </td><td>  691</td><td>4    </td></tr>
	<tr><td>7420 </td><td>34548</td><td>4    </td></tr>
	<tr><td>8946 </td><td>   42</td><td>4    </td></tr>
</tbody>
</table>



As you can see above, user **3204** rated book **8946** five times. We eliminate these cases.


```R
ratings <- ratings[ratings$total == 1,]
```


```R
cat(' Number of ratings: ',format(nrow(ratings),big.mark=",",scientific=FALSE),'\n',
    'Number of Books: ',format(length(unique(ratings$book_id)),big.mark=",",scientific=FALSE),'\n',
    'Number of Users: ',format(length(unique(ratings$user_id)),big.mark=",",scientific=FALSE))
```

     Number of ratings:  977,269 
     Number of Books:  10,000 
     Number of Users:  53,380

For this problem, I decided to worh with books that have atleast 100 reviews, and user who gave more than 20 reviews.  Doing that I am significantly reducing reducing the number books and users. It helped me to reduce process time consumption.


```R
ratings<-ratings%>%group_by(user_id)%>%mutate(total=n())%>%filter(total>20)
ratings<-ratings%>%group_by(book_id)%>%mutate(total=n())%>%filter(total==100)
cat(' Number of ratings: ',format(nrow(ratings),big.mark=",",scientific=FALSE),'\n',
    'Number of Books: ',format(length(unique(ratings$book_id)),big.mark=",",scientific=FALSE),'\n',
    'Number of Users: ',format(length(unique(ratings$user_id)),big.mark=",",scientific=FALSE))
```

     Number of ratings:  145,600 
     Number of Books:  1,456 
     Number of Users:  6,735

Now, we have 9,806,160 combinatations of (**book**, **user**), and only 145,600 ratings. It means that less than 2% of these possible combination have been rated.

## Understand more about our data: Descriptive analysis

### What's the most reviewed book?


```R
books[books$ratings_count==max(books$ratings_count),]
```


<table>
<thead><tr><th scope=col>id</th><th scope=col>book_id</th><th scope=col>best_book_id</th><th scope=col>work_id</th><th scope=col>books_count</th><th scope=col>isbn</th><th scope=col>isbn13</th><th scope=col>authors</th><th scope=col>original_publication_year</th><th scope=col>original_title</th><th scope=col>⋯</th><th scope=col>ratings_count</th><th scope=col>work_ratings_count</th><th scope=col>work_text_reviews_count</th><th scope=col>ratings_1</th><th scope=col>ratings_2</th><th scope=col>ratings_3</th><th scope=col>ratings_4</th><th scope=col>ratings_5</th><th scope=col>image_url</th><th scope=col>small_image_url</th></tr></thead>
<tbody>
	<tr><td>1                                                         </td><td>2767052                                                   </td><td>2767052                                                   </td><td>2792775                                                   </td><td>272                                                       </td><td>439023483                                                 </td><td>9.780439e+12                                              </td><td>Suzanne Collins                                           </td><td>2008                                                      </td><td>The Hunger Games                                          </td><td>⋯                                                         </td><td>4780653                                                   </td><td>4942365                                                   </td><td>155254                                                    </td><td>66715                                                     </td><td>127936                                                    </td><td>560092                                                    </td><td>1481305                                                   </td><td>2706317                                                   </td><td>https://images.gr-assets.com/books/1447303603m/2767052.jpg</td><td>https://images.gr-assets.com/books/1447303603s/2767052.jpg</td></tr>
</tbody>
</table>



![title](https://images.gr-assets.com/books/1447303603m/2767052.jpg)

Hunger Games - Suzanne Collins with 4,780,653 reviews

### Which books has the highest average rating?


```R
books[books$average_rating==max(books$average_rating),]
```


<table>
<thead><tr><th></th><th scope=col>id</th><th scope=col>book_id</th><th scope=col>best_book_id</th><th scope=col>work_id</th><th scope=col>books_count</th><th scope=col>isbn</th><th scope=col>isbn13</th><th scope=col>authors</th><th scope=col>original_publication_year</th><th scope=col>original_title</th><th scope=col>⋯</th><th scope=col>ratings_count</th><th scope=col>work_ratings_count</th><th scope=col>work_text_reviews_count</th><th scope=col>ratings_1</th><th scope=col>ratings_2</th><th scope=col>ratings_3</th><th scope=col>ratings_4</th><th scope=col>ratings_5</th><th scope=col>image_url</th><th scope=col>small_image_url</th></tr></thead>
<tbody>
	<tr><th scope=row>3628</th><td>3628                                                    </td><td>24812                                                   </td><td>24812                                                   </td><td>25599                                                   </td><td>14                                                      </td><td>740748475                                               </td><td>9.780741e+12                                            </td><td>Bill Watterson                                          </td><td>2005                                                    </td><td>The Complete Calvin and Hobbes                          </td><td>⋯                                                       </td><td>28900                                                   </td><td>29968                                                   </td><td>861                                                     </td><td>120                                                     </td><td>154                                                     </td><td>693                                                     </td><td>3117                                                    </td><td>25884                                                   </td><td>https://images.gr-assets.com/books/1473064526m/24812.jpg</td><td>https://images.gr-assets.com/books/1473064526s/24812.jpg</td></tr>
</tbody>
</table>



![title](https://images.gr-assets.com/books/1473064526m/24812.jpg)

The Complete Calvin and Hobbes - Bill Watterson with average of **4.82** stars

### How is the rating distribution?


```R
ratings%>%
ggplot(aes(factor(rating)))+
geom_bar(fill="orange",col="navy")+xlab("stars")+ylab("")+theme_minimal()
```




![png](output_31_1.png)


It seems that most of ratings are between 4-5 stars. Rarely a reader rate it as 1-2 star.

### Number of ratings by user


```R
ratings%>%group_by(user_id)%>%summarise(total=n())%>%
ggplot(aes(total))+
geom_histogram(bins=25,fill="orange",col="navy")+theme_minimal()+xlab("Ratings by user")+ylab("")
```




![png](output_34_1.png)


### Rating average by user


```R
ratings%>%group_by(user_id)%>%summarise(rating=mean(rating))%>%
ggplot(aes(rating))+
geom_histogram(bins=30,fill="orange",col="navy")+theme_minimal()+xlab("Mean of rating by user")+ylab("")+
geom_vline(xintercept = mean(ratings$rating),col="red",lty=2)
```




![png](output_36_1.png)


### Rating average by book


```R
ratings%>%group_by(book_id)%>%summarise(rating=mean(rating))%>%
ggplot(aes(rating))+
geom_histogram(bins=30,fill="orange",col="navy")+theme_minimal()+xlab("rating average by book")+ylab("")+
geom_vline(xintercept = mean(ratings$rating),col="red",lty=2)
```




![png](output_38_1.png)


### Item-Based Collaborative Filter

The idea here is to suggest books that are similar to other books.

We have to create a matrix which each row represents an user and each column a book. The entries of this matrix are the the ratings. For example the element $r_{i,j}$ is the rate from the user $i$ for the book $j$.

As I mentioned before, less than 2% of the combinations of user and book are rated. It means that this matrix has a huge amount of missing data. For these cases we assume value 0.


```R
#Create Matrix from Ratings dataset: rows -> user ; columns->books
rating_matrix<-spread(ratings%>%select(book_id,user_id,rating),book_id,rating)
row.names(rating_matrix)<-rating_matrix$user_id
rating_matrix<-rating_matrix[,!(names(rating_matrix)%in%"user_id")] #remove 'user_id' column
```


```R
rating_matrix<-as.matrix(rating_matrix)
```


```R
dim(rating_matrix)
```


<ol class=list-inline>
	<li>6735</li>
	<li>1456</li>
</ol>




```R
#Rows->Users; Columns->Books
head(rating_matrix)
```


<table>
<thead><tr><th></th><th scope=col>1</th><th scope=col>2</th><th scope=col>3</th><th scope=col>4</th><th scope=col>5</th><th scope=col>6</th><th scope=col>7</th><th scope=col>8</th><th scope=col>9</th><th scope=col>10</th><th scope=col>⋯</th><th scope=col>4309</th><th scope=col>4333</th><th scope=col>4579</th><th scope=col>4793</th><th scope=col>4892</th><th scope=col>5195</th><th scope=col>5507</th><th scope=col>5775</th><th scope=col>7041</th><th scope=col>7150</th></tr></thead>
<tbody>
	<tr><th scope=row>7</th><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>⋯</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td></tr>
	<tr><th scope=row>35</th><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>⋯</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td></tr>
	<tr><th scope=row>41</th><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>⋯</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td></tr>
	<tr><th scope=row>75</th><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>⋯</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td></tr>
	<tr><th scope=row>89</th><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>⋯</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td></tr>
	<tr><th scope=row>143</th><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>⋯</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td></tr>
</tbody>
</table>



### Similarity

How to identify if 2 books are similar? There are a lot of different ways to do this, here we are going to use the cosine similarity.

It calculates the cosine of the angle between two vectors. If the angle is *0* degrees meaning that both vectors are on the same direction the cosine is *1*. If the angle is *180* degrees it means that these vectors are in opposite directions and the cosine is *-1*.

![](https://www.safaribooksonline.com/library/view/statistics-for-machine/9781788295758/assets/2b4a7a82-ad4c-4b2a-b808-e423a334de6f.png)

What are the vectors here? A vector is the rates for a book given by all users. Some users did not read the book in this case the rate is *0*.

For example:

| Movie         | User 1| User 2| User 3| User 4| User 5|
| ------------- |:-----:|:-----:|:-----:|:-----:|:-----:|
| A             | 2     |3    |.|.|.|
| B             | 5     |    .|3|5|5|
| C             | 4     |    5|4|3|.|

The vector for the movie A is:

| User 1| User 2| User 3| User 4| User 5|
|:-----:|:-----:|:-----:|:-----:|:-----:|
| 2     |3    |0|0|0|

### recommenderLab library

The **recommenderlab** is R library providing functions to create recommendation models.


```R
library("recommenderlab")
```


```R
rating_matrix[is.na(rating_matrix)]<-0
```


```R
sparse_rating_matrix<-as(rating_matrix,"sparseMatrix")
```


```R
ratings <- new("realRatingMatrix", data = sparse_rating_matrix)
```

### Model


```R
model <- Recommender(ratings, method = "IBCF",param=list(method="Cosine"))
```


```R
model
```


    Recommender of type ‘IBCF’ for ‘realRatingMatrix’ 
    learned using 6735 users.


### Cross-Validation

Let's evaluate how our model is performing comparing to a random recommendation system.


```R
scheme <- evaluationScheme(real_ratings, method = "cross-validation", k = 10, given = -1, goodRating = 5)
```


```R
models <- list("random" = list(name = "RANDOM", param = NULL),
                   "IBCF" = list(name = "IBCF",param=list(method="Cosine")),
                   "UBCF"= list(name = "UBCF",param=list(method="Cosine"))
                   )
```


```R
error <- evaluate(scheme, models, type = "ratings")
```

    RANDOM run fold/sample [model time/prediction time]
    	 1  [0.002sec/0.55sec] 
    	 2  [0.002sec/0.517sec] 
    	 3  [0.002sec/0.592sec] 
    	 4  [0.002sec/0.647sec] 
    	 5  [0.002sec/0.565sec] 
    	 6  [0.001sec/0.561sec] 
    	 7  [0.002sec/0.529sec] 
    	 8  [0.001sec/0.532sec] 
    	 9  [0.002sec/0.612sec] 
    	 10  [0.002sec/0.556sec] 
    IBCF run fold/sample [model time/prediction time]
    	 1  [53.083sec/0.19sec] 
    	 2  [51.356sec/0.189sec] 
    	 3  [53.515sec/0.197sec] 
    	 4  [49.942sec/0.189sec] 
    	 5  [54.115sec/0.377sec] 
    	 6  [58.095sec/0.22sec] 
    	 7  [57.663sec/0.19sec] 
    	 8  [55.698sec/0.194sec] 
    	 9  [54.132sec/0.247sec] 
    	 10  [54.339sec/0.209sec] 
    UBCF run fold/sample [model time/prediction time]
    	 1  [0.019sec/22.7sec] 
    	 2  [0.016sec/22.781sec] 
    	 3  [0.017sec/21.724sec] 
    	 4  [0.017sec/21.382sec] 
    	 5  [0.016sec/22.386sec] 
    	 6  [0.017sec/22.766sec] 
    	 7  [0.016sec/23.16sec] 
    	 8  [0.017sec/22.506sec] 
    	 9  [0.016sec/23.052sec] 
    	 10  [0.017sec/22.739sec] 



```R
lapply(unlist(lapply(results, function(x) slot(x,"results"))), function(x) x@cm[,"RMSE"])%>%
    as.data.frame()#%>%
    #gather(key="Algorithm",value="RMSE")
```


<table>
<thead><tr><th scope=col>random1</th><th scope=col>random2</th><th scope=col>random3</th><th scope=col>random4</th><th scope=col>random5</th><th scope=col>random6</th><th scope=col>random7</th><th scope=col>random8</th><th scope=col>random9</th><th scope=col>random10</th><th scope=col>⋯</th><th scope=col>UBCF1</th><th scope=col>UBCF2</th><th scope=col>UBCF3</th><th scope=col>UBCF4</th><th scope=col>UBCF5</th><th scope=col>UBCF6</th><th scope=col>UBCF7</th><th scope=col>UBCF8</th><th scope=col>UBCF9</th><th scope=col>UBCF10</th></tr></thead>
<tbody>
	<tr><td>1.185968 </td><td>1.200743 </td><td>1.208958 </td><td>1.149076 </td><td>1.177499 </td><td>1.20353  </td><td>1.147874 </td><td>1.166884 </td><td>1.227453 </td><td>1.168076 </td><td>⋯        </td><td>0.9376655</td><td>0.8903662</td><td>0.8835769</td><td>0.8972772</td><td>0.8969304</td><td>0.8856585</td><td>0.9088058</td><td>0.9031941</td><td>0.9341926</td><td>0.8647512</td></tr>
</tbody>
</table>




```R
lappy(lapply(results, function(x) slot(x,"results")), function(x) slot(x,"cm"))
```


    Error in lappy(lapply(results, function(x) slot(x, "results")), function(x) slot(x, : could not find function "lappy"
    Traceback:




```R
tmp<-lapply(error,function(x) slot(x,"results"))
res <- tmp %>% 
  lapply(function(x) unlist(lapply(x, function(x) unlist(x@cm[ ,"RMSE"])))) %>% 
  as.data.frame() %>% 
  gather(key = "Algorithm", value = "RMSE")    
```


```R
ggplot(res,aes(x=Algorithm,y=RMSE,fill=Algorithm))+
geom_bar(stat='summary')+geom_errorbar(stat = "summary", width = 0.3, size = 0.8)
```

    No summary function supplied, defaulting to `mean_se()
    No summary function supplied, defaulting to `mean_se()





![png](output_66_2.png)


Apperantly, we were not able to create an **IBCF** much better than a random choice. The **UBCF**, similar to the one presented on [kaggle kernel](https://www.kaggle.com/philippsp/book-recommender-collaborative-filtering-shiny/notebook), performed way better in this case. It seems that is better to find similar users than find a similar book. Maybe we could improve the results by applying other techniques. Here, the idea is to provide a simple approach and see how it works. The challenge to provide a good recommendation has been widely discussed by the comunity. Hope we
