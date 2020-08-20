if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
#--------------------------------------#
# 1. A few useful Data transformations # 
#--------------------------------------#

  #  convert timestamp to the date &  extract movie release year as separate
  # column using regex @ https://regex101.com/r/Bllx8Z/2
library(lubridate)
fun_transform <- function(x){
  x %>% 
    mutate(date_posted=year(as_datetime(timestamp))) %>% 
    group_by(title) %>% 
    extract(title,into=c("title","year"), "^(.*?)\\((\\d{4})\\)") %>% 
    mutate(year=as.integer(year)) %>% 
    ungroup %>%   
    select(-c(timestamp))
  }

edx <- fun_transform(edx)
validation <- fun_transform(validation)

#----------------------------------#
# 2. Create edx train and test set #
#----------------------------------#

set.seed(1, sample.kind="Rounding")
# 2.1  test set will be 10% of edx data
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
temp <- edx[test_index,]
# 2.2 Make sure userId and movieId in edx_test set are also in edx_train
edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")
# 2.3 Add 17 obs. removed from edx_test back into edx_train set
removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)
# 2.4 cleanup the temp variables
rm(test_index, temp, removed, fun_transform)     

# 3.  Summary
glimpse(edx)
summary(edx)

# 3.1. Premise: popular movies have above avg ratings
# stratifying the post-1993 movies by ratings per year and compute their average ratings.
# To calculate number of ratings per year, let's use 2018 as the end year. Make a plot of average rating
# versus ratings per year and show an estimate of the trend. 

df1 <- edx %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  arrange(desc(rate)) 

df1 %>% ggplot(aes(rating, rate)) + geom_point() +  geom_smooth(method='auto', formula = y ~ poly(x))
rm(df1)
# ----------------------------------------------------------------------------------------------------

#Display that the matrix of 10 most active users rating 10 most rated movies
# would still have gaps, i.e., the data is far from complete

maxUser <- tail(names(sort(table(edx$userId))),10)  # top 10 users
maxMovie <- tail(names(sort(table(edx$movieId))),10) # top 10 movies
reduced <- edx[edx$userId %in% maxUser,]   # reduced 10 users df
reduced <- reduced[reduced$movieId %in% maxMovie,]  # reduced more for 10 movies
tenx10 <- reduced[, c("userId", "title", "rating"), drop=FALSE] %>% spread(title, rating)
tenx10_long <- gather(tenx10 %>% mutate(userId = factor(userId)), title, rating, -userId)
a <- ggplot(tenx10_long, aes(userId, title, fill=rating))+ geom_tile() +
  theme(axis.text.x = element_text(angle = 90))

# do the same on n < 100 random users and movies
randId <- sample(unique(edx$userId),100) # random < = 100 users
randMovie <- sample(unique(edx$movieId),100) # random < = 100 movies
rand <-edx %>% filter(userId %in% randId) %>% filter(movieId %in% randMovie)
rand <- rand[, c("userId", "title", "rating"), drop=FALSE] %>% spread(title, rating)
rand10long <- gather(rand %>% mutate(userId = factor(userId)), title, rating, -userId)
b <- ggplot(rand10long, aes(userId, title, fill=rating))+ geom_tile()+
  theme(axis.text.x = element_blank())
# paste on the same plot
require(gridExtra)
grid.arrange(a, b)
# clean up
rm(rand, rand10long, reduced, tenx10, tenx10_long, maxMovie, maxUser, randId, randMovie, a, b)

# ----------------------------------------------------------------

# The data also has a genres column. This column includes every genre that applies to 
# the movie. Some movies fall under several genres. Let's define a category as whatever
# combination appears in this column and keep only categories with more than 1,000 ratings.
# Then let's compute the average and standard error for each category. 

df1 <- edx %>% 
  group_by(genres) %>%
  summarize(cnt = n(),
            avg = mean(rating),
            se= sd(rating)/sqrt(cnt)) %>%
  filter(cnt>1000) %>% 
  mutate(genres = reorder(genres, avg))

# Here is averages by genre plotted as error bar plots showing that genre
# has a strong effect
df1%>%
  ggplot(aes(x = genres, y = avg, color='coral', ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() + 
  geom_errorbar() + 
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle = 90, hjust = 1))
rm(df1)
# ----------------------------------------------------------------
# long tailed plots 
# after about 5,000 most active users and 500 most popular movies the ratings tail off. 
# Because of the rarity of observed ratings in the long tail it is generally more difficult
# to provide robust rating predictions in the long tail. In fact, many recommendation algorithms
# have a tendency to suggest popular rather than infrequent movies
rm(a,b)

