rm(list = ls())

library(ggplot2)
library(caret)
library(httr)
library(tibble)
library(tidyverse)
library(magrittr)
library(purrr)
library(repurrrsive)
library(rlist)
library(glue)
library(patchwork)
library(data.table)
library(kableExtra)
library(h2o)
library(skimr)
library(recipes)
library(stringr)
library(tracerer)
library(ggthemes)

token = "30f473d0cf614b5b82c6e13f9f7df5ad"
football_api = GET("https://api.football-data.org//v2/competitions",
                       add_headers("X-Auth-Token"= token)
)
api_content = content(football_api)
competitions = api_content$competitions

id_vector = map_int(competitions, ~.$id)
name_vector = map_chr(competitions, ~.$name)

all_competitions = data.frame(
  id = id_vector, 
  name = name_vector
)

all_competitions

avail_competition_id = c("2000", "2001", "2002", "2003", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2021","2152")

# Available competitions 
avail_competitions = all_competitions %>%
  filter(all_competitions$id %in% avail_competition_id)

avail_competitions

#Retrieving all match details from the API corresponding to the list of available competitions
match_content = list()

for(id in avail_competitions$id){
  url = glue("http://api.football-data.org/v2/competitions/{id}/matches?dateFrom=2021-05-30&dateTo=2021-12-01")
  matches_comp = GET(url, add_headers("X-Auth-Token"= token))
  match_content = append(match_content, content(matches_comp))
}

all_matches = match_content[names(match_content) == "matches"]


#The data retrieved from the API is in the form of a list. We need to unnest it and convert it into a dataframe.

matches_df = data.frame()

for(x in 1:11){
  tbl <- as_tibble(all_matches[x])
  matches_df <- bind_rows(matches_df, unnest_wider(tbl,matches))
}

matches_df

matches_df <- subset (matches_df, select = -c(id, season, referees, group))

# Data cleaning, wrangling and tidying

#Un-nesting list data for odds column:
  
matches_df  <- matches_df %>%
  mutate(df = map(odds, ~ .x %>% 
                    map_df(magrittr::extract,)))

matches_df  <- matches_df  %>%
  select(-odds) %>%
  unnest(df)

#Renaming the columns for enhance understanding 
matches_df <- matches_df %>% 
  dplyr::rename(
    Odds.homeWin = homeWin,
    Odds.draw = draw,
    Odds.awayWin = awayWin
  )

#Un-nesting list data for homeTeam column

matches_df  <- matches_df %>%
  mutate(df = map(homeTeam, ~ .x %>% 
                    map_df(magrittr::extract, 
                    )))

matches_df  <- matches_df  %>%
  select(-homeTeam) %>%
  unnest(df)

matches_df <- subset (matches_df, select = -c(id))
matches_df <- matches_df %>% 
  dplyr::rename(
    homeTeam = name
  )

#Un-nesting list data for awayTeam column

matches_df  <- matches_df %>%
  mutate(df = map(awayTeam, ~ .x %>% 
                    map_df(magrittr::extract, 
                    )))
matches_df  <- matches_df  %>%
  select(-awayTeam) %>%
  unnest(df)

matches_df <- subset (matches_df, select = -c(id))

matches_df <- matches_df %>% 
  dplyr::rename(
    awayTeam = name
  )

# Un-nesting list data for scores column. This will yield full time and half time goals of the homeTeam and awayTeam

scores = matches_df$score
winner_vector = map_chr(scores,"winner", .default = NA)

fullTimeScores = map(scores,"fullTime")
fTHome = map_int(fullTimeScores,"homeTeam", .default = NA)
fTAway = map_int(fullTimeScores,"awayTeam", .default = NA)

halfTimeScores = map(scores,"halfTime")
hTHome = map_int(halfTimeScores,"homeTeam", .default = NA)
hTAway = map_int(fullTimeScores,"awayTeam", .default = NA)

matches_df$winner = winner_vector
matches_df$fTHome = fTHome
matches_df$fTAway = fTAway
matches_df$hTHome = hTHome
matches_df$hTAway = hTAway

football_df <- subset (matches_df, select = -c(score))
football_df <- football_df[, c("Odds.homeWin","Odds.draw","Odds.awayWin","hTHome", "hTAway","fTHome", "fTAway", "homeTeam", "awayTeam","winner")]

football_df

# Secondary Data

#We are using secondary data to enrich the existing primary data retrieved from the API.

for (j in c('/SP1','/SP2','/E0')){ 
  url <- c('http://football-data.co.uk/mmz4281/',paste(c(j,'.csv'), collapse=""))
  for(i in 0:18){
    if (i < 9){
      season_start <- paste(c('0',i), collapse="")
      season_end <- paste(c('0',i+1), collapse="")
    } 
    else {
      if (i == 9){
        season_start <- paste(c('0',i), collapse="")
        season_end <- i+1
      }
      else {
        season_start <- i
        season_end <- i+1
      }
    }
    sec_df <- read.csv(file = paste(url, collapse=paste(c(season_start,season_end), collapse="")))
    sec_df <- sec_df[, c("IWH", "IWD", "IWA","HTHG","HTAG","FTHG","FTAG","HomeTeam","AwayTeam","FTR")]
    
    setnames(sec_df, old = names(sec_df), 
             new = c("Odds.homeWin","Odds.draw","Odds.awayWin","hTHome", "hTAway","fTHome","fTAway","homeTeam", "awayTeam","winner"))
    
    
    sec_df$winner <- as.character(sec_df$winner)
    sec_df$winner <- ifelse(sec_df$winner == "H","HOME_TEAM",sec_df$winner)
    sec_df$winner <- ifelse(sec_df$winner == "A","AWAY_TEAM",sec_df$winner)
    sec_df$winner <- ifelse(sec_df$winner == "D","DRAW",sec_df$winner)
    
    football_df <- rbind(football_df, sec_df) 
  }
}

football_df

#Check and remove any NULL values from the data.

#Check for any NULL values
colSums(is.na(football_df))

#Drop null valuess
football_df <- drop_na(football_df)
```

#The secondary data and the API data had team names written in different way, so renaming
#the team names is important to remove any inconsistency.

football_df$homeTeam <- gsub('1. FC', 'dummy', football_df$homeTeam)
football_df$awayTeam <- gsub('1. FC', 'dummy', football_df$awayTeam)
football_df$homeTeam <- gsub(' FC', '', football_df$homeTeam)
football_df$awayTeam <- gsub(' FC', '', football_df$awayTeam)
football_df$homeTeam <- gsub(' CF', '', football_df$homeTeam)
football_df$awayTeam <- gsub(' CF', '', football_df$awayTeam)
football_df$homeTeam <- gsub('dummy', '1. FC', football_df$homeTeam)
football_df$awayTeam <- gsub('dummy', '1. FC', football_df$awayTeam)
football_df$homeTeam <- gsub(' AFC', '', football_df$homeTeam)
football_df$awayTeam <- gsub(' AFC', '', football_df$awayTeam)
football_df$homeTeam <- gsub('Barcelona', 'FC Barcelona', football_df$homeTeam)
football_df$awayTeam <- gsub('Barcelona', 'FC Barcelona', football_df$awayTeam)
football_df$homeTeam <- gsub('Bournemouth', 'AFC Bournemouth', football_df$homeTeam)
football_df$awayTeam <- gsub('Bournemouth', 'AFC Bournemouth', football_df$awayTeam)
football_df$homeTeam <- gsub('Brighton', 'Brighton & Hove Albion', football_df$homeTeam)
football_df$awayTeam <- gsub('Brighton', 'Brighton & Hove Albion', football_df$awayTeam)
football_df$homeTeam <- gsub('Club Atl?tico de Madrid', 'Ath Madrid', football_df$homeTeam)
football_df$awayTeam <- gsub('Club Atl?tico de Madrid', 'Ath Madrid', football_df$awayTeam)
football_df$homeTeam <- gsub('Coventry', 'Coventry City', football_df$homeTeam)
football_df$awayTeam <- gsub('Coventry', 'Coventry City', football_df$awayTeam)
football_df$homeTeam <- gsub('Eibar', 'SD Eibar', football_df$homeTeam)
football_df$awayTeam <- gsub('Eibar', 'SD Eibar', football_df$awayTeam)
football_df$homeTeam <- gsub('Huddersfield', 'Huddersfield Town', football_df$homeTeam)
football_df$awayTeam <- gsub('Huddersfield', 'Huddersfield Town', football_df$awayTeam)
football_df$homeTeam <- gsub('Leeds', 'Leeds United', football_df$homeTeam)
football_df$awayTeam <- gsub('Leeds', 'Leeds United', football_df$awayTeam)
football_df$homeTeam <- gsub('Levante', 'Levante UD', football_df$homeTeam)
football_df$awayTeam <- gsub('Levante', 'Levante UD', football_df$awayTeam)
football_df$homeTeam <- gsub('Man City', 'Manchester City', football_df$homeTeam)
football_df$awayTeam <- gsub('Man City', 'Manchester City', football_df$awayTeam)
football_df$homeTeam <- gsub('Man United', 'Manchester United', football_df$homeTeam)
football_df$awayTeam <- gsub('Man United', 'Manchester United', football_df$awayTeam)
football_df$homeTeam <- gsub('Newcastle', 'Newcastle United', football_df$homeTeam)
football_df$awayTeam <- gsub('Newcastle', 'Newcastle United', football_df$awayTeam)
football_df$homeTeam <- gsub('Norwich', 'Norwich City', football_df$homeTeam)
football_df$awayTeam <- gsub('Norwich', 'Norwich City', football_df$awayTeam)
football_df$homeTeam <- gsub('Osasuna', 'CA Osasuna', football_df$homeTeam)
football_df$awayTeam <- gsub('Osasuna', 'CA Osasuna', football_df$awayTeam)
football_df$homeTeam <- gsub('Real Sociedad de F?tbol', 'Real Sociedad', football_df$homeTeam)
football_df$awayTeam <- gsub('Real Sociedad de F?tbol', 'Real Sociedad', football_df$awayTeam)
football_df$homeTeam <- gsub('Sociedad', 'Real Sociedad', football_df$homeTeam)
football_df$awayTeam <- gsub('Sociedad', 'Real Sociedad', football_df$awayTeam)
football_df$homeTeam <- gsub('Tottenham', 'Tottenham Hotspur', football_df$homeTeam)
football_df$awayTeam <- gsub('Tottenham', 'Tottenham Hotspur', football_df$awayTeam)
football_df$homeTeam <- gsub('West Brom', 'West Bromwich Albion', football_df$homeTeam)
football_df$awayTeam <- gsub('West Brom', 'West Bromwich Albion', football_df$awayTeam)
football_df$homeTeam <- gsub('West Ham', 'West Ham United', football_df$homeTeam)
football_df$awayTeam <- gsub('West Ham', 'West Ham United', football_df$awayTeam)
football_df$homeTeam <- gsub('Wolves', 'Wolverhampton Wanderers', football_df$homeTeam)
football_df$awayTeam <- gsub('Wolves', 'Wolverhampton Wanderers', football_df$awayTeam)

#The dataset has 24458 rows:
  
nrow(football_df)

# Ratings Data to enrich the existing data

# We're also using secondary data in the form of ratings of teams. This will also be used for modeling. 

ratings_df <- read.csv(file = "https://raw.githubusercontent.com/AyanB15/Big-Data-Anaytics--R/main/SoccerClubRatings.csv")

#Check for any NULL values
colSums(is.na(ratings_df))

#Drop null values
ratings_df <- drop_na(ratings_df)

ratings_df

# Removing some unnecessary columns from the ratings file.

ratings_df <- select(ratings_df,-c('ATT','MID','DEF','League'))
names(ratings_df)[names(ratings_df) == "ï..Name"] <- "Teams" 
ratings_df$Teams <- as.character(ratings_df$Teams)
ratings_df$Teams <- gsub('1. FC', 'dummy', ratings_df$Teams)
ratings_df$Teams <- gsub('dummy', '1. FC', ratings_df$Teams)

ratings_df$Teams <- gsub(' FC', '', ratings_df$Teams)
ratings_df$Teams <- gsub(' AFC', '', ratings_df$Teams)
ratings_df$Teams <- gsub(' CF', '', ratings_df$Teams)

ratings_df$OVR <- ratings_df$OVR/100


# Now, we have 2 datasets: first one from the API and secondary data and the second one is the ratings dataset. Both the datasets needs to be merged using joins to find out the corresponding team ratings from the ratings file.

setnames(ratings_df, old = c("Teams","OVR"), 
         new = c("homeTeam","homePerformancerating"))

football_df2 <- inner_join(football_df, ratings_df)

setnames(ratings_df, old = c("homeTeam","homePerformancerating"), 
         new = c("awayTeam","awayPerformancerating"))

football_df2 <- inner_join(football_df2, ratings_df)

# Assessing the winner based on the outcome of the match
#0: means match ended in a Draw
#1: Home team Won
#2: Away team Won
football_df2$winner <- ifelse(football_df2$winner == "DRAW",0,football_df2$winner)
football_df2$winner <- ifelse(football_df2$winner == "HOME_TEAM",1,football_df2$winner)
football_df2$winner <- ifelse(football_df2$winner == "AWAY_TEAM",2,football_df2$winner)
football_df2$winner <- as.factor(football_df2$winner)

# The final dataset after data cleaning, wrangling and tidying. 

football_df2

# Data Visualizations

#1) Top 10 Home and Away Team Winners
  
count_homewins =subset(football_df2, winner==1) %>% count(homeTeam )
count_homewins <- count_homewins[with(count_homewins,order(-n)),]
count_homewins <- count_homewins[1:10,]

ggplot(data = count_homewins, aes(x = reorder(homeTeam, -n), y = n, fill = homeTeam)) +
  xlab("Team Name") +
  ylab("wins")+ 
  labs(title = "Top 10 Home Team Winners ") + 
  geom_bar(stat = "identity", position = 'dodge')+
  theme_gdocs()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6))+
  geom_text(aes(label = n), vjust = 1.5, colour = "black")+
  theme(legend.position="none")

count_awaywins =subset(football_df2, winner==2) %>% count(awayTeam )
count_awaywins <- count_awaywins[with(count_awaywins,order(-n)),]
count_awaywins <- count_awaywins[1:10,]

ggplot(data = count_awaywins, aes(x = reorder(awayTeam, -n), y = n, fill = awayTeam)) +
  xlab("Team Name") +
  ylab("Wins")+ 
  labs(title = "Top 10 Away Team Winners ") + 
  geom_bar(stat = "identity", position = 'dodge')+
  theme_gdocs()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  geom_text(aes(label = n), vjust = 1.5, colour = "black")+
  theme(legend.position="none")


#2) Home Win / Draw / Away Win**
  
ggplot(football_df, aes(x=winner, fill=as.factor(winner) )) + 
  geom_bar( ) +
  scale_fill_hue(c("red", "green", "blue")) +
  geom_text( stat='count', aes(label=..count..), vjust= 2, position = position_dodge(.9) ) +
  theme(legend.position="topright")


# 3) Top 3 home winning teams: matches draw, won and lost

count_homewins_top3 <- count_homewins[1:3,]
count_homewins_top3 <- count_homewins_top3[, c('homeTeam')]
three_team_df <- football_df2[, c('homeTeam', 'winner')] %>% 
  filter(grepl('Manchester United|Arsenal|Chelsea', homeTeam))%>% 
  group_by(homeTeam)

ggplot(three_team_df, aes(homeTeam)) + 
  geom_bar(aes(fill = winner), position="dodge")+
  theme_gdocs()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
  geom_text( stat='count', aes(fill = winner, label=..count..), vjust= 2, position = position_dodge(.9) )


# ML Procedure Summary

# Split, Organize and Skim the Data

#Split the dataset into train (70%) and test (30%).

intrain <- createDataPartition(y = football_df2$winner,
                               p = 0.7,
                               list = FALSE)

application_train <- football_df2[intrain,]
application_test <- football_df2[-intrain,]
application_test <- application_test %>%
  select(-winner)

#Below is the train data:
  
head(application_train)

#Below is the test data:
  
head(application_test)

#Organize the data:
  
x_train_tbl <- application_train %>% 
  select(-winner)
y_train_tbl <- application_train %>% 
  select(winner)

x_test_tbl <- application_test


#Use skim for data inspection:
  
x_train_tbl_skim = partition(skim(x_train_tbl))
names(x_train_tbl_skim)

# Data Wrangling for ML

string_2_factor_names <- x_train_tbl_skim$character$skim_variable
string_2_factor_names


unique_numeric_values_tbl <- x_train_tbl %>%
  select(x_train_tbl_skim$numeric$skim_variable) %>%
  map_df(~ unique(.) %>% length()) %>%
  gather() %>%
  arrange(value) %>%
  mutate(key = as_factor(key))
unique_numeric_values_tbl

factor_limit <- 15
num_2_factor_names <- unique_numeric_values_tbl %>%
  filter(value < factor_limit) %>%
  arrange(desc(value)) %>%
  pull(key) %>% # pull out a single variable as a vector
  as.character()
num_2_factor_names

# Recipes for ML Data Transformation

rec_obj <- recipe(~ ., data = x_train_tbl) %>%
  step_string2factor(all_of(string_2_factor_names)) %>%
  step_num2factor(all_of(num_2_factor_names),
                  levels=str_c(0:330),
                  transform = function(x) x + 1) %>%
  step_impute_mean(all_numeric()) %>% # missing values in numeric columns
  step_impute_mode(all_nominal()) %>% # missing values in factor columns
  prep(training = x_train_tbl)

rec_obj


#Start baking:
  
x_train_processed_tbl <- bake(rec_obj, x_train_tbl)
x_test_processed_tbl <- bake(rec_obj, x_test_tbl)
```

#Transform the outcome variable (y):
  

rec_obj_for_y <- recipe(~ ., data = y_train_tbl) %>%
  prep(stringsAsFactors = FALSE)
y_train_processed_tbl <- bake(rec_obj_for_y, y_train_tbl)

# Use H2o

#We will use h2o 

library(h2o)
h2o.init(nthreads = -1)

h2o.clusterInfo()


data_h2o <- as.h2o(
  bind_cols(y_train_processed_tbl, x_train_processed_tbl),
  destination_frame= "train.hex") 

new_data_h2o <- as.h2o(
  x_test_processed_tbl,
  destination_frame= "test.hex" #destination_frame is optional
)

data_h2o_no_destination <- as.h2o(
  bind_cols(y_train_processed_tbl, x_train_processed_tbl)
)

#Splitting the training data into 3 subsets:
 
splits <- h2o.splitFrame(data = data_h2o,
                         ratios = c(0.7, 0.15), # 70/15/15 split
                         seed = 1234)
train_h2o <- splits[[1]] # from training data
valid_h2o <- splits[[2]] # from training data
test_h2o <- splits[[3]]  # from training data

# Deep Learning Model 1

y <- "winner" # column name for outcome
x <- colnames(select(football_df2,-c('winner','homeTeam','awayTeam','fTHome','fTAway')))

m1 <- h2o.deeplearning(
  model_id = "dl_model_first",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o, 
  epochs = 100 
)

summary(m1)

# Deep Learning Model 2 with some serious tuning

m2 <- h2o.deeplearning(
  model_id="dl_model_tuned",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  overwrite_with_best_model = F, ## Return the final model after 100 epochs,
  ## even if not the best
  hidden = c(150,150,150), ## more hidden layers -> more complex interactions
  epochs = 100, 
  score_validation_samples = 10000, ## downsample validation set for faster scoring
  score_duty_cycle = 0.025, ## don't score more than 2.5% of the wall time
  adaptive_rate = F, ## manually tuned learning rate
  rate = 0.01,
  rate_annealing = 2e-6,
  momentum_start = 0.2, ## manually tuned momentum
  momentum_stable = 0.4,
  momentum_ramp = 1e7,
  l1 = 1e-5, ## add some L1/L2 regularization
  l2 = 1e-5,
  max_w2 = 10 ## helps stability for Rectifier
)

summary(m2)

# Hyper-parameter tuning w/grid search

hyper_params <- list(
  hidden = list( c(50,50,50), c(100,100) ),
  input_dropout_ratio = c(0, 0.05),
  rate = c(0.01, 0.02),
  rate_annealing = c(1e-8, 1e-7, 1e-6)
)

grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  epochs = 100,
  stopping_metric = "misclassification",
  stopping_tolerance = 1e-2, 
  stopping_rounds = 2,
  score_validation_samples = 10000, ## downsample validation set for faster scoring
  score_duty_cycle = 0.025, ## don't score more than 2.5% of the wall time
  adaptive_rate = F, #manually tuned learning rate
  momentum_start = 0.5, #manually tuned momentum
  momentum_stable = 0.9,
  momentum_ramp = 1e7,
  l1 = 1e-5,
  l2 = 1e-5,
  activation = c("Rectifier"),
  max_w2 = 10, #can help improve stability for Rectifier
  hyper_params = hyper_params
)

grid <- h2o.getGrid("dl_grid", sort_by="logloss", decreasing=FALSE)
dl_grid_summary_table <- grid@summary_table
dl_grid_summary_table


#To find best model in the grid

dl_grid_best_model <- h2o.getModel(dl_grid_summary_table$model_ids[1])
summary(dl_grid_best_model)

#To find parameters used in the best model

dl_grid_best_model_params <- dl_grid_best_model@allparameters
dl_grid_best_model_params


# AutoML in h2o

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs = 900 
)

automl_leaderboard <- automl_models_h2o@leaderboard
automl_leaderboard

automl_leader <- automl_models_h2o@leader

performance_h2o <- h2o.performance(automl_leader, newdata = test_h2o)
performance_h2o %>%
  h2o.confusionMatrix()

#Close the h2o:
  
h2o.shutdown(prompt = F)