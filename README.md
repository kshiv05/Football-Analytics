# Football-Analytics

## Purpose

Football is the world’s most popular ball game in terms of participants and spectators. It is played by 250 million players in over 200 countries. Owing to its magnitude, the outcome of a football match is of a significant impact to the team owners, management, and of course, the betters. Sports betting is a $500 billion market according to Sydney Herald.

Off late, it has been associated with important monetary transactions and financial sponsoring. People and organizations have turned towards investing in the football clubs and leagues and therefore, the outcome of a match not only affects the team management but also numerous other people associated wiht it. Predicting an outcome of a football match hence drives the market on the whole.

Through this project done in R programming, we’ll be using in-game statistics which would help us evaluate a team’s performance instead of just leveraging the actual number of “goals scored” metric from prior matches. We would combine some key metrics of a team like their half time scores, team’s overall offensive and defensive ratings which are updated after each game to build model predicting the outcome of future matches.

## About the dataset
The primary data for the project has been sourced from [Football API](https://www.football-data.org/documentation/quickstart)
This API gave us privilege of accessing data for 13 competitions in free version of subscription.  

Click [here] (https://github.com/kshiv05/Football-Analytics/blob/main/Data.rar) for all the data used (primary data from API and secondary data in the form of ratings).

## Dataset description
The data consists of the competition names, matches played in those competitions, team names, and data (including live results, lineups/subs, goalscorers, bookings, squads).

## Conclusion

  1. Manchester United and Chelsea are consistent in the home and away win categories. They both secured top 2 spots.
  2. Newcastle United is performing well and winning on home grounds, but they are not in picture when it comes to top 10 winning teams on away grounds.
  3. Aston Villa team is winning matches on away grounds than that of top 10 on home grounds.
  4. A win of the home-based team has always been the most likely event.
  5. A win of the visiting team (away team) is the second most likely outcome.
  6. A draw has been the least likely outcome, although the difference with the count of wins for away_team is nominal
  7. For all the top 3 winning teams, 66% matches have been won on home ground
