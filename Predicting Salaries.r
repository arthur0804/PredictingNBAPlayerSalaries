
library(ggplot2)
library(tidyr)
library(dplyr)
library(lsa)
library(rpart)
library(rpart.plot)
library(faraway)

# read datas
data_stat <- read.csv("nbastats/Stats.csv", header = TRUE, stringsAsFactors = FALSE, sep=",")
data_salary <- read.csv("nbastats/NBA_Salary.csv", header = TRUE, stringsAsFactors = FALSE, sep=",")

data_stat <- data.frame(data_stat)
data_salary <- data.frame(data_salary)

dim(data_stat)
dim(data_salary)

head(data_stat)

colnames(data_salary)[1] <- "Year"
head(data_salary)

summary(data_salary)

summary(data_stat)

# change the salary from character to number
data_salary$Salary <- as.numeric (data_salary$Salary)
# convert na to 0
data_salary[is.na(data_salary)] <- 0
is.numeric(data_salary$Salary)

# change the salary by calculating the inflation rate
# convert all to 2017

# create a data frame to store all the inflation index
year <- c (1991 : 2017)
cpi <- c (136.2, 140.3, 144.5, 148.2, 152.4, 156.9, 160.5, 163.0, 166.6, 172.2, 177.1, 179.9,
         184.0, 188.9, 195.3, 201.6, 207.3, 215.3, 214.5, 218.0, 225.0, 230.0, 233.2, 236.7, 
         237.9, 240.0, 245.1)
data_cpi <- data.frame (year, cpi)
head(data_cpi)

# Calculate
for (i in 1991:2017){
  x <- data_cpi[data_cpi$year == i,]$cpi
  data_salary[data_salary$Year == i,]$Salary <- data_salary[data_salary$Year == i,]$Salary * 245.1 / x
}
# convertion done

distinct(data_salary,Team)
distinct(data_stat,Tm)

data_salary[data_salary$Team == 'Atlanta Hawks',]$Team <- 'ATL'
data_salary[data_salary$Team == 'Boston Celtics',]$Team <- 'BOS'
data_salary[data_salary$Team == 'Brooklyn Nets',]$Team <- 'BRK'
data_salary[data_salary$Team == 'Chicago Bulls',]$Team <- 'CHI'
data_salary[data_salary$Team == 'Cleveland Cavaliers',]$Team <- 'CLE'
data_salary[data_salary$Team == 'Dallas Mavericks',]$Team <- 'DAL'
data_salary[data_salary$Team == 'Denver Nuggets',]$Team <- 'DEN'
data_salary[data_salary$Team == 'Detroit Pistons',]$Team <- 'DET'
data_salary[data_salary$Team == 'Golden State Warriors',]$Team <- 'GSW'
data_salary[data_salary$Team == 'Houston Rockets',]$Team <- 'HOU'
data_salary[data_salary$Team == 'Indiana Pacers',]$Team <- 'IND'
data_salary[data_salary$Team == 'Los Angeles Clippers',]$Team <- 'LAC'
data_salary[data_salary$Team == 'Los Angeles Lakers',]$Team <- 'LAL'
data_salary[data_salary$Team == 'Memphis Grizzlies',]$Team <- 'MEM'
data_salary[data_salary$Team == 'Miami Heat',]$Team <- 'MIA'
data_salary[data_salary$Team == 'Milwaukee Bucks',]$Team <- 'MIL'
data_salary[data_salary$Team == 'Minnesota Timberwolves',]$Team <- 'MIN'
data_salary[data_salary$Team == 'New Jersey Nets',]$Team <- 'NJN'
data_salary[data_salary$Team == 'New York Knicks',]$Team <- 'NYK'
data_salary[data_salary$Team == 'Oklahoma City Thunder',]$Team <- 'OKC'
data_salary[data_salary$Team == 'Orlando Magic',]$Team <- 'ORL'
data_salary[data_salary$Team == 'Philadelphia 76ers',]$Team <- 'PHI'
data_salary[data_salary$Team == 'Phoenix Suns',]$Team <- 'PHO'
data_salary[data_salary$Team == 'Portland Trailblazers',]$Team <- 'POR'
data_salary[data_salary$Team == 'Sacramento Kings',]$Team <- 'SAC'
data_salary[data_salary$Team == 'San Antonio Spurs',]$Team <- 'SAS'
data_salary[data_salary$Team == 'Seattle SuperSonics',]$Team <- 'SEA'
data_salary[data_salary$Team == 'Toronto Raptors',]$Team <- 'TOR'
data_salary[data_salary$Team == 'Utah Jazz',]$Team <- 'UTA'
data_salary[data_salary$Team == 'Vancouver Grizzlies',]$Team <- 'VAN'
data_salary[data_salary$Team == 'Washington Bullets',]$Team <- 'WSB'
data_salary[data_salary$Team == 'Washington Wizards',]$Team <- 'WAS'

# Special teams with changed names
# Charlotte Hornets 
# (1991-2002) : CHH - Charlotte Hornets
# (2005-2014) : CHA - Charlotte Bobcats
# (2015-2017) : CHO - Charlotte Hornets
for (i in 1991:2002){
    data_salary[(data_salary$Team == 'Charlotte Hornets') & (data_salary$Year == i) ,]$Team <- 'CHH'
}
for (i in 2005:2014){
    data_salary[(data_salary$Team == 'Charlotte Bobcats') & (data_salary$Year == i) ,]$Team <- 'CHA'
}
for (i in 2015:2017){
    data_salary[(data_salary$Team == 'Charlotte Hornets') & (data_salary$Year == i) ,]$Team <- 'CHO'
}

# New Orleans Hornets 
# (2003-2005) : NOH - New Orleans Hornets
# (2006-2007) : NOK - New Orleans Hornets
# (2008-2013) : NOH - New Orleans Hornets
# (2014-2017) : NOP - New Orleans Pelicans
for (i in 2003:2005){
    data_salary[(data_salary$Team == 'New Orleans Hornets') & (data_salary$Year == i) ,]$Team <- 'NOH'
}
for (i in 2006:2007){
    data_salary[(data_salary$Team == 'New Orleans Hornets') & (data_salary$Year == i) ,]$Team <- 'NOK'
}
for (i in 2008:2013){
    data_salary[(data_salary$Team == 'New Orleans Hornets') & (data_salary$Year == i) ,]$Team <- 'NOH'
}
for (i in 2014:2017){
    data_salary[(data_salary$Team == 'New Orleans Pelicans') & (data_salary$Year == i) ,]$Team <- 'NOP'
}

colnames(data_stat)[5] <- "Team"
head(data_stat)

# join in year, player and team
data_merged <- inner_join(data_stat, data_salary, by = c('Year','Player','Team'))

dim(data_merged)

summary(data_merged)

write.csv(data_merged, file = "MyData2.csv")

count = 0
for (i in 1:11008){
    for(j in 1:51){
        if(is.na(data_merged[i,j])){
                count = count +1
        }
    }
}
print(count)

count_2 = 0
for (i in 1:11008){
    for(j in 1:5){
        if(is.na(data_merged[i,j])){
                count_2 = count_2 +1
        }
    }
}
print(count_2)
# no missing values in Year, player, pos, age, team

# all missing values are numeric, so assign it to 0
for (i in 1:11008){
    for(j in 1:51){
        if(is.na(data_merged[i,j])){
                data_merged[i,j] = 0
        }
    }
}
# see the result
count_3 = 0
for (i in 1:11008){
    for(j in 1:51){
        if(is.na(data_merged[i,j])){
                count_3 = count_3 +1
        }
    }
}
print(count_3)

head(data_merged)

# converting salary unit to k
data_merged$Salary = data_merged$Salary / 1000
head(data_merged)

# trunc the salary
data_merged$Salary <- trunc(data_merged$Salary)
head(data_merged)

pl <- ggplot(data_merged,aes(x=Salary))+geom_histogram()
print(pl)

# shuffle the rows so that it won't be ordered in yaer
data_merged_shuffled <- data_merged[sample(1:nrow(data_merged)), ]
head(data_merged_shuffled)

head(data_merged)

# There are 2012 players in total since 1990
dim(distinct(data_merged,Player))

# First sample players with two or more seasons
player_names <- distinct(data_merged,Player)
player_names <- unlist(player_names, use.names=FALSE)
is.vector(player_names)
player_names

# choose players with 2 and over seasons
player_names_new <- c('')
for (i in player_names){
    if(nrow(data_merged[(data_merged$Player == i),]) >= 2){
        player_names_new <- append(player_names_new,data_merged[(data_merged$Player == i),]$Player)
    }
}

# done!
player_names_new <- player_names_new[-1]
player_names_new <- unique (player_names_new)
player_names_new
length(player_names_new)

player_names_new <- player_names_new[!is.na(player_names_new)]
length(player_names_new)
is.vector(player_names_new)

# 1615 players played 2 seasons or more in NBA
# calculate the average
total_sum = 0
for (x in player_names_new){
    # first get all the seaons he played
    year <- data_merged[(data_merged$Player == x),]$Year
    salary_0 <- data_merged[(data_merged$Player == x) & (data_merged$Year == year[1]),]$Salary
    salary_1 <- data_merged[(data_merged$Player == x) & (data_merged$Year == year[length(year)]),]$Salary
    avg_change = (salary_1 - salary_0) / (length(year)-1)
    avg_change = trunc(avg_change)
    total_sum = total_sum + avg_change[1]
}
total_avg = total_sum / 1615
print(total_avg)

df_averaging <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging)[1] <- "realSalary"
colnames(df_averaging)[2] <- "predictedSalary"
df_averaging

# start to predict by averaing all the past seasons
for (x in player_names_new){
    # first get all the seaons he played
    year <- data_merged[(data_merged$Player == x),]$Year
    for (i in 1:(length(year)-1)){
        realSalary = data_merged[(data_merged$Player == x) & (data_merged$Year == year[i+1]) ,]$Salary
        predicted_sum = 0

        for (j in 1:i){
           predicted_sum = predicted_sum + data_merged[(data_merged$Player == x) & (data_merged$Year == year[j]),]$Salary
        }
 
        predictedSalary = trunc(predicted_sum / i)

        # add into data frame
        observation <- c(realSalary, predictedSalary)
        df_averaging <- rbind (df_averaging,observation)
    }
}

dim(df_averaging)

# remove NAs
df_averaging_filtered <- df_averaging %>% drop_na()

dim(df_averaging_filtered)
df_averaging_filtered

# calculate mse and rmse
sum1 = 0
for(i in 1:nrow(df_averaging_filtered)){
    sum1 = sum1 + (df_averaging_filtered[i,1] - df_averaging_filtered[i,2]) * (df_averaging_filtered[i,1] - df_averaging_filtered[i,2])
}
mse1 = sum1 / nrow(df_averaging_filtered)
rmse1 <- sqrt(mse1)
mse1
rmse1

df_averaging_2_1991 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_1991)[1] <- "realSalary"
colnames(df_averaging_2_1991)[2] <- "predictedSalary"
df_averaging_2_1991

# 1991
df_averaging_2_1991 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_1991)[1] <- "realSalary"
colnames(df_averaging_2_1991)[2] <- "predictedSalary"  

stats_1991 <- subset(data_merged, Year == 1991)
stats_1991$index <- c(1:nrow(stats_1991))
player_names_1991 <- stats_1991$Player
for(x in player_names_1991){
    # get the position
    position <- stats_1991[(stats_1991$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_1991[(stats_1991$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_1991[stats_1991$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_1991_others <- player_names_1991[ - which(player_names_1991 %in% x)]
     for(y in player_names_1991_others){
         # get players with the same position
         if(stats_1991[stats_1991$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_1991[stats_1991$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_1991[stats_1991$Player == y,]$index,similarity,
                                stats_1991[stats_1991$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_1991 <- rbind(df_averaging_2_1991,Observation)
} 
colnames(df_averaging_2_1991)[1] <- "realSalary"
colnames(df_averaging_2_1991)[2] <- "predictedSalary"

dim(df_averaging_2_1991)   

# 1992
df_averaging_2_1992 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_1992)[1] <- "realSalary"
colnames(df_averaging_2_1992)[2] <- "predictedSalary"  

stats_1992 <- subset(data_merged, Year == 1992)
stats_1992$index <- c(1:nrow(stats_1992))
player_names_1992 <- stats_1992$Player
for(x in player_names_1992){
    # get the position
    position <- stats_1992[(stats_1992$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_1992[(stats_1992$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_1992[stats_1992$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_1992_others <- player_names_1992[ - which(player_names_1992 %in% x)]
     for(y in player_names_1992_others){
         # get players with the same position
         if(stats_1992[stats_1992$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_1992[stats_1992$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_1992[stats_1992$Player == y,]$index,similarity,
                                stats_1992[stats_1992$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_1992 <- rbind(df_averaging_2_1992,Observation)
} 
colnames(df_averaging_2_1992)[1] <- "realSalary"
colnames(df_averaging_2_1992)[2] <- "predictedSalary"
dim(df_averaging_2_1992)

# 1993
df_averaging_2_1993 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_1993)[1] <- "realSalary"
colnames(df_averaging_2_1993)[2] <- "predictedSalary"  

stats_1993 <- subset(data_merged, Year == 1993)
stats_1993$index <- c(1:nrow(stats_1993))
player_names_1993 <- stats_1993$Player
for(x in player_names_1993){
    # get the position
    position <- stats_1993[(stats_1993$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_1993[(stats_1993$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_1993[stats_1993$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_1993_others <- player_names_1993[ - which(player_names_1993 %in% x)]
     for(y in player_names_1993_others){
         # get players with the same position
         if(stats_1993[stats_1993$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_1993[stats_1993$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_1993[stats_1993$Player == y,]$index,similarity,
                                stats_1993[stats_1993$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_1993 <- rbind(df_averaging_2_1993,Observation)
} 
colnames(df_averaging_2_1993)[1] <- "realSalary"
colnames(df_averaging_2_1993)[2] <- "predictedSalary"
dim(df_averaging_2_1993)

# 1994
df_averaging_2_1994 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_1994)[1] <- "realSalary"
colnames(df_averaging_2_1994)[2] <- "predictedSalary"  

stats_1994 <- subset(data_merged, Year == 1994)
stats_1994$index <- c(1:nrow(stats_1994))
player_names_1994 <- stats_1994$Player
for(x in player_names_1994){
    # get the position
    position <- stats_1994[(stats_1994$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_1994[(stats_1994$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_1994[stats_1994$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_1994_others <- player_names_1994[ - which(player_names_1994 %in% x)]
     for(y in player_names_1994_others){
         # get players with the same position
         if(stats_1994[stats_1994$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_1994[stats_1994$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_1994[stats_1994$Player == y,]$index,similarity,
                                stats_1994[stats_1994$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_1994 <- rbind(df_averaging_2_1994,Observation)
} 
colnames(df_averaging_2_1994)[1] <- "realSalary"
colnames(df_averaging_2_1994)[2] <- "predictedSalary"
dim(df_averaging_2_1994)

# 1995
# here I noticed some player's name appear more than once, so need to remove the duplicate
df_averaging_2_1995 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_1995)[1] <- "realSalary"
colnames(df_averaging_2_1995)[2] <- "predictedSalary"  

stats_1995 <- subset(data_merged, Year == 1995)
stats_1995$index <- c(1:nrow(stats_1995))
player_names_1995 <- stats_1995$Player
stats_1995 <- stats_1995[!duplicated(stats_1995$Player),]

for(x in player_names_1995){
    # get the position
    position <- stats_1995[(stats_1995$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_1995[(stats_1995$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_1995[stats_1995$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_1995_others <- player_names_1995[ - which(player_names_1995 %in% x)]
     for(y in player_names_1995_others){
         # get players with the same position
         if(stats_1995[stats_1995$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_1995[stats_1995$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_1995[stats_1995$Player == y,]$index,similarity,
                                stats_1995[stats_1995$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_1995 <- rbind(df_averaging_2_1995,Observation)
} 
colnames(df_averaging_2_1995)[1] <- "realSalary"
colnames(df_averaging_2_1995)[2] <- "predictedSalary"
dim(df_averaging_2_1995)

# 1996
df_averaging_2_1996 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_1996)[1] <- "realSalary"
colnames(df_averaging_2_1996)[2] <- "predictedSalary"  

stats_1996 <- subset(data_merged, Year == 1996)
stats_1996$index <- c(1:nrow(stats_1996))
player_names_1996 <- stats_1996$Player
stats_1996 <- stats_1996[!duplicated(stats_1996$Player),]

for(x in player_names_1996){
    # get the position
    position <- stats_1996[(stats_1996$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_1996[(stats_1996$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_1996[stats_1996$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_1996_others <- player_names_1996[ - which(player_names_1996 %in% x)]
     for(y in player_names_1996_others){
         # get players with the same position
         if(stats_1996[stats_1996$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_1996[stats_1996$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_1996[stats_1996$Player == y,]$index,similarity,
                                stats_1996[stats_1996$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_1996 <- rbind(df_averaging_2_1996,Observation)
} 
colnames(df_averaging_2_1996)[1] <- "realSalary"
colnames(df_averaging_2_1996)[2] <- "predictedSalary"
dim(df_averaging_2_1996)

# 1997
df_averaging_2_1997 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_1997)[1] <- "realSalary"
colnames(df_averaging_2_1997)[2] <- "predictedSalary"  

stats_1997 <- subset(data_merged, Year == 1997)
stats_1997$index <- c(1:nrow(stats_1997))
player_names_1997 <- stats_1997$Player
stats_1997 <- stats_1997[!duplicated(stats_1997$Player),]

for(x in player_names_1997){
    # get the position
    position <- stats_1997[(stats_1997$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_1997[(stats_1997$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_1997[stats_1997$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_1997_others <- player_names_1997[ - which(player_names_1997 %in% x)]
     for(y in player_names_1997_others){
         # get players with the same position
         if(stats_1997[stats_1997$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_1997[stats_1997$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_1997[stats_1997$Player == y,]$index,similarity,
                                stats_1997[stats_1997$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_1997 <- rbind(df_averaging_2_1997,Observation)
} 
colnames(df_averaging_2_1997)[1] <- "realSalary"
colnames(df_averaging_2_1997)[2] <- "predictedSalary"
dim(df_averaging_2_1997)

# 1998
df_averaging_2_1998 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_1998)[1] <- "realSalary"
colnames(df_averaging_2_1998)[2] <- "predictedSalary"  

stats_1998 <- subset(data_merged, Year == 1998)
stats_1998$index <- c(1:nrow(stats_1998))
player_names_1998 <- stats_1998$Player
for(x in player_names_1998){
    # get the position
    position <- stats_1998[(stats_1998$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_1998[(stats_1998$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_1998[stats_1998$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_1998_others <- player_names_1998[ - which(player_names_1998 %in% x)]
     for(y in player_names_1998_others){
         # get players with the same position
         if(stats_1998[stats_1998$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_1998[stats_1998$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_1998[stats_1998$Player == y,]$index,similarity,
                                stats_1998[stats_1998$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_1998 <- rbind(df_averaging_2_1998,Observation)
} 
colnames(df_averaging_2_1998)[1] <- "realSalary"
colnames(df_averaging_2_1998)[2] <- "predictedSalary"
dim(df_averaging_2_1998)

# 1999
df_averaging_2_1999 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_1999)[1] <- "realSalary"
colnames(df_averaging_2_1999)[2] <- "predictedSalary"  

stats_1999 <- subset(data_merged, Year == 1999)
stats_1999$index <- c(1:nrow(stats_1999))
player_names_1999 <- stats_1999$Player
stats_1999 <- stats_1999[!duplicated(stats_1999$Player),]

for(x in player_names_1999){
    # get the position
    position <- stats_1999[(stats_1999$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_1999[(stats_1999$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_1999[stats_1999$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_1999_others <- player_names_1999[ - which(player_names_1999 %in% x)]
     for(y in player_names_1999_others){
         # get players with the same position
         if(stats_1999[stats_1999$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_1999[stats_1999$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_1999[stats_1999$Player == y,]$index,similarity,
                                stats_1999[stats_1999$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_1999 <- rbind(df_averaging_2_1999,Observation)
} 
colnames(df_averaging_2_1999)[1] <- "realSalary"
colnames(df_averaging_2_1999)[2] <- "predictedSalary"
dim(df_averaging_2_1999)

# 2000
df_averaging_2_2000 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2000)[1] <- "realSalary"
colnames(df_averaging_2_2000)[2] <- "predictedSalary"  

stats_2000 <- subset(data_merged, Year == 2000)
stats_2000$index <- c(1:nrow(stats_2000))
player_names_2000 <- stats_2000$Player
stats_2000 <- stats_2000[!duplicated(stats_2000$Player),]

for(x in player_names_2000){
    # get the position
    position <- stats_2000[(stats_2000$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2000[(stats_2000$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2000[stats_2000$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2000_others <- player_names_2000[ - which(player_names_2000 %in% x)]
     for(y in player_names_2000_others){
         # get players with the same position
         if(stats_2000[stats_2000$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2000[stats_2000$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2000[stats_2000$Player == y,]$index,similarity,
                                stats_2000[stats_2000$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2000 <- rbind(df_averaging_2_2000,Observation)
} 
colnames(df_averaging_2_2000)[1] <- "realSalary"
colnames(df_averaging_2_2000)[2] <- "predictedSalary"
dim(df_averaging_2_2000)

# 2001
df_averaging_2_2001 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2001)[1] <- "realSalary"
colnames(df_averaging_2_2001)[2] <- "predictedSalary"  

stats_2001 <- subset(data_merged, Year == 2001)
stats_2001$index <- c(1:nrow(stats_2001))
player_names_2001 <- stats_2001$Player
stats_2001 <- stats_2001[!duplicated(stats_2001$Player),]

for(x in player_names_2001){
    # get the position
    position <- stats_2001[(stats_2001$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2001[(stats_2001$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2001[stats_2001$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2001_others <- player_names_2001[ - which(player_names_2001 %in% x)]
     for(y in player_names_2001_others){
         # get players with the same position
         if(stats_2001[stats_2001$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2001[stats_2001$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2001[stats_2001$Player == y,]$index,similarity,
                                stats_2001[stats_2001$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2001 <- rbind(df_averaging_2_2001,Observation)
} 
colnames(df_averaging_2_2001)[1] <- "realSalary"
colnames(df_averaging_2_2001)[2] <- "predictedSalary"
dim(df_averaging_2_2001)

# 2002
df_averaging_2_2002 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2002)[1] <- "realSalary"
colnames(df_averaging_2_2002)[2] <- "predictedSalary"  

stats_2002 <- subset(data_merged, Year == 2002)
stats_2002$index <- c(1:nrow(stats_2002))
player_names_2002 <- stats_2002$Player
stats_2002 <- stats_2002[!duplicated(stats_2002$Player),]

for(x in player_names_2002){
    # get the position
    position <- stats_2002[(stats_2002$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2002[(stats_2002$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2002[stats_2002$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2002_others <- player_names_2002[ - which(player_names_2002 %in% x)]
     for(y in player_names_2002_others){
         # get players with the same position
         if(stats_2002[stats_2002$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2002[stats_2002$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2002[stats_2002$Player == y,]$index,similarity,
                                stats_2002[stats_2002$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2002 <- rbind(df_averaging_2_2002,Observation)
} 
colnames(df_averaging_2_2002)[1] <- "realSalary"
colnames(df_averaging_2_2002)[2] <- "predictedSalary"
dim(df_averaging_2_2002)

# 2003
df_averaging_2_2003 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2003)[1] <- "realSalary"
colnames(df_averaging_2_2003)[2] <- "predictedSalary"  

stats_2003 <- subset(data_merged, Year == 2003)
stats_2003$index <- c(1:nrow(stats_2003))
player_names_2003 <- stats_2003$Player
stats_2003 <- stats_2003[!duplicated(stats_2003$Player),]

for(x in player_names_2003){
    # get the position
    position <- stats_2003[(stats_2003$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2003[(stats_2003$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2003[stats_2003$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2003_others <- player_names_2003[ - which(player_names_2003 %in% x)]
     for(y in player_names_2003_others){
         # get players with the same position
         if(stats_2003[stats_2003$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2003[stats_2003$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2003[stats_2003$Player == y,]$index,similarity,
                                stats_2003[stats_2003$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2003 <- rbind(df_averaging_2_2003,Observation)
} 
colnames(df_averaging_2_2003)[1] <- "realSalary"
colnames(df_averaging_2_2003)[2] <- "predictedSalary"
dim(df_averaging_2_2003)

# 2004
df_averaging_2_2004 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2004)[1] <- "realSalary"
colnames(df_averaging_2_2004)[2] <- "predictedSalary"  

stats_2004 <- subset(data_merged, Year == 2004)
stats_2004$index <- c(1:nrow(stats_2004))
player_names_2004 <- stats_2004$Player
stats_2004 <- stats_2004[!duplicated(stats_2004$Player),]

for(x in player_names_2004){
    # get the position
    position <- stats_2004[(stats_2004$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2004[(stats_2004$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2004[stats_2004$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2004_others <- player_names_2004[ - which(player_names_2004 %in% x)]
     for(y in player_names_2004_others){
         # get players with the same position
         if(stats_2004[stats_2004$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2004[stats_2004$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2004[stats_2004$Player == y,]$index,similarity,
                                stats_2004[stats_2004$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2004 <- rbind(df_averaging_2_2004,Observation)
} 
colnames(df_averaging_2_2004)[1] <- "realSalary"
colnames(df_averaging_2_2004)[2] <- "predictedSalary"
dim(df_averaging_2_2004)

# 2005
df_averaging_2_2005 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2005)[1] <- "realSalary"
colnames(df_averaging_2_2005)[2] <- "predictedSalary"  

stats_2005 <- subset(data_merged, Year == 2005)
stats_2005$index <- c(1:nrow(stats_2005))
player_names_2005 <- stats_2005$Player
stats_2005 <- stats_2005[!duplicated(stats_2005$Player),]

for(x in player_names_2005){
    # get the position
    position <- stats_2005[(stats_2005$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2005[(stats_2005$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2005[stats_2005$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2005_others <- player_names_2005[ - which(player_names_2005 %in% x)]
     for(y in player_names_2005_others){
         # get players with the same position
         if(stats_2005[stats_2005$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2005[stats_2005$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2005[stats_2005$Player == y,]$index,similarity,
                                stats_2005[stats_2005$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2005 <- rbind(df_averaging_2_2005,Observation)
} 
colnames(df_averaging_2_2005)[1] <- "realSalary"
colnames(df_averaging_2_2005)[2] <- "predictedSalary"
dim(df_averaging_2_2005)

# 2006
df_averaging_2_2006 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2006)[1] <- "realSalary"
colnames(df_averaging_2_2006)[2] <- "predictedSalary"  

stats_2006 <- subset(data_merged, Year == 2006)
stats_2006$index <- c(1:nrow(stats_2006))
player_names_2006 <- stats_2006$Player
stats_2006 <- stats_2006[!duplicated(stats_2006$Player),]

for(x in player_names_2006){
    # get the position
    position <- stats_2006[(stats_2006$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2006[(stats_2006$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2006[stats_2006$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2006_others <- player_names_2006[ - which(player_names_2006 %in% x)]
     for(y in player_names_2006_others){
         # get players with the same position
         if(stats_2006[stats_2006$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2006[stats_2006$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2006[stats_2006$Player == y,]$index,similarity,
                                stats_2006[stats_2006$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2006 <- rbind(df_averaging_2_2006,Observation)
} 
colnames(df_averaging_2_2006)[1] <- "realSalary"
colnames(df_averaging_2_2006)[2] <- "predictedSalary"
dim(df_averaging_2_2006)

# 2007
df_averaging_2_2007 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2007)[1] <- "realSalary"
colnames(df_averaging_2_2007)[2] <- "predictedSalary"  

stats_2007 <- subset(data_merged, Year == 2007)
stats_2007$index <- c(1:nrow(stats_2007))
player_names_2007 <- stats_2007$Player
stats_2007 <- stats_2007[!duplicated(stats_2007$Player),]

for(x in player_names_2007){
    # get the position
    position <- stats_2007[(stats_2007$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2007[(stats_2007$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2007[stats_2007$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2007_others <- player_names_2007[ - which(player_names_2007 %in% x)]
     for(y in player_names_2007_others){
         # get players with the same position
         if(stats_2007[stats_2007$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2007[stats_2007$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2007[stats_2007$Player == y,]$index,similarity,
                                stats_2007[stats_2007$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2007 <- rbind(df_averaging_2_2007,Observation)
} 
colnames(df_averaging_2_2007)[1] <- "realSalary"
colnames(df_averaging_2_2007)[2] <- "predictedSalary"
dim(df_averaging_2_2007)

# 2008
df_averaging_2_2008 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2008)[1] <- "realSalary"
colnames(df_averaging_2_2008)[2] <- "predictedSalary"  

stats_2008 <- subset(data_merged, Year == 2008)
stats_2008$index <- c(1:nrow(stats_2008))
player_names_2008 <- stats_2008$Player
stats_2008 <- stats_2008[!duplicated(stats_2008$Player),]

for(x in player_names_2008){
    # get the position
    position <- stats_2008[(stats_2008$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2008[(stats_2008$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2008[stats_2008$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2008_others <- player_names_2008[ - which(player_names_2008 %in% x)]
     for(y in player_names_2008_others){
         # get players with the same position
         if(stats_2008[stats_2008$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2008[stats_2008$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2008[stats_2008$Player == y,]$index,similarity,
                                stats_2008[stats_2008$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2008 <- rbind(df_averaging_2_2008,Observation)
} 
colnames(df_averaging_2_2008)[1] <- "realSalary"
colnames(df_averaging_2_2008)[2] <- "predictedSalary"
dim(df_averaging_2_2008)

# 2009
df_averaging_2_2009 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2009)[1] <- "realSalary"
colnames(df_averaging_2_2009)[2] <- "predictedSalary"  

stats_2009 <- subset(data_merged, Year == 2009)
stats_2009$index <- c(1:nrow(stats_2009))
player_names_2009 <- stats_2009$Player
stats_2009 <- stats_2009[!duplicated(stats_2009$Player),]

for(x in player_names_2009){
    # get the position
    position <- stats_2009[(stats_2009$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2009[(stats_2009$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2009[stats_2009$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2009_others <- player_names_2009[ - which(player_names_2009 %in% x)]
     for(y in player_names_2009_others){
         # get players with the same position
         if(stats_2009[stats_2009$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2009[stats_2009$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2009[stats_2009$Player == y,]$index,similarity,
                                stats_2009[stats_2009$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2009 <- rbind(df_averaging_2_2009,Observation)
} 
colnames(df_averaging_2_2009)[1] <- "realSalary"
colnames(df_averaging_2_2009)[2] <- "predictedSalary"
dim(df_averaging_2_2009)

# 2010
df_averaging_2_2010 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2010)[1] <- "realSalary"
colnames(df_averaging_2_2010)[2] <- "predictedSalary"  

stats_2010 <- subset(data_merged, Year == 2010)
stats_2010$index <- c(1:nrow(stats_2010))
player_names_2010 <- stats_2010$Player
stats_2010 <- stats_2010[!duplicated(stats_2010$Player),]

for(x in player_names_2010){
    # get the position
    position <- stats_2010[(stats_2010$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2010[(stats_2010$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2010[stats_2010$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2010_others <- player_names_2010[ - which(player_names_2010 %in% x)]
     for(y in player_names_2010_others){
         # get players with the same position
         if(stats_2010[stats_2010$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2010[stats_2010$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2010[stats_2010$Player == y,]$index,similarity,
                                stats_2010[stats_2010$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2010 <- rbind(df_averaging_2_2010,Observation)
} 
colnames(df_averaging_2_2010)[1] <- "realSalary"
colnames(df_averaging_2_2010)[2] <- "predictedSalary"
dim(df_averaging_2_2010)

# 2011
df_averaging_2_2011 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2011)[1] <- "realSalary"
colnames(df_averaging_2_2011)[2] <- "predictedSalary"  

stats_2011 <- subset(data_merged, Year == 2011)
stats_2011$index <- c(1:nrow(stats_2011))
player_names_2011 <- stats_2011$Player
stats_2011 <- stats_2011[!duplicated(stats_2011$Player),]

for(x in player_names_2011){
    # get the position
    position <- stats_2011[(stats_2011$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2011[(stats_2011$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2011[stats_2011$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2011_others <- player_names_2011[ - which(player_names_2011 %in% x)]
     for(y in player_names_2011_others){
         # get players with the same position
         if(stats_2011[stats_2011$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2011[stats_2011$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2011[stats_2011$Player == y,]$index,similarity,
                                stats_2011[stats_2011$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2011 <- rbind(df_averaging_2_2011,Observation)
} 
colnames(df_averaging_2_2011)[1] <- "realSalary"
colnames(df_averaging_2_2011)[2] <- "predictedSalary"
dim(df_averaging_2_2011)

# 2012
df_averaging_2_2012 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2012)[1] <- "realSalary"
colnames(df_averaging_2_2012)[2] <- "predictedSalary"  

stats_2012 <- subset(data_merged, Year == 2012)
stats_2012$index <- c(1:nrow(stats_2012))
player_names_2012 <- stats_2012$Player
stats_2012 <- stats_2012[!duplicated(stats_2012$Player),]

for(x in player_names_2012){
    # get the position
    position <- stats_2012[(stats_2012$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2012[(stats_2012$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2012[stats_2012$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2012_others <- player_names_2012[ - which(player_names_2012 %in% x)]
     for(y in player_names_2012_others){
         # get players with the same position
         if(stats_2012[stats_2012$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2012[stats_2012$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2012[stats_2012$Player == y,]$index,similarity,
                                stats_2012[stats_2012$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2012 <- rbind(df_averaging_2_2012,Observation)
} 
colnames(df_averaging_2_2012)[1] <- "realSalary"
colnames(df_averaging_2_2012)[2] <- "predictedSalary"
dim(df_averaging_2_2012)

# 2013
df_averaging_2_2013 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2013)[1] <- "realSalary"
colnames(df_averaging_2_2013)[2] <- "predictedSalary"  

stats_2013 <- subset(data_merged, Year == 2013)
stats_2013$index <- c(1:nrow(stats_2013))
player_names_2013 <- stats_2013$Player
stats_2013 <- stats_2013[!duplicated(stats_2013$Player),]

for(x in player_names_2013){
    # get the position
    position <- stats_2013[(stats_2013$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2013[(stats_2013$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2013[stats_2013$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2013_others <- player_names_2013[ - which(player_names_2013 %in% x)]
     for(y in player_names_2013_others){
         # get players with the same position
         if(stats_2013[stats_2013$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2013[stats_2013$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2013[stats_2013$Player == y,]$index,similarity,
                                stats_2013[stats_2013$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2013 <- rbind(df_averaging_2_2013,Observation)
} 
colnames(df_averaging_2_2013)[1] <- "realSalary"
colnames(df_averaging_2_2013)[2] <- "predictedSalary"
dim(df_averaging_2_2013)

# 2014
df_averaging_2_2014 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2014)[1] <- "realSalary"
colnames(df_averaging_2_2014)[2] <- "predictedSalary"  

stats_2014 <- subset(data_merged, Year == 2014)
stats_2014$index <- c(1:nrow(stats_2014))
player_names_2014 <- stats_2014$Player
stats_2014 <- stats_2014[!duplicated(stats_2014$Player),]

for(x in player_names_2014){
    # get the position
    position <- stats_2014[(stats_2014$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2014[(stats_2014$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2014[stats_2014$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2014_others <- player_names_2014[ - which(player_names_2014 %in% x)]
     for(y in player_names_2014_others){
         # get players with the same position
         if(stats_2014[stats_2014$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2014[stats_2014$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2014[stats_2014$Player == y,]$index,similarity,
                                stats_2014[stats_2014$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2014 <- rbind(df_averaging_2_2014,Observation)
} 
colnames(df_averaging_2_2014)[1] <- "realSalary"
colnames(df_averaging_2_2014)[2] <- "predictedSalary"
dim(df_averaging_2_2014)

# 2015
df_averaging_2_2015 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2015)[1] <- "realSalary"
colnames(df_averaging_2_2015)[2] <- "predictedSalary"  

stats_2015 <- subset(data_merged, Year == 2015)
stats_2015$index <- c(1:nrow(stats_2015))
player_names_2015 <- stats_2015$Player
stats_2015 <- stats_2015[!duplicated(stats_2015$Player),]

for(x in player_names_2015){
    # get the position
    position <- stats_2015[(stats_2015$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2015[(stats_2015$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2015[stats_2015$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2015_others <- player_names_2015[ - which(player_names_2015 %in% x)]
     for(y in player_names_2015_others){
         # get players with the same position
         if(stats_2015[stats_2015$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2015[stats_2015$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2015[stats_2015$Player == y,]$index,similarity,
                                stats_2015[stats_2015$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2015 <- rbind(df_averaging_2_2015,Observation)
} 
colnames(df_averaging_2_2015)[1] <- "realSalary"
colnames(df_averaging_2_2015)[2] <- "predictedSalary"
dim(df_averaging_2_2015)

# 2016
df_averaging_2_2016 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2016)[1] <- "realSalary"
colnames(df_averaging_2_2016)[2] <- "predictedSalary"  

stats_2016 <- subset(data_merged, Year == 2016)
stats_2016$index <- c(1:nrow(stats_2016))
player_names_2016 <- stats_2016$Player
stats_2016 <- stats_2016[!duplicated(stats_2016$Player),]

for(x in player_names_2016){
    # get the position
    position <- stats_2016[(stats_2016$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2016[(stats_2016$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2016[stats_2016$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2016_others <- player_names_2016[ - which(player_names_2016 %in% x)]
     for(y in player_names_2016_others){
         # get players with the same position
         if(stats_2016[stats_2016$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2016[stats_2016$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2016[stats_2016$Player == y,]$index,similarity,
                                stats_2016[stats_2016$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2016 <- rbind(df_averaging_2_2016,Observation)
} 
colnames(df_averaging_2_2016)[1] <- "realSalary"
colnames(df_averaging_2_2016)[2] <- "predictedSalary"
dim(df_averaging_2_2016)

# 2017
df_averaging_2_2017 <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_averaging_2_2017)[1] <- "realSalary"
colnames(df_averaging_2_2017)[2] <- "predictedSalary"  

stats_2017 <- subset(data_merged, Year == 2017)
stats_2017$index <- c(1:nrow(stats_2017))
player_names_2017 <- stats_2017$Player
stats_2017 <- stats_2017[!duplicated(stats_2017$Player),]

for(x in player_names_2017){
    # get the position
    position <- stats_2017[(stats_2017$Player == x),]$Pos
    
    # write real salary
    realSalary <- stats_2017[(stats_2017$Player == x),]$Salary
    
    # computing similarities
    # creating a data frame to store similarity
    df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    
    this_stat <- as.vector(stats_2017[stats_2017$Player == x,6:50])
    this_stat <- unlist(this_stat, use.names=FALSE)
    
    # get other players
    player_names_2017_others <- player_names_2017[ - which(player_names_2017 %in% x)]
     for(y in player_names_2017_others){
         # get players with the same position
         if(stats_2017[stats_2017$Player == y,]$Pos == position){
                other_stat <- as.vector(stats_2017[stats_2017$Player == y,6:50])
                other_stat <- unlist(other_stat, use.names=FALSE)
                 
                # computing
                similarity <- cosine(this_stat,other_stat)
                # write into data frame
                observation <- c(stats_2017[stats_2017$Player == y,]$index,similarity,
                                stats_2017[stats_2017$Player == y,]$Salary)
                df_similarity <- rbind(df_similarity,observation)
         }
    }
   
    # write predicted salary
    # sort
    colnames(df_similarity)[1] <- "row index"
    colnames(df_similarity)[2] <- "similarity"
    colnames(df_similarity)[3] <- "salary"
    df_similarity_sorted <- arrange(df_similarity,desc(similarity))
    df_similarity_top <- df_similarity_sorted[1:10,]
    predictedSalary <- trunc(sum(df_similarity_top$salary)/10)
    
    Observation <- c(realSalary,predictedSalary)
    df_averaging_2_2017 <- rbind(df_averaging_2_2017,Observation)
} 
colnames(df_averaging_2_2017)[1] <- "realSalary"
colnames(df_averaging_2_2017)[2] <- "predictedSalary"
dim(df_averaging_2_2017)

data_averaging_2 <- rbind(df_averaging_2_1991,df_averaging_2_1992)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_1993)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_1994)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_1995)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_1996)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_1997)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_1998)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_1999)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2000)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2001)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2002)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2003)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2004)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2005)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2006)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2007)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2008)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2009)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2010)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2011)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2012)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2013)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2014)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2015)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2016)
data_averaging_2 <- rbind(data_averaging_2,df_averaging_2_2017)
dim(data_averaging_2)

data_averaging_2

# calculate MSE and RMSE
sum2 = 0
for(i in 1:nrow(data_averaging_2)){
    sum2 = sum2 + (data_averaging_2[i,1] - data_averaging_2[i,2]) * (data_averaging_2[i,1] - data_averaging_2[i,2])
}
mse2 = sum2 / nrow(data_averaging_2)
rmse2 <- sqrt(mse2)
mse2
rmse2

dim(data_merged_shuffled)
head(data_merged_shuffled)
colnames(data_merged_shuffled)

data_merged_shuffled <- data_merged_shuffled[,-c(1,2,3,5)]
head(data_merged_shuffled)

set.seed(111)
trainingRowIndex <- sample(1:nrow(data_merged_shuffled), 0.8*nrow(data_merged_shuffled)) 
trainingData <- data_merged_shuffled[trainingRowIndex, ] 
testData <- data_merged_shuffled[-trainingRowIndex, ]

m1 <- lm(Salary ~. -TRB. -WS -WS.48 -BPM -TRB -AST -STL -BLK -TOV , data = trainingData )
summary(m1)

# variable selection based on AIC
m2 <- step(m1)

summary(m2)

distPred <- trunc(predict(m2, testData))
actuals_preds <- data.frame(cbind(actuals=testData$Salary, predicteds=distPred))
head(actuals_preds)

# calculate MSE and RMSE
sum3 = 0
for(i in 1:nrow(actuals_preds)){
    sum3 = sum3 + (actuals_preds[i,1] - actuals_preds[i,2]) * (actuals_preds[i,1] - actuals_preds[i,2])
}
mse3 = sum3 / nrow(actuals_preds)
rmse3 <- sqrt(mse3)
mse3
rmse3

# check the distribution of residuals
qqnorm(residuals(m2),ylab="Residuals",main="") 
qqline(residuals(m2))

set.seed(222)
trainingRowIndex <- sample(1:nrow(data_merged_shuffled), 0.8*nrow(data_merged_shuffled)) 
trainingData_2 <- data_merged_shuffled[trainingRowIndex, ] 
testData_2 <- data_merged_shuffled[-trainingRowIndex, ]

r1 <- rpart(Salary~. , data = trainingData_2)

rpart.plot(r1)

summary(r1)

p.r1 <- predict(r1,testData_2)

actuals_preds_2 <- data.frame(cbind(actuals=testData$Salary, predicteds=p.r1))
head(actuals_preds_2)

# calculate MSE and RMSE
sum4 = 0
for(i in 1:nrow(actuals_preds_2)){
    sum4 = sum4 + (actuals_preds_2[i,1] - actuals_preds_2[i,2]) * (actuals_preds_2[i,1] - actuals_preds_2[i,2])
}
mse4 = sum4 / nrow(actuals_preds_2)
rmse4 <- sqrt(mse4)
mse4
rmse4

r2 <- rpart(Salary ~. -TRB. -WS -WS.48 -BPM -TRB -AST -STL -BLK -TOV -X3PAr -FTr -AST.
         -BLK. -DWS -VORP -X3P -X3PA -X2P -X2PA -eFG. -FT -FTA -PTS , data = trainingData_2)
p.r2 <- predict(r2,testData_2)
actuals_preds_3 <- data.frame(cbind(actuals=testData$Salary, predicteds=p.r2))
head(actuals_preds_3)

# calculate MSE and RMSE
sum5 = 0
for(i in 1:nrow(actuals_preds_3)){
    sum5 = sum5 + (actuals_preds_3[i,1] - actuals_preds_3[i,2]) * (actuals_preds_3[i,1] - actuals_preds_3[i,2])
}
mse5 = sum5 / nrow(actuals_preds_3)
rmse5 <- sqrt(mse5)
mse5
rmse5
