rm(list=ls())

# Problem 1

for (i in 1:7){
  print(i^3)
}


# Problem 2
set.seed(14)
for(i in 1:1000){
  dice <- sample(1:6,size=2,replace=TRUE)
  print(dice)
  rollResult <- sum(dice)
  if(i ==1 & rollResult > 7){
    print ("avg number of dice cast per game is 1") 
    break
  } else if (i!=1 & (dice[1]==2 | dice[2]==2 | dice[1]==6 | dice[2]==6)){
    print (paste("avg number of dice cast per game is", i))
    break}
  
  
}

#problem3
p3ds <-read.csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")

vote.choice <- function(x){
  if(x=="Trump"){
    trumpVotes <- length(p3ds$pres16[p3ds$pres16==x])
    return(trumpVotes)
  }
  if(x=="Clinton"){
    cliVotes <- length(p3ds$pres16[p3ds$pres16==x])
    return(cliVotes)
  }else if(x=="Other"){
    otherVotes <- length(p3ds$pres16[p3ds$pres16!="Trump" && p3ds$pres16[p3ds$pres16!="Clinton"]])
    return(otherVotes)
  }else{
    print("Please enter either 'Trump' 'Clinton' or 'Other' into the function to return a valid response")
    }
}
vote.choice("Clinton")
vote.choice("Trump")
vote.choice("Danny Kim")


# problem 4

## Install the library
install.packages('fivethirtyeight')
library(fivethirtyeight)


## call cabinent_turnover

## Create a dataframe to make it easier to calculate days


temp<-cabinet_turnover
tempP4 <- subset(temp)
appoint <- function(x){
  President <- c("Carter","Reagan","Bush 41","Clinton","Bush 43","Obama","Trump")
  term1 <- 1461
  term2 <- 2922
  termTrump <- 1105
  pplMean <- as.numeric(mean(tempP4$days[tempP4$president==x],na.rm=T))
  ## I added na.rm, because there is a missing value and if I dont put it, I cannot get my Reagan and Trump data to work
  if (x == "Bush 41" | x == "Carter"){
      return(pplMean/term1)
    } else if (x == "Obama" | x == "Clinton" | x == "Bush 43" | x == "Reagan") {
      return(pplMean/term2)
    } else if (x == "Trump") {
      return(pplMean/termTrump)
    } 
      }

appoint("Reagan")
#.7327033
appoint("Carter")
# .8678


# problem 5
 
tempP5 <- congress_age
congress <- congress_age$congress
age <- congress_age$age
state<- congress_age$state
## return the average age of congressmemebers for each congressional era
## congress -> 113 
congress_stats <- function(x) {
  if (x == "congress") {
    # For creating a list of congress 
    era <- unique(congress)
    avgAge <- c(NULL)
    for(i in era) {
      ## na.rm <- as mentioned in the previous problem
      mean1 <- mean(age[congress==i],na.rm= T)
      avgAgeEra <- c(avgAge, mean1)
    }
    return(data.frame(avgAgeEra, era))
  } else if (x == "state") {
    ## For creating a list of state 
    state <- unique(state)
    ## avgAgeS for avoiding a collision
    avgAgeS <- c(NULL)
    for(i in state) {
      mean2 <- mean(age[state==i],na.rm= T)
      avgAgeState <- c(avgAgeS, mean2)
    }
    return(data.frame(avgAgeState, state))
  } else{
    print("Type congress or state")  }
}

#test to see if it works
congress_stats("congress")
congress_stats("state")
congress_stats("Danny Kim")



