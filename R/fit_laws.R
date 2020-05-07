### Load packages #############################################################

  library(tidyverse)
  library(HMDHFDplus)
  library(MortalityLaws)


### Get death counts for some examples ########################################

  # From Github
  github1 <- "https://raw.githubusercontent.com/timriffe/covid_age/master/Data/"
  github2 <- "Output_10.csv"
  github <- paste0(github1,github2)
  deaths <- read_csv(github,skip=1)
  
  # Select example countries and dates
  countrylist <- c("Germany","Italy","SouthKorea")
  codes <- c("KR30.04.2020","ITbol02.04.2020","DE_26.04.2020")
  
  deaths <- deaths %>% 
              filter(Sex=="m") %>%
              filter(Country%in%countrylist) %>%
              filter(Region =="All") %>%
              filter(Code%in%codes) %>%
              select(Country,Date,Age,Deaths)
  

### Get population counts #####################################################

  # Load from HMD
  DEpop <- readHMDweb("DEUTNP", "Population",us,pw)
  SKpop <- readHMDweb("KOR", "Population",us,pw)
  ITpop <- readHMDweb("ITA", "Population",us,pw)
  
  # Restrict to last year
  DEpop <- DEpop %>% filter(Year==max(DEpop$Year)) %>% select(Age,Male2)
  SKpop <- SKpop %>% filter(Year==max(SKpop$Year)) %>% select(Age,Male2)
  ITpop <- ITpop %>% filter(Year==max(ITpop$Year)) %>% select(Age,Male2)
  
  # Recode age
  values <- seq(0,110,by=10)
  for(value in values) {
    DEpop$Age[DEpop$Age%in%value:(value+9)] <- value
    SKpop$Age[SKpop$Age%in%value:(value+9)] <- value
    ITpop$Age[ITpop$Age%in%value:(value+9)] <- value
  }
  
  # Aggregate age
  DEpop <- DEpop %>% group_by(Age) %>%
    summarize(Male = sum(Male2)) %>% filter(Age<110)
  
  SKpop <- SKpop %>% group_by(Age) %>%
    summarize(Male = sum(Male2)) %>% filter(Age<110)
  
  ITpop <- ITpop %>% group_by(Age) %>%
    summarize(Male = sum(Male2)) %>% filter(Age<110)
  
  # Add country variable
  DEpop <- DEpop %>% mutate(Country="Germany")
  SKpop <- SKpop %>% mutate(Country="SouthKorea")
  ITpop <- ITpop %>% mutate(Country="Italy")
  
  # Merge population counts
  pop <- rbind(DEpop,SKpop,ITpop)

  # Merge with deaths
  data <- inner_join(deaths,pop)
  
  # For fitting: Zero deaths problematic, replace with 1s
  data <- data %>% mutate(Deaths=ifelse(Deaths<1,1,Deaths))
  
  
### Quick plot ################################################################
  
  data <- data %>% mutate(Rate = Deaths/Male)
   
  ggplot(data,aes(x=Age,y=log(Rate),color=Country)) + 
    geom_line() + geom_point()
  
  
### Fit Gompertz/Makeham mortality laws #######################################
  
  # Italy
  ITdata <- data %>% filter(Country=="Italy")
  ITfit1 <- MortalityLaw(x=ITdata$Age,Dx=ITdata$Deaths,Ex=ITdata$Male,law="gompertz")
  ITfit2 <- MortalityLaw(x=ITdata$Age,Dx=ITdata$Deaths,Ex=ITdata$Male,law="makeham")

  # Germany
  DEdata <- data %>% filter(Country=="Germany")
  DEfit1 <- MortalityLaw(x=DEdata$Age,Dx=DEdata$Deaths,Ex=DEdata$Male,law="gompertz")
  DEfit2 <- MortalityLaw(x=DEdata$Age,Dx=DEdata$Deaths,Ex=DEdata$Male,law="makeham")

  # South Korea
  SKdata <- data %>% filter(Country=="SouthKorea")
  SKfit1 <- MortalityLaw(x=SKdata$Age,Dx=SKdata$Deaths,Ex=SKdata$Male,law="gompertz")
  SKfit2 <- MortalityLaw(x=SKdata$Age,Dx=SKdata$Deaths,Ex=SKdata$Male,law="makeham")
  
  
### Compare results ###########################################################
  
  # Gompertz
  coef(ITfit1)
  coef(DEfit1)
  coef(SKfit1)
  
  # Makeham
  coef(ITfit2)
  coef(DEfit2)
  coef(SKfit2) 

  # Look at examples
  plot(DEfit1)  
  plot(DEfit2)  
  