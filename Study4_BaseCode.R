#Load required packages
library(tidyverse)
library(arules)
library(readxl)
library(reshape)
library(reshape2)
library(kableExtra)
library(formattable)
library(ggpubr)
library(cowplot)

###Match Simulation

#load dataset
Simulation_Data <- read.csv("Data/training_database.csv", na.strings=c("","NA"))

#Select & Filter for rows which have either kick/handball recorded
Simulation_Data_filter <- Simulation_Data %>%
  filter(`Disposal.Type` == "Kick Effective" | `Disposal.Type` == "Kick Ineffective" | `Disposal.Type` == "Handball Effective" | `Disposal.Type` == "Handball Ineffective") %>%
  select(Disposal.Type, Disposal.Pressure, Time.in.Poss, Drill) %>%
  filter(Drill == "Match Simulation") %>%
  select(Disposal.Type, Disposal.Pressure, Time.in.Poss) %>%
  na.omit()

#Cleaning data: break up disposal type and resut
#Split disposal coloumn
Simulation_Data_filter <- separate(data = Simulation_Data_filter, col = `Disposal.Type`, into = c("Type", "Result"), sep = "\\ ")

#Rename Time in Possessionession
colnames(Simulation_Data_filter)[colnames(Simulation_Data_filter) == 'Time.in.Poss'] <- 'Time in Possession'

#change time collected
#first convert to factor
Simulation_Data_filter$`Time in Possession` <- as.vector(Simulation_Data_filter$`Time in Possession`)

#Regroup/Re-name constraints
Simulation_Data_filter$'Time in Possession'[Simulation_Data_filter$'Time in Possession'== "'0-1" ] <- "<2"
Simulation_Data_filter$'Time in Possession'[Simulation_Data_filter$'Time in Possession'== "'2-3" ] <- ">2"
Simulation_Data_filter$'Time in Possession'[Simulation_Data_filter$'Time in Possession'== "'4-5" ] <- ">2"
Simulation_Data_filter$'Time in Possession'[Simulation_Data_filter$'Time in Possession'== "6+" ] <- ">2"

#Change pressure
#convert pressure type
Simulation_Data_filter$`Disposal.Pressure` <- as.vector(Simulation_Data_filter$`Disposal.Pressure`)

#Regroup pressure types
Simulation_Data_filter$`Disposal.Pressure`[Simulation_Data_filter$`Disposal.Pressure` == "Chase"] <- "Pressure"
Simulation_Data_filter$`Disposal.Pressure`[Simulation_Data_filter$`Disposal.Pressure` == "Frontal"] <- "Pressure"
Simulation_Data_filter$`Disposal.Pressure`[Simulation_Data_filter$`Disposal.Pressure` == "Physical"] <- "Pressure"
Simulation_Data_filter$`Disposal.Pressure`[Simulation_Data_filter$`Disposal.Pressure` == "None"] <- "No Pressure"

#Re-convert everything to a factor
Simulation_Data_filter$Type <- as.factor(Simulation_Data_filter$Type)
Simulation_Data_filter$Result <- as.factor(Simulation_Data_filter$Result)
Simulation_Data_filter$`Disposal.Pressure` <- as.factor(Simulation_Data_filter$`Disposal.Pressure`)
Simulation_Data_filter$`Time in Possession` <- as.factor(Simulation_Data_filter$`Time in Possession`)



### 2019 Simulation Association Rules

#Association Rules - using the arules package
#Generate Rules for the 2018 Simulation Data - Effective Kicks
#Effective Kicks

#Generate Rules
Simulation_Rules <- apriori(Simulation_Data_filter, parameter = list(supp = 0.00002, conf = 0.00002, minlen=4))

#Sort and filter rules by outcome
Simulation_Rules_Effective <- sort(subset(Simulation_Rules, subset = rhs %in% c('Result=Effective')), by = "confidence", decreasing = T)


#Split lhs column into 3 seperate columns
Simulation_Rules_Effective <- transform(b, lhs = colsplit(LHS, ",", names = c("A", "B", "C")))

#Convert to data.table to seperate data.frame from within data.frame
Simulation_Rules_Effective <- as.data.frame.table(Simulation_Rules_Effective)

#Remove irrelevant rows
Simulation_Rules_Effective <- subset(Simulation_Rules_Effective, select = -c(1,2,3))

#Filter to remove duplicates
Simulation_Rules_Effective_Unique <- unique(Simulation_Rules_Effective)

#Rename columns 
colnames(Simulation_Rules_Effective_Unique) <- c("Result","Support", "Confidence", "Lift", "Count","Type","Pressure", "Time")

#Remove irrelevant apriori wording from each row
#First make columns vectors
Simulation_Rules_Effective_Unique$Type <- as.vector(Simulation_Rules_Effective_Unique$Type)
Simulation_Rules_Effective_Unique$Time <- as.vector(Simulation_Rules_Effective_Unique$Time)
Simulation_Rules_Effective_Unique$Pressure <- as.vector(Simulation_Rules_Effective_Unique$Pressure)
Simulation_Rules_Effective_Unique$Result <- as.vector(Simulation_Rules_Effective_Unique$Result)


#Change name
Simulation_Rules_Effective_Unique$Type[Simulation_Rules_Effective_Unique$Type == "{Type=Kick"] <- "Kick"
Simulation_Rules_Effective_Unique$Type[Simulation_Rules_Effective_Unique$Type == "{Type=Handball"] <- "Handball"
Simulation_Rules_Effective_Unique$Time[Simulation_Rules_Effective_Unique$Time == "Time in Possession=>2}"] <- ">2"
Simulation_Rules_Effective_Unique$Time[Simulation_Rules_Effective_Unique$Time == "Time in Possession=<2}"] <- "<2"
Simulation_Rules_Effective_Unique$Pressure[Simulation_Rules_Effective_Unique$Pressure == "Disposal.Pressure=No Pressure"] <- "No Pressure"
Simulation_Rules_Effective_Unique$Pressure[Simulation_Rules_Effective_Unique$Pressure == "Disposal.Pressure=Pressure"] <- "Pressure"
Simulation_Rules_Effective_Unique$Result[Simulation_Rules_Effective_Unique$Result == "{Result=Effective}"] <- "Effective"
Simulation_Rules_Effective_Unique$Result[Simulation_Rules_Effective_Unique$Result == "{Result=Ineffective}"] <- "Ineffective"

#View Rules
Simulation_Rules_Effective_Unique





###2019 Simulation Association Rules - Combined
Simulation_Combined_Rules <- rbind(Simulation_Rules_Effective_Unique)


#Assign unique ID to each rule & change order so all kicks together
Simulation_Combined_Rules <- arrange(Simulation_Combined_Rules, desc(Type))
Simulation_Combined_Rules$id <- rownames(Simulation_Combined_Rules)
Simulation_Combined_Rules$id <- as.vector(Simulation_Combined_Rules$id)

Rules_ID <- Simulation_Combined_Rules

Rules_ID <- subset(Rules_ID, select = c(6,7,8,9))


#Adding the rules to the dataset *Simulation NAKE SURE TO ALWAYS MERGE WITH RULES ID
Simulation_Data_ID <- left_join(Simulation_Data_filter, Rules_ID, by = c("Type", "Disposal.Pressure" = "Pressure", "Time in Possession" = "Time"))
Simulation_Data_ID <- na.omit(Simulation_Data_ID)

#Shift passing up column for second pass
Simulation_Data_ID$second <- Simulation_Data_ID$id
Simulation_Data_ID <- transform(Simulation_Data_ID, second = c(second[-1], NA))


#Shift passing up column for third pass
Simulation_Data_ID$third <- Simulation_Data_ID$second
Simulation_Data_ID <- transform(Simulation_Data_ID, third = c(third[-1], NA))

#Change id from numeric to alphabet
colnames(Simulation_Data_ID)[colnames(Simulation_Data_ID) == 'id'] <- 'first'


Simulation_Data_ID$first[Simulation_Data_ID$first == "1"] <- "A"
Simulation_Data_ID$first[Simulation_Data_ID$first == "2"] <- "B"
Simulation_Data_ID$first[Simulation_Data_ID$first == "3"] <- "C"
Simulation_Data_ID$first[Simulation_Data_ID$first == "4"] <- "D"
Simulation_Data_ID$first[Simulation_Data_ID$first == "5"] <- "E"
Simulation_Data_ID$first[Simulation_Data_ID$first == "6"] <- "F"
Simulation_Data_ID$first[Simulation_Data_ID$first == "7"] <- "G"
Simulation_Data_ID$first[Simulation_Data_ID$first == "8"] <- "H"
Simulation_Data_ID$first[Simulation_Data_ID$first == "9"] <- "I"
Simulation_Data_ID$first[Simulation_Data_ID$first == "10"] <- "J"
Simulation_Data_ID$first[Simulation_Data_ID$first == "11"] <- "K"
Simulation_Data_ID$first[Simulation_Data_ID$first == "12"] <- "L"
Simulation_Data_ID$first[Simulation_Data_ID$first == "13"] <- "M"
Simulation_Data_ID$first[Simulation_Data_ID$first == "14"] <- "N"
Simulation_Data_ID$first[Simulation_Data_ID$first == "15"] <- "O"

Simulation_Data_ID$second[Simulation_Data_ID$second == "1"] <- "A"
Simulation_Data_ID$second[Simulation_Data_ID$second == "2"] <- "B"
Simulation_Data_ID$second[Simulation_Data_ID$second == "3"] <- "C"
Simulation_Data_ID$second[Simulation_Data_ID$second == "4"] <- "D"
Simulation_Data_ID$second[Simulation_Data_ID$second == "5"] <- "E"
Simulation_Data_ID$second[Simulation_Data_ID$second == "6"] <- "F"
Simulation_Data_ID$second[Simulation_Data_ID$second == "7"] <- "G"
Simulation_Data_ID$second[Simulation_Data_ID$second == "8"] <- "H"
Simulation_Data_ID$second[Simulation_Data_ID$second == "9"] <- "I"
Simulation_Data_ID$second[Simulation_Data_ID$second == "10"] <- "J"
Simulation_Data_ID$second[Simulation_Data_ID$second == "11"] <- "K"
Simulation_Data_ID$second[Simulation_Data_ID$second == "12"] <- "L"
Simulation_Data_ID$second[Simulation_Data_ID$second == "13"] <- "M"
Simulation_Data_ID$second[Simulation_Data_ID$second == "14"] <- "N"
Simulation_Data_ID$second[Simulation_Data_ID$second == "15"] <- "O"

Simulation_Data_ID$third[Simulation_Data_ID$third == "1"] <- "A"
Simulation_Data_ID$third[Simulation_Data_ID$third == "2"] <- "B"
Simulation_Data_ID$third[Simulation_Data_ID$third == "3"] <- "C"
Simulation_Data_ID$third[Simulation_Data_ID$third == "4"] <- "D"
Simulation_Data_ID$third[Simulation_Data_ID$third == "5"] <- "E"
Simulation_Data_ID$third[Simulation_Data_ID$third == "6"] <- "F"
Simulation_Data_ID$third[Simulation_Data_ID$third == "7"] <- "G"
Simulation_Data_ID$third[Simulation_Data_ID$third == "8"] <- "H"
Simulation_Data_ID$third[Simulation_Data_ID$third == "9"] <- "I"
Simulation_Data_ID$third[Simulation_Data_ID$third == "10"] <- "J"
Simulation_Data_ID$third[Simulation_Data_ID$third == "11"] <- "K"
Simulation_Data_ID$third[Simulation_Data_ID$third == "12"] <- "L"
Simulation_Data_ID$third[Simulation_Data_ID$third == "13"] <- "M"
Simulation_Data_ID$third[Simulation_Data_ID$third == "14"] <- "N"
Simulation_Data_ID$third[Simulation_Data_ID$third == "15"] <- "O"


###2018 Simulation Data - Calculate Frequencies
#Convert from Character to strong
Simulation_Data_ID$first <- as.vector(Simulation_Data_ID$first)
Simulation_Data_ID$second <- as.vector(Simulation_Data_ID$second)
Simulation_Data_ID$third <- as.vector(Simulation_Data_ID$third)


#Calculate frequency of each rule for one pass
Simulation_ID_onepass <- Simulation_Data_ID %>%
  select(first) %>%
  group_by(first) %>%
  summarise(n=n()) %>%
  mutate(freq = n / 1298) %>%
  select(first, freq)

#Calculate frequency of each rule for two passes
Simulation_ID_twopasses <- Simulation_Data_ID %>%
  select(first, second) %>%
  group_by(first, second) %>%
  summarise(n=n()) %>%
  mutate(count = sum(n)) %>%
  mutate(freq = (n / count)*100) %>%
  select(first, second, freq) %>%
  spread(second, freq)

Simulation_ID_twopasses_gather <- Simulation_Data_ID %>%
  select(first, second) %>%
  group_by(first, second) %>%
  summarise(n=n()) %>%
  mutate(count = sum(n)) %>%
  mutate(freq = n / count) %>%
  select(first, second, freq)


#Calculate frequency of each rule for three passes
Simulation_ID_threepasses <- Simulation_Data_ID %>%
  select(first, second, third) %>%
  group_by(first, second, third) %>%
  summarise(n = n()) %>%
  select(first, second, third, n)

#Calculate the change of the third pass being effective
#Combine first & second pass  
Simulation_ID_threepasses_eff <- Simulation_ID_threepasses %>%
  unite("One-Two", first:second)

Simulation_ID_threepasses_eff$third[Simulation_ID_threepasses_eff$third == "A"] <- "0.6959459"
Simulation_ID_threepasses_eff$third[Simulation_ID_threepasses_eff$third == "B"] <- "0.6388889"
Simulation_ID_threepasses_eff$third[Simulation_ID_threepasses_eff$third == "C"] <- "0.6229508"
Simulation_ID_threepasses_eff$third[Simulation_ID_threepasses_eff$third == "D"] <- "0.4898990"
Simulation_ID_threepasses_eff$third[Simulation_ID_threepasses_eff$third == "E"] <- "0.9615385"
Simulation_ID_threepasses_eff$third[Simulation_ID_threepasses_eff$third == "F"] <- "0.9172932"
Simulation_ID_threepasses_eff$third[Simulation_ID_threepasses_eff$third == "G"] <- "0.8000000"
Simulation_ID_threepasses_eff$third[Simulation_ID_threepasses_eff$third == "H"] <- "0.7619048"

#Convert to numeric
Simulation_ID_threepasses_eff$third <- as.numeric(Simulation_ID_threepasses_eff$third)

#Calculate frequency of each rule for three passes
Simulation_ID_threepasses_eff_ <- Simulation_ID_threepasses_eff %>%
  group_by(`One-Two`) %>%
  mutate(DisCount = sum(n)) %>%
  mutate(Freq = (n / DisCount)) %>%
  mutate(Eff = (third*Freq)) %>%
  mutate(Com = sum(Eff)*100) %>%
  select(`One-Two`, Com)

#Convert to data frame
Simulation_ID_threepasses_eff_2 <- as.data.frame(Simulation_ID_threepasses_eff_)

#Manipulate data for visual
Simulation_ID_threepasses_eff_2 <- Simulation_ID_threepasses_eff_2 %>%
  group_by(`One-Two`, Com) %>%
  summarise(n=n())

Simulation_ID_threepasses_eff_2 <- Simulation_ID_threepasses_eff_2 %>%
  separate(`One-Two`, c("first", "second"))

#produce a bar plot showing the frequency of each rule
ggplot(Simulation_ID_onepass, aes(x= first, y= freq)) +
  geom_bar(stat = "identity")



#### 2018 Competition Matches
#load dataset
Match_Data <- read_excel("Data/2018_Match_Event_Data.xlsm", 
                         sheet = "PreSorter")

#Select & Filter for rows which have either kick/handball recorded
Match_Data_filter <- Match_Data %>%
  filter(`Disposal Type` == "Kick Effective" | `Disposal Type` == "Kick Ineffective" | `Disposal Type` == "Handball Effective" | `Disposal Type` == "Handball Ineffective") %>%
  select(`Disposal Type`, `Disposal Pressure`, `Time in Poss`)


#Cleaning data: break up disposal type and resut
#Split disposal coloumn
Match_Data_filter <- separate(data = Match_Data_filter, col = `Disposal Type`, into = c("Type", "Result"), sep = "\\ ")

#Rename Time in Possessionession
colnames(Match_Data_filter)[colnames(Match_Data_filter) == 'Time in Poss'] <- 'Time in Possession'

#change time collected
#first convert to factor
Match_Data_filter$`Time in Possession` <- as.vector(Match_Data_filter$`Time in Possession`)

#Regroup constraints
Match_Data_filter$'Time in Possession'[Match_Data_filter$'Time in Possession'== "'0-1" ] <- "<2"
Match_Data_filter$'Time in Possession'[Match_Data_filter$'Time in Possession'== "'2-3" ] <- ">2"
Match_Data_filter$'Time in Possession'[Match_Data_filter$'Time in Possession'== "'4-5" ] <- ">2"
Match_Data_filter$'Time in Possession'[Match_Data_filter$'Time in Possession'== "6+" ] <- ">2"

#Change pressure
#convert pressure type
Match_Data_filter$`Disposal Pressure` <- as.vector(Match_Data_filter$`Disposal Pressure`)
#Regroup pressure types
Match_Data_filter$`Disposal Pressure`[Match_Data_filter$`Disposal Pressure` == "Chase"] <- "Pressure"
Match_Data_filter$`Disposal Pressure`[Match_Data_filter$`Disposal Pressure` == "Frontal"] <- "Pressure"
Match_Data_filter$`Disposal Pressure`[Match_Data_filter$`Disposal Pressure` == "Physical"] <- "Pressure"
Match_Data_filter$`Disposal Pressure`[Match_Data_filter$`Disposal Pressure` == "None"] <- "No Pressure"

#Re-convert everything to a factor
Match_Data_filter$Type <- as.factor(Match_Data_filter$Type)
Match_Data_filter$Result <- as.factor(Match_Data_filter$Result)
Match_Data_filter$`Disposal Pressure` <- as.factor(Match_Data_filter$`Disposal Pressure`)
Match_Data_filter$`Time in Possession` <- as.factor(Match_Data_filter$`Time in Possession`)



###2018 Match Association Rules - Effective
#Association Rules - using the arules package

#Generate Rules for the 2018 Match Data - Effective Kicks
#Effective Kicks
#Generate Rules
Match_Rules <- apriori(Match_Data_filter, parameter = list(supp = 0.00002, conf = 0.00002, minlen=4))
#Sort and filter rules by outcome
Match_Rules_Effective <- sort(subset(Match_Rules, subset = rhs %in% c('Result=Effective')), by = "confidence", decreasing = T)
#Split lhs column into 3 seperate columns


Match_Rules_Effective <- as.data.frame(transform(a, lhs = colsplit(LHS, "\\,", names = c("A", "B", "C"))))
#Convert to data.table to seperate data.frame from within data.frame
Match_Rules_Effective <- as.data.frame.table(Match_Rules_Effective)
#Remove irrelevant rows
Match_Rules_Effective <- subset(Match_Rules_Effective, select = -c(1,2,3))
#Filter to remove duplicates
Match_Rules_Effective_Unique <- unique(Match_Rules_Effective)
#Rename columns 
colnames(Match_Rules_Effective_Unique) <- c("Result","Support", "Confidence", "Lift", "Count","Type","Pressure", "Time")

#Remove irrelevant apriori wording from each row
#First make columns vectors
Match_Rules_Effective_Unique$Type <- as.vector(Match_Rules_Effective_Unique$Type)
Match_Rules_Effective_Unique$Time <- as.vector(Match_Rules_Effective_Unique$Time)
Match_Rules_Effective_Unique$Pressure <- as.vector(Match_Rules_Effective_Unique$Pressure)
Match_Rules_Effective_Unique$Result <- as.vector(Match_Rules_Effective_Unique$Result)


#Change name
Match_Rules_Effective_Unique$Type[Match_Rules_Effective_Unique$Type == "{Type=Kick"] <- "Kick"
Match_Rules_Effective_Unique$Type[Match_Rules_Effective_Unique$Type == "{Type=Handball"] <- "Handball"
Match_Rules_Effective_Unique$Time[Match_Rules_Effective_Unique$Time == "Time in Possession=>2}"] <- ">2"
Match_Rules_Effective_Unique$Time[Match_Rules_Effective_Unique$Time == "Time in Possession=<2}"] <- "<2"
Match_Rules_Effective_Unique$Pressure[Match_Rules_Effective_Unique$Pressure == "Disposal Pressure=No Pressure"] <- "No Pressure"
Match_Rules_Effective_Unique$Pressure[Match_Rules_Effective_Unique$Pressure == "Disposal Pressure=Pressure"] <- "Pressure"
Match_Rules_Effective_Unique$Result[Match_Rules_Effective_Unique$Result == "{Result=Effective}"] <- "Effective"
Match_Rules_Effective_Unique$Result[Match_Rules_Effective_Unique$Result == "{Result=Ineffective}"] <- "Ineffective"

#View rules
Match_Rules_Effective_Unique




###2018 Match Association Rules - Combined
Match_Combined_Rules <- rbind(Match_Rules_Effective_Unique)


#Don't create rule ID here. Just use the one from match simulation where 15 rules were created. 


#Adding the rules to the dataset *MATCH SURE TO ALWAYS MERGE WITH RULES ID
Match_Data_ID <- left_join(Match_Data_filter, Rules_ID, by = c("Type", "Disposal Pressure" = "Pressure", "Time in Possession" = "Time"))
Match_Data_ID <- na.omit(Match_Data_ID)

#Shift passing up column for second pass
Match_Data_ID$second <- Match_Data_ID$id
Match_Data_ID <- transform(Match_Data_ID, second = c(second[-1], NA))


#Shift passing up column for third pass
Match_Data_ID$third <- Match_Data_ID$second
Match_Data_ID <- transform(Match_Data_ID, third = c(third[-1], NA))

#Change id from numeric to alphabet
colnames(Match_Data_ID)[colnames(Match_Data_ID) == 'id'] <- 'first'


Match_Data_ID$first[Match_Data_ID$first == "1"] <- "A"
Match_Data_ID$first[Match_Data_ID$first == "2"] <- "B"
Match_Data_ID$first[Match_Data_ID$first == "3"] <- "C"
Match_Data_ID$first[Match_Data_ID$first == "4"] <- "D"
Match_Data_ID$first[Match_Data_ID$first == "5"] <- "E"
Match_Data_ID$first[Match_Data_ID$first == "6"] <- "F"
Match_Data_ID$first[Match_Data_ID$first == "7"] <- "G"
Match_Data_ID$first[Match_Data_ID$first == "8"] <- "H"
Match_Data_ID$first[Match_Data_ID$first == "9"] <- "I"
Match_Data_ID$first[Match_Data_ID$first == "10"] <- "J"
Match_Data_ID$first[Match_Data_ID$first == "11"] <- "K"
Match_Data_ID$first[Match_Data_ID$first == "12"] <- "L"
Match_Data_ID$first[Match_Data_ID$first == "13"] <- "M"
Match_Data_ID$first[Match_Data_ID$first == "14"] <- "N"
Match_Data_ID$first[Match_Data_ID$first == "15"] <- "O"

Match_Data_ID$second[Match_Data_ID$second == "1"] <- "A"
Match_Data_ID$second[Match_Data_ID$second == "2"] <- "B"
Match_Data_ID$second[Match_Data_ID$second == "3"] <- "C"
Match_Data_ID$second[Match_Data_ID$second == "4"] <- "D"
Match_Data_ID$second[Match_Data_ID$second == "5"] <- "E"
Match_Data_ID$second[Match_Data_ID$second == "6"] <- "F"
Match_Data_ID$second[Match_Data_ID$second == "7"] <- "G"
Match_Data_ID$second[Match_Data_ID$second == "8"] <- "H"
Match_Data_ID$second[Match_Data_ID$second == "9"] <- "I"
Match_Data_ID$second[Match_Data_ID$second == "10"] <- "J"
Match_Data_ID$second[Match_Data_ID$second == "11"] <- "K"
Match_Data_ID$second[Match_Data_ID$second == "12"] <- "L"
Match_Data_ID$second[Match_Data_ID$second == "13"] <- "M"
Match_Data_ID$second[Match_Data_ID$second == "14"] <- "N"
Match_Data_ID$second[Match_Data_ID$second == "15"] <- "O"

Match_Data_ID$third[Match_Data_ID$third == "1"] <- "A"
Match_Data_ID$third[Match_Data_ID$third == "2"] <- "B"
Match_Data_ID$third[Match_Data_ID$third == "3"] <- "C"
Match_Data_ID$third[Match_Data_ID$third == "4"] <- "D"
Match_Data_ID$third[Match_Data_ID$third == "5"] <- "E"
Match_Data_ID$third[Match_Data_ID$third == "6"] <- "F"
Match_Data_ID$third[Match_Data_ID$third == "7"] <- "G"
Match_Data_ID$third[Match_Data_ID$third == "8"] <- "H"
Match_Data_ID$third[Match_Data_ID$third == "9"] <- "I"
Match_Data_ID$third[Match_Data_ID$third == "10"] <- "J"
Match_Data_ID$third[Match_Data_ID$third == "11"] <- "K"
Match_Data_ID$third[Match_Data_ID$third == "12"] <- "L"
Match_Data_ID$third[Match_Data_ID$third == "13"] <- "M"
Match_Data_ID$third[Match_Data_ID$third == "14"] <- "N"
Match_Data_ID$third[Match_Data_ID$third == "15"] <- "O"

###2018 Match Data - Calculate Frequencies
#Convert from Character to strong
Match_Data_ID$first <- as.vector(Match_Data_ID$first)
Match_Data_ID$second <- as.vector(Match_Data_ID$second)
Match_Data_ID$third <- as.vector(Match_Data_ID$third)


#Calculate frequency of each rule for one pass
Match_ID_onepass <- Match_Data_ID %>%
  select(first) %>%
  group_by(first) %>%
  summarise(n=n()) %>%
  mutate(freq = n / 3478) %>%
  select(first, freq)

#Calculate frequency of each rule for two passes
Match_ID_twopasses <- Match_Data_ID %>%
  select(first, second) %>%
  group_by(first, second) %>%
  summarise(n=n()) %>%
  mutate(count = sum(n)) %>%
  mutate(freq = (n / count)*100) %>%
  select(first, second, freq) %>%
  spread(second, freq)

Match_ID_twopasses_gather <- Match_Data_ID %>%
  select(first, second) %>%
  group_by(first, second) %>%
  summarise(n=n()) %>%
  mutate(count = sum(n)) %>%
  mutate(freq = n / count) %>%
  select(first, second, freq) 

#Calculate frequency of each rule for three passes
Match_ID_threepasses <- Match_Data_ID %>%
  select(first, second, third) %>%
  group_by(first, second, third) %>%
  summarise(n = n()) %>%
  select(first, second, third, n)

#Combine first & second pass  
Match_ID_threepasses_eff <- Match_ID_threepasses %>%
  unite("One-Two", first:second)

Match_ID_threepasses_eff$third[Match_ID_threepasses_eff$third == "A"] <- "0.7399381"
Match_ID_threepasses_eff$third[Match_ID_threepasses_eff$third == "B"] <- "0.6993865"
Match_ID_threepasses_eff$third[Match_ID_threepasses_eff$third == "C"] <- "0.5750000"
Match_ID_threepasses_eff$third[Match_ID_threepasses_eff$third == "D"] <- "0.5366218"
Match_ID_threepasses_eff$third[Match_ID_threepasses_eff$third == "E"] <- "1.0000000"
Match_ID_threepasses_eff$third[Match_ID_threepasses_eff$third == "F"] <- "0.9923372"
Match_ID_threepasses_eff$third[Match_ID_threepasses_eff$third == "G"] <- "0.9608177"
Match_ID_threepasses_eff$third[Match_ID_threepasses_eff$third == "H"] <- "0.9870130"

#Convert to numeric
Match_ID_threepasses_eff$third <- as.numeric(Match_ID_threepasses_eff$third)

#Calculate frequency of occurance for three sequential passes
Match_ID_threepasses_eff_ <- Match_ID_threepasses_eff %>%
  group_by(`One-Two`) %>%
  mutate(DisCount = sum(n)) %>%
  mutate(Freq = (n / DisCount)) %>%
  mutate(Eff = (third*Freq)) %>%
  mutate(Com = sum(Eff)*100) %>%
  select(`One-Two`, Com)

#Data manipulation - convert to data frame
Match_ID_threepasses_eff_2 <- as.data.frame(Match_ID_threepasses_eff_)

Match_ID_threepasses_eff_2 <- Match_ID_threepasses_eff_2 %>%
  group_by(`One-Two`, Com) %>%
  summarise(n=n())

Match_ID_threepasses_eff_2 <- Match_ID_threepasses_eff_2 %>%
  separate(`One-Two`, c("first", "second"))

#produce a bar plot showing the frequency of each rule
ggplot(Match_ID_onepass, aes(x= first, y= freq)) +
  geom_bar(stat = "identity")






#### 2019 Training - SSG
#load dataset
SSG_Data <- read.csv("Data/training_database.csv", na.strings=c("","NA"))

#Select & Filter for rows which have either kick/handball recorded
SSG_Data_filter <- SSG_Data %>%
  filter(`Disposal.Type` == "Kick Effective" | `Disposal.Type` == "Kick Ineffective" | `Disposal.Type` == "Handball Effective" | `Disposal.Type` == "Handball Ineffective") %>%
  select(Disposal.Type, Disposal.Pressure, Time.in.Poss, Drill) %>%
  filter(Drill == "9v9" | Drill == "8v8" | Drill == "4v3" | Drill == "7v4 Keepings Off Kick" | Drill == "6v6" | Drill == "Inside Out" | Drill == "Half Ground Football" | Drill == "7v6 - Mark" | Drill == "7v6 - Open" | Drill == "7v5 - Mark" | Drill == "7v5 - Open") %>%
  select(Disposal.Type, Disposal.Pressure, Time.in.Poss) %>%
  na.omit()

#Cleaning data: break up disposal type and resut
#Split disposal coloumn
SSG_Data_filter <- separate(data = SSG_Data_filter, col = `Disposal.Type`, into = c("Type", "Result"), sep = "\\ ")

#Rename Time in Possessionession
colnames(SSG_Data_filter)[colnames(SSG_Data_filter) == 'Time.in.Poss'] <- 'Time in Possession'

#change time collected
#first convert to factor
SSG_Data_filter$`Time in Possession` <- as.vector(SSG_Data_filter$`Time in Possession`)

#Regroup constraints
SSG_Data_filter$'Time in Possession'[SSG_Data_filter$'Time in Possession'== "'0-1" ] <- "<2"
SSG_Data_filter$'Time in Possession'[SSG_Data_filter$'Time in Possession'== "'2-3" ] <- ">2"
SSG_Data_filter$'Time in Possession'[SSG_Data_filter$'Time in Possession'== "'4-5" ] <- ">2"
SSG_Data_filter$'Time in Possession'[SSG_Data_filter$'Time in Possession'== "6+" ] <- ">2"

#Change pressure
#convert pressure type
SSG_Data_filter$`Disposal.Pressure` <- as.vector(SSG_Data_filter$`Disposal.Pressure`)

#Regroup pressure types
SSG_Data_filter$`Disposal.Pressure`[SSG_Data_filter$`Disposal.Pressure` == "Chase"] <- "Pressure"
SSG_Data_filter$`Disposal.Pressure`[SSG_Data_filter$`Disposal.Pressure` == "Frontal"] <- "Pressure"
SSG_Data_filter$`Disposal.Pressure`[SSG_Data_filter$`Disposal.Pressure` == "Physical"] <- "Pressure"
SSG_Data_filter$`Disposal.Pressure`[SSG_Data_filter$`Disposal.Pressure` == "None"] <- "No Pressure"

#Re-convert everything to a factor
SSG_Data_filter$Type <- as.factor(SSG_Data_filter$Type)
SSG_Data_filter$Result <- as.factor(SSG_Data_filter$Result)
SSG_Data_filter$`Disposal.Pressure` <- as.factor(SSG_Data_filter$`Disposal.Pressure`)
SSG_Data_filter$`Time in Possession` <- as.factor(SSG_Data_filter$`Time in Possession`)



#2019 SSG Association Rules - Effective
#Association Rules - using the arules package

#Generate Rules for the 2018 SSG Data - Effective Kicks
#Effective Kicks
#Generate Rules
SSG_Rules <- apriori(SSG_Data_filter, parameter = list(supp = 0.0000002, conf = 0.000000002, minlen=4))
#Sort and filter rules by outcome
SSG_Rules_Effective <- sort(subset(SSG_Rules, subset = rhs %in% c('Result=Effective')), by = "confidence", decreasing = T)

c <- DATAFRAME(SSG_Rules_Effective, separate = TRUE)
#Split lhs column into 3 seperate columns
SSG_Rules_Effective <- transform(c, lhs = colsplit(LHS, "\\,", names = c("A", "B", "C")))
#Convert to data.table to seperate data.frame from within data.frame
SSG_Rules_Effective <- as.data.frame.table(SSG_Rules_Effective)
#Remove irrelevant rows
SSG_Rules_Effective <- subset(SSG_Rules_Effective, select = -c(1,2,3))
#Filter to remove duplicates
SSG_Rules_Effective_Unique <- unique(SSG_Rules_Effective)
#Rename columns 
colnames(SSG_Rules_Effective_Unique) <- c("Result","Support", "Confidence", "Lift", "Count","Type","Pressure", "Time")

#Remove irrelevant apriori wording from each row
#First make columns vectors
SSG_Rules_Effective_Unique$Type <- as.vector(SSG_Rules_Effective_Unique$Type)
SSG_Rules_Effective_Unique$Time <- as.vector(SSG_Rules_Effective_Unique$Time)
SSG_Rules_Effective_Unique$Pressure <- as.vector(SSG_Rules_Effective_Unique$Pressure)
SSG_Rules_Effective_Unique$Result <- as.vector(SSG_Rules_Effective_Unique$Result)


#Change name
SSG_Rules_Effective_Unique$Type[SSG_Rules_Effective_Unique$Type == "{Type=Kick"] <- "Kick"
SSG_Rules_Effective_Unique$Type[SSG_Rules_Effective_Unique$Type == "{Type=Handball"] <- "Handball"
SSG_Rules_Effective_Unique$Time[SSG_Rules_Effective_Unique$Time == "Time in Possession=>2}"] <- ">2"
SSG_Rules_Effective_Unique$Time[SSG_Rules_Effective_Unique$Time == "Time in Possession=<2}"] <- "<2"
SSG_Rules_Effective_Unique$Pressure[SSG_Rules_Effective_Unique$Pressure == "Disposal.Pressure=No Pressure"] <- "No Pressure"
SSG_Rules_Effective_Unique$Pressure[SSG_Rules_Effective_Unique$Pressure == "Disposal.Pressure=Pressure"] <- "Pressure"
SSG_Rules_Effective_Unique$Result[SSG_Rules_Effective_Unique$Result == "{Result=Effective}"] <- "Effective"
SSG_Rules_Effective_Unique$Result[SSG_Rules_Effective_Unique$Result == "{Result=Ineffective}"] <- "Ineffective"

#View rules
SSG_Rules_Effective_Unique

### 2019 SSG Association Rules - Combined
#Combine Effective & Ineffective Rules

SSG_Combined_Rules <- rbind(SSG_Rules_Effective_Unique)

#Don't create rules_ID -> use the one from Match Simulation


#Adding the rules to the dataset *SSG SURE TO ALWAYS MERGE WITH RULES ID
SSG_Data_ID <- left_join(SSG_Data_filter, Rules_ID, by = c("Type", "Disposal.Pressure" = "Pressure", "Time in Possession" = "Time"))
SSG_Data_ID <- na.omit(SSG_Data_ID)

#Shift passing up column for second pass
SSG_Data_ID$second <- SSG_Data_ID$id
SSG_Data_ID <- transform(SSG_Data_ID, second = c(second[-1], NA))


#Shift passing up column for third pass
SSG_Data_ID$third <- SSG_Data_ID$second
SSG_Data_ID <- transform(SSG_Data_ID, third = c(third[-1], NA))

#Change id from numeric to alphabet
colnames(SSG_Data_ID)[colnames(SSG_Data_ID) == 'id'] <- 'first'

#Asign rule ID to dataset
SSG_Data_ID$first[SSG_Data_ID$first == "1"] <- "A"
SSG_Data_ID$first[SSG_Data_ID$first == "2"] <- "B"
SSG_Data_ID$first[SSG_Data_ID$first == "3"] <- "C"
SSG_Data_ID$first[SSG_Data_ID$first == "4"] <- "D"
SSG_Data_ID$first[SSG_Data_ID$first == "5"] <- "E"
SSG_Data_ID$first[SSG_Data_ID$first == "6"] <- "F"
SSG_Data_ID$first[SSG_Data_ID$first == "7"] <- "G"
SSG_Data_ID$first[SSG_Data_ID$first == "8"] <- "H"
SSG_Data_ID$first[SSG_Data_ID$first == "9"] <- "I"
SSG_Data_ID$first[SSG_Data_ID$first == "10"] <- "J"
SSG_Data_ID$first[SSG_Data_ID$first == "11"] <- "K"
SSG_Data_ID$first[SSG_Data_ID$first == "12"] <- "L"
SSG_Data_ID$first[SSG_Data_ID$first == "13"] <- "M"
SSG_Data_ID$first[SSG_Data_ID$first == "14"] <- "N"
SSG_Data_ID$first[SSG_Data_ID$first == "15"] <- "O"

SSG_Data_ID$second[SSG_Data_ID$second == "1"] <- "A"
SSG_Data_ID$second[SSG_Data_ID$second == "2"] <- "B"
SSG_Data_ID$second[SSG_Data_ID$second == "3"] <- "C"
SSG_Data_ID$second[SSG_Data_ID$second == "4"] <- "D"
SSG_Data_ID$second[SSG_Data_ID$second == "5"] <- "E"
SSG_Data_ID$second[SSG_Data_ID$second == "6"] <- "F"
SSG_Data_ID$second[SSG_Data_ID$second == "7"] <- "G"
SSG_Data_ID$second[SSG_Data_ID$second == "8"] <- "H"
SSG_Data_ID$second[SSG_Data_ID$second == "9"] <- "I"
SSG_Data_ID$second[SSG_Data_ID$second == "10"] <- "J"
SSG_Data_ID$second[SSG_Data_ID$second == "11"] <- "K"
SSG_Data_ID$second[SSG_Data_ID$second == "12"] <- "L"
SSG_Data_ID$second[SSG_Data_ID$second == "13"] <- "M"
SSG_Data_ID$second[SSG_Data_ID$second == "14"] <- "N"
SSG_Data_ID$second[SSG_Data_ID$second == "15"] <- "O"

SSG_Data_ID$third[SSG_Data_ID$third == "1"] <- "A"
SSG_Data_ID$third[SSG_Data_ID$third == "2"] <- "B"
SSG_Data_ID$third[SSG_Data_ID$third == "3"] <- "C"
SSG_Data_ID$third[SSG_Data_ID$third == "4"] <- "D"
SSG_Data_ID$third[SSG_Data_ID$third == "5"] <- "E"
SSG_Data_ID$third[SSG_Data_ID$third == "6"] <- "F"
SSG_Data_ID$third[SSG_Data_ID$third == "7"] <- "G"
SSG_Data_ID$third[SSG_Data_ID$third == "8"] <- "H"
SSG_Data_ID$third[SSG_Data_ID$third == "9"] <- "I"
SSG_Data_ID$third[SSG_Data_ID$third == "10"] <- "J"
SSG_Data_ID$third[SSG_Data_ID$third == "11"] <- "K"
SSG_Data_ID$third[SSG_Data_ID$third == "12"] <- "L"
SSG_Data_ID$third[SSG_Data_ID$third == "13"] <- "M"
SSG_Data_ID$third[SSG_Data_ID$third == "14"] <- "N"
SSG_Data_ID$third[SSG_Data_ID$third == "15"] <- "O"

###2019 SSG Data - Calculate Frequencies
#Convert from Character to strong
SSG_Data_ID$first <- as.vector(SSG_Data_ID$first)
SSG_Data_ID$second <- as.vector(SSG_Data_ID$second)
SSG_Data_ID$third <- as.vector(SSG_Data_ID$third)


#Calculate frequency of each rule for one pass
SSG_ID_onepass <- SSG_Data_ID %>%
  select(first) %>%
  group_by(first) %>%
  summarise(n=n()) %>%
  mutate(freq = n / 2677) %>%
  select(first, freq)

#Calculate frequency of each rule for two passes
SSG_ID_twopasses <- SSG_Data_ID %>%
  select(first, second) %>%
  group_by(first, second) %>%
  summarise(n=n()) %>%
  mutate(count = sum(n)) %>%
  mutate(freq = (n / count)*100) %>%
  select(first, second, freq) %>%
  spread(second, freq)


SSG_ID_twopasses_gather <- SSG_Data_ID %>%
  select(first, second) %>%
  group_by(first, second) %>%
  summarise(n=n()) %>%
  mutate(count = sum(n))  %>%
  mutate(freq = n / count) %>%
  select(first, second, freq)

#Calculate frequency of each rule for three passes
SSG_ID_threepasses <- SSG_Data_ID %>%
  select(first, second, third) %>%
  group_by(first, second, third) %>%
  summarise(n = n()) %>%
  # mutate(freq = n / 2677) %>%
  select(first, second, third, n)

#Combine first & second pass  
SSG_ID_threepasses_eff <- SSG_ID_threepasses %>%
  unite("One-Two", first:second)

SSG_ID_threepasses_eff$third[SSG_ID_threepasses_eff$third == "A"] <- "0.8274933"
SSG_ID_threepasses_eff$third[SSG_ID_threepasses_eff$third == "B"] <- "0.8202677"
SSG_ID_threepasses_eff$third[SSG_ID_threepasses_eff$third == "C"] <- "0.7883895"
SSG_ID_threepasses_eff$third[SSG_ID_threepasses_eff$third == "D"] <- "0.7660377"
SSG_ID_threepasses_eff$third[SSG_ID_threepasses_eff$third == "E"] <- "1.0"
SSG_ID_threepasses_eff$third[SSG_ID_threepasses_eff$third == "F"] <- "0.9137931"
SSG_ID_threepasses_eff$third[SSG_ID_threepasses_eff$third == "G"] <- "0.8984375"
SSG_ID_threepasses_eff$third[SSG_ID_threepasses_eff$third == "H"] <- "0.7931034"

SSG_ID_threepasses_eff$third <- as.numeric(SSG_ID_threepasses_eff$third)

SSG_ID_threepasses_eff_ <- SSG_ID_threepasses_eff %>%
  group_by(`One-Two`) %>%
  mutate(DisCount = sum(n)) %>%
  mutate(Freq = (n / DisCount)) %>%
  mutate(Eff = (third*Freq)) %>%
  mutate(Com = sum(Eff)*100) %>%
  select(`One-Two`, Com)

SSG_ID_threepasses_eff_2 <- as.data.frame(SSG_ID_threepasses_eff_)

SSG_ID_threepasses_eff_2 <- SSG_ID_threepasses_eff_2 %>%
  group_by(`One-Two`, Com) %>%
  summarise(n=n())

SSG_ID_threepasses_eff_2 <- SSG_ID_threepasses_eff_2 %>%
  separate(`One-Two`, c("first", "second"))




#### Tables/Figures
#Assign Rule ID to frame - I waited until now as need to be numeric above
Rules_ID$id[Rules_ID$id == "1"] <- "A"
Rules_ID$id[Rules_ID$id == "2"] <- "B"
Rules_ID$id[Rules_ID$id == "3"] <- "C"
Rules_ID$id[Rules_ID$id == "4"] <- "D"
Rules_ID$id[Rules_ID$id == "5"] <- "E"
Rules_ID$id[Rules_ID$id == "6"] <- "F"
Rules_ID$id[Rules_ID$id == "7"] <- "G"
Rules_ID$id[Rules_ID$id == "8"] <- "H"
Rules_ID$id[Rules_ID$id == "9"] <- "I"
Rules_ID$id[Rules_ID$id == "10"] <- "J"
Rules_ID$id[Rules_ID$id == "11"] <- "K"
Rules_ID$id[Rules_ID$id == "12"] <- "L"
Rules_ID$id[Rules_ID$id == "13"] <- "M"
Rules_ID$id[Rules_ID$id == "14"] <- "N"
Rules_ID$id[Rules_ID$id == "15"] <- "O"

#Change order of Rules_ID
Rules_ID <- Rules_ID[,c(4,1:3)]

#View Rules_ID
Rules_ID

###Table with breakdown of results}
#Combine rule outcomes together
All_rules_p1 <- merge(Match_Combined_Rules,Simulation_Combined_Rules, by=c("Type","Pressure","Time","Result"), all= TRUE)
All_rules <- merge(All_rules_p1,SSG_Combined_Rules, by=c("Type","Pressure","Time","Result"), all = TRUE)

#change names of columns - if I get it working in the Rmd file type should look at merged cells for match, sim and SSG
names(All_rules) <- c("Type", "Pressure", "Time", "Result", "Match - Support", "Match - Confidence", "Match - Lift", "Match - Count", "Simulation - Support", "Simulation - Confidence", "Simulation - Lift", "Simulation - Count", "ID", "SSG - Support", "SSG - Confidence", "SSG - Lift", "SSG - Count")

#Reassign ID
All_rules$ID[All_rules$ID == "1"] <- "A"
All_rules$ID[All_rules$ID == "2"] <- "B"
All_rules$ID[All_rules$ID == "3"] <- "C"
All_rules$ID[All_rules$ID == "4"] <- "D"
All_rules$ID[All_rules$ID == "5"] <- "E"
All_rules$ID[All_rules$ID == "6"] <- "F"
All_rules$ID[All_rules$ID == "7"] <- "G"
All_rules$ID[All_rules$ID == "8"] <- "H"
All_rules$ID[All_rules$ID == "9"] <- "I"
All_rules$ID[All_rules$ID == "10"] <- "J"
All_rules$ID[All_rules$ID == "11"] <- "K"
All_rules$ID[All_rules$ID == "12"] <- "L"
All_rules$ID[All_rules$ID == "13"] <- "M"
All_rules$ID[All_rules$ID == "14"] <- "N"
All_rules$ID[All_rules$ID == "15"] <- "O"

#Reorder columns 
All_rules <- All_rules[,c(13,1:12,14:17)]

#View Table
All_rules

#One pass visualisations
#produce a bar plot showing the frequency of each rule
Match_ID_onepass$id <- "Match"
Simulation_ID_onepass$id <- "Match Simulation"
SSG_ID_onepass$id <- "SSG"

Onepass_combo <- rbind(Match_ID_onepass, Simulation_ID_onepass, SSG_ID_onepass)

Onepass_combo$freq <- Onepass_combo$freq * 100


one_combo <- spread(Onepass_combo, id, freq)



#one pass confidence
All_rules_confidence <- subset(All_rules, select = c(1,7,11,15))

All_rules_confidence_difference <- All_rules_confidence %>%
  mutate(SimCon = `Match - Confidence` - `Simulation - Confidence`) %>%
  mutate(SSGCon = `Match - Confidence` - `SSG - Confidence`) %>%
  select(ID, SimCon, SSGCon)

All_rules_confidence_difference <- gather(All_rules_confidence_difference, `SimCon`, `SSGCon`, key = "Group", value = "Confidence")

All_rules_confidence_threegroups <- gather(All_rules_confidence, `Match - Confidence`, `Simulation - Confidence`, `SSG - Confidence`, key = "Group", value = "Confidence")

ggplot(All_rules_confidence_threegroups, aes(x = ID, y = Confidence, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Rule ID") +
  ylab("Confidence")

#one pass support
All_rules_support <- subset(All_rules, select = c(1,6,10,14))

All_rules_support_difference <- All_rules_support %>%
  mutate(SimSup = `Match - Support` - `Simulation - Support`) %>%
  mutate(SSGSup = `Match - Support` - `SSG - Support`) %>%
  select(ID, SimSup, SSGSup)

All_rules_support_difference <- gather(All_rules_support_difference, `SimSup`, `SSGSup`, key = "Group", value = "Support")





#Match minus Sim & SSG game to see the difference
SSTM <- data.matrix(SSG_ID_twopasses)
MTM <- data.matrix(Match_ID_twopasses)
SIT <- data.matrix(Simulation_ID_twopasses)
MIMSIT <- round(MTM - SIT,5)
MIMSSTM <- round(MTM - SSTM,5)



#Calculate observed frequency of third disposal effectiveness 

#SSG
SSG_two_three <- merge(SSG_ID_twopasses_gather, SSG_ID_threepasses_eff_2, by = c("first", "second"), all=TRUE)

SSG_two_three <- SSG_two_three %>%
  mutate(frequ = round(freq * 100,2))

SSG_two_three$first <- as.factor(SSG_two_three$first)
SSG_two_three$first <- factor(SSG_two_three$first, levels=rev(levels(SSG_two_three$first)))



#Competiton Matchs
Match_two_three <- merge(Match_ID_twopasses_gather, Match_ID_threepasses_eff_2, by = c("first", "second"), all=TRUE)

Match_two_three <- Match_two_three %>%
  mutate(frequ = round(freq * 100,2))

Match_two_three$first <- as.factor(Match_two_three$first)
Match_two_three$first <- factor(Match_two_three$first, levels=rev(levels(Match_two_three$first)))


#Match Simulation
Simulation_two_three <- merge(Simulation_ID_twopasses_gather, Simulation_ID_threepasses_eff_2, by = c("first", "second"), all=TRUE)

Simulation_two_three <- Simulation_two_three %>%
  mutate(frequ = round(freq * 100,2))

Simulation_two_three$first <- as.factor(Simulation_two_three$first)
Simulation_two_three$first <- factor(Simulation_two_three$first, levels=rev(levels(Simulation_two_three$first)))





#FIGURES

#Figure 1
SupDif <- ggplot(All_rules_support_difference, aes(x = reorder(ID, desc(ID)), y = Support , fill = Group))+
  geom_bar(stat = "identity", position = "dodge") +
  ylim(-0.24,0.24) +
  theme_minimal() +
  xlab("Rule ID") +
  ylab("Support") +
  coord_flip() +
  scale_fill_discrete(name = "", labels = c("Simulation", "SSG"))

ConDif <- ggplot(All_rules_confidence_difference, aes(x = reorder(ID, desc(ID)), y = Confidence, fill = Group))+
  geom_bar(stat = "identity", position = "dodge") +
  ylim(-0.24,0.24) +
  theme_minimal() +
  xlab("Rule ID") +
  ylab("Confidence") +
  coord_flip() +
  scale_fill_discrete(name = "", labels = c("Simulation", "SSG"))

ggarrange(SupDif, ConDif, labels = c('A', 'B'), align="hv", nrow=2, common.legend = T, legend = "bottom")



#Figure 2 - Observed Frequency between environments
LaMat <- ggplot(data = subset(Match_two_three, !is.na(second)), aes(x = second, y = first, group = Com, label = round(Com, digits = 0))) +
  geom_point(aes(colour = Com), size = 13.5) +
  geom_text() +
  scale_colour_gradient2(low = "orange", mid ="white", high = "lightblue", midpoint = 67.5, limits = c(45,90)) +
  geom_point(shape = 1, colour = "black", size = 13.5, limits = c(45,90)) +
  theme_minimal() +
  scale_x_discrete(position = "top") +
  labs(x = "Second Pass", 
       y = "First Pass",
       colour = "Effectiveness (%)") +
  guides(size=FALSE)


LaSim <- ggplot(data = subset(Simulation_two_three, !is.na(second)), aes(x = second, y = first, group = Com, label = round(Com, digits = 0))) + 
  geom_point(aes(size = Com, colour = Com)) +
  geom_text() +
  scale_colour_gradient2(low = "orange", mid ="white", high = "lightblue", midpoint = 67.5, limits = c(45,90)) +
  geom_point(shape = 1, colour = "black", range = c(6, 13.5), limits = c(45,90)) +
  theme_minimal() +
  labs(x = "Second Pass", 
       y = "First Pass",
       colour = "Effectiveness (%)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  guides(size=FALSE)

LaSS <- ggplot(data = subset(SSG_two_three, !is.na(second)), aes(x = second, y = first, group = Com, label = round(Com, digits = 0))) + 
  geom_point(aes(size = Com, colour = Com)) +
  geom_point(shape = 1, colour = "black", range = c(6, 13.5), limits = c(45,90)) +
  scale_colour_gradient2(low = "orange", mid ="white", high = "lightblue", midpoint = 67.5, limits = c(45,90)) +
  geom_text() +
  theme_minimal() +
  labs(x = "Second Pass", 
       y = "First Pass",
       colour = "Effectiveness (%)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  guides(size=FALSE) 


ggarrange(LaMat, LaSim, LaSS, labels = c('A', 'B', 'C'), align="hv", nrow=3, common.legend = T)



#Figure 3 - Density Plot
ggplot() + 
  geom_density(data = Simulation_two_three, aes(x=Com), alpha=.2, fill="red") +
  geom_density(data = SSG_two_three, aes(x=Com), alpha=.2, fill="blue") +
  geom_density(data = Match_two_three, aes(x=Com), alpha=.2, fill="green") +
  theme_minimal() +
  labs(x = "Third Disposal Effectiveness (%)")
