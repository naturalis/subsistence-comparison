#load libraries
library(phylolm)


#WRITING
##Cleaning up SCCS149: Writing (binary)
###Load in D-PLACE variable
writing.df <- read.csv("../data/d-place/SCCS149_Writing and Records.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(writing.df)[names(writing.df) == "society_id"] <- "Society"
names(writing.df)[names(writing.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
writing.df <- writing.df[(writing.df$Name %in% VIF.variables.df$Name),]

###change the data into a binary variable, with writing (1) and non-writing (0).
writing.df$code[writing.df$code==1] <- 0
writing.df$code[writing.df$code==2] <- 0
writing.df$code[writing.df$code==3] <- 0
writing.df$code[writing.df$code==4] <- 1
writing.df$code[writing.df$code==5] <- 1

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
writing.data <- (writing.df[,c(1:2,4,8)])
writing.data <- merge(as.data.frame(writing.data), as.data.frame(VIF.variables.df), by='Society', all=TRUE)
writing.data <- (writing.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(writing.data) <- writing.data$language_glottocode




#MONEY
##Cleaning up SCCS155: Money (binary)
###Load in D-PLACE variable
money.df <- read.csv("../data/d-place/SCCS155_Money.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(money.df)[names(money.df) == "society_id"] <- "Society"
names(money.df)[names(money.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
money.df <- money.df[(money.df$Name %in% VIF.variables.df$Name),]

###change the data into a binary variable, with none (0) and some form of monetary currency (1).
money.df$code[money.df$code==1] <- 0
money.df$code[money.df$code==2] <- 1
money.df$code[money.df$code==3] <- 1
money.df$code[money.df$code==4] <- 1
money.df$code[money.df$code==5] <- 1

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
money.data <- (money.df[,c(1:2,4,8)])
money.data <- merge(as.data.frame(money.data), as.data.frame(VIF.variables.df), by='Society', all=TRUE)
money.data <- (money.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(money.data) <- money.data$language_glottocode




#FOREIGN DISEASES
##Cleaning up SCCS1834: Introduction of Foreign Diseases (binary)
###Load in D-PLACE variable
foreign.diseases.df <- read.csv("../data/d-place/SCCS1834_Introduction of Foreign Diseases.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(foreign.diseases.df)[names(foreign.diseases.df) == "society_id"] <- "Society"
names(foreign.diseases.df)[names(foreign.diseases.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
foreign.diseases.df <- foreign.diseases.df[(foreign.diseases.df$Name %in% VIF.variables.df$Name),]

###Data is already in binary format, with (0) being none and (1) being 
###the presence of foreign diseases.

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
foreign.diseases.data <- (foreign.diseases.df[,c(1:2,4,8)])
foreign.diseases.data <- merge(as.data.frame(foreign.diseases.data), as.data.frame(VIF.variables.df), by='Society', all=F)
foreign.diseases.data <- (foreign.diseases.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(foreign.diseases.data) <- foreign.diseases.data$language_glottocode




#WARFARE
##Cleaning up SCCS1648: Frequency of warfare (continuous)
###Load in D-PLACE variable
warfare.df <- read.csv("../data/d-place/SCCS1648_Overall Frequency of warfare.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(warfare.df)[names(warfare.df) == "society_id"] <- "Society"
names(warfare.df)[names(warfare.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
warfare.df <- warfare.df[(warfare.df$Name %in% VIF.variables.df$Name),]

###warfare is not a binary state but rather continuous from 1 to 18 
###(or from 1 to 5 at 0.25 intervals - with the addition of 4, which 
###is inserted between 1.5 and 1.75 (at 1.625)).
warfare.df$code[warfare.df$code==1] <- 1
warfare.df$code[warfare.df$code==2] <- 1.25
warfare.df$code[warfare.df$code==3] <- 1.5
warfare.df$code[warfare.df$code==4] <- 1.65
warfare.df$code[warfare.df$code==5] <- 1.75
warfare.df$code[warfare.df$code==6] <- 2
warfare.df$code[warfare.df$code==7] <- 2.25
warfare.df$code[warfare.df$code==8] <- 2.5
warfare.df$code[warfare.df$code==9] <- 2.75
warfare.df$code[warfare.df$code==10] <- 3
warfare.df$code[warfare.df$code==11] <- 3.25
warfare.df$code[warfare.df$code==12] <- 3.5
warfare.df$code[warfare.df$code==13] <- 3.75
warfare.df$code[warfare.df$code==14] <- 4
warfare.df$code[warfare.df$code==15] <- 4.25
warfare.df$code[warfare.df$code==16] <- 4.5
warfare.df$code[warfare.df$code==17] <- 4.75
warfare.df$code[warfare.df$code==18] <- 5

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
warfare.data <- (warfare.df[,c(1:2,4,8)])
warfare.data <- merge(as.data.frame(warfare.data), as.data.frame(VIF.variables.df), by='Society', all=F)
warfare.data <- (warfare.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(warfare.data) <- warfare.data$language_glottocode




#COMMUNITY.SIZE
##Cleaning up SCCS235: Mean size of local communities (continuous)
###Load in D-PLACE variable
community.size.df <- read.csv("../data/d-place/SCCS235_Mean size local communities.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(community.size.df)[names(community.size.df) == "society_id"] <- "Society"
names(community.size.df)[names(community.size.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
community.size.df <- community.size.df[(community.size.df$Name %in% VIF.variables.df$Name),]

###community.size is not a binary state but rather continuous from 
###<50 to 50,000+. We will be taking the midpoint of all these ranges 
###to simulate continuous data (<50 = 25, ).
community.size.df$code[community.size.df$code==1] <- 25
community.size.df$code[community.size.df$code==2] <- 75
community.size.df$code[community.size.df$code==3] <- 150
community.size.df$code[community.size.df$code==4] <- 300
community.size.df$code[community.size.df$code==5] <- 700
community.size.df$code[community.size.df$code==6] <- 2500
community.size.df$code[community.size.df$code==7] <- 27500
community.size.df$code[community.size.df$code==8] <- 50000

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
community.size.data <- (community.size.df[,c(1:2,4,8)])
community.size.data <- merge(as.data.frame(community.size.data), as.data.frame(VIF.variables.df), by='Society', all=F)
community.size.data <- (community.size.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(community.size.data) <- community.size.data$language_glottocode




#POP.DENSITY
##Cleaning up SCCS1130 Population density (continuous)
###Load in D-PLACE variable
pop.density.df <- read.csv("../data/d-place/SCCS1130_Population Density.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(pop.density.df)[names(pop.density.df) == "society_id"] <- "Society"
names(pop.density.df)[names(pop.density.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
pop.density.df <- pop.density.df[(pop.density.df$Name %in% VIF.variables.df$Name),]

###pop.density is not a binary state but rather categorical, with a 
###range of "<1 per square mile" up to "500+ per square mile". We 
###will be taking the midpoint of all these range categories to simulate 
###continuous data (all ranges are per square mile: <1 = 0.5, 1-4.9 = 3,
###5-24.9 = 15, 25-99.9 = 62.5, 100-499 = 299.5, 500+ = 500).
pop.density.df$code[pop.density.df$code==2] <- 0.5
pop.density.df$code[pop.density.df$code==3] <- 3
pop.density.df$code[pop.density.df$code==4] <- 15
pop.density.df$code[pop.density.df$code==5] <- 62.5
pop.density.df$code[pop.density.df$code==6] <- 299.5
pop.density.df$code[pop.density.df$code==7] <- 500

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
pop.density.data <- (pop.density.df[,c(1:2,4,8)])
pop.density.data <- merge(as.data.frame(pop.density.data), as.data.frame(VIF.variables.df), by='Society', all=F)
pop.density.data <- (pop.density.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(pop.density.data) <- pop.density.data$language_glottocode




#PATHOGEN.STRESS
##Cleaning up SCCS1260 Total Pathogen Stress (continuous)
###Load in D-PLACE variable
pathogen.stress.df <- read.csv("../data/d-place/SCCS1260_Total Pathogen Stress.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(pathogen.stress.df)[names(pathogen.stress.df) == "society_id"] <- "Society"
names(pathogen.stress.df)[names(pathogen.stress.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
pathogen.stress.df <- pathogen.stress.df[(pathogen.stress.df$Name %in% VIF.variables.df$Name),]

###pathogen.stress is not a binary state but rather continuous count data,
###with a range of "7" to "21". These numbers are derived from the 
###presence and severity of the following pathogens: Leishmanias, 
###Trypanosomes, Malaria, Schistosomes, Filariae, Spirochetes, and 
###Leprosy (SCCS variables 1253-1259); each is scored "1" for "absence", 
###"2" for "presence, non-serious threat", and "3" for "presence, 
###serious threat". Thus the higher the score the greater the total 
###pathogen stress on the society.

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
pathogen.stress.data <- (pathogen.stress.df[,c(1:2,4,8)])
pathogen.stress.data <- merge(as.data.frame(pathogen.stress.data), as.data.frame(VIF.variables.df), by='Society', all=F)
pathogen.stress.data <- (pathogen.stress.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(pathogen.stress.data) <- pathogen.stress.data$language_glottocode




#FAMINE
##Cleaning up SCCS1265: Occurrences of Famine (binary)
###Load in D-PLACE variable
famine.df <- read.csv("../data/d-place/SCCS1265_Occurrences of Famine.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(famine.df)[names(famine.df) == "society_id"] <- "Society"
names(famine.df)[names(famine.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
famine.df <- famine.df[(famine.df$Name %in% VIF.variables.df$Name),]

###change the data into a binary variable, with low and very low as (0) and high and very high as (1).
famine.df$code[famine.df$code==1] <- 0
famine.df$code[famine.df$code==2] <- 0
famine.df$code[famine.df$code==3] <- 1
famine.df$code[famine.df$code==4] <- 1


###remove all unneeded columns and combine with societal niche data in VIF.variables.df
famine.data <- (famine.df[,c(1:2,4,8)])
famine.data <- merge(as.data.frame(famine.data), as.data.frame(VIF.variables.df), by='Society', all=F)
famine.data <- (famine.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(famine.data) <- famine.data$language_glottocode



#POLITICAL.INTEGRATION
##Cleaning up SCCS157: Political Integration (binary)
###Load in D-PLACE variable
political.integration.df <- read.csv("../data/d-place/SCCS157_Political Integration.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(political.integration.df)[names(political.integration.df) == "society_id"] <- "Society"
names(political.integration.df)[names(political.integration.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
political.integration.df <- political.integration.df[(political.integration.df$Name %in% VIF.variables.df$Name),]

###change the data into a binary variable, with "None" and "Autonomous local communities" (0), and "1 level -","2 levels -" and "3 levels above local community" (1)
political.integration.df$code[political.integration.df$code==1] <- 0
political.integration.df$code[political.integration.df$code==2] <- 0
political.integration.df$code[political.integration.df$code==3] <- 1
political.integration.df$code[political.integration.df$code==4] <- 1
political.integration.df$code[political.integration.df$code==5] <- 1

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
political.integration.data <- (political.integration.df[,c(1:2,4,8)])
political.integration.data <- merge(as.data.frame(political.integration.data), as.data.frame(VIF.variables.df), by='Society', all=F)
political.integration.data <- (political.integration.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(political.integration.data) <- political.integration.data$language_glottocode




#SOCIAL.STRATIFICATION
##Cleaning up SCCS158: Social Stratification (binary)
###Load in D-PLACE variable
social.strat.df <- read.csv("../data/d-place/SCCS158_Social Stratification.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(social.strat.df)[names(social.strat.df) == "society_id"] <- "Society"
names(social.strat.df)[names(social.strat.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
social.strat.df <- social.strat.df[(social.strat.df$Name %in% VIF.variables.df$Name),]

###change the data into a binary variable, with 2+ classes as (1) and egalitarian/1 class societies as (0). (For both the "0" and "1" state here, the presence of slaves is not considered, such that it is possible for a 1 class society to also possess slaves.)
social.strat.df$code[social.strat.df$code==1] <- 0
social.strat.df$code[social.strat.df$code==2] <- 0
social.strat.df$code[social.strat.df$code==3] <- 1
social.strat.df$code[social.strat.df$code==4] <- 1
social.strat.df$code[social.strat.df$code==5] <- 1

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
social.strat.data <- (social.strat.df[,c(1:2,4,8)])
social.strat.data <- merge(as.data.frame(social.strat.data), as.data.frame(VIF.variables.df), by='Society', all=F)
social.strat.data <- (social.strat.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(social.strat.data) <- social.strat.data$language_glottocode




#VIOLENCE
##Cleaning up SCCS906: Violence as a solution to problems (binary)
###Load in D-PLACE variable
violence.df <- read.csv("../data/d-place/SCCS906_Violence as a solution to problems.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(violence.df)[names(violence.df) == "society_id"] <- "Society"
names(violence.df)[names(violence.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
violence.df <- violence.df[(violence.df$Name %in% VIF.variables.df$Name),]

###The data is already in binary, we will change "no" to (0) and "yes" will remain as (1).
violence.df$code[violence.df$code==2] <- 0

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
violence.data <- (violence.df[,c(1:2,4,8)])
violence.data <- merge(as.data.frame(violence.data), as.data.frame(VIF.variables.df), by='Society', all=F)
violence.data <- (violence.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(violence.data) <- violence.data$language_glottocode



#Revenge
##Cleaning up SCCS914: Revenge as motive for conflict (binary)
###Load in D-PLACE variable
revenge.df <- read.csv("../data/d-place/SCCS914_Revenge as motive for conflict.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(revenge.df)[names(revenge.df) == "society_id"] <- "Society"
names(revenge.df)[names(revenge.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
revenge.df <- revenge.df[(revenge.df$Name %in% VIF.variables.df$Name),]

###The data is already in binary, we will change "absent or not mentioned" to (0) and "present" will remain as (1).
revenge.df$code[revenge.df$code==2] <- 0

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
revenge.data <- (revenge.df[,c(1:2,4,8)])
revenge.data <- merge(as.data.frame(revenge.data), as.data.frame(VIF.variables.df), by='Society', all=F)
revenge.data <- (revenge.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(revenge.data) <- revenge.data$language_glottocode




#DEFENCE
##Cleaning up SCCS915: Defence as motive for conflict (binary)
###Load in D-PLACE variable
defence.df <- read.csv("../data/d-place/SCCS915_Defence as motive for conflict.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(defence.df)[names(defence.df) == "society_id"] <- "Society"
names(defence.df)[names(defence.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
defence.df <- defence.df[(defence.df$Name %in% VIF.variables.df$Name),]

###The data is already in binary, we will change "absent or not mentioned" to (0) and "present" will remain as (1).
defence.df$code[defence.df$code==2] <- 0

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
defence.data <- (defence.df[,c(1:2,4,8)])
defence.data <- merge(as.data.frame(defence.data), as.data.frame(VIF.variables.df), by='Society', all=F)
defence.data <- (defence.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(defence.data) <- defence.data$language_glottocode



#DISASTER
##Cleaning up SCCS1684: Threat of Weather or Pest Disasters (binary)
###Load in D-PLACE variable
disasters.df <- read.csv("../data/d-place/SCCS1684_Threat of Weather of Pest Disasters.csv")

###Give every row the name of the relevant society_id and rename the column "society_id" "Society"
names(disasters.df)[names(disasters.df) == "society_id"] <- "Society"
names(disasters.df)[names(disasters.df) == "society_name"] <- "Name"

###remove all information relating to societies we are not interested in
disasters.df <- disasters.df[(disasters.df$Name %in% VIF.variables.df$Name),]

###change the data into a binary variable, with "No resolved rating" and "Don't Know" being removed ("NA"), a "low threat of severe natural disrupters of the food supply" ("0") and "moderate to high threat of severe natural disrupters of food supply" given ("1"). 
disasters.df$code[disasters.df$code==0] <- NA
disasters.df$code[disasters.df$code==1] <- 0
disasters.df$code[disasters.df$code==2] <- 1
disasters.df$code[disasters.df$code==3] <- 1
disasters.df$code[disasters.df$code==4] <- 1
disasters.df$code[disasters.df$code==8] <- NA

###remove "NA" values
disasters.df <- disasters.df[!(is.na(disasters.df$code)),]

###remove all unneeded columns and combine with societal niche data in VIF.variables.df
disasters.data <- (disasters.df[,c(1:2,4,8)])
disasters.data <- merge(as.data.frame(disasters.data), as.data.frame(VIF.variables.df), by='Society', all=F)
disasters.data <- (disasters.data[,-c(5:6)])

###rename the rows for the glottocodes, as found in the language tree
row.names(disasters.data) <- disasters.data$language_glottocode