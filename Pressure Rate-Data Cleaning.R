library(stringr)


#GOAL IS TO PREDICT PASS PRESSURE from a standard drop back pass play
#Here we prep the data



#Changing the edge types of plays to either pass or rush. All others, like fumbles, interceptions or other types which are not traditional pass or rush plays are eliminated
Charting$PLTYPE<- ifelse(Charting$PLTYPE == '2pt pass', 'pass', ifelse(Charting$PLTYPE == '2pt rush', 'rushed', Charting$PLTYPE))
Charting$PLTYPE<- ifelse(Charting$PLTYPE == 'post-play fumble', 'fumble', ifelse(Charting$PLTYPE == 'pre-play fumble', 'fumble', ifelse(Charting$PLTYPE == 'pre-punt fumble', 'fumble', ifelse(Charting$PLTYPE == 'rush', 'rushed', Charting$PLTYPE))))
Charting$PLTYPE<- ifelse(Charting$PLTYPE == 'penalties offsetting', 'penalty', ifelse(Charting$PLTYPE == 'penalty declined', 'penalty', ifelse(Charting$PLTYPE == 'penalty superseded', 'penalty', ifelse(Charting$PLTYPE == 'post-abort pass', 'pass', Charting$PLTYPE))))
head(Charting)                       
table(Charting$PLTYPE)
#Sorting data set by teams, year, week, time
Charting <- Charting[order(Charting$Year, Charting$WEEK, Charting$HOME, Charting$QTR, -Charting$Time),]
#Eliminate all penalty plays and fumbles(these are essentially duplicates of a play)
Charting<- Charting[!(Charting$PLTYPE=='penalty'| Charting$PLTYPE=='fumble'),]


table(Charting$PLTYPE)

#filling plays with no marking for pass pressure as 0
Charting$PP[is.na(Charting$PP)] <- 0

#Creates a variable that is coded with 1 if the team's prior play was a rush
for (i in 2:length(Charting$R)) {
  Charting$Rush[i] = ifelse( Charting$Team[i] == Charting$Team[i-1] & Charting$PLTYPE[i-1] == 'rushed',1, 0)
}

#Creates a variable that is coded with 1 if the team's prior play was a pass
for (i in 2:length(Charting$R)) {
  Charting$Pass[i] = ifelse( Charting$Team[i] == Charting$Team[i-1] & Charting$PLTYPE[i-1] == 'pass',1, 0)
}

#Creates a variable that is records the yardage gained(or lost) on the team's prior play
Charting$YARDS[is.na(Charting$YARDS)] <- 4

for (i in 2:length(Charting$R)) {
  Charting$Yardage[i] = ifelse( Charting$Team[i] == Charting$Team[i-1], Charting$YARDS[i-1], 4)
}

#Creates a variable that is coded with 1 if the team's prior play was resulted in pressure
for (i in 2:length(Charting$R)) {
  Charting$Autocorrelation[i] = ifelse( Charting$Team[i] == Charting$Team[i-1] & Charting$PP[i-1] == 1, 1, 0)
}


#Fill missing values with 0
Charting$Pass[is.na(Charting$Pass)] <- 0
Charting$Rush[is.na(Charting$Rush)] <- 0


#Changes the time variable from minutes remaining in a quarter to minutes remaining in the entire game
Charting$Times_1<- ifelse(Charting$QTR == 1, Charting$Times + 45,ifelse(Charting$QTR == 2,Charting$Times + 30, ifelse( Charting$QTR == 3, Charting$Times + 15, Charting$Times )))

#Used in the calculation of win probability. The score variable here refers to the pre-game spread
Charting$Scores<- ifelse(Charting$Score > 0, log(Charting$Score), ifelse(Charting$Score < 0, -log(abs(Charting$Score)), 0))
Charting$GAPS<- ifelse(Charting$GAP > 0, log(Charting$GAP), ifelse(Charting$GAP < 0, -log(abs(Charting$GAP)), 0))

#Very simple win probabiity model. The logs help smooth some of the severe jumps with early score or incorrect spreads
Charting$WP<- (1-pnorm((-3*Charting$GAPS + 0.5) * 2.5*log(60/Charting$Times_1),-1.15*Charting$Scores * (Charting$Times_1 / 60), 13.45/(sqrt(60/Charting$Times_1)), lower.tail = TRUE)) + .5 * (pnorm((-3*Charting$GAPS + 0.5) * 2.5*log(60/Charting$Times_1),
                                                                                                                                                                                                     -1.15*Charting$Scores * (Charting$Times_1 / 60), 13.45/(sqrt(60/Charting$Times_1)), lower.tail = TRUE)
                                                                                                                                                                                               - pnorm((-3*Charting$GAPS + 0.5) * 2.5*log(60/Charting$Times_1),-1.15*Charting$Scores * (Charting$Times_1 / 60), 13.45/(sqrt(60/Charting$Times_1)), lower.tail = TRUE))

sum(is.na(Charting$WP))

Charting$OFFENSE<- as.character(Charting$OFFENSE)
Charting$DEFENSE<- as.character(Charting$DEFENSE)
Charting$HOME<- as.character(Charting$HOME)

#Changes the St.Louis Rams to the LA Rams and the San Diego Chargers to the LA Chargers
Charting$OFFENSE<- ifelse(Charting$OFFENSE == 'STL', 'LARM', ifelse(Charting$OFFENSE == 'SD', 'LAC', Charting$OFFENSE))
Charting$DEFENSE<- ifelse(Charting$DEFENSE == 'STL', 'LARM', ifelse(Charting$DEFENSE == 'SD', 'LAC', Charting$DEFENSE))
Charting$HOME<- ifelse(Charting$HOME == 'STL', 'LARM', ifelse(Charting$HOME == 'SD', 'LAC', Charting$HOME))
table(Charting$OFFENSE)


## We are now going to scrub the list of players who threw passes so that we're left with just quarterbacks. The first issue is that 
#the players have a number, a dash, and then the name. 
sum(is.na(Charting$PLAYER))
Charting$PLAYER<- as.character(Charting$PLAYER)
sort(table(Charting$PLAYER),decreasing=F)
#Once we remove the number and dash, we will have only the first letter of the first name and the last name. That will result in D.Carr, for both David and Derrick
#Since they are two different players, we need to distinguish the two
#DA refers to David Carr
Charting$PLAYER[Charting$PLAYER=="5-D.Carr"]<-"5-Da.Carr"
Charting$PLAYER[Charting$PLAYER=="8-D.Carr"]<-"8-Da.Carr"
sort(table(Charting$PLAYER))

x<- str_split_fixed(Charting$PLAYER, "-", 2)
Charting$QBR<- x[,2]

sort(table(Charting$QBR), decreasing = F)
#There are a few missups on the player list. For example, Matt Moore is M.Moore and also Ma.Moore. Need to consolidate
Charting$QBR<- as.character(Charting$QBR)
Charting$QBR[Charting$QBR=="Ma.Moore"]<-"M.Moore"
Charting$QBR[Charting$QBR=="Matt.Moore"]<-"M.Moore"
Charting$QBR[Charting$QBR=="Sh.Hill"]<-"S.Hill"
Charting$QBR[Charting$QBR=="Jo.Freeman"]<-"J.Freeman"
Charting<- Charting[,-1]
sort(table(Charting$QBR), decreasing = F)
table(Charting$RECEPT)
#We need to get rid of Aborted Snaps.  The lost yardage is important for our yards variable, but aborted snaps leave no chance to achieve pressure
Charting<- Charting[!(Charting$RECEPT=='aborted snap'),]

sort(table(Charting$QBR), decreasing = F)
Charting$QBR<- factor(Charting$QBR)

#Next, we want to condense the number of player who threw passes. Since we care about standard drop back pass plays, we want to get rid of pass plays by wide receivers, running backs, etc etc.
#We rename those players 'Other'
levels(Charting$QBR)[rank(table(Charting$QBR)) < 183] <- "Other"
Charting$QBR<- as.character(Charting$QBR)

sort(table(Charting$QBR), decreasing = F)

#in order to rename and see the table, we have to switch back between the variable being a factor and a string
Charting$QBR[Charting$QBR=="R.Brown"]<-"Other"
Charting$QBR[Charting$QBR=="J.Cribbs"]<-"Other"

Charting$QBR<- as.character(Charting$QBR)
sort(table(Charting$QBR), decreasing = F)

Charting$QBR<- factor(Charting$QBR)
#Next, since third stringers make spot appearances here and there, we can consolidate such a group into a type we call 'bench'
#Attempts threshold was set at 50
levels(Charting$QBR)[rank(table(Charting$QBR)) < 42] <- "bench"
Charting$QBR<- as.character(Charting$QBR)
Charting$QBR[Charting$QBR=="J.Webb"]<-"bench"

sort(table(Charting$QBR), decreasing = F)

Charting$QBR<- factor(Charting$QBR)
#Next are primarily backup quarterbacks. Across 10 seasons of data, these are players who are primarily backups who have never started for prolonged periors of time
#Determined by if the minimum # of attempts is less than 300
levels(Charting$QBR)[rank(table(Charting$QBR)) < 29] <- "backup"
Charting$QBR<- as.character(Charting$QBR)
sort(table(Charting$QBR), decreasing = F)

table(Charting$RECEPT)


Charting$QBR<- factor(Charting$QBR)
Charting <- Charting[order(Charting$Year,Charting$WEEK,Charting$OFFENSE, Charting$QTR, Charting$R),] 
Charting$Year<- as.numeric(Charting$Year)
Charting$WEEK<- as.numeric(Charting$WEEK)
Charting$OFFENSE<- as.character(Charting$OFFENSE)
Charting$DEFENSE<- as.character(Charting$DEFENSE)



head(Charting)
#We create a variable that measures length of down and distance. Here, this helps capture some of the qualitative differences in distance
#We know teams treat distances differently than in just a continuous fashion. We know play calling significantly changes between 3 yards to go and 5 yards to go versus
#20 yards to go and 22 yards to go. 
Charting$Distance<- ifelse(Charting$TOGO <4, 'Short', ifelse(Charting$TOGO >3 & Charting$TOGO < 7, 'Medium', ifelse(Charting$TOGO > 6 & Charting$TOGO < 11, 'Long', ifelse(
  Charting$TOGO > 10 & Charting$TOGO < 16, 'Longer', ifelse(Charting$TOGO > 15 & Charting$TOGO < 21, 'Very Long', ifelse(Charting$TOGO > 20 & Charting$TOGO <26, 'Too Long','Forever')
  )))))

Charting$DOWN<- as.factor(Charting$DOWN)

table(Charting$OFFENSE)

#There are some games played where neither team had home field advantage - think games played in London. Instead, we code if the team on offense was at home with 1 and a 0 otherwise.
Charting$H<- ifelse(Charting$Year == 2007 & (Charting$OFFENSE == 'MIA' | Charting$OFFENSE == 'NYG'), 0,
                    ifelse(Charting$Year == 2008 & (Charting$OFFENSE == 'LAC' | Charting$OFFENSE == 'NO'),0,
                           ifelse(Charting$Year == 2009 & (Charting$OFFENSE == 'NE'|  Charting$OFFENSE == 'TB'),0,
                                  ifelse(Charting$Year == 2010 & (Charting$OFFENSE == 'DEN'|  Charting$OFFENSE == 'SF'),0,
                                         ifelse(Charting$Year == 2011 & (Charting$OFFENSE == 'CHI'|  Charting$OFFENSE == 'TB'),0,
                                                ifelse(Charting$Year == 2012 & (Charting$OFFENSE == 'NE'|  Charting$OFFENSE == 'LARM'),0,
                                                       ifelse(Charting$Year == 2013 & (Charting$OFFENSE == 'PIT' | Charting$OFFENSE == 'MIN'),0,
                                                              ifelse(Charting$Year == 2013 & (Charting$OFFENSE == 'SF' | Charting$OFFENSE == 'JAC'),0,
                                                                     ifelse(Charting$Year == 2014 & (Charting$OFFENSE == 'MIA' | Charting$OFFENSE == 'OAK'),0,
                                                                            ifelse(Charting$Year == 2014 & (Charting$OFFENSE == 'DET' | Charting$OFFENSE == 'ATL'),0,
                                                                                   ifelse(Charting$Year == 2014 & (Charting$OFFENSE == 'DAL' | Charting$OFFENSE == 'JAC'),0,
                                                                                          ifelse(Charting$Year == 2015 & (Charting$OFFENSE == 'NYJ' | Charting$OFFENSE == 'MIA'),0,
                                                                                                 ifelse(Charting$Year == 2015 & (Charting$OFFENSE == 'BUF' | Charting$OFFENSE == 'JAC'),0,
                                                                                                        ifelse(Charting$Year == 2015 & (Charting$OFFENSE == 'DET' | Charting$OFFENSE == 'KC'),0,
                                                                                                               ifelse(Charting$OFFENSE == Charting$HOME,1,0)))))))))))))))
sort(table(Charting$QBR), decreasing = FALSE)

Charting$`XTRA NOTE`<- as.character(Charting$`XTRA NOTE`)
head(Charting)
table(Charting$`XTRA NOTE`)

#Some plays feature gadget or wildcat plays. These are usuall whacky plays where the formation is wrong or some sort of trickery
Charting$Gadget<- ifelse(Charting$`XTRA NOTE` == '(6-P.White QB)'| Charting$`XTRA NOTE` =='(7-M.Vick QB)'|
                           Charting$`XTRA NOTE` =='(7-M.Vick QB)' | Charting$`XTRA NOTE` == '(Field Goal formation)' | 
                           Charting$`XTRA NOTE` =='(Punt formation)' | Charting$`XTRA NOTE` =='(Shotgun, 15-T.Tebow QB)'| Charting$`XTRA NOTE`== '(Shotgun, 7-M.Vick QB)'|Charting$`XTRA NOTE`=='(Wildcat)' |Charting$`XTRA NOTE`=='(Wildcat, 7-M.Vick QB)' |
                           Charting$`XTRA NOTE`=='(Wildcat)/Flea Flicker', 1, 0)


#We code which types of passes were fleak flickers
Charting$Flea_Flicker<- ifelse(Charting$`XTRA NOTE` == 'Flea flicker'| Charting$`XTRA NOTE` =='Flea Flicker'|
                                 Charting$`XTRA NOTE` =='(No Huddle)/Flea Flicker' | Charting$`XTRA NOTE` == '(Shotgun, Flea Flicker)' | 
                                 Charting$`XTRA NOTE` =='(Shotgun)/Flea Flicker' | Charting$`XTRA NOTE` =='(Wildcat)/Flea Flicker', 1, 0)

#We code which types of passes were in Shotgun
Charting$Shotgun<- ifelse(Charting$`XTRA NOTE` == ' (No Huddle, Shotgun)'| Charting$`XTRA NOTE` =='(Shotgun)'|
                            Charting$`XTRA NOTE` =='(Shotgun, 15-T.Tebow QB)' | Charting$`XTRA NOTE` == '(Shotgun, Flea Flicker)' | 
                            Charting$`XTRA NOTE` =='(Shotgun)/Flea Flicker' | Charting$`XTRA NOTE` =='(Shotgun, 6-P.White QB)' | Charting$`XTRA NOTE` =='(Shotgun, 7-M.Vick QB)', 1, 0)

#We code which types of passes were No Huddle
Charting$NoHuddle<- ifelse(Charting$`XTRA NOTE` == ' (No Huddle, Shotgun)'| Charting$`XTRA NOTE` =='(No Huddle)'|
                             Charting$`XTRA NOTE` =='(No Huddle)/Flea Flicker', 1, 0)
head(Charting$Shotgun)

#We get rid of plays where the pass was thrown by a non quarterback(other)
Charting$Other<- ifelse(Charting$QBR == 'Other',1, 0)
table(Charting$Other)
Charting<- Charting[!Charting$Other == 1, ]
table(Charting$Other)



#Fill any missing plays for gadget, shotgun, flea flicker, and no huddle with 0
Charting$Gadget[is.na(Charting$Gadget)]<- 0
Charting$Shotgun[is.na(Charting$Shotgun)]<- 0
Charting$NoHuddle[is.na(Charting$NoHuddle)]<- 0
Charting$Flea_Flicker[is.na(Charting$Flea_Flicker)]<- 0

#These variables tell us how many running backs, wrs and tight ends were present on the field at the time of the play
mean(Charting$RB, na.rm = TRUE)
mean(Charting$WR, na.rm = TRUE)
mean(Charting$TE, na.rm = TRUE)

#Some plays are missing these variables, so we fill with mean values
Charting$RB[is.na(Charting$RB)]<- 2
Charting$WR[is.na(Charting$WR)]<- 2
Charting$TE[is.na(Charting$TE)]<- 1

#Check if any columns have missing fields
colnames(Charting)[colSums(is.na(Charting)) > 0]

sort(table(Charting$QBR))
#We have a lot of extra columns, We want to create a truncated table but wish to preserve the original data set
Chart<- data.frame(cbind(as.numeric(Charting$PP),Charting$T, Charting$Gadget, Charting$Flea_Flicker, Charting$Shotgun, Charting$NoHuddle, Charting$WEEK, Charting$QTR,
                         Charting$ChartONE, Charting$DOWN, Charting$TOGO, Charting$RB, Charting$WR, Charting$TE, Charting$Autocorrelation, Charting$Offense, Charting$Defense,
                         Charting$WP, Charting$Cold, Charting$Hot, Charting$QBR, Charting$Rush, Charting$Pass, Charting$Yardage, Charting$Distance, Charting$H, Charting$Year, Charting$Times_1))


head(Chart)

#Name our Columns
colnames(Chart)<- c('PP','T','Gadget', 'Flea_Flicker', 'Shotgun', 'NoHuddle', 'WEEK', 'QTR', 'ChartONE','DOWN',
                    'TOGO','RB','WR','TE','Autocorrelation', 'Offense', 'Defense', 'WP', 'Cold', 'Hot',
                    'QBR','Rush','Pass','Yardage', 'Distance', 'H', 'Year', 'Times')

Chart$Times<- as.numeric(as.character(Chart$Times))

str(Chart)

Chart$WP<- as.numeric(as.character(Chart$WP))
summary(Chart$WP)
head(Chart)

head(total3)

#Replace overtime with 4th quarter designation
Chart$QTR<- ifelse(Chart$QTR==5,4,Chart$QTR)
table(Chart$QTR)
Chart$QTR<- as.factor(as.character(Chart$QTR))
Chart$ChartONE<- as.factor(Chart$ChartONE)
Chart$Distance<- as.factor(Chart$Distance)
Chart$DOWN<- as.factor(Chart$DOWN)
Chart$QBR<- as.factor(Chart$QBR)
Chart$Year<- as.factor(as.character(Chart$Year))
#Creates a team Variable
Chart$Team<- as.factor(as.character(paste(as.character(Chart$Year),as.character(Chart$OFFENSE), sep="&")))

#Convert our character variables into Dummy Variables
A<- model.matrix(PP ~ ChartONE + Distance + DOWN + WEEK +  QBR + Team, Chart)

Chart<- data.frame(cbind(Chart, A))
head(Chart)
str(Chart)
#removes intercept
Chart<- Chart[,-29]
Chart<- data.frame(cbind(Chart,))
head(Chart)
head(Chart)

Chart$T<- as.numeric(Chart$T)
Chart$WR<- as.numeric(Chart$WR)
Chart$Offense<- as.numeric(Chart$Offense)
Chart$Defense<- as.numeric(Chart$Defense)
Chart$TOGO<- as.numeric(Chart$TOGO)

str(Chart)

summary(Chart$Times)

Chart$Yardage<- as.numeric(as.character(Chart$Yardage))
Chart$RB<- as.numeric(as.character(Chart$RB))
Chart$TE<- as.numeric(as.character(Chart$TE))
str(Chart)
#Since we created a matrix of dummy variables, we can drop those character variables
Chart<- Chart[,-1]
Chart<- Chart[,-7]
Chart<- Chart[,-7]
Chart<- Chart[,-7]
Chart<- Chart[,-17]
Chart<- Chart[,-20]
Chart<- Chart[,-2]

head(Chart)

#If we are going to run a NN, we need to normalize the data. Turns out, in h20, it defaults to standardizing the data, so this step is unncessary
#Furthermore, h20 also recognizes character and factor variables and turns them into dummies so the prior dummy matrix was unncessary as well
normaliz<- function(x) {
  return ((x - min(x))/ (max(x) -min(x)))
}

str(Chart)

#Normalizes our continuous variables
Chart$TOGO<-normaliz(Chart$TOGO)
Chart$RB<-normaliz(Chart$RB)
Chart$WR<-normaliz(Chart$WR)
Chart$TE<-normaliz(Chart$TE)
Chart$Offense<-normaliz(Chart$Offense)
Chart$Defense<-normaliz(Chart$Defense)
Chart$WP<-normaliz(Chart$WP)
Chart$Yardage<-normaliz(Chart$Yardage)
str(Chart)

