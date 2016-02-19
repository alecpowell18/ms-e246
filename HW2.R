library(ISLR)
?Weekly
View(Weekly)
pairs(Weekly)
#5
#a)
glm.fit = glm(Direction~.-Today-Year, data = Weekly, family = binomial)
#b)
glm.probs = predict (glm.fit, type = "response")
glm.pred = rep("Down",length(glm.probs))
glm.pred [glm.probs > 0.5] = "Up"
table(glm.pred, Weekly$Direction)
#The columns tell you actual values, the rows tell you the predicted values
#c)
train_X = Weekly[Weekly$Year <= 2008,]
test_X = Weekly[Weekly$Year > 2008,]
#compute confusion matrix
glm.fit_1 = glm(Direction~(Lag1+Lag2+Lag3), data = train_X, family = binomial)
glm.probs = predict (glm.fit_1, newdata = test_X, type = "response")
glm.pred = rep("Down",length(glm.probs))
glm.pred [glm.probs > 0.5] = "Up"
table(glm.pred, test_X$Direction)
#d)
library(MASS)
lda.fit = lda(Direction~(Lag1+Lag2+Lag3), data = train_X)
lda.fit.predict = predict(lda.fit, newdata = test_X)
lda.test.conf = table(lda.fit.predict$class, test_X$Direction)
print(lda.test.conf)
lda.predict.ratio = (lda.test.conf[1,1]+lda.test.conf[2,2])/(lda.test.conf[1,1]+lda.test.conf[1,2]+lda.test.conf[2,1]+lda.test.conf[2,2])
print(lda.predict.ratio)
#e)
library(class)
set.seed(2016)
knn.fit = knn(train_X[,2:4], test_X[,2:4], Weekly$Direction[Weekly$Year <= 2008], k = 1)
knn.test.conf = table(knn.fit, test_X$Direction)
print(knn.test.conf)
knn.predict.ratio = (knn.test.conf[1,1]+knn.test.conf[2,2])/(knn.test.conf[1,1]+knn.test.conf[1,2]+knn.test.conf[2,1]+knn.test.conf[2,2])
print(knn.predict.ratio)
#6
#a)
games <- read.csv("http://statweb.stanford.edu/~jgorham/games.csv",as.is=TRUE)
teams <- read.csv("http://statweb.stanford.edu/~jgorham/teams.csv",as.is=TRUE)
all.teams <- sort(unique(c(teams$team,games$home,games$away)))
y <- with(games, homeScore-awayScore)
## Construct a data frame of the right dimensions, with all zeros
X0 <- as.data.frame(matrix(0,nrow(games),length(all.teams)))
names(X0) <- all.teams
## Fill in the columns, one by one
for(tm in all.teams) {
  X0[[tm]] <- 1*(games$home==tm) - 1*(games$away==tm)
}
X = X0[,names(X0) != "stanford-cardinal"]
reg.season.games = which(games$gameType == "REG")
homeAdv <- 1 - games$neutralLocation
Xh = cbind(homeAdv=homeAdv, X)
#Remove playoff games
margin = with(games, homeScore-awayScore)
margin.label=rep(0,times = 5395)
margin.label[margin>0] = 1
glm.fit_2 = glm(margin.label~0+., data = Xh, subset = reg.season.games, family = binomial)
coeffs = coef(summary(glm.fit_2))
team_coeff = coeffs[-1,1] 
sort(team_coeff, decreasing = TRUE)
#b)
X_games = abs(X)
total_num_games = colSums(X_games, na.rm = TRUE)
less_5 = total_num_games < 5
names.remove = names(less_5)[less_5]
games.remove = (games$home %in% names.remove)|games$away %in%names.remove
process.games = games[!games.remove,]
margin.process = with(process.games, homeScore-awayScore)

#Removing teams that have played fewer than five games from all.teams vector
all.teams.boolean = all.teams %in% names.remove
all.teams = all.teams[all.teams.boolean == FALSE]

#Removing teams that have played fewer than five games from teams dataframe
teams.boolean = teams$team %in% all.teams
teams.modified = teams[teams.boolean,]

#Construct a data from of the right dimensions, with all zeros
X0.process = as.data.frame(matrix(0,nrow(process.games),length(all.teams)))
names(X0.process) = all.teams

#Fill in columns
for(tm in all.teams) {
    X0.process[[tm]] = 1*(process.games$home == tm) - 1*(process.games$away == tm)
}

#Forcing coefficient for Stanford to equal 0
X.process  = X0.process[,names(X0.process) != "stanford-cardinal"]
reg.games.process = which(process.games$gameType == "REG")

#Addition of Home-field Advantage to model
homeAdv.process = 1-process.games$neutralLocation
Xh.process = cbind(homeAdv.process = homeAdv.process, X.process)

#Logistic Regression model on Processed Dataset
margin.label.process = rep(0, times = length(margin.process))
margin.label.process[margin.process>0] = 1
glm.fit.process = glm(margin.label.process~0+., data=Xh.process, subset = reg.games.process, family = binomial)

#Linear Regression model on Processed Dataset
homeAdv.process_1 = lm(margin.process~0+., data = Xh.process, subset = reg.games.process)

#Creating vector of logistic reg coeffs
logistic.regression.coefficients = coef(summary(glm.fit.process))[-1,1]
Stanford.entry = matrix(0, 1, 1)
names(Stanford.entry) = '`stanford-cardinal`'
logistic.regression.coefficients = append(logistic.regression.coefficients, Stanford.entry, after = 276)

#Creating vector of linear regression coefficients
linear.regression.coefficients = coef(summary(homeAdv.process_1))[-1,1]
linear.regression.coefficients = append(linear.regression.coefficients, Stanford.entry, after = 276)

#Sorted ranking of Teams by Linear Regression and Log. Reg. coeffs
linear.regression.rank = rank(-linear.regression.coefficients, ties.method = "min")
logistic.regression.rank = rank(-logistic.regression.coefficients, ties.method = "min")

#Sorting modified teams dataset by team name in alphabetical order
teams.modified = teams.modified[with(teams.modified, order(team, decreasing =  FALSE)),]
rank.table = cbind("Logistic Regression Ranking" = logistic.regression.rank, "Linear Regression Ranking" = linear.regression.rank, "AP Ranking" = teams.modified$apRank, "USA Today Ranking"= teams.modified$usaTodayRank)

logistic.regression.top25 = order(logistic.regression.rank, decreasing = FALSE)
logistic.regression.top25 = logistic.regression.top25[1:25]
rank.table.top25 = rank.table[logistic.regression.top25,]

#c)
lm.values = coef(summary(homeAdv.process_1))[-1,4]
glm.values = coef(summary(glm.fit.process))[-1,4]

lm.values.lab = rep(0, times = length(lm.values))
glm.values.lab = rep(0, times = length(glm.values))

lm.values.lab[lm.values <= .05] = 1
glm.values.lab[glm.values <= .05] = 1

#Fraction of teams better than Stanford based on lin. reg.
lm.portion.better = sum(lm.values.lab)/length(lm.values.lab)

#Number of teams better than Stanford based on log. reg.
glm.portion.better = sum(glm.values.lab)/length(glm.values.lab)

print(glm.portion.better)
print(lm.portion.better)

#d)
set.seed(2016)

#Removing non-regular season games from processed games data frame
process_boolean=(process.games$gameType=="REG")
regular.games=process.games[process_boolean,]

#Shuffling the rows in the games data frame to assist in creating 10 randomly divided groups
sort.games=regular.games[sample(nrow(regular.games)),]
margin.games=with(sort.games, homeScore-awayScore)

## Construct a data frame of the right dimensions, with all zeros 
reg_season_X0=as.data.frame(matrix(0,nrow(sort.games),length(all.teams))) 
names(reg_season_X0)=all.teams
## Fill in the columns, one by one
for(tm in all.teams) {
  reg_season_X0[[tm]]=1*(sort.games$home==tm) - 1*(sort.games$away==tm)
}

#Forcing coefficient for Stanford to equal 0
X.process.reg.season=reg_season_X0[,names(reg_season_X0) != "stanford-cardinal"]

#Addition of Home-field Advantage to model
home_advantage=1-sort.games$neutralLocation
reg_season_Xh=cbind(home_advantage=home_advantage, X.process.reg.season)

#Creating Logistic Regression Labels on Processed Dataset
regular_season_margin=rep(0, times=length(margin.games))
regular_season_margin[margin.games>0]=1

idx=matrix(0, nrow=10, ncol=2)
elem=floor(nrow(regular.games)/10)

#Determine index edges of each of ten groups 
for (i in 0:9){
  idx[i+1,1]=(i*elem)+1
  idx[i+1,2]=(i+1)*elem
}
cv_error=matrix(0, nrow=10, ncol=2)
contingency_cv=matrix(0, nrow=2, ncol=2)

for (i in 1:10){
  cv_Xh=reg_season_Xh[-(idx[i,1]:idx[i,2]),]
  test.Xh=reg_season_Xh[(idx[i,1]:idx[i,2]),]
  cv_margin=regular_season_margin[-(idx[i,1]:idx[i,2])]
  cv_margin_test=regular_season_margin[(idx[i,1]:idx[i,2])]
  margin=margin.games[-(idx[i,1]:idx[i,2])]
  margin_set=margin.games[(idx[i,1]:idx[i,2])]
  glm.cv=glm(cv_margin~0+., data=cv_Xh, family=binomial)
  lm.cv=lm(margin~0+., data=cv_Xh)
  lm_predict=predict(lm.cv, newdata=test.Xh)
  glm_predict=predict(glm.cv, newdata=test.Xh, type="response")
  lin.wr=0
  glm.wr=0
  glm_label_pred=rep(0, times=length(glm_predict))
  glm_label_pred[glm_predict>=0.5]=1
  for(j in 1:length(lm_predict)){
    lin.indv=0
    glm.indv=0
    if((lm_predict[j]<0 & margin_set[j]>0)|(lm-predict[j]>0 & margin_set[j]<0)){
      lin.wr=lin.wr+1
    }else{
      lin.indv=1
    }
    if((glm_label_pred[j]==1 & cv_margin_test[j]==0)|(glm_label_pred[j]==0 & cv_margin_test[j]==1)){
      glm.wr=glm.wr+1
    }else{
      glm.indv=1
    }
    if(lin.indv==1 & glm.indv==1){
      contingency_cv[1,1]=contingency_cv[1,1]+1
    }else if(lin.indv==1 & glm.indv==0){
      contingency_cv[1,2]=contingency_cv[1,2]+1
    }else if(lin.indv==0 & glm.indv==1){
      contingency_cv[2,1]=contingency_cv[2,1]+1
    }else if(lin.indv==0 & glm.indv==0){
      contingency_cv[2,2]=contingency_cv[2,2]+1
    } 
  }
  cv_error[i,1]=lin.wr
  cv_error[i,2]=glm.wr
}

cv.lm.err=sum(cv_error[,1])/(elem*10)
cv.glm.err=sum(cv_error[,2])/(elem*10)
print(cv.lm.err)
print(cv.glm.err)
linear.names=c("linear right","linear wrong")
logistic.names=c("logistic right", "logistic wrong")
rownames(contingency_cv)=linear.names
colnames(contingency_cv)=logistic.names
print(contingency_cv)

#e) first compare the linear and logistic on the test data. make predictions on the test data set
D = contingency_cv[2,1]+contingency_cv[1,2]
n_21 = contingency_cv[2,1]
n_12 = contingency_cv[1,2]
print(n_21)
print(n_12)
mcnemar_prac = 2*(1 - pnorm(contingency_cv[1,2], D/2 , (D/4)^(1/2))) 
print(mcnemar_prac)
mcnemar.test(contingency_cv)
