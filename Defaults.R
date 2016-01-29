
data <- read.csv("SBA_Loan_data.csv")
data$defaulted <- data$GrossChargeOffAmount > 0
pruned <- data

#put names of columns you want to delete here
deleteRows <- c("","","")
data[,!(names(data) %in% drops)]

#alternatively, give names of variables to keep
keeps <- c("","")
pruned <- data[,keeps]


#transition states
states <- c("Current", "30", "60", "90+", "foreclosure", "REO","Paid Off")
spmarkov <- matrix(c(93,5,0,0,.01,0,2,30,45,23,0,.2,.004,1.8,10.5,16,35,32,5,.01,1.5,3.5,1,2,82,9,.3,2.2,2,.4,.3,6.5,85.4,4,1.4,0,0,0,0,0,100,0,0,0,0,0,0,0,100),
                  nrow = length(states), ncol = length(states))
pmarkov <- matrix(c(97,1.4,0,0,.001,0,1.6,34.6,44.6,19,0,.004,.003,1.8,12,16.8,34.5,34,1.6,.009,1.1,4.1,1.4,2.6,80.2,10,.3,1.3,1.9,.3,.1,6.8,87,2.5,1.3,0,0,0,0,0,100,0,0,0,0,0,0,0,100),
                  nrow = length(states), ncol = length(states))
#multiply everything by .01 (percentages)

#try out glm logistic regression
defaultLog <- glm(defaulted ~ GrossApproval, data = pruned, family = binomial())
summary(defaultLog)

defaultLin <- lm(GrossChargeOffAmount ~ GrossApproval, data = pruned)
summary(defaultLin)

plot(defaultLin)

#trying out forward stepwise model selection. isnt working for some reason.
default.step.forward = step(glm(defaulted ~ GrossApproval + ApprovalFiscalYear + BusinessType, data = pruned, family = binomial()), list(upper = ~.),
                             direction="forward")



##nmarkov chain model

markovModel <- function(mat, numTransitions){
  output <- mat %*% mat
  for(ii in 2:numTransitions){
    output <- output %*% mat
  }
  return(output)
}

finalState <- markovModel(spmarkov, 12)
