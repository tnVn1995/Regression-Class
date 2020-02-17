# Read in data from file
data1=faithful
data=data1[order(data1$waiting),]
waiting=data[,2]
eruptions=data[,1]

# Plot the data
plot(waiting,eruptions)

# Fit Regression Model
eruption.lm=lm(eruptions~waiting, data=data)

# Display Summaries for Regression Information
summary(eruption.lm)

# Display ANOVA Table
anova(eruption.lm)

# Display the line of best fit with the data
conf.int =  predict(eruption.lm,interval="confidence")
fitted.values = conf.int[,1]
plot(waiting,eruptions)
lines(waiting,fitted.values,col="red",lwd=2)

# Add the confidence bands for E[Ynew]
conf.lower=predict(eruption.lm,interval="confidence")[ , "lwr" ]
conf.upper=predict(eruption.lm,interval="confidence")[ , "upr" ]
plot(waiting,eruptions)
lines(waiting,fitted.values,col="red",lwd=2)
lines(waiting,conf.lower,lwd=2,col="blue")
lines(waiting,conf.upper,lwd=2,col="blue")

# Add the prediction bands for Ynew
pred.lower=predict(eruption.lm,interval="prediction")[ , "lwr" ]
pred.upper=predict(eruption.lm,interval="prediction")[ , "upr" ]
plot(waiting,eruptions)
lines(waiting,fitted.values,col="red",lwd=2)
lines(waiting,conf.lower,lwd=2,col="blue")
lines(waiting,conf.upper,lwd=2,col="blue")
lines(waiting,pred.lower,lwd=2,col="green")
lines(waiting,pred.upper,lwd=2,col="green")
