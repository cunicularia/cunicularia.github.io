watermonth <- function(year) { #Hey! This function is there to chart trends in sea level based on monthly averages.
  #To use, run watermonth(XXXX). e.g. watermonth(1909)
  deep4 <- read.csv("deep4.csv") #It's called deep4 because I had a lot of versions of it.
  par(mfrow=c(1,1)) #Makes sure that this is the only plot being done
  if (year == 1909) { #Because there are only a few 1909 months in the data and I'm bad at R, I made a special case for 1909
    deepgress <- lm(V2~V1, data = deep4) #lm = linear model. This does a linear regression of the graph.
    print(summary(deepgress)) #This is what gives you all the slopes and equations and p-values and stats stuff.
    plot(V2~V1, data = deep4, type = "l", main = "Sea Level since 1909", xlab = "Year", ylab = "Sea Level (cm)") #Plots out the actual data. If you want just points, use type = 'p' instead. For points and line, use type 'l'.
    abline(lm(V2~V1, data = deep4), col = "blue") #Draws the line of best fit. You can change the colour if you want.
    deepintercept <- summary(deepgress)$coefficients[1, 1] #Grabs the y-intercept of the line of best fit's equation.
    deepslope <- summary(deepgress)$coefficients[2, 1] #Likewise, but for the slope.
    forecast <- deepslope * 2100 + deepintercept #Grabs the forecast for 2100 by plugging in 2100 into the equation. 
    pizzavalue <- summary(deepgress)$coefficients[2, 4] #Grabs your p-value
    cat("Your equation since", year, "is:", deepslope, "* year +", deepintercept, ".\nIn 2100, the sea level is forecast to be", forecast, "cm.\n") #Prints the equation and forecast out to make everything easier for you (i.e. you don't have to check everything in the summary)
    cat("Please note, your p-value is", pizzavalue, ", so this result MAY not be significant depending on what it is.") #Prints p-value for you
  } 
  if (year > 2016 || year < 1909) { #Just in case your input doesn't work
    print("Hi! All data was collected between 1909 and 2016.") 
  }
  if (year > 1909 && year < 2017) { #Bootlegged it up for when you don't want your data to start at 1909.
    shallowgress<- lm(V2~V1, data = subset(deep4, V1 > year))
    print(summary(shallowgress))
    title <- paste("Sea Level since", year, sep = " ") #This is one way to combine a string (the sea level part) with a number/object (the year) in order to get them both into the title.
    plot(V2~V1, data = subset(deep4, V1 > year), type = "l", main = title, xlab = "Year", ylab = "Sea Level (cm)")
    abline(lm(V2~V1, data = subset(deep4, V1 > year)), col = "blue")
    shallowintercept <- summary(shallowgress)$coefficients[1, 1]
    shallowslope <- summary(shallowgress)$coefficients[2, 1]
    forecast <- shallowslope * 2100 + shallowintercept
    pizzavalue <-summary(shallowgress)$coefficients[2, 4]
    cat("Your equation since", year, "is:", shallowslope, "* year +", shallowintercept,".\nIn 2100, the sea level is forecast to be", forecast, "cm.\n")
    cat("Please note, your p-value is", pizzavalue, ", so this result MAY not be significant depending on what it is.")
  }
}
watermonthloop <- function(loopy) { #A quick way to plot out the parameters of the linear regression over time! NOTE: This uses monthly averages. Also note that this function switches from the decimal representation of the month and uses a NUMERICAL DATE CODE instead as I did not know how to use the for() loop with the decimals. As a result, this may not be even less accurate than what I have now.
  deep5 <- read.csv("deep5.csv") #Gotta read the data first
  monthcodelist <- c() #Sets an empty list, to be filled later!
  yearslopelist <- c() #For all slopes
  yearforecastlist <- c() #For all forecasts
  pizzavaluelist <- c() #For all p-values
  significant <- c() #For all months that are significant
  for (month in 1:1285) { #Loops EVERYTHING so that all months are examined
    pizzavalue <- summary(yeargress)$coefficients[2,4] #Find the p-values ^_^
    if (pizzavalue <= 0.05) { #Checks if the month that is in the loop right now has a p-value less than 0.05 (i.e. is a great starting point for analysis)
      significant <- c(significant, month) 
    } 
    monthcodelist <- c(monthcodelist, month) #Adds the month to a list for plotting purposes later.
    yeargress <- lm(V2~V1, data = subset(deep5, MonthCode >= month)) #Performs the linear regression since the month
    yearintercept <- summary(yeargress)$coefficients[1, 1] #Grabs the associated intercept (this isn't plotted, it's just for the forecast)
    yearslope <- summary(yeargress)$coefficients[2, 1] #And the slope
    yearslopelist <- c(yearslopelist, yearslope) #Adds the slope to the list
    forecast <- yearslope * 2283 + yearintercept #Calculates the forecast
    yearforecastlist <- c(yearforecastlist, forecast) #Adds the forecast to the list
    pizzavalue <- summary(yeargress)$coefficients[2,4] #Grabs the p-value
    pizzavaluelist <- c(pizzavaluelist, pizzavalue) #Now adds it to the list
  }
  par(mfrow=c(2,2)) #Clears the way!
  plot(yearslopelist~monthcodelist, type = "l", main = "Slope of the Linear Regression over Time", xlab = "Date Code", ylab = "Slope (cm/month)") #Plot the slope vs the month 
  plot(yearforecastlist~monthcodelist, type = "l", main = "Forecasted 2100 Sea Level over Time", xlab = "Date Code", ylab = "Forecasted Sea Level in 2100 (cm)")
  plot(pizzavaluelist~monthcodelist, type = "l" , main = "p-value of the Linear Regression over Time", xlab = "Date Code", ylab = "p-value")
  abline(h=0.05, col = "blue") #Draws a line at the alpha value of 0.05.
  print(significant) #Print out the list of the date codes of all SIGNIFICANT month. May not be accurate.
}
wateryear <- function(year) {
  deepest <- read.csv("deepest2.csv") #Still have to read all the data first (/*o*)/
  par(mfrow=c(1,1)) #Makes sure that this is the only plot being done
  if (year > 2016 || year < 1911) { #Checks that the year works!
    print("Hi! All yearly averages were calculated between 1911 and 2016")
  }
  if (year > 1910 && year < 2015) { 
    yeargress <- lm(V2~V1, data = subset(deepest, V1 >= year)) #Linear regression
    print(summary(yeargress)) #Prints out the linear regression
    title <- paste("Sea Level since", year, sep = " ") #Re-using methods :D
    plot(V2~V1, data = subset(deepest, V1 >= year), type = "l", main = title, xlab = "Year", ylab = "Sea Level (cm)")
    abline(lm(V2~V1, data = subset(deepest, V1 >= year)), col = "blue")
    yearintercept <- summary(yeargress)$coefficients[1, 1]
    yearslope <- summary(yeargress)$coefficients[2, 1]
    forecast <- yearslope * 2100 + yearintercept
    pizzavalue <- summary(yeargress)$coefficients[2,4]
    cat("Your equation since", year, "is:", yearslope, "* year +", yearintercept,".\nIn 2100, the sea level is forecast to be", forecast, "cm.\n") #By the way, \n signifies a line break.
    cat("Please note, your p-value is", pizzavalue, ", so this result MAY not be significant depending on what it is.")
  }
  if (year == 2016) { #Can't really do all the stuff with just one year, heh.
    print("Hi! This function will not work. Sorry!")
  }
}
wateryearloop <- function(loopy) { #like watermonthloop() but with the yearly average instead!
  deepest <- read.csv("deepest2.csv") #REAAAAAAAAAAAADING the data
  yearlist <- c()
  yearslopelist <- c()
  yearforecastlist <- c()
  pizzavaluelist <- c()
  significant<-c()
  for (year in 1911:2015) {
    pizzavalue <- summary(yeargress)$coefficients[2,4]
    if (pizzavalue <= 0.05) {
      significant <- c(significant, year)
    } 
    pizzavaluelist <- c(pizzavaluelist, pizzavalue)
    yearlist <- c(yearlist, year)
    yeargress <- lm(V2~V1, data = subset(deepest, V1 >= year))
    yearintercept <- summary(yeargress)$coefficients[1, 1]
    yearslope <- summary(yeargress)$coefficients[2, 1]
    yearslopelist <- c(yearslopelist, yearslope)
    forecast <- yearslope * 2100 + yearintercept
    yearforecastlist <- c(yearforecastlist, forecast)
  }
  par(mfrow=c(2,2))
  plot(yearslopelist~yearlist, type = "l", main = "Slope of the Linear Regression over Time", xlab = "Year", ylab = "Slope (cm/year)")
  plot(yearforecastlist~yearlist, type = "l", main = "Forecasted 2100 Sea Level over Time", xlab = "Year", ylab = "Forecasted Sea Level in 2100 (cm)")
  plot(pizzavaluelist~yearlist, type = "l", main = "p-value of the Linear Regression over Time", xlab = "Year", ylab = "p-value")
  abline(h=0.05, col = "blue") #Draws a blue line at the alpha value of 0.05.
  print(significant) #Print out the list of the date codes of all SIGNIFICANT month. May not be accurate.
  checkem <- length(significant) #checks how many significant years there were
  bootlegcheckem <- rep(1, checkem) #makes sure that the years are plottable
  plot(bootlegcheckem~significant, type = "h", main = "Significant Years", xlab = "Year", ylab = " ") #shows you which years were significant
}