yukon <- function(aqhi) { #You'll need to input in a threshold Air Quality Health Index. This function will calculate the percentage of days each month exceeding this threshold
  if (aqhi %% 1 == 0) { #This is just for the graph title. If you input in "3" or "2", it'll show up as "3.0" or "2.0".
    aqhititle <- paste(aqhi,".0", sep="")
  } else {
    aqhititle <- aqhi # If you put in "3.2" or others, it'll be the same thing in the title.
  }
  airquality <- read.csv("airquality2.csv") #Have to read the air quality CSV
  numbertype <- readline(prompt="AQHI type? max / avg / min : ") #Do you want to count the number of days where the max AQHI, average AQHI, or minimum AQHI exceeded the threshold.
  if (numbertype == "max") { #If you picked max...
    columnnumber <- 4 #The fourth column of the CSV contains max daily AQHI values. 
    bootlegtitle <- paste("Proportion of Days Each Month Whose Air Quality Health Index Exceeded", aqhititle, sep = " ") #Makes your title
    bootlegylab <- paste("Percentage of Days with Recorded AQHI over", aqhititle, "(%)", sep = " ") #Makes your y axis label
  }
  else if (numbertype == "avg") {
    columnnumber <- 5
    bootlegtitle <- paste("Proportion of Days Each Month Whose Average Air Quality Health Index Exceeded", aqhititle, sep = " ")
    bootlegylab <- paste("Percentage of Days with Recorded Average AQHI over", aqhititle, "(%)", sep = " ")
    }
  else if (numbertype == "min") {
    columnnumber <- 6
    bootlegtitle <- paste("Proportion of Days Each Month Whose Minimum Air Quality Health Index Exceeded", aqhititle, sep = " ")
    bootlegylab <- paste("Percentage of Days with Recorded Minimum AQHI over", aqhititle, "(%)", sep = " ")
    } 
  else {
    stop("Hey! Please input 'max' (maximum), 'avg' (average), or 'min' (minimum).") #Just in case you don't input in anything
  }
  monthcodelist <- c() #We'll be adding in the month codes for all analyzed months in the loop here.
  percentagelist <- c() #The same thing, but for the percentage of days with the chosen daily values above the threshold
  emptyboyes <- c()
  for (monthcode in 1:118) { #There are 118 months in the data. This loop will analyze each of those months.
    bootlegfloorcheck <- monthcode - 1 #The next three lines are just holdovers from older versions. I've kept them here just in case I ever need them again.
    bootlegmultiple <- floor(bootlegfloorcheck/12) 
    bootlegyear <- 2008 + bootlegmultiple
    skipblush <- 0 #A starting point for the number of days in the month currently being analyzed in the loop.
    totalblush <- 0 #Also a starting point but for the total number of days in the analyzed month that were looked at.
    for (rownumber in 1:3592) { #There are 3592 days in the data. This loop makes it so that for each month, we'll go down the whole spreadsheet.
      if (airquality[rownumber,1] == monthcode && airquality[rownumber,7] != "Caution") { #If the current row being analyzed is associated with the month currently being analyzed and there is no caution (i.e. there is no blank space in the data), the day is counted in totalblush.
        totalblush <- totalblush + 1 #Adding 1 to totalblush
        if (airquality[rownumber, columnnumber] > aqhi) { #On top of that, if the daily AQHI value exceeds the threshold, it gets added to skipblush.
          skipblush <- skipblush + 1
        } #If the month code does not match or there's a caution sign, nothing happens to skipblush and totalblush. There's probably a more efficient way to do this, but I'm not experienced enough to figure it out. I'm pretty sure there's a way so that the loop doesn't have to look at ALL of the rows of data, but I'm not sure how to do it.
      } 
    }
    percentage <- skipblush/totalblush * 100 #Calculate the percentage of ALL examined days that were above the threshold for the month being analyzed in the loop.
    percentagelist <- c(percentagelist, percentage) #Adds the percentage to the list.
    monthcodelist <- c(monthcodelist, monthcode) #Adds the analyzed month to the list.
  } 
  print(monthcodelist) #Prints out the months for you
  print(percentagelist) #Prints out the percentages for you
  paste(monthcodelist, percentagelist) #Prints out both months and percentages.
  #It might be a bit redundant, but I thought it might make things easier for you to glance at.
  caution <- c(1,2,5,12,18,23,24,26,29,30,31,32,33,36,37,45,49,87) #I couldn't figure out a way for the function to check if the month had any days with "Caution" on it, so I just manually put it  in this function.
  plot(percentagelist~monthcodelist, main = bootlegtitle, xlab = "Month Code", ylab = bootlegylab, ylim = c(0,100)) #Plots it out
  points(percentagelist[caution]~caution, col = "blue") #Colours all the caution points blue.
}