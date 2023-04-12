
Packages installation
#  libraries
#  if needed remove comments:

#      install.packages("gclus") 
#      install.packages("lattice")
#      install.packages("gclus")
#      install.packages("lattice")
#      install.packages("mgcv")
#      install.packages("rgdal")
#      install.packages("tmap")
#
#


library(gclus)
library(lattice)
library(mgcv)
library(rgdal)    
library(tmap)


#Read ReferendumResults into a dataframe RefData
RefData <- read.csv("ReferendumResults.csv")

#extract rows with lack of data for leave
unwanted.rows <- grepl("-1", RefData$Leave)

#save rows with data for leave
leave_data <- RefData[!unwanted.rows, ]

#save rows with no data for leave
leave_nodata <- RefData[unwanted.rows, ]

#Change -1 to NA
RefData[, 5][RefData[, 5] == -1] <- NA

#introducing new age variables
Age_18to29 <- RefData[, 17] + RefData[, 18] + RefData[, 19]
Age_30to44 <- RefData[, 20]
Age_45to64 <- RefData[, 21] + RefData[, 22]
Age_65plus <- RefData[, 23] + RefData[, 24] + RefData[, 25] + RefData[, 26]

#cleaning up redundant age variables and removing area type
RefData <- RefData[, -(11:26)]
RefData <- RefData[, -2]

#introducing the new variables into the dataframe
RefData$Age_18to29 <- Age_18to29
RefData$Age_30to44 <- Age_30to44
RefData$Age_45to64 <- Age_45to64
RefData$Age_65plus <- Age_65plus

#and ordering them
RefData <- RefData[, c(1:9, 33:36, 10:32)]

#creating new variable probleave that calculates the probability of each ward choosing "Leave"
probleave <- RefData$Leave / RefData$NVotes

#incorporating it into our dataframes
RefData$probleave <- probleave
leave_data$probleave <- leave_data$Leave / leave_data$NVotes

#ordering data
RefData <- RefData[, c(1:4, 37, 5:36)]

#                       managing regions
# this collects data from each region and stores them to an appropriate name

{
  EastMidrows <- grepl("East Midlands", RefData$RegionName)
  EastEnglandrows <- grepl("East of England", RefData$RegionName)
  Londonrows <- grepl("London", RefData$RegionName)
  NorthEastrows <- grepl("North East", RefData$RegionName)
  NorthWestrows <- grepl("North West", RefData$RegionName)
  SouthEastrows <- grepl("South East", RefData$RegionName)
  SouthWestrows <- grepl("South West", RefData$RegionName)
  WestMidrows <- grepl("West Midlands", RefData$RegionName)
  Yorkshirerows <-
    grepl("Yorkshire and The Humber", RefData$RegionName)
  
  EastMid <- RefData[EastMidrows, ]
  EastEng <- RefData[EastEnglandrows, ]
  London <- RefData[Londonrows, ]
  NorthEast <- RefData[NorthEastrows, ]
  NorthWest <- RefData[NorthWestrows, ]
  SouthEast <- RefData[SouthEastrows, ]
  SouthWest <- RefData[SouthWestrows, ]
  WestMid <- RefData[WestMidrows, ]
  Yorkshire <- RefData[Yorkshirerows, ]
  
}

#                               MAPS
# the following code is used to produce the regional allocation of votes 
# into their respective region. 

# quantifying how many leave votes per region
{sum(EastMid$Leave)
sum(EastEng$Leave)
sum(London$Leave) 
sum(NorthEast$Leave) 
sum(NorthWest$Leave) 
sum(SouthEast$Leave) 
sum(SouthWest$Leave) 
sum(WestMid$Leave) 
sum(Yorkshire$Leave) 

# quantifying how many remain votes per region
sum(EastMid$NVotes-EastMid$Leave)
sum(EastEng$NVotes-EastEng$Leave)
sum(London$NVotes-London$Leave) 
sum(NorthEast$NVotes-NorthEast$Leave) 
sum(NorthWest$NVotes-NorthWest$Leave) 
sum(SouthEast$NVotes-SouthEast$Leave) 
sum(SouthWest$NVotes-SouthWest$Leave) 
sum(WestMid$NVotes-WestMid$Leave) 
sum(Yorkshire$NVotes-Yorkshire$Leave) 

# introducing new dataframe with respective regions, their leave and remain votes.
votes = as.data.frame(
  cbind(
    c("EAST MIDLANDS",
      "EAST OF ENGLAND",
      "LONDON",
      "NORTH EAST",
      "NORTH WEST",
      "SOUTH EAST",
      "WEST MIDLANDS",
      "YORKSHIRE AND THE HUMBER",
      "SOUTH WEST"),
    c(55762, 166353, 537687, 97163, 380857, 205689, 534926, 332736, 74528),
    c(78484, 337524, 1216040, 116105, 524050, 313457, 644313, 393088, 93875)
  ), stringsAsFactors = FALSE)

names(votes) = c("Regions", "Leave", "Remain")

# storing matrix value as a single numeric value
votes$Leave = as.numeric(votes$Leave)
votes$Remain = as.numeric(votes$Remain)
votes$Result = colnames(votes)[max.col(votes[,2:3])+1]

# accessing library's regional data and mapping our matrix into the spacial frame
# the following proceeds for remain, leave maps and saves them as a png.
geoEN = readOGR(".", "maghistengland")

mapEN = tmaptools::append_data(geoEN, votes, key.shp = "NAME", key.data = "Regions")
}
tm_shape(mapEN) +
  tm_fill("Remain", title = "Number of Remain votes", palette = "Blues", n=5) +
  tm_borders(alpha=1)

dev.print(png,
          "remain_map.png",
          width = 480,
          height = 480)
dev.off()

tm_shape(mapEN) +
  tm_fill("Leave", title = "Number of Leave votes", palette = "Reds", n=5) +
  tm_borders(alpha=1)
dev.print(png,
          "leave_map.png",
          width = 480,
          height = 480)
dev.off()

###################################
###################################
###################################


# Starting exploratory analysis on socioeconomic factors

# plot probleave against different age groups
# into a 2x2 matrix 
par(mfrow = c(2, 2),
    mar = c(3, 4, 3, 2),
    mgp = c(2, 1, 0))
plot(
  RefData$Age_18to29,
  RefData$probleave,
  pch = 4,
  col = "blue",
  xlab = "Percentage of years 18 to 29",
  ylab = ""
)
plot(
  RefData$Age_30to44,
  RefData$probleave,
  pch = 4,
  col = "blue",
  xlab = "Percentage of years 30 to 44",
  ylab = ""
)
plot(
  RefData$Age_45to64,
  RefData$probleave,
  pch = 4,
  col = "blue",
  xlab = "Percentage of years 45 to 64",
  ylab = ""
)
plot(
  RefData$Age_65plus,
  RefData$probleave,
  pch = 4,
  col = "blue",
  xlab = "Percentage of years 65 plus",
  ylab = ""
)
title(
  "Probleave against percentage of permanent residents in each group",
  line = -2,
  outer = TRUE
)

dev.print(png, "probvsage.png", width = 480, height = 480)
dev.off()



# plotting probleave in each region (boxplot)
# the same idea has been carried on all other groups
{
  par(mfrow = c(1, 1),
      mar = c(9, 3, 5, 3),
      mgp = c(3, 1, 0))
  boxplot(
    probleave ~ RegionName,
    data = RefData,
    las = 2,
    main = "Regional probabilities of choosing Leave",
    ylab = "probability to leave",
    names = c(
      "EastMidlands",
      "EastofEngland",
      "London",
      "NorthEast",
      "NorthWest",
      "SouthEast",
      "SouthWest",
      "WestMidlands",
      "Yorkshire&Humber"
    ),
    col = "Grey"
  )
  dev.print(png, "regionalprobleave.png", width = 480, height = 480)
  dev.off()
}


  
# plot adultmeanavg vs probleave
#  same idea......
{
  par(mfrow = c(3, 3),
      mar = c(4, 3, 2, 2),
      mgp = c(3, 1, 0))
  plot(
    EastMid$AdultMeanAge,
    EastMid$probleave,
    xlab = "EastMidlands",
    ylab = "",
    ylim = c(0, 0.8),
    cex.lab = 1.5
  )
  plot(
    EastEng$AdultMeanAge,
    EastEng$probleave,
    xlab = "EastEng",
    ylab = "",
    ylim = c(0, 0.8),
    cex.lab = 1.5
  )
  plot(
    London$AdultMeanAge,
    London$probleave,
    xlab = "London",
    ylab = "",
    ylim = c(0, 0.8),
    cex.lab = 1.5
  )
  plot(
    NorthEast$AdultMeanAge,
    NorthEast$probleave,
    xlab = "NorthEast",
    ylab = "",
    ylim = c(0, 0.8),
    cex.lab = 1.5
  )
  plot(
    NorthWest$AdultMeanAge,
    NorthWest$probleave,
    xlab = "NorthWest",
    ylab = "",
    ylim = c(0, 0.8),
    cex.lab = 1.5
  )
  plot(
    SouthEast$AdultMeanAge,
    SouthEast$probleave,
    xlab = "SouthEast",
    ylab = "",
    ylim = c(0, 0.8),
    cex.lab = 1.5
  )
  plot(
    SouthWest$AdultMeanAge,
    SouthWest$probleave,
    xlab = "SouthWest",
    ylab = "",
    ylim = c(0, 0.8),
    cex.lab = 1.5
  )
  plot(
    WestMid$AdultMeanAge,
    WestMid$probleave,
    xlab = "WestMid",
    ylab = "",
    ylim = c(0, 0.8),
    cex.lab = 1.5
  )
  plot(
    Yorkshire$AdultMeanAge,
    Yorkshire$probleave,
    xlab = "Yorkshire",
    ylab = "",
    ylim = c(0, 0.8),
    cex.lab = 1.5
  )
  title("Plotting AdultMeanAvg against probleave",
        line = -1,
        outer = TRUE)
  
  dev.print(png,
            "probvsadultmean.png",
            width = 480,
            height = 480)
  dev.off()
  
}

# plotting probleave by ethnicity
# same idea? yes.

{
        par(
          mfrow = c(5, 1),
          mar = c(2.3, 4, 2, 2),
          mgp = c(0.2, 1, 0)
        )
        plot(
          RefData$White,
          RefData$probleave,
          xlim = c(0, 100),
          xlab = "White",
          ylab = "",
          las = 2,
          col = "green",
          pch = 4,
          axes = F,
          cex.lab = 1.5
        )
        axis(2, c(0, 0.2, 0.4, 0.6, 0.8, 1))
        axis(1, c(0, 20, 40, 60, 80, 100))
        plot(
          RefData$Black,
          RefData$probleave,
          xlab = "Black",
          xlim = c(0, 100),
          ylab = "",
          las = 2,
          col = "black",
          pch = 4,
          axes = F,
          cex.lab = 1.5
        )
        axis(2, c(0, 0.2, 0.4, 0.6, 0.8, 1))
        axis(1, c(0, 20, 40, 60, 80, 100))
        plot(
          RefData$Asian,
          RefData$probleave,
          xlab = "Asian",
          xlim = c(0, 100),
          ylab = "",
          las = 2,
          col = "orange",
          pch = 4,
          axes = F,
          cex.lab = 1.5
        )
        axis(2, c(0, 0.2, 0.4, 0.6, 0.8, 1))
        axis(1, c(0, 20, 40, 60, 80, 100))
        plot(
          RefData$Indian,
          RefData$probleave,
          xlab = "Indian",
          xlim = c(0, 100),
          ylab = "",
          las = 2,
          col = "blue",
          pch = 4,
          axes = F,
          cex.lab = 1.5
        )
        axis(2, c(0, 0.2, 0.4, 0.6, 0.8, 1))
        axis(1, c(0, 20, 40, 60, 80, 100))
        plot(
          RefData$Pakistani,
          RefData$probleave,
          xlab = "Pakistani",
          xlim = c(0, 100),
          ylab = "",
          las = 2,
          col = "maroon",
          pch = 4,
          axes = F,
          cex.lab = 1.5
        )
        axis(2, c(0, 0.2, 0.4, 0.6, 0.8, 1))
        axis(1, c(0, 20, 40, 60, 80, 100))
        title(
          "Probabilities of voting leave varying with percentage ethnicity",
          line = -1,
          outer = TRUE
        )
        
        dev.print(png,
                  "probvsethnicity.png",
                  width = 480,
                  height = 480)
        dev.off()
}


# the following was an idea to split the regions into regions with heavy percentages of each ethnicity
# ie regions with heavy white population and regions without,
# regions with heavy black population and without
# et cetera.

# it seemed a good idea but in the end the results are not substantial, as you can see
# only heavy white areas actually behave differently and one can question if regions of other
# social minorities actually exist? For example, can you find a region with mostly indian population?

# Well...not exactly...
# but there is no such thing as bad data!
# Ive "hidden" them in a bracket to "tidy" the code. Feel free to investigate!

{
#splitting regions with higher than 90 percent whites etc

  par(
    mfrow = c(2, 1),
    mar = c(3, 3, 2.3, 3),
    mgp = c(2, 1, 0)
  )
  
  plot(
    leave_data$White[leave_data$White < 80],
    leave_data$probleave[leave_data$White < 80],
    xlab = "Percentage of White residents",
    ylab = "probleave",
    ylim = c(0, 1)
  )
  lines(lowess(leave_data$White[leave_data$White < 80], leave_data$probleave[leave_data$White < 80]), col = "blue")
  
  plot(
    leave_data$White[leave_data$White >= 80],
    leave_data$probleave[leave_data$White >= 80],
    xlab = "Percentage of White residents",
    ylab = "probleave",
    ylim = c(0, 1)
  )
  lines(lowess(leave_data$White[leave_data$White >= 80], leave_data$probleave[leave_data$White >= 80]), col = "red")
  
  
  title(
    "Probabilites of voting leave varying with percentage of white voters",
    line = -1,
    outer = TRUE
  )
  
  dev.print(png,
            "probleavewithpercwhite.png",
            width = 480,
            height = 480)
  dev.off()
  
#splitting regions with higher than 30 percent black population
  par(
    mfrow = c(2, 1),
    mar = c(3, 3, 2.3, 3),
    mgp = c(2, 1, 0)
  )
  
  plot(
    leave_data$Black[leave_data$Black < 30],
    leave_data$probleave[leave_data$Black < 30],
    xlab = "Percentage of Black residents",
    ylab = "probleave",
    ylim = c(0, 1)
  )
  lines(lowess(leave_data$Black[leave_data$Black < 30], leave_data$probleave[leave_data$Black < 30]), col = "blue")
  
  plot(
    leave_data$Black[leave_data$Black >= 30],
    leave_data$probleave[leave_data$Black >= 30],
    xlab = "Percentage of Black residents",
    ylab = "probleave",
    ylim = c(0, 1)
  )
  lines(lowess(leave_data$Black[leave_data$Black >= 30], leave_data$probleave[leave_data$Black >= 30]), col = "red")
  
  
  title(
    "Probabilites of voting leave varying with percentage of Black voters",
    line = -1,
    outer = TRUE
  )
  dev.print(png,
            "probleavewithpercblack.png",
            width = 480,
            height = 480)
  dev.off()
  
#splitting regions with higher than 30 percent asian population
  par(
    mfrow = c(2, 1),
    mar = c(3, 3, 2.3, 3),
    mgp = c(2, 1, 0)
  )
  
  plot(
    leave_data$Asian[leave_data$Asian < 30],
    leave_data$probleave[leave_data$Asian < 30],
    xlab = "Percentage of Asian residents",
    ylab = "probleave",
    ylim = c(0, 1)
  )
  lines(lowess(leave_data$Asian[leave_data$Asian < 30], leave_data$probleave[leave_data$Asian < 30]), col = "blue")
  
  plot(
    leave_data$Asian[leave_data$Asian >= 30],
    leave_data$probleave[leave_data$Asian >= 30],
    xlab = "Percentage of Asian residents",
    ylab = "probleave",
    ylim = c(0, 1)
  )
  lines(lowess(leave_data$Asian[leave_data$Asian >= 30], leave_data$probleave[leave_data$Asian >= 30]), col = "red")
  
  
  title(
    "Probabilites of voting leave varying with percentage of Asian voters",
    line = -1,
    outer = TRUE
  )
  
  dev.print(png,
            "probleavewithpercasian.png",
            width = 480,
            height = 480)
  dev.off()
  
#splitting regions with higher than 30 percent indian population
  par(
    mfrow = c(2, 1),
    mar = c(3, 3, 2.3, 3),
    mgp = c(2, 1, 0)
  )
  
  plot(
    leave_data$Indian[leave_data$Indian < 30],
    leave_data$probleave[leave_data$Indian < 30],
    xlab = "Percentage of Indian residents",
    ylab = "probleave",
    ylim = c(0, 1)
  )
  lines(lowess(leave_data$Indian[leave_data$Indian < 30], leave_data$probleave[leave_data$Indian < 30]), col = "blue")
  
  plot(
    leave_data$Indian[leave_data$Indian >= 30],
    leave_data$probleave[leave_data$Indian >= 30],
    xlab = "Percentage of Indian residents",
    ylab = "probleave",
    ylim = c(0, 1)
  )
  lines(lowess(leave_data$Indian[leave_data$Indian >= 30], leave_data$probleave[leave_data$Indian >= 30]), col = "red")
  
  
  title(
    "Probabilites of voting leave varying with percentage of Indian voters",
    line = -1,
    outer = TRUE
  )
  dev.print(png,
            "probleavewithpercindian.png",
            width = 480,
            height = 480)
  dev.off()
  
#splitting regions with higher than 30 percent pakistani population
  par(
    mfrow = c(2, 1),
    mar = c(3, 3, 2.3, 3),
    mgp = c(2, 1, 0)
  )
  
  plot(
    leave_data$Pakistani[leave_data$Pakistani < 30],
    leave_data$probleave[leave_data$Pakistani < 30],
    xlab = "Percentage of Pakistani residents",
    ylab = "probleave",
    ylim = c(0, 1)
  )
  lines(lowess(leave_data$Pakistani[leave_data$Pakistani < 30], leave_data$probleave[leave_data$Pakistani < 30]), col = "blue")
  
  plot(
    leave_data$Pakistani[leave_data$Pakistani >= 30],
    leave_data$probleave[leave_data$Pakistani >= 30],
    xlab = "Percentage of Pakistani residents",
    ylab = "probleave",
    ylim = c(0, 1)
  )
  lines(lowess(leave_data$Pakistani[leave_data$Pakistani >= 30], leave_data$probleave[leave_data$Pakistani >= 30]), col = "red")
  
  
  title(
    "Probabilites of voting leave varying with percentage of Pakistani voters",
    line = -1,
    outer = TRUE
  )
  dev.print(png,
            "probleavewithpercpakistani.png",
            width = 480,
            height = 480)
  dev.off()
  
}

# the following investigates if students in each region have anything to do
# with the behaviour of the voting patterns. Do they?
# Well.. plot regional students vs probleave !
{
  par(mfrow = c(3, 3), mar = c(4, 2, 3, 2))
  plot(EastMid$Students,
       EastMid$probleave,
       xlab = "EastMid",
       ylab = "",
       cex.lab=1.2)
  plot(EastEng$Students,
       EastEng$probleave,
       xlab = "EastEng",
       ylab = "",
       cex.lab=1.2)
    plot(London$Students,
       London$probleave,
       xlab = "London",
       ylab = "",
       cex.lab=1.2)
  plot(NorthEast$Students,
       NorthEast$probleave,
       xlab = "NorthEast",
       ylab = "",
       cex.lab=1.2)
  plot(NorthWest$Students,
       NorthWest$probleave,
       xlab = "NorthWest",
       ylab = "",
       cex.lab=1.2)
  plot(SouthEast$Students,
       SouthEast$probleave,
       xlab = "SouthEast",
       ylab = "",
       cex.lab=1.2)
  plot(SouthWest$Students,
       SouthWest$probleave,
       xlab = "SouthWest",
       ylab = "",
       cex.lab=1.2)
  plot(WestMid$Students,
       WestMid$probleave,
       xlab = "WestMid",
       ylab = "",
       cex.lab=1.2)
  plot(Yorkshire$Students,
       Yorkshire$probleave,
       xlab = "Yorkshire",
       ylab = "",
       cex.lab=1.2)
  title("Student proportion vs probleave in each region",
        line = -2,
        outer = TRUE)
  
  dev.print(png,
            "regionalprobleave_students.png",
            width = 480,
            height = 480)
  dev.off()
  
  
}
# Not a surprise, they do. Seems rational to ignore the "dimension" regions bring and combine the data
# to form a natonial image.
# plot student vs probleave national
{
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 3))
  plot(RefData$Students,
       RefData$probleave,
       xlab = "Proportion of students",
       ylab = "Probability to vote Leave")
  title(
    "Student proportion and Probability to vote Leave (Nationwide)",
    line = -2,
    outer = TRUE
  )
  dev.print(png,
            "probvsstudent.png",
            width = 480,
            height = 480)
  dev.off()
  
}

# same old same old
# plot qualifications against probleave
{
par(mfrow=c(3,1), mar=c(3,4,2,2),mgp=c(2,1,0))
plot(
  leave_data$NoQuals,
  leave_data$probleave,
  xlab = "Proportion of residents with no qualification",
  ylab = "probleave",
  col="red",
  cex.lab=1.4)
plot(
  leave_data$L1Quals,
  leave_data$probleave,
  xlab = "Proportion of residents with Level 1 qualification",
  ylab = "probleave",
  col="orange",
  cex.lab=1.4)
plot(
  leave_data$L4Quals_plus,
  leave_data$probleave,
  xlab = "Proportion of residents with degree qualification or above",
  ylab = "probleave",
  col="green",
  cex.lab=1.4)
title(
  "Qualifications and respective probability of voting leave",
  line=-1,
  outer=TRUE
)
dev.print(png,
          "probvsquals.png",
          width = 480,
          height = 480)
dev.off()
}

#plot social class against probleave
{
  
  par(mfrow = c(3, 1),
      mar = c(3, 4, 2, 2),
      mgp = c(2, 1, 0))
  plot(
    leave_data$HigherOccup,
    leave_data$probleave,
    xlim = c(0, 100),
    xlab = "Percentage of residents in Higher Occupations",
    ylab = "",
    col = "red",
    cex.lab=1.3
  )
  plot(
    leave_data$RoutineOccupOrLTU,
    leave_data$probleave,
    xlim = c(0, 100),
    xlab = "Percentage of residents in Routine Occupations",
    ylab = "",
    col = "orange",
    cex.lab=1.3
  )
  plot(
    leave_data$UnempRate_EA,
    leave_data$probleave,
    xlim = c(0, 100),
    xlab = "Percentage of economically active unemployed residents ",
    ylab = "",
    col = "cyan",
    cex.lab=1.3
  )
  title(
    "Percentage of permanent residents in different social classes with respective probleave",
    line = -1,
    outer = TRUE
  )
dev.print(png,
          "probvssocialclass.png",
          width = 480,
          height = 480)
dev.off()
}

#
#
#
#
# finally the exploratory analysis comes to an end.

#PRINCIPLE COMPONENT ANALYSIS

# the following code consists of the PCA and model building
# the basic idea is to carry out a test to check for codependence betweeen
# covariates in each group. then the PCA is carried and we store variables that
# explain a significant portion of the variation into a new variable 
# of the form   ***.pc* 


   
  
# AGES

#correlation study
  age.my.abs     <- abs(cor(RefData[, 11:14]))
  age.my.colors  <- dmat.color(age.my.abs)
  age.my.ordered <- order.single(cor(RefData[, 11:14]))
  cpairs(RefData[, 11:14], age.my.ordered, panel.colors = age.my.colors, gap =
           0.5)
  title("Correlation between ages",line=-1,outer=TRUE)
  dev.print(png,
            "cpairs_age.png",
            width = 480,
            height = 480)
  dev.off()
  
  age.my.prc <- prcomp(RefData[, 11:14], center = TRUE, scale = TRUE)
  
  
  par(mfrow=c(2,1),mar=c(3,3,2,2),mgp=c(2,1,0))
  scree_bar <-
    screeplot(age.my.prc, xlab = "Components",main="")
  scree_line <-
    screeplot(age.my.prc, type = "line",main="")
  title("Scree Plots for Age Principal Components",line=-1,outer=TRUE)
  dev.print(png,
            "scree_age.png",
            width = 480,
            height = 480)
  dev.off()
  
  
  age.load    <- age.my.prc$rotation
  age.sorted.loadings <- age.load[order(age.load[, 1]), 1]
  age.myTitle <- "Loadings Plot for PC on age"
  age.myXlab  <- "Variable Loadings"
  
  dotplot(
    age.sorted.loadings,
    main = age.myTitle,
    xlab = age.myXlab,
    cex = 1.5,
    col = "red"
  )
  
  dev.print(png,
            "dot_age.png",
            width = 480,
            height = 480)
  dev.off()
  


#pca age
  #principal analysis
    cat("Principle Component Analysis for Ages:\n")
  print(age.my.prc)
  summary(age.my.prc)
  
  
  age.pc1 <- age.my.prc$x[,1]
  age.pc2 <- age.my.prc$x[,2]
  age.pc3 <- age.my.prc$x[,3]


# ETHNICITY

 #correlation study  
  ethnicity.my.abs     <- abs(cor(RefData[, 15:19]))
  ethnicity.my.colors  <- dmat.color(ethnicity.my.abs)
  ethnicity.my.ordered <- order.single(cor(RefData[, 15:19]))
  cpairs(RefData[, 15:19], ethnicity.my.ordered, panel.colors = ethnicity.my.colors, gap =
           0.5)
  title("Correlation between ethnicities",line=-1,outer=TRUE)
  dev.print(png,
            "cpairs_ethnicity.png",
            width = 480,
            height = 480)
  dev.off()
  
  ethnicity.my.prc <- prcomp(RefData[, 15:19], center = TRUE, scale = TRUE)
  
  par(mfrow=c(2,1),mar=c(3,3,2,2),mgp=c(2,1,0))
  scree_bar <-
    screeplot(ethnicity.my.prc, main = "", xlab = "Components")
  scree_line <-
    screeplot(ethnicity.my.prc, main = "", type = "line")
  title("Scree Plots for Ethnicity Principal Components",line=-1,outer=TRUE)
  dev.print(png,
            "scree_ethnicity.png",
            width = 480,
            height = 480)
  dev.off()
  
  ethnicity.load    <- ethnicity.my.prc$rotation
  ethnicity.sorted.loadings <- ethnicity.load[order(ethnicity.load[, 1]), 1]
  ethnicity.myTitle <- "Loadings Plot for PC on ethnicity"
  ethnicity.myXlab  <- "Variable Loadings"
  dotplot(
    ethnicity.sorted.loadings,
    main = ethnicity.myTitle,
    xlab = ethnicity.myXlab,
    cex = 1.5,
    col = "red"
  )
  dev.print(png,
            "dot_ethnicity.png",
            width = 480,
            height = 480)
  dev.off()

#pca ethnicity
  #principal analysis
  cat("Principle Component Analysis for Ethnicity:\n")
  print(ethnicity.my.prc)
  
  summary(ethnicity.my.prc)
  
  ethnicity.pc1 <- ethnicity.my.prc$x[,1]
  ethnicity.pc2 <- ethnicity.my.prc$x[,2]
  ethnicity.pc3 <- ethnicity.my.prc$x[,3]

# SOCIAL CLASS

  #correlation study
  social.my.abs     <- abs(cor(RefData[, 29:31]))
  social.my.colors  <- dmat.color(social.my.abs)
  social.my.ordered <- order.single(cor(RefData[, 29:31]))
  
  cpairs(RefData[, 29:31], social.my.ordered, panel.colors = social.my.colors, gap =
           0.5)
  title("Correlation between Occupational status",line=-1,outer=TRUE)
  dev.print(png,
            "cpairs_social.png",
            width = 480,
            height = 480)
  dev.off()
  
  social.my.prc <- prcomp(RefData[, 29:31], center = TRUE, scale = TRUE)
  
  par(mfrow=c(2,1),mar=c(3,3,2,2),mgp=c(2,1,0))
  scree_bar <-
    screeplot(social.my.prc, main = "", xlab = "Components")
  scree_line <-
    screeplot(social.my.prc, main = "", type = "line")
  title("Scree Plots for Social Principal Components",line=-1,outer=TRUE)
  dev.print(png,
            "scree_social.png",
            width = 480,
            height = 480)
  dev.off()
  
  social.load    <- social.my.prc$rotation
  social.sorted.loadings <- social.load[order(social.load[, 1]), 1]
  social.myTitle <- "Loadings Plot for PC3"
  social.myXlab  <- "Variable Loadings"
  dotplot(
    social.sorted.loadings,
    main = social.myTitle,
    xlab = social.myXlab,
    cex = 1.5,
    col = "red"
  )
  dev.print(png,
            "dot_social.png",
            width = 480,
            height = 480)
  dev.off()

#pca social
  
  cat("Principle Component Analysis for Social Classes:\n")
  print(social.my.prc)
  
  summary(social.my.prc)
  
  social.pc1 <- social.my.prc$x[,1]
  social.pc2 <- social.my.prc$x[,2]



# QUALIFICATIONS

# correlation study
  quals.my.abs     <- abs(cor(RefData[, 24:26]))
  quals.my.colors  <- dmat.color(quals.my.abs)
  quals.my.ordered <- order.single(cor(RefData[, 24:26]))
  
  cpairs(RefData[, 24:26], quals.my.ordered, panel.colors = quals.my.colors, gap =
           0.5)
  title("Correlation between qualifications",line=-1,outer=TRUE)
  dev.print(png,
            "cpairs_quals.png",
            width = 480,
            height = 480)
  dev.off()
  
  quals.my.prc <- prcomp(RefData[, 24:26], center = TRUE, scale = TRUE)
  
  par(mfrow=c(2,1),mar=c(3,3,2,2),mgp=c(2,1,0))
  scree_bar <-
    screeplot(quals.my.prc, main = "", xlab = "Components")
  scree_line <-
    screeplot(quals.my.prc, main = "", type = "line")
  title("Scree Plots for Qualification Principal Components",line=-1,outer=TRUE)
  dev.print(png,
            "scree_quals.png",
            width = 480,
            height = 480)
  dev.off()
  
  quals.load    <- quals.my.prc$rotation
  quals.sorted.loadings <- quals.load[order(quals.load[, 1]), 1]
  quals.myTitle <- "Loadings Plot for PC4"
  quals.myXlab  <- "Variable Loadings"
  dotplot(
    quals.sorted.loadings,
    main = quals.myTitle,
    xlab = quals.myXlab,
    cex = 1.5,
    col = "red"
  )
  dev.print(png,
            "dot_quals.png",
            width = 480,
            height = 480)
  dev.off()

#pca qualifications
    
  cat("Principle Component Analysis for Qualifications:\n")
  print(quals.my.prc)
  summary(quals.my.prc)
  
  quals.pc1 <- quals.my.prc$x[,1]
  quals.pc2 <- quals.my.prc$x[,2]
  
  
  
# correlation plots on pca variables to 
# see their patterns
  
  cpairs(RefData[, c(39,41,43,44)], panel.colors = age.my.colors, gap =
           0.5)
  title("Correlation between principal components",line=-1,outer=TRUE)
  
  dev.print(png,
            "pairs_pca.png",
            width = 480,
            height = 480)
  dev.off()
  
  cpairs(withdata[, c(39,41,43,44)], panel.colors = age.my.colors, gap =
           0.5)
  title("Correlation between components - with data",line=-1,outer=TRUE)
  dev.print(png,
            "pairs_pca_data.png",
            width = 480,
            height = 480)
  dev.off()
  
  cpairs(nodata[, c(39,41,43,44)], panel.colors = age.my.colors, gap =
           0.5)
  title("Correlation between components - without data",line=-1,outer=TRUE)
  dev.print(png,
            "pairs_pca_nodata.png",
            width = 480,
            height = 480)
  dev.off()
  

# London

# This is an after thought in the sense that it came after all the studies 
# done but it makes sense to include it here.
# this introduces a new binary variable to be introduced in the final model
# to take into consideration the fact that a ward is situated in London

RefData$London <- as.numeric(RefData$RegionName == "London")


############################################################
############################################################
#                                                          #
# linear models, some can be ignored they were studied to  #
#            see which one performs better                 #
#                                                          #
############################################################
############################################################

lm1 <- lm(probleave ~ age.pc1 + age.pc2 + ethnicity.pc1 + ethnicity.pc2,data=RefData)
lm1.2 <- update(lm1, .~. +quals.pc1 +social.pc1)

lm2 <- update(lm1.2, .~. +I(age.pc1^2) +I(ethnicity.pc1^2))
lm3 <- update(lm2, .~. +age.pc1:age.pc2)
lm4 <- update(lm3, .~. +ethnicity.pc1:ethnicity.pc2)
lm5 <- update(lm4, .~. +social.pc1:ethnicity.pc1)

lm5.1 <-update(lm5,.~.-age.pc2)
lm5.2 <-update(lm5.1,.~.-age.pc1:age.pc2,data=RefData)

lm5.3 <-update(lm5.2, .~. +London:ethnicity.pc1,data=RefData)
lm5.4 <-update(lm5.3, .~. +London:ethnicity.pc2,data=RefData)


# the above shows the thought process to achieve the final 
# linear model lm5.4, now saved as lmfinal... which makes sense.

lmfinal <- lm5.4


# linear before london is added

par(mfrow=c(2,2),    
    mar=c(3,3.2,2,2),
    mgp=c(2,0.75,0)  
)
plot(lm5.2,which=1:4,cex.lab=1.3)
dev.print(png,
          "lm_linear.png",
          width = 480,
          height = 480)
dev.off()

# linear after london added
par(mfrow=c(2,2),    
    mar=c(3,3.2,2,2),
    mgp=c(2,0.75,0)  
)
plot(lm5.4,which=1:4,cex.lab=1.3)
dev.print(png,
          "lm_linear_finalmodel.png",
          width = 480,
          height = 480)
dev.off()

# the general linear models
# initially these were the ones i would have expected to use but
# they perform, rather, underwhelmingly...

# i want to point out that due to the step fashion of the regression some glms
# have been removed from the code. This was partly on purpose and partly on
# having too many different models that were useless, so at some point, i am sure
# i must have erased models that i considered.
# so, believe me, more glms have been tested i just decided you wont be really interested in
# seeing half finished bad models.

glm1 <-
  glm(
    probleave ~ age.pc1 + ethnicity.pc1+ethnicity.pc2+quals.pc1+social.pc1,
    weights = NVotes,
    family = gaussian(link="cloglog"),
    data=RefData[,]
  )


glm2 <-
  update(glm1, .~. +I(age.pc1^2)+I(ethnicity.pc1^2))

glm3 <-
  update(glm2, .~. +age.pc1:ethnicity.pc1)

glm1.1 <- update(glm3,family=binomial,data=RefData)
  
# see the gaussian family? i know this is just a linear model now i just crossreferenced
# the two models to check if they agree
# again this is not in the "proper" timeline of my investigation, still works though.


par(mfrow=c(2,2),    
    mar=c(3,3.2,2,2),
    mgp=c(2,0.75,0)  
)
plot(glm1.1,which=1:4,cex.lab=1.3)
dev.print(png,
          "glm_binomial.png",
          width = 480,
          height = 480)
dev.off()

##################################################################################
##################################################################################



# Testing the mean score to see performance of model


RefData$age.pc1 <- age.pc1
RefData$age.pc2 <- age.pc2
RefData$ethnicity.pc1 <- ethnicity.pc1
RefData$ethnicity.pc2 <- ethnicity.pc2
RefData$social.pc1 <- social.pc1
RefData$quals.pc1 <- quals.pc1


nodata.rows <- which(is.na(RefData$Leave))
withdata.rows <-which(!is.na(RefData$Leave))
nodata <- RefData[nodata.rows,]
withdata <-RefData[withdata.rows,]

# the following is a test to roughly check my models performance on the score function
{
test<- predict(lm5.2,newdata=withdata,se.fit=TRUE,type="response")
test.sdpred <- sqrt( (test$se.fit^2) + (test$residual.scale)^2)
mean(log(test.sdpred)+(((withdata$probleave - test$fit)^2 )/ (2*test.sdpred^2)))

test1<- predict(lm5.4,newdata=withdata,se.fit=TRUE,type="response")
test1.sdpred <- sqrt( (test1$se.fit^2) + (test1$residual.scale)^2)
mean(log(test1.sdpred)+(((withdata$probleave - test1$fit)^2 )/ (2*test1.sdpred^2)))

test3 <- predict(glm1.1, newdata=withdata,se.fit=TRUE,type="response")
test3.sdpred <- sqrt( (test3$se.fit^2) + (test3$residual.scale)^2)
mean(log(test3.sdpred)+(((withdata$probleave - test3$fit)^2 )/ (2*test3.sdpred^2)))

}


# ACTUAL PREDICTIONS
# this is it. the following is the code to predict the
# data for the missing wards.

finalpred <- predict( lmfinal, newdata=nodata , se.fit=TRUE, type="response")
finalpred$df <- NULL
finalpred$residual.scale <- NULL
write.table(finalpred,file="15013494_pred.dat",col.names=F,quote=F)
