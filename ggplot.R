#Assign 7
#ggplot
install.packages("rlang")
install.packages(c("ggplot2", "gcookbook"))
library(ggplot2)
library(gcookbook)


#Figure 2.2, page 9 scattterplot
mtcars_1<-mtcars
#qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data=mtcars, geom=c("line", "point"))
# This is equivalent to:
ggplot(mtcars_1, aes(x=wt, y=mpg)) + geom_point()

#Figure 2.7, page 13 barplot
# Convert the x variable to a factor, so that it is treated as discrete
# cyl is continuous here
#par(mfrow=c(1,2))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

#ggplot(mtcars,aes(x=cyl))+geom_bar()
#ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

View(ToothGrowth)

#Figure 2.11, page 16 boxplot
#plot(ToothGrowth$supp, ToothGrowth$len )
#boxplot(len~supp+dose,data = ToothGrowth)
#qplot(ToothGrowth$supp, ToothGrowth$len, geom='boxplot')
#Figure 2-11. Left: box plot with qplot(); right: with multiple grouping variables

ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
ggplot(ToothGrowth,aes(x=interaction(supp,dose), y=len))+geom_boxplot()



#Figure 3.4, page 23 #Grouping Bars together

ggplot(cabbage_exp, aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat='identity',position = 'dodge')

#Figure 3.11, page 29 Coloring Negative and Positive Bars Differently
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
View(csub)
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos))+geom_bar(stat='identity',position = 'identity')

#Figure 4.19, page 64 Graph with a Shaded Area

sunspotyear <-data.frame(Year= as.numeric(time(sunspot.year)), Sunspots = as.numeric(sunspot.year))
View(sunspotyear)
ggplot(sunspotyear,aes(x=Year,y=Sunspots))+geom_area(fill='blue',alpha=.2)+geom_line()

#Figure 4.24, page 68 Making a Proportional Stacked Area Graph

library(gcookbook) # For the data set
library(plyr) # For the ddply() function


uspopage_prop<- ddply(uspopage,'Year',transform,Percent= Thousands/sum(Thousands)*100)
View(uspopage_prop)

ggplot(uspopage_prop,aes(x=Year,y=Percent,fill=AgeGroup))+geom_area(colour='black',size=.2,alpha=.4)+scale_fill_brewer(palette = 'Blues',breaks=rev(levels(uspopage$AgeGroup)))




















