file.choose()
my.dir<- 'A:\\Semester_Three\\719\\hw\\6\\'
crime <- read.csv(paste0(my.dir,'crimeRatesByState2005.csv'), sep="," , header=TRUE,stringsAsFactors = FALSE)
colnames(crime)
View(crime)

crime[1:3,]

crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime2$state != "United States",]

#Scatterplot Matrix, Figure 6-9
plot(crime2[,2:9], panel=panel.smooth, main='Scatterplot Matrix in R using Crime data')

#Bubble chart, Figure 6-15

crime_tsv <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv",header=TRUE, sep="\t")
View(crime_tsv)
radius <- sqrt( crime_tsv$population/ pi )
symbols(crime_tsv$murder, crime_tsv$burglary, circles=radius,inches=0.35,
        fg="white", bg="red", xlab="Murder Rate", ylab="Burglary Rate", main = 'Murder Vs Burglaries(Size = Population)')
text(crime$murder, crime$burglary, crime$state, cex=0.5)

#with square
#symbols(crime$murder, crime$burglary,squares=sqrt(crime$population), inches=0.5)


#Histogram, Figure 6-24
birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
stem(birth$X2008)
hist(birth$X2008, col='darkolivegreen1',xlab = 'Live births per 1000 population', ylab = 'Country Count', main = 'Global Distribution of Birth Rates')

#Density Plot. Figure 6-32

birth2008 <- birth$X2008[!is.na(birth$X2008)]

#pass it into the density() function to estimate a curve and store the results in d2008.
d2008 <- density(birth2008)
d2008
#but with the type set to "n" for no plotting. Then use the latter to actually draw the shape; set the fill color to a dark red and the border to a light gray.
plot(d2008, type = 'n', main = 'Global Distribution of Birth Rates in 2008',xlab = 'Live Births per 1000 population',ylab = 'Density')
polygon(d2008, col="#821122", border="#cccccc")


#make 2-3 small multiple dimension plots using data of your choice, see Fig. 6-38 & 6-40

file.choose()
my.dir<- 'A:\\Semester_Three\\719\\labs\\3\\'
sales <- read.csv(paste0(my.dir,'sales(3).csv') , header=TRUE,stringsAsFactors = FALSE)
View(sales)

# Set breaks for histograms
breaks = seq(0, 1000, by=50)
# Set the layout
par(mfrow=c(3,2))
# Draw histograms, one by one
hist(sales[sales$year == 2010,]$recipt,main='Sales Recipt in Yr 2010',xlab = 'Receipt in 2010',col = 'indianred1')
hist(sales[sales$year == 2011,]$recipt,main='Sales Recipt in Yr 2011',xlab = 'Receipt in 2011',col = 'indianred1')
hist(sales[sales$year == 2012,]$recipt,main='Sales Recipt in Yr 2012',xlab = 'Receipt in 2012',col = 'indianred1')
hist(sales[sales$year == 2013,]$recipt,main='Sales Recipt in Yr 2013',xlab = 'Receipt in 2013',col = 'indianred1')
hist(sales[sales$year == 2014,]$recipt,main='Sales Recipt in Yr 2014',xlab = 'Receipt in 2014',col = 'indianred1')

par(mfrow = c(9,6))


#make 2-3 small multiple dimension plots using data of your choice, see Fig. 6-38 & 6-40
my.dir.avc<- 'A:\\Semester_Three\\719\\hw\\5\\'
avocado <- read.csv(paste0(my.dir.avc,'avocado.csv'), header=TRUE,stringsAsFactors = FALSE)

par(mfrow = c(2,2))
hist(avocado[avocado$year == 2015,]$AveragePrice,xlab='Avg Price',main='Avergae Price in Yr 2015',col = 'indianred1')
hist(avocado[avocado$year == 2016,]$AveragePrice,xlab='Avg Price',main='Avergae Price in Yr 2016',col = 'indianred1')
hist(avocado[avocado$year == 2017,]$AveragePrice,xlab='Avg Price',main='Avergae Price in Yr 2017',col = 'indianred1')
hist(avocado[avocado$year == 2018,]$AveragePrice,xlab='Avg Price',main='Avergae Price in Yr 2018',col = 'indianred1')

#barplot(table(avocado$year),main='single dimension plot')
par(mfrow = c(4,4))
reg_avoc<-avocado[avocado$region == 'Syracuse' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Syracuse",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'Seattle' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Seattle",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'Atlanta' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Atlanta",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'West' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "West",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'Albany' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Albany",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'Boston' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Boston",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'California' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "California",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'Boise' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Boise",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'BuffaloRochester' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "BuffaloRochester",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'Sacramento' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Sacramento",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))
unique(avocado$region)

reg_avoc<-avocado[avocado$region == 'Orlando' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Orlando",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'LosAngeles' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "LosAngeles",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'Midsouth' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Midsouth",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'Columbus' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Columbus",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'Detroit' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "Detroit",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

reg_avoc<-avocado[avocado$region == 'NewYork' ,]
avoc_year_units<-aggregate(reg_avoc$Total.Volume,list(reg_avoc$year),sum)
options(scipen = 999)
barplot(with(avoc_year_units, setNames(x, Group.1)), main = "NewYork",
        xlab = "Years", ylab = "Value",col=c('darkolivegreen1','cyan','indianred1','gray47'))

#unique_reg<-unique(avocado$region)
#reg_list<-c(unique_reg)
##for(i in reg_list){
#}



