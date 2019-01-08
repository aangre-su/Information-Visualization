
file.choose()

install.packages('plotrix')

google.dir<-"A:\\Semester_Three\\719\\project_dataset_research\\others\\google-play-store-apps\\"
google_play <- read.csv(paste0(google.dir,'googleplaystore.csv'), header=TRUE,stringsAsFactors = FALSE)
View(google_play)

library(stringr)
library(ggplot2)
library(dplyr)
library(lubridate)
options(scipen = 999)

dataset<-unique(google_play)  
summary(dataset)

sapply(dataset,function(x)sum(is.na(x)))
str(dataset)

#2) Cleaning the Data

#Installs column has values which are not suitable for manipulation hence using stringr we will convert them.
#Converting "+" and "," and changing it to numeric format
#The size is given in MB's and KB we are converting them to numeric as well
#It is sometimes better to convert numeric data into catergories with a specific range.
#Here we are converting Installs into a Category of Grades A+>>A>>B>>C.Meaning C is the lowest grade possible.
#Converting Character Date to R POSIXct object using Lubridate Package.(I really recommend using this package).


dataset$Installs <- str_replace_all(dataset$Installs,"[+]","")
dataset$Installs <- str_replace_all(dataset$Installs,",","")
dataset$Installs <- as.numeric(dataset$Installs)
dataset$Reviews <- as.numeric(dataset$Reviews)

#Cleaning 2
##Converting "M" and "K" and changing it to numeric format
dataset$Size <- str_replace_all(dataset$Size,"M","")
dataset$Size <- str_replace_all(dataset$Size,"k","")
dataset$Size <- as.numeric(dataset$Size)
dataset[is.na(dataset$Size),"Size"] <- median(dataset$Size,na.rm=T)

#Converting "$"
#The Price column has "$" in it.Since this is a special character "[]" has to be mentioned for it.
dataset$Price <- str_replace_all(dataset$Price,"[$]","")

#Cleaning 3
#We can use nested ifelse to convert into category.
Install_Category<-ifelse(dataset$Installs <10001,"Grade C",ifelse(dataset$Installs <1000001,"Grade B",ifelse(dataset$Installs <100000001,"Grade A","Grade A+")))
Install_Category <- factor(Install_Category,levels = c("Grade C","Grade B","Grade A","Grade A+"))
dataset<- cbind(dataset,as.data.frame(Install_Category))

#Cleaning 4
#The mdy specifies the Month Day Year.
class(dataset$Last.Updated)
test_date<-dataset

#head(test_date$Last.Updated)
#test_date$Last.Updated<-as.Date(test_date$Last.Updated, format = "%d-%b-%y")
#test_date$Month<-month(test_date$Last.Updated)
#test_date$Month<-month.abb[test_date$Month]
#test_date$Year<-year(test_date$Last.Updated)

head(dataset$Last.Updated)
dataset$Last.Updated<-as.Date(dataset$Last.Updated, format = "%d-%b-%y")
dataset$Month<-month(dataset$Last.Updated)
dataset$Month<-month.abb[dataset$Month]
dataset$Year<-year(dataset$Last.Updated)
#Now we dont need the Last.Updated Field hence Removing it
dataset$Last.Updated<- NULL

View(dataset)

colnames(dataset)[colnames(dataset)=="Month"] <- "LastUpdated_Month"
colnames(dataset)[colnames(dataset)=='Year']<-'LastUpdated_Yr'

dataset<-dataset[-10473,]#this row has some missing values hence excluding it

unique(dataset$Rating)#Ratings column still has some NA values

#JUST REPLACING NANS WITH MEAN OF THE COLUMN
dataset$Rating[is.nan(dataset$Rating)] <-3.596988

#DEDUPLICATING
copy_dataset<-dataset
dataset <- dataset[!duplicated(dataset$App),]

#removing rows with ""
dataset<-dataset[!(dataset$Content.Rating==""),]
dataset<-dataset[!(dataset$Content.Rating=="Adults only 18+"),]

View(dataset)




#Q.1
#Firstly, we visualize the most installed (one billion) apps's #categories.

dataset_catg<-dataset[dataset$Installs >= 500000000, ]
dataset_catg<-aggregate(dataset_catg$Installs, list(dataset_catg$Category), FUN=sum)

View(a)
a <- dataset %>%
  select(Category, Installs) %>%
  filter(Installs == 1000000000) %>%
  group_by(Category) %>%
  arrange(Category)

ggplot(a, aes(x= Installs, fill = Category)) +
  geom_bar(position = "dodge") +
  coord_flip()

#How many times Communication has got 1 billion

unique(dataset$Installs)

#Top 5 paid categories
dataset %>%
  filter(Type == "Paid") %>%
  group_by(Category) %>%
  summarize(totalInstalls = sum(Installs)) %>%
  arrange(desc(totalInstalls)) %>%
  head(5) %>%
  ggplot(aes(x = reorder(Category,-totalInstalls), y = totalInstalls,fill=Category)) +
  geom_bar(stat="identity", width=.5) +
  labs(title= "Top 5 Paid Categories" ) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Top 10 free categories
dataset %>%
  filter(Type == "Free") %>%
  group_by(Category) %>%
  summarize(totalInstalls = sum(Installs)) %>%
  arrange(desc(totalInstalls)) %>%
  head(10) %>%
  ggplot(aes(x = Category, y = totalInstalls)) +
  geom_bar(stat="identity", width=.5,  fill="indianred") +
  labs(title= "Top10 Free Categories" ) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#Sizing Strategy - Light Vs Bulky?
#How do app sizes impact the app rating?
dataset<-dataset[!(dataset$Rating== 19.0000),]
attach(dataset)
plot(Rating,Size , main="Sizing Strategy", col='hotpink1',xlab="Rating ", ylab="Size ", pch=19)

#Ans:Most top rated apps are optimally sized between ~2MB to ~40MB - neither too light nor too heavy.

#Pricing Strategy - Free Vs Paid?
#How do app prices impact app rating? 
attach(dataset)
plot(Price,Rating , main="PricingStrategy", col='gold',xlab="Price ", ylab="Rating ", pch=19)

#Most top rated apps are optimally priced between ~0$ to ~30$. There are only a very few apps priced above 20$.
#You can also conclude that most free apps are top rated, so its pretty obvious that if the app is free its top rated.


#How do the sizes of paid apps and free apps vary?


#Majority of the paid apps that are highly rated have small sizes. This means that most paid apps are designed and developed to cater to specific functionalities and hence are not bulky.
#Users prefer to pay for apps that are light-weighted. A paid app that is bulky may not perform well in the market.



#Let's create another plot, this time a line graph showing the changes in the average number of reviews of apps by month.

#Here is a summary of what we will plot:
  c_data <-dataset[dataset$Category == 'COMMUNICATION',]
  c_data %>% group_by(LastUpdated_Month) %>% summarize(Reviews=mean(Reviews))
  
  # Creating a group-means data set
  gd <- c_data %>% group_by(LastUpdated_Month) %>% summarize(Reviews=mean(Reviews))
  View(gd)
  
  ggplot(c_data, aes(x=LastUpdated_Month, y=Reviews, group=12)) +
    scale_y_continuous(trans='log10') +
    geom_point(data=gd, color="red") +
    geom_line(data=gd, color="blue2") +
    scale_x_discrete(limits = month.abb) +
    labs(title = "Average number of Communication App reviews and month updated",
         y = "Number of Reviews",
         x = "Month it was Updated") + 
    theme_linedraw()
  

game_data <-dataset[dataset$Category == 'GAME',]
game_data %>% group_by(LastUpdated_Month) %>% summarize(Reviews=mean(Reviews))

# Creating a group-means data set
gd <- game_data %>% group_by(LastUpdated_Month) %>% summarize(Reviews=mean(Reviews))
View(gd)

ggplot(game_data, aes(x=LastUpdated_Month, y=Reviews, group=12)) +
  scale_y_continuous(trans='log10') +
  geom_point(data=gd, color="red") +
  geom_line(data=gd, color="blue2") +
  scale_x_discrete(limits = month.abb) +
  labs(title = "Average number of Game Category App reviews and month updated",
       y = "Number of Reviews",
       x = "Month it was Updated") + 
  theme_linedraw()


game_data <-dataset[dataset$Category == 'GAME',]
game_data %>% group_by(LastUpdated_Month) %>% summarize(Reviews=mean(Reviews))

# Creating a group-means data set
gd <- game_data %>% group_by(LastUpdated_Month) %>% summarize(Reviews=mean(Reviews))
View(gd)

ggplot(game_data, aes(x=LastUpdated_Month, y=Reviews, group=12)) +
  scale_y_continuous(trans='log10') +
  geom_point(data=gd, color="red") +
  geom_line(data=gd, color="blue2") +
  scale_x_discrete(limits = month.abb) +
  labs(title = "Average number of Games Category App reviews and month updated",
       y = "Number of Reviews",
       x = "Month it was Updated") + 
  theme_linedraw()

gd <- dataset %>% group_by(LastUpdated_Month) %>% summarize(Installs=mean(Installs))
View(gd)

ggplot(dataset, aes(x=LastUpdated_Month, y=Installs, group=12)) +
  scale_y_continuous(trans='log10') +
  geom_point(data=gd, color="red") +
  geom_line(data=gd, color="blue2") +
  scale_x_discrete(limits = month.abb) +
  labs(title = "Average number of Android app Installs and month updated",
       y = "Number of Installs",
       x = "Month it was Updated") + 
  theme_linedraw()

gd <- dataset %>% group_by(LastUpdated_Yr) %>% summarize(Reviews=mean(Reviews))
View(gd)

ggplot(dataset, aes(x=LastUpdated_Yr, y=Reviews, group=12)) +
  scale_y_continuous(trans='log10') +
  geom_point(data=gd, color="red") +
  geom_line(data=gd, color="blue2") +
  #scale_x_discrete(limits = month.abb) +
  labs(title = "Average number of Android app Reviews and Year updated",
       y = "Number of Reviews",
       x = "Year it was Updated") + 
  theme_linedraw()

#From these plots, we can see that more reviews often means high #ratings. And developers should consider releasing apps and #updates in the summer months, preferably August.

colnames(dataset)
library(plotrix)
barplot(table(dataset$Type),main='App_Pricing_Type', col=c('darkolivegreen1','indianred1'))
pie(table(dataset$Type))
ds_price<-data.frame(table(dataset$Type))

pie3D(ds_price$Freq,labels=ds_price$Var1,explode=0.6,
      main="Pie Chart of Pricing")

##Experiment with hw7 ggplot graph

csub <- subset(dataset, Category=="COMMUNICATION")
View(csub)

csub$pos <- csub$Rating >= mean(csub$Rating)
View(csub)
ggplot(csub, aes(x=LastUpdated_Yr, y=Rating, fill=pos))+geom_bar(stat='identity',position = 'identity')

boxplot(csub$Rating)
mean(csub$Rating)
median(csub$Rating)
colnames(csub)


##Experimenting

ggplot(dataset,aes(x=Price,y=Rating))+geom_area(fill='blue',alpha=.2)+geom_line()

ggplot(dataset,aes(x=Rating,y=Size,fill=Price))+geom_area(colour='black',size=.2,alpha=.4)+scale_fill_brewer(palette = 'Blues',breaks=rev(levels(dataset$Price)))

## Experiment with hw 6 
View(dataset)
colnames(dataset)


comm_data <-dataset[dataset$Category == 'COMMUNICATION',]
comm_data<-comm_data[comm_data$Installs > 100000000, ]
comm_data<-comm_data[comm_data$App != 'imo free video calls and chat', ]
View(comm_data)

options(scipen = 999)
radius <- sqrt( comm_data$Installs/ pi )
symbols(comm_data$Rating, comm_data$Reviews, circles=radius,inches=0.35,fg="white", bg="indianred1", xlab="Rating", ylab="Reviews", main = 'Rating Vs Review')
text(comm_data$Rating, comm_data$Reviews, comm_data$App, cex=0.5)

comm_data$Installs

lim<-par()
img.dir <- "A:\\Semester_Three\\719\\labs\\5\\"
ima<-readPNG(paste0(img.dir,"Lab5_bottles.png"))
#r1<-readPNG(paste0(my.dir,"Lab5_r1.png"))
#rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#rect(lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4]
 #    , col=rgb(1,1,1, 0.85)
  #   , border="white"
#)

r1.x1 <- comm_data$Rating[comm_data$App=="Hangouts"]
r1.x2 <- r1.x1+0.1
r1.y1 <-comm_data$Reviews[comm_data$App=="Gmail"]
r1.y2 <-r1.y1 +650000

rasterImage(r1, r1.x1,r1.y1,r1.x2,r1.y2)


########### New Experiment - Pie Chart for Content Rating###############
########3Audience Market Share Analysis
unique(dataset$Content.Rating)
pie(table(dataset$Content.Rating))

  ######### Bar plot for installs per content rating


CR_Categ<-c('Everyone','Everyone 10+','Mature 17+','Teen')
Total_AppsM<-c(7903,322,393,1036) #table(dataset$Content.Rating) - total apps in each category
Installs_CRCateg<-c(52179352961,4016271795,2437986878,16487275393) #Total installs category wise
Installs_per_app<-Installs_CRCateg/Total_AppsM #total installs / total apps in each category
CR_measures_df<-data.frame(CR_Categ,Total_AppsM,Installs_CRCateg,Installs_per_app)
CR_measures_df<-CR_measures_df[order(CR_measures_df$Installs_per_app,decreasing = TRUE),]


########FUN FACT

dataset[which.max(dataset$Price),]

#The most expensive App is IM Rich Trump which is $400 and it has been installed more than 10 thousand times..People have paid 400 thousads dollars so far for it.
#The App basically says you are rich to you in Trumps fashion

#######bubble graph replacement  experiment ########

library('png')
file.choose()
google.dir.bubble <-'A:\\Semester_Three\\719\\project_dataset_research\\others\\google-play-store-apps\\bubble_images\\'
bcg_playtore<-readPNG(paste0(google.dir.bubble,"playstore2.png"))
whatsapp<-readPNG(paste0(google.dir.bubble,"Lab5_r1.png"))
#w1<-readPNG(paste0(google.dir.bubble,"Lab5_w1.png"))

pch<-rep("w",7)#7 ws
pch[agg.data$type=="red"]<-"R"


agg.data.recipts<-aggregate(sales$recipt, list(type=sales$type, sales$wine), FUN=sum)
View(agg.data.recipts)
colnames(agg.data)<-c("type","wine","units")
agg.data$recipts<-agg.data.recipts$x

comm_data <-dataset[dataset$Category == 'COMMUNICATION',]
comm_data<-comm_data[comm_data$Installs > 100000000, ]
comm_data<-comm_data[comm_data$App != 'imo free video calls and chat',]
View(comm_data)
options(scipen = 999)
plot(comm_data$Rating, comm_data$Reviews, bty="n"
     , pch=15, cex=2
     #, xlim =c(0,(1.25*max(agg.data$units)))
     #, ylim =c(0,(1.25*max(agg.data$recipts)))
     , xlab="Rating", ylab="Reviews"
     , main="Rating Vs Reviews"
)

lim<-par()
rasterImage(bcg_playtore, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
rect(lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4]
     , col=rgb(1,1,1, 0.85)
     , border="white"
)
View(comm_data)
r1.x1 <- comm_data$Rating[comm_data$App=="WhatsApp Messenger"]
r1.x2 <- r1.x1
r1.y1 <-comm_data$Reviews[comm_data$App=="WhatsApp Messenger"]
r1.y2 <-r1.y1-1000000

rasterImage(whatsapp, r1.x1,r1.y1,r1.x2,r1.y2)
text(agg.data$units +2000
     , agg.data$recipts
     , labels=agg.data$wine
     , adj=0, cex=1.2)

w1.x1 <-agg.data$units[agg.data$type=="white"]
w1.x2 <-w1.x1 + 3000
w1.y1 <-agg.data$recipts[agg.data$type=="white"]
w1.y2 <-w1.y1 + 0.1
rasterImage(w1, w1.x1, w1.y1, w1.x2, w1.y2)
















