#
#Author:Apoorva Angre
#Purpose: Week2 Homework
#working with data files,data interogation functions

#loading required data
my.file.name<-file.choose()
my.file.name
art<-read.csv(file = my.file.name, header= TRUE, stringsAsFactors = FALSE)
colnames(art)
View(art)


#1. Which sales representative had the highest number of sales (assume that each row in the data is a single sale)?

#Method1
fill_colors<-c()
for(i in 1:length(art$store)){
  if(art$store[i]=="Syracuse"){
    fill_colors<-c(fill_colors,"orange")
  }
  else{
    fill_colors<-c(fill_colors,"#cccccc")
  }
}
barplot(table(art$rep),col=fill_colors,main='Units sold by Sales Rep',xlab ='Sales Representative',ylab='No. of Sales')

#2. How do sales of watercolor and drawing paper differ across the stores? (Which store sells the most watercolor/drawing paper?)

barplot(table(art$paper,art$store),beside= TRUE,col = c("aquamarine","firebrick1"), main='Paper type sales according to store',xlab='Stores',ylab='No. of sales')
legend.text = c("Drawing paper", "Watercolor")
legend("topleft", legend.text,pch=15, col=c("aquamarine","firebrick1"))

#3.What is the distribution of total sales across the stores?
#Hint: a boxplot is a good option for visualizing and comparing distributions of a #continuous variable (e.g., price, sales)

boxplot(art$total.sale ~ art$store, col=c('firebrick1','aquamarine','darkolivegreen1','darkgoldenrod1'),main='Distribution of total sales across stores',xlab='Stores',ylab='No. of Sales')

#4. How does the distribution of total sales of drawing paper compare to that of watercolor?
par(mfrow = c(1,2))
art.drawing <- art[art$paper == "drawing",]
art.watercolor <- art[art$paper == "watercolor",]
boxplot(art.drawing$total.sale, col=c('darkolivegreen1'),main='Drawing Sales',ylab='No. of Sales')
boxplot(art.watercolor$total.sale, col=c('darkgoldenrod1'),main='Watercolor Sales',ylab='No. of Sales')

#5.What was the number of sales across the years?

plot(table(art$year),type ='s' ,main='Sales across years', xlab='Year',ylab = 'No. of sales',col='darkolivegreen1',lwd = 4)

hex.cols


