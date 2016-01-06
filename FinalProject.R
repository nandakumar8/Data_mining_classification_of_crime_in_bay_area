###IST 687 - FINAL PROJECT CODE ###
###Project Name: SAN FRANCISCO CRIME CLASSIFICATION###
###GROUP MEMBERS: 
###1. NANDAKUMAR UDAYAKUMAR
###2. HARIHARAN ARIYANAYAKIPURAM RAMANARAYANAN
###3. GOWRI SHANKAR SURESH

###**********************INSTALLING THE REQUIRED PACKAGING AND LOADING IT*******************###
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggmap")
install.packages("kernlab")
install.packages("caret")
install.packages("lattice")
library(lubridate)
library(randomForest)
library(ggplot2)
library(dplyr)
library(ggmap)
library(kernlab)
library(caret)
library(lattice)

###**********************READING THE DATA SET/FILES*******************###
#Reading the train and test dataset files for San Francisco Crime Classification
train<-read.csv("C:\\Users\\Nandakumar\\Desktop\\ist 687\\project\\train.csv")
test<-read.csv("C:\\Users\\Nandakumar\\Desktop\\ist 687\\project\\test.csv")

trainViz<-read.csv("C:\\Users\\Nandakumar\\Desktop\\ist 687\\project\\train.csv")
trainnames <- sort(unique(as.character(train$Category)))

#Display the contents of train and test data files
View(train)
View(test)



###**********************DATA PREPROCESSING STEPS*******************###
#Cleaning the train and test data with junk columns irrelevant to our analysis
train <-train[,-6:-7] #Removing the resolution and address columns from the training data set
length(train)

test<-test[,-5]

#Renaming the train and test columns for better understanding of the columns
names(train)<-c("Dates","Category","Description","DayOfWeek","PdDistrict","Longitude","Latitude")
names(test)<-c("Id","Dates","DayOfWeek","PdDistrict","Longitude","Latitude")

#Extracting year, month, day, hour, minute, and second from the Dates column using ymd_hms function
train$Year<- year(ymd_hms(train$Dates))
train$Month<- month(ymd_hms(train$Dates))
train$Day<- day(ymd_hms(train$Dates))
train$Hour<- hour(ymd_hms(train$Dates))
train$Minute<- minute(ymd_hms(train$Dates))
train$Second<- second(ymd_hms(train$Dates))
View(train)
train<-train[,-3] #Removing the Seconds column because all records have NULL Values
test$Year<- year(ymd_hms(test$Dates))
test$Month<- month(ymd_hms(test$Dates))
test$Day<- day(ymd_hms(test$Dates))
test$Hour<- hour(ymd_hms(test$Dates))
test$Minute<- minute(ymd_hms(test$Dates))
test$Second<- second(ymd_hms(test$Dates))
test<-test[,-3] #Removing the Seconds column because all records have NULL Values
View(test)



###**********************PREDICTION MODEL USING DATA MINING*******************###
### MODEL 1: LINEAR REGRESSION MODEL ###

View(trainsample)

#Creating a smaller dataset 'trainsample' to build a train model
set.seed(50)
trainsample <- train[sample(1:nrow(train), round(nrow(train)/50, 0)),]

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

#To find out the missing category variables 
outersect(unique(trainsample$Category), unique(train$Category))

#Add the missing category variables, "TREA" is missing, hence, adding it 
trainsample <- rbind(trainsample, train[head(which(train$Category=="TREA"),1),])
trainsample <- trainsample[order(trainsample$Dates),]
trainsample <- droplevels(trainsample)
View(trainsample)

crime <- unique(trainsample$Category)
submission<- data.frame(test$Id)
for(i in crime){
  response<- data.frame(trainsample$Category)
  print(i)
  response$flag<-0
  response[response$trainsample.Category==i, ]$flag<- 1
  #print(response[response$train.Category==i,]$flag)
  str(response) 
  model<-lm(response[,2]~DayOfWeek+PdDistrict+Longitude+Latitude+Hour+Month, trainsample)
  result<- predict(model, test, type="response")
  submission[,i] <-result
  assign(paste("model",i,sep ="_"),value =model)
  
}
View(submission)
summary(model_WARRANTS)
summary(model_ARSON)
summary(`model_OTHER OFFENSES`)              
summary(`model_LARCENY/THEFT`)
summary(`model_VEHICLE THEFT`)
summary(model_VANDALISM)
summary(`model_NON-CRIMINAL`)
summary(model_ROBBERY)
summary(model_ASSAULT)
summary(`model_WEAPON LAWS`)
summary(model_BURGLARY)
summary(`model_SUSPICIOUS OCC`)
summary(model_DRUNKENNESS)
summary(`model_FORGERY/COUNTERFEITING`)
summary(`model_DRUG/NARCOTIC`)
summary(`model_STOLEN PROPERTY`)
summary(`model_SECONDARY CODES`)
summary(model_TRESPASS)
summary(`model_MISSING PERSON`)
summary(model_FRAUD)
summary(model_KIDNAPPING)
summary(model_RUNAWAY)
summary(`model_DRIVING UNDER THE INFLUENCE`)
summary(`model_SEX OFFENSES FORCIBLE`)
summary(model_PROSTITUTION)
summary(`model_DISORDERLY CONDUCT`)
summary(model_ARSON)
summary(`model_FAMILY OFFENSES`)
summary(`model_LIQUOR LAWS`)
summary(model_BRIBERY)
summary(model_EMBEZZLEMENT)
summary(model_SUICIDE)



### MODEL 2: RANDOM FOREST MODEL ###
set.seed(1234)

#Here, we are goining to train and build a model using "Random Forest" package. 
#The target variable is "Category: and all the other variables are the predictors that are used.
#The data set is the sample set that contains only 2% of the original dataset
modelfit <- randomForest(Category ~ DayOfWeek + PdDistrict + Longitude + Latitude + Year + Month + Day + Hour + Minute, data = trainsample)

#The test data is supplied to the built model and the prediction is performed
model_prediction <- predict(modelfit, test, type="prob")

#This is to bind the ID with the output prediction result that was obtained
model_prediction <- cbind(Id=test$Id, model_prediction)
View(model_prediction)
names(model_prediction) <- c("Id", trainnames)

#The final prediction outcome i.e. in case of Random Forest, it is the proximity values is written into a new .CSV file
write.csv(model_prediction, "Predictions.csv", row.names=F)




###**********************VISUALIZATION AND EXPLORATORY DATA ANALYSIS*******************###
sf_map<-get_map("san francisco",zoom=12,maptype="toner-hybrid",source="stamen")
sf_map<-ggmap(sf_map)
head(trainViz)

#using aggregate function to find the total count of each category of crime
df<-aggregate(Dates~Category,trainViz,length)

#finding the top ten crimes in the city
df<-df[order(-df$Dates),]

#removing other offenses apart from top 10
df<-df[-2,]

#consdering the top ten offenses
df<-df[1:10,]
head(df)
tmp<-df$Category

#Creating a spatial map to see the spread of top ten crimes in SF city
map_data<-trainViz[trainViz$Category %in% tmp,]
head(map_data)
sf<-sf_map+geom_point(aes(x=X,y=Y,color=factor(Category)),alpha=0.05,data=map_data)
sf+guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),title="Type of Crime"))


#Creating a bar plot to get an acuurant count of the top 10 crimes
bar<-ggplot(df,aes(x=df$Category,y=df$Dates,fill=df$Category))
bar+geom_bar(stat="identity")+ggtitle(label = "Top ten crimes")+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0),axis.text.x=element_blank())+
  xlab(label ="Category")+ylab(label = "Count")+theme(panel.background = element_rect(fill = 'white'))+guides(fill=guide_legend(title="Category"))

#Creating a bar plot to find maximum crime occurrences for each district
district<-aggregate(Dates~PdDistrict+Category,trainViz,length)
g <- ggplot(district,aes(x=Category, y=Dates,fill =Category)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  facet_grid(.~PdDistrict) +
  theme(legend.position = "none")+theme(panel.background = element_rect(fill = 'white'))+xlab(label ="Category")+ylab(label ="Crime Count")

#Creating a bar plot to find maximum crime occurrences by day of week
week<-aggregate(Dates~DayOfWeek+Category,trainViz,length)
wp <- ggplot(week,aes(x=Category, y=Dates,fill =Category)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  facet_grid(.~DayOfWeek) +
  theme(legend.position = "none")+theme(panel.background = element_rect(fill = 'white'))+xlab(label ="Category")+ylab(label ="Crime Count")



### DATA SET SOURCE: SFOPENDATA (https://data.sfgov.org/)###
### DETAILED REPORT ON EACH MODELS AND ANALYSIS IS ENCLOSED ###
### END OF PROJECT CODE ###
