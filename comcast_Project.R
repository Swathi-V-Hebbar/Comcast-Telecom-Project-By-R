#setwd(choose.dir())
#getwd()



Comcast <- read.csv("C://Users//admin//Desktop//R_class//Comcast Telecom Complaints data.csv")
View(Comcast)
str(Comcast)

names(Comcast) <- c("Ticket","Customer complaint","Date","Time",
                    "Received Via","City","State","Zip Code","Status","Filling for others")



#install.packages("lubridate")
library(lubridate)
Comcast$Date_Modified <- parse_date_time(Comcast$Date,"%d!/%m!/%Y!")
View(Comcast)
str(Comcast)


sum(is.na(Comcast))
apply(Comcast,2,FUN = function(x){sum(is.na(x))})

Comcast=Comcast[(is.na(Comcast$Ticket))==F,]
sum(is.na(Comcast))


# 
# factorize <- function(Comcast){
#   for(i in 5:9) Comcast[i] = as.factor(Comcast[i])
#   return(Comcast)
# }
# factorize(Comcast)


Comcast$Ticket<- as.numeric(Comcast$Ticket)
Comcast$`Received Via`<- as.factor(Comcast$`Received Via`)
Comcast$City<- as.factor(Comcast$City)
Comcast$State<- as.factor(Comcast$State)
Comcast$`Zip Code`  <- as.factor(Comcast$`Zip Code`)
Comcast$Status <- as.factor(Comcast$Status)
Comcast$`Filling for others` <- as.factor(Comcast$`Filling for others`)

str(Comcast)



# 

# 
# unfactorize
# str(Comcast)


# library(dplyr)
# factor_var  <- c("Received via","City","State","Zip Code","Status", "Filling for others")
# factor_var
# Comcast[factor_var]
# lapply(Comcast$`Received Via`,factor)
# Comcast$State <- lapply(Comcast$State,factor)
# Comcast$City <-  lapply(Comcast$City,factor)
# Comcast$`Zip Code`<- lapply(Comcast$`Zip Code`,factor)
# Comcast$Status <- lapply(Comcast$Status,factor)
# Comcast$`Filling for others`<- lapply(Comcast$`Filling for others`,factor)



#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
library(magrittr)


Dialy_report<- Comcast %>% group_by(Date_Modified) %>% summarise(No_of_complaints_Dialy=n())
View(Dialy_report)
Monthly_report<-Comcast %>% group_by(Month=month(Comcast$Date_Modified)) %>%  summarise(No_of_complaints_monthly=n())
View(Monthly_report)

ggplot(Dialy_report,aes(Date_Modified,No_of_complaints_Dialy,label=No_of_complaints_Dialy)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 0.5) +
  geom_text()+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))



ggplot(Monthly_report,aes(Month,No_of_complaints_monthly,label=No_of_complaints_monthly)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 0.5) +
  geom_text()+
  scale_x_continuous(breaks = Monthly_report$S)+
  labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
  theme(plot.title = element_text(hjust = 0.5))




# 
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("RColorBrewer")
# install.packages("wordcloud")
library(tm)
library(SnowballC)
library("wordcloud")
library("RColorBrewer")

corpus = Corpus(VectorSource(Comcast$`Customer complaint`))

corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)

#Removing Punctuation
corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, c("and", stopwords("english")))

# Stemming
corpus = tm_map(corpus, stemDocument)

# Eliminate white spaces
corpus = tm_map(corpus, stripWhitespace)

DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)
head(dat, 5)

set.seed(124)
wordcloud(words = dat$word, freq = dat$freq, random.order=TRUE)


#library(dplyr)
Internet_Complaints<- contains(Comcast$`Customer complaint`,match="Internet",ignore.case = T)
Service_Complaints <- contains(Comcast$`Customer complaint`,match="Service",ignore.case = T)
Billing_Complaints <- contains(Comcast$`Customer complaint`,match="Billing",ignore.case = T)
Charges_Complaints <- contains(Comcast$`Customer complaint`,match="Charges",ignore.case=T)


Comcast$ComplaintType[Internet_Complaints]<- "Internet"
Comcast$ComplaintType[Service_Complaints]<- "Service"
Comcast$ComplaintType[Billing_Complaints]<- "Billing"
Comcast$ComplaintType[Charges_Complaints]<- "Charges"

Comcast$ComplaintType[-c(Internet_Complaints,
                         Service_Complaints,Billing_Complaints,Charges_Complaints)]<- "Others"
View(Comcast)

table(Complaint_type=Comcast$ComplaintType)


# Complaint_Table<-Comcast %>% group_by(ComplaintType) %>% summarise(Total_Count=n())
# View(Complaint_Table)
# Complaint_Table %>% filter(Total_Count==max(Complaint_Table$Total_Count))



Comcast<- transform(Comcast,Status_New = ifelse(Status %in% c("Pending","Open"),"Open",
                                                     if(Comcast$Status %in% c("Closed","Solved")){
                                                       "Closed"
                                                       }
                                                     ))

Comcast$Status_New<- as.factor(Comcast$Status_New)
View(Comcast)
str(Comcast)


library(ggplot2)

Comcast<- group_by(Comcast,State,Status_New)
X_axis_Chart<- summarise(Comcast,Count = n())
X_axis_Chart
ggplot(as.data.frame(X_axis_Chart) , aes(State,Count))+
  geom_col(aes(fill = Status_New),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        title = element_text(size = 16,colour = "#00AFBB"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Ticket Status Stacked Bar Chart ",
       x = "States",y = "Number of Tickets",
       fill= "Status")



State_Report<- Comcast %>% group_by(State) %>% summarise(Total=n())
View(State_Report)
State_Report %>% filter(Total==max(State_Report$Total))




Table1<-table(Comcast$State,Comcast$Status_New)
table_df<- as.data.frame.matrix(Table1)
# View(table_df)
# str(table_df)
table_df2<-group_by(Comcast,State) %>% summarise(Total_val=n())
# str(table_df2)
# View(table_df2)


table_df2$open<-cbind(table_df$Open)
table_df2$closed<-cbind(table_df$Closed)
View(table_df2)
Statewise_df<-transform(table_df2,Unresolved_percentage=round(table_df2$open/table_df2$Total_val*100,2))
Statewise_df<-transform(Statewise_df,Resolved_percentage=round(table_df2$closed/table_df2$Total_val*100,2))
View(Statewise_df)
Statewise_df %>% filter(Unresolved_percentage==max(Statewise_df$Unresolved_percentage))



table(Comcast$Status_New,Comcast$Received.Via)
nrow(Comcast)
resolved_data <- group_by(Comcast,Status_New)
total_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))*100) 
total_resloved
resolved_data <- group_by(Comcast,Received.Via,Status_New)
Category_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))*100) 
Category_resloved

