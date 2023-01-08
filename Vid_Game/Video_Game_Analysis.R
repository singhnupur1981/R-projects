library(tidyverse)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(data.table)


options(warn=-1)
options(repr.plot.width =20, repr.plot.height =12)
options(dplyr.summarise.inform = FALSE)


#Preparing the data:
data <- read_csv("vgsales.csv",show_col_types =FALSE)
names(data) <- str_replace_all(names(data), c(" " = "_"))
data <- data[data$Year>=2010 & data$Year<=2020,]
head(data)

all <- data[c("Publisher","Global_Sales")] %>% group_by(Publisher)%>%summarise(sales_sum = sum(Global_Sales))  # Review 的平均值來比較

Top6gp2012 <- all[order(-all$sales_sum),]
Top6gp2012 <- Top6gp2012[1:6,]
Top6gp2012

#Data visualization Top 6 Publishers:

ggplot(head(all[order(-all$sales_sum),]),aes(x=Publisher,y=sales_sum))+
  geom_col(fill = "bisque4")+
  ggtitle("Top 6 Publisher")+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.title = element_text(size = 5),
        axis.text = element_text(size = 5),
        axis.title.x=element_text(size = 10),
        axis.title.y=element_text(size = 10),
        legend.title=element_text(size = 10),
        legend.text =element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5))



Top6pf2012 <- data[data$Year == 2012,c("Platform","Year","Global_Sales")] %>% group_by(Platform)%>%summarise(gss=sum(Global_Sales))
Top6pf2012 <- head(Top6pf2012[order(-Top6pf2012$gss),])
Top6pf2012

#Data visualization Top 6 Publishers:

ggplot(Top6pf2012)+
  geom_col(aes(x=Platform,y=gss,fill=Platform), fill = "bisque4")+
  ylab("sales_sum")+
  ggtitle("Top 6 Game Platform")+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title.x=element_text(size = 10),
        axis.title.y=element_text(size = 10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        plot.title = element_text(size = 15, hjust = 0.5))


#Data visualization Genre Saless:

ab <- data %>% group_by(Genre) %>% summarise(sales_sum = sum(Global_Sales))
ab[order(-ab$sales_sum),]
ggplot(ab,aes(x=Genre,y=sales_sum))+
  geom_col(fill="bisque4")+
  ggtitle("Genre Sales")+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),     
        axis.title.x=element_text(size = 10),
        axis.title.y=element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 20, hjust = 0.5))

selected <- data[data$Publisher%in%Top6gp2012$Publisher,c("Publisher","JP_Sales","EU_Sales","NA_Sales","Other_Sales","Global_Sales") ]
long <- melt(selected, 
             id.var= "Publisher",
             variable.name = "country",
             value.name = "sales")

gj<-long  %>% group_by(country,Publisher)%>%
  summarise(sales_sum = sum(sales))
gj<-as.data.frame(gj)
gj<-transform(gj, country=as.character(country))
dd <- gj %>% group_by(country) %>% summarise(max_sales = max(sales_sum))
dd$Publisher =gj[gj$sales_sum %in% dd$max_sales, "Publisher"]
dd

#Data visualization on Publisher game sales for each country
ggplot(gj,aes(x=Publisher,y=sales_sum,group =country, colour=country))+
  geom_line()+
  ggtitle("Publisher game sales for each country")+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1),     
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title.x=element_text(size = 10),
        axis.title.y=element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 20, hjust = 0.5))

#Data visualization on total sales publisher wise for each country 
ggplot(gj)+
  geom_col(aes(x=country,y=sales_sum,fill=Publisher))+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x=element_text(size = 12),
        axis.title.y=element_text(size = 12),
        legend.title=element_text(size = 10),
        legend.text = element_text(size= 10),
        plot.title = element_text(size = 20, hjust = 0.5))


  selected <- data[data$Platform%in%Top6pf2012$Platform,c("Platform","JP_Sales","EU_Sales","NA_Sales","Other_Sales","Global_Sales") ]
long <- melt(selected, 
             id.var= "Platform",
             variable.name = "country",
             value.name = "sales")

gj<-long  %>% group_by(country,Platform)%>%
  summarise(sales_sum = sum(sales))
gj<-as.data.frame(gj)
gj<-transform(gj, country=as.character(country))
dd <- gj %>% group_by(country) %>% summarise(max_sales = max(sales_sum))
dd$Platform =gj[gj$sales_sum %in% dd$max_sales, "Platform"]
dd

#Data visualization on platform game sales for each country -Linechart:
ggplot(gj,aes(x=Platform,y=sales_sum,group =country, colour=country))+
  geom_line()+
  ggtitle("Platform game sales for each country")+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1),     
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x=element_text(size = 12),
        axis.title.y=element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 20, hjust = 0.5))

#Data visualization on platform game sales for each country -Barchart:
ggplot(gj)+
  geom_col(aes(x=country,y=sales_sum,fill=Platform))+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x=element_text(size = 12),
        axis.title.y=element_text(size = 12),
        legend.title=element_text(size = 10),
        legend.text = element_text(size= 10),
        plot.title = element_text(size = 20, hjust = 0.5))


  selected <- data[,c("Genre","JP_Sales","EU_Sales","NA_Sales","Other_Sales","Global_Sales") ]
long <- melt(selected, 
             id.var= "Genre",
             variable.name = "country",
             value.name = "sales")

gj<-long  %>% group_by(country,Genre)%>%
  summarise(sales_sum = sum(sales))
gj<-as.data.frame(gj)
gj<-transform(gj, country=as.character(country))
dd <- gj %>% group_by(country) %>% summarise(max_sales = max(sales_sum))
dd$Genre =gj[gj$sales_sum %in% dd$max_sales, "Genre"]
dd

# Visualzation for Sales sum by genre of each country- line graph:
ggplot(gj,aes(x=Genre,y=sales_sum,group =country, colour=country))+
  geom_line()+
  ggtitle("Sales sum by genre of each country")+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),     
        axis.title.x=element_text(size = 12),
        axis.title.y=element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 20, hjust = 0.5))

# Visualzation for Sales sum by genre of each country- histogram:
ggplot(gj)+
  geom_col(aes(x=country,y=sales_sum,fill=Genre))+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title.x=element_text(size = 22),
        axis.title.y=element_text(size = 22),
        legend.title=element_text(size = 16),
        legend.text = element_text(size= 16),
        plot.title = element_text(size = 32, hjust = 0.5))
  
  year_sales <- data%>% count(Year)

#Visualzation for Yearly total game sales - line graph:
ggplot(year_sales,aes(x=Year,y=n,group=1))+
  geom_line(fill="bisque4")+
  ggtitle("Yearly total game sales")+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),     
        axis.title.x=element_text(size = 22),
        axis.title.y=element_text(size = 22),
        plot.title = element_text(size = 32, hjust = 0.5))

  
  selected <- data[,c("Year","JP_Sales","EU_Sales","NA_Sales","Other_Sales","Global_Sales") ]
long <- melt(selected, 
             id.var= "Year",
             variable.name = "country",
             value.name = "sales")

gj<-long  %>% group_by(country,Year)%>%
  summarise(sales_sum = sum(sales))
gj<-as.data.frame(gj)
gj<-transform(gj, country=as.character(country))

  
  selected <- data[data$Publisher%in%Top6gp2012$Publisher,c("Publisher","Global_Sales","Year")]
selected <- selected %>% group_by(Year,Publisher)%>%summarise(gss = sum(Global_Sales))


#Visualzation for top 6 publishers yearly sales change over the years - line graph:

ggplot(selected)+
  geom_line(aes(x=Year,y=gss,group=Publisher,colour=Publisher))+
  ggtitle("Top 6 Publishers yearly sales change")+
  theme_economist() +
  ylab("sales_sum")+
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x=element_text(size = 12),
        axis.title.y=element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 20, hjust = 0.5))

#Visualzation for top 6 publishers yearly sales change over the years - line graph:
  
  selected <- data[data$Publisher%in%Top6gp2012$Publisher,c("Publisher","Global_Sales","Year")]
selected <- selected %>% group_by(Year,Publisher)%>%summarise(gss = sum(Global_Sales))
ggplot(selected)+
  geom_line(aes(x=Year,y=gss,group=Publisher,colour=Publisher))+
  ggtitle("Top 6 Publishers yearly sales change")+
  theme_economist() +
  ylab("sales_sum")+
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x=element_text(size = 12),
        axis.title.y=element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 20, hjust = 0.5))

#Visualzation for top 6 platforms  yearly sales change over the years - line graph:
  
  selected <- data[data$Platform%in%Top6pf2012$Platform,c("Platform","Global_Sales","Year")]
selected <- selected %>% group_by(Year,Platform)%>%summarise(gss=sum(Global_Sales))
ggplot(selected)+
  geom_line(aes(x=Year,y=gss,group=Platform,colour=Platform))+
  ggtitle("Top 6 Platform yearly Sales change")+
  ylab("Global_Sales")+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x=element_text(size = 12),
        axis.title.y=element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 20, hjust = 0.5))
  
#Visualzation for genre  yearly sales change over the years - line graph:
  
  selected <- data[,c("Genre","Global_Sales","Year")]
selected <- selected %>% group_by(Year,Genre)%>%summarise(gss=sum(Global_Sales))
ggplot(selected)+
  geom_line(aes(x=Year,y=gss,group=Genre,colour=Genre))+
  ggtitle("Genre yearly Sales change")+
  ylab("Global_Sales")+
  theme_economist() +
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "bisque3", size = 1), 
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x=element_text(size = 12),
        axis.title.y=element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 20, hjust = 0.5))
  