#load data
library(readr)
library(dplyr)
library(tidyverse)

dataset <- read.csv("compromised.csv", stringsAsFactors = FALSE)
#View(dataset)

# library(readxl)
# vv<- as.data.frame(read.delim("compromised-passwords.tsv", stringsAsFactors = FALSE))
# class(vv)

#passwords <- read_excel("SamplePass.xlsx")
passwords <- dataset[,1]
length(passwords)
tail(passwords, 50)

#remove nulls
pass <- passwords[complete.cases(passwords)]
length(pass)

#Analysis
#Unique=========================================
uniques <- length(unique(pass))
uniques

#Duplicates=============================
duplicates <- length(pass[duplicated(pass)])
duplicates

#Rankings======================================================
#Top 10 Passwords
#Counts
head(sort(table(passwords), decreasing = T),10)

#Percentages
head(sort(prop.table(table(passwords)), decreasing = T),10)

#Tables

#Base words ====================
library(tm)
docs <- tm_map(docs, removeWords, stopwords("english"))



# 2. Number of characters
Passwordlength <- nchar(pass)

fre <- as.data.frame(table(Passwordlength))
sum(fre$Freq)

#plot1 number of characters distribution
library(ggplot2)
data2 <- data.frame(x = fre$Passwordlength, y = fre$Freq)


plot1_CharactersDist <- ggplot(data2, aes(x = data2$x, y = data2$y)) +
  geom_segment(aes(x = data2$x, xend = data2$x, y = 0, yend = data2$y), 
               color = ifelse( data2$x %in% c(6,8), 'orange1','grey69'),
               size = ifelse( data2$x %in% c(6,8), 1.3,0.9)) +
  geom_point( size = ifelse( data2$x %in% c(6,8), 5,3),
              color = ifelse( data2$x %in% c(6,8), 'orange1','grey69')) + 
  theme_light() +
  theme(
    # plot.background = element_rect(fill = "grey4"),
    # panel.background = element_rect(fill = 'grey10'),
    panel.grid.major.x = element_blank(),
    #panel.grid.major.y = element_blank(),
    #panel.grid.minor.y = element_blank(),
    #panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text( hjust = 0.5, vjust = 0, colour = 'grey13'),
    axis.text.x = element_text(colour = 'grey25'),
    axis.text.y = element_text(colour = 'grey25', angle = 90, hjust = 0.5),
    axis.title.x = element_text(colour = 'grey15'),
    axis.title.y = element_text(colour = 'grey15')
  ) +
  xlab('Password Length') +
  ylab('Frequency') 

plot1_CharactersDist +
  annotate("text", x = data2$x[which(data2$x == 6)], y = data2$y[which(data2$x == 6)]*1,
           label = paste("6 characters(Minimum recommended)\n (val=",data2$y[which(data2$x == 6)] %>% round(2),")",sep = "" ),
           color = "firebrick4",size = 3.3, angle = 0, fontface = "italic", hjust = -0.05) +
  annotate("text", x = data2$x[which(data2$x == 7)], y = data2$y[which(data2$x == 7)]*1,
           label = paste("7 Characters\n (val=",data2$y[which(data2$x == 7)] %>% round(2),")",sep = "" ), 
           color = "orchid4", size = 3.3, angle = 0, fontface = "italic", hjust = -0.05) +
  annotate("text", x = data2$x[which(data2$x == 8)], y = data2$y[which(data2$x == 8)]*1,
           label = paste("8 Characters(Recommended by most websites)\n (val=",data2$y[which(data2$x == 8)] %>% round(2),")",sep = "" ), 
           color = "palegreen4", size = 3.3, angle = 0, fontface = "italic", hjust = -0.05) +
  annotate("text", x = data2$x[which(data2$x == 0:5)], y = data2$y[which(data2$x == 0:5)]*1,
           label = paste("\n ",data2$y[which(data2$x == 0:5)] %>% round(2),"",sep = "" ), 
           color = "firebrick3", size = 3.3, angle = 90, fontface = "italic", hjust = -0.05, vjust = 0.05) +
  annotate("text", x = data2$x[which(data2$x %in% c( 9:41))], y = data2$y[which(data2$x %in% c( 9:41))]*1,
           label = paste("\n ",data2$y[which(data2$x %in% c( 9:41))] %>% round(2),"",sep = "" ), 
           color = "palegreen4", size = 3.3, angle = 90, fontface = "plain", hjust = -0.05, vjust = 0.1) +
  # annotate("rect", xmin = data2$x[which(data2$x == 6)], xmax = data2$x[which(data2$x == 41)], ymin = 0, ymax = 0,
  #            fill = 'green', alpha = .5) +
  # annotate("rect", xmin = data2$x[which(data2$x == 0)], xmax = data2$x[which(data2$x == 6)], ymin = 0, ymax = 0,
  #          fill = 'red', alpha = 1) +
  # annotate("segment", x = 7, xend = 10, y = 5000,
  #          yend = 5000, colour = "green", size = 0.5, alpha = 0.25) +
  # annotate("segment", x = 3, xend = 7, y = 5000,
  #          yend = 5000, colour = "red", size = 0.5, alpha = 0.25)
  ggtitle("Distribution Of Number Of Characters in Passwords") 

#dev.off()
