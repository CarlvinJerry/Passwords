#load packages
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(devtools)
library(remotes)
library(ggplot2)
library(ggrepel)
library(ggplot2)
library(ggrepel)
library(scales)


#The Data...
#Read Kaggle data for compromised passwords
#dataset <- read.csv("compromised.csv", stringsAsFactors = FALSE)
dataset <- read.csv("J:/Personalprojects/Blogsite/sources/content/post/password-etiquette/compromised.csv", stringsAsFactors = FALSE)

#Read #TidyTuesday data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

#Subset, bind password cols and check data
passwordDf = data.frame(Password = c(passwords$password,dataset$X0), stringsAsFactors = FALSE)
nrow(passwordDf) #Check rows to confirm new dataset

#remove nulls
passDf <- passwordDf %>% drop_na(Password)
nrow(passDf) #Check rows to confirm new dataset

#Save new dataSet
#write.csv(passDf,"J:/Personalprojects/Blogsite/sources/content/post/password-etiquette/compromisedPasswords.csv")

#Analysis
#Unique=========================================
uniques <- length(unique(passDf))
uniques

#Duplicates=============================
# duplicates <- length(passDf[duplicated(passDf)])
# duplicates

#Rankings======================================================
#Top 10 Passwords
#Counts
head(sort(table( passDf$Password ), decreasing = T),10)

#Percentages
head(sort(prop.table(table(passDf$Password)), decreasing = T),10)

#Tables

#Base words ====================
# library(tm)
# docs <- tm_map(docs, removeWords, stopwords("english"))
# 
# 

# 2. Number of characters
Passwordlength <- nchar(passDf$Password)

freq <- as.data.frame(table(Passwordlength))
sum(freq$Freq)


#plot1 number of characters distribution
data2 <- data.frame(x = freq$Passwordlength, y = freq$Freq)


plot1_CharactersDist <- ggplot(data2, aes(x = data2$x, y = data2$y)) +
  geom_segment(aes(x = data2$x, xend = data2$x, y = 0, yend = data2$y), 
               color = ifelse( data2$x %in% c(6,8), 'orange1','grey69'),
               size = ifelse( data2$x %in% c(6,8), 1.3,0.5)) +
  geom_point( size = ifelse( data2$x %in% c(6,8), 3,2),
              color = ifelse( data2$x %in% c(6,8), 'orange1','grey69')) + 
  theme_light() +
  theme(
    plot.background = element_rect(fill = "grey4"),
    panel.background = element_rect(fill = 'grey4'),
    panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line( size = .012, linetype = "dashed", colour = "grey"), # element_blank(),#
    panel.grid.minor.y = element_blank(),# element_line( size = .1, linetype = "dashed"),
    #panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text( hjust = 0.5, vjust = 0, colour = 'grey'),
    axis.text.x = element_text(colour = 'grey'),
    axis.text.y = element_text(colour = 'grey', angle = 90, hjust = 0.5),
    axis.title.x = element_text(colour = 'grey'),
    axis.title.y = element_text(colour = 'grey')
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




###PASSWORD STRENGTH ALGO
library(stringi)
library(stringr)

passMark <- function(passwords){
  #Additions
  #Number of characters----
  numChars = nchar(passwords)
  
  #Upper case letters ----
  upperCase = stringi::stri_count(passwords, regex  = "[A-Z]")
  
  #Lower case letters ----
  lowerCase = stringi::stri_count(passwords, regex  = "[a-z]")
  
  #Numbers----
  nums = stringi::stri_count(passwords, regex  = "[0-9]")
  
  #symbols
  symbols = c("~", "!", "@", "#", "\\$", "%", "\\^", "&", "\\*", "\\(" ,"\\)", "-", "\\+", "\\_", "=", "`" ,
              "\\{" ,"\\}" ,"\\[" ,"\\]",":", ";" , "<" , ">", "\\?" ,"," ,"\\.", "\\'", "@", "#", noquote("\""))
  
  num_symbols = stringr::str_count(passwords, paste(symbols, collapse = "|"))
  
  
  #In the Middle numbers
  midnums = stringi::stri_count(gsub('^.|.$', '', passwords), regex  = "[0-9]")
  
  #middle symbols
  
  mid_symbols = stringr::str_count(gsub('^.|.$', '', passwords), paste(symbols, collapse = "|"))
  
  requirements = c(upperCase, lowerCase, nums, num_symbols)
  
  requirements_score = 0
  for (i in requirements) {
    if(i > 0) requirements_score = requirements_score + 1 
  }
  
  requirements_score = ifelse(numChars < 8, 0, ifelse((requirements_score) < 3, 0, (requirements_score + 1)))
  
  character_count_score = (numChars * 4)
  uppercase_score = ifelse(upperCase == 0, 0, ((numChars - upperCase)*2))
  lowercase_score = ifelse(lowerCase == 0, 0, ((numChars - lowerCase)*2))
  numbers_score = ifelse(upperCase > 0 | lowerCase > 0 | num_symbols > 0, (nums * 4), 0) 
  symbols_score = (num_symbols * 6)
  mid_nums_symbol_score = ((midnums + mid_symbols) * 2)
  requirements_score = requirements_score * 2
  
  summed = 0 + character_count_score + uppercase_score + lowercase_score + numbers_score + symbols_score + mid_nums_symbol_score + requirements_score
  
  #Deductions
  #letters only
  letters_only = ifelse(numChars == (upperCase + lowerCase), numChars, 0)
  
  #numbers only
  numbers_only = ifelse(numChars == (nums), numChars, 0)
  
  split_function = function(password){
    password = str_extract_all(password, paste(c("[a-z]", "[A-Z]", "[0-9]", symbols), collapse = "|"))
    password = password[[1]]
    return(password)
  }
  
  #consecutive upper
  split_pass = split_function(passwords)
  consecutive_upper = ifelse(split_pass %in% LETTERS, 1, 0)
  r = rle(consecutive_upper)
  consecutive_upper = (r$lengths[r$values == 1]) - 1
  consecutive_upper = sum(consecutive_upper)
  
  #consecutive lower
  split_pass = split_function(passwords)
  consecutive_lower = ifelse(split_pass %in% letters, 1, 0)
  r = rle(consecutive_lower)
  consecutive_lower = (r$lengths[r$values == 1]) - 1
  consecutive_lower = sum(consecutive_lower)
  
  #Consecutive numbers
  split_pass = split_function(passwords)
  consecutive_numbers = ifelse(split_pass %in% (0:9), 1, 0)
  r = rle(consecutive_numbers)
  consecutive_numbers = (r$lengths[r$values == 1]) - 1
  consecutive_numbers = sum(consecutive_numbers)
  
  
  #Sequence checker function
  sequence_checker = function(split_pass, var_type) {
    
    if(var_type == "number"){
      
      sequence_check = as.integer(ifelse(split_pass %in% (0:9), split_pass, 0))
      
    } else if (var_type == "symbols"){
      
      sequence_check = ifelse(split_pass %in% symbols[1:13], match(split_pass, symbols[1:13]), 0)
      
    } else {
      
      sequence_check = ifelse(tolower(split_pass) %in% letters, match(tolower(split_pass), letters), 0)
      
    }
    sequence_check = abs(diff(sequence_check))
    sequence_check = ifelse(sequence_check == 1, 1, 0)
    r = rle(sequence_check)
    sequence_check = (r$lengths[r$values == 1]) - 1
    sequence_check = sum(sequence_check)
    return(sequence_check)
  }
  
  #letter sequence
  sequence_letters = sequence_checker(split_pass, "letters")
  
  #num sequence
  sequence_num = sequence_checker(split_pass, "number")
  
  #symbols
  sequence_symbols = sequence_checker(split_pass, "symbols")
  
  
  letters_only_score = letters_only
  numbers_only_score = numbers_only
  consecutive_upper_score = consecutive_upper * 2
  consecutive_lower_score = consecutive_lower * 2
  consecutive_numbers_score = consecutive_numbers * 2
  sequence_letters_score = sequence_letters * 3
  sequence_num_score = sequence_num * 3
  sequence_symbols_score = sequence_symbols * 3
  
  deductions = letters_only_score +
    numbers_only_score +
    consecutive_upper_score + 
    consecutive_lower_score + 
    consecutive_numbers_score +
    sequence_letters_score + 
    sequence_num_score +
    sequence_symbols_score
  
  total_score = ifelse((summed - deductions) < 0 , 0, ifelse((summed - deductions) > 100, 100, (summed - deductions)))
  
  # return(c(paste("Additions\nNumber of characters", character_count_score, sep = ": ") ,
  #          paste("\nUpper case characters", uppercase_score, sep = ":"),
  #          paste("\nLower case characters", lowercase_score, sep = ": "),
  #          paste("\nNumbers", numbers_score, sep = ": "),
  #          paste("\nSymbols", symbols_score, sep = ": "),
  #          paste("\nMiddle Numbers and symbols", mid_nums_symbol_score, sep = ": "),
  #          paste("\nRequirements", requirements_score, sep = ": "),
  #          paste("\n\nDeductions", sep = ""),
  #          paste("\nLetters only", letters_only_score, sep = ": "),
  #          paste("\nNumbers only", numbers_only_score, sep = ": "),
  #          paste("\nConsecutive upper", consecutive_upper_score, sep = ": "),
  #          paste("\nConsecutive lower", consecutive_lower_score, sep = ": "),
  #          paste("\nConsecutive numbers", consecutive_numbers_score, sep = ": "),
  #          paste("\nSequence letters", sequence_letters_score, sep = ": "),
  #          paste("\nSequence numbers", sequence_num_score, sep = ": "),
  #          paste("\nSequence symbols", sequence_symbols_score, sep = ": "),
         return(paste("", total_score))
  
  
}



#Subset data randomly to get 1500 passwords for strenght measure
df<-sample_n(passDf, 1000)



strenghtdf <- df %>% select(Password) %>% mutate(Strength = passMark(Password)) 

#ChatterPlot

# construct plot
ggplot(aes(avg_rank, n, label = word)) + 
  geom_text_repel(segment.alpha = 0, aes(colour=avg_rank,
                                         size=n)) + 
  scale_color_gradient(low="green3", high="violetred", trans = "log10",
                       guide = guide_colourbar(direction = "horizontal",
                                               title.position ="top")) +
  scale_size_continuous(range = c(3, 10),
                        guide = FALSE) +
  scale_x_log10() +
  ggtitle(paste0("Top 100 words from ",
                 nrow(hn_comments), 
                 " Hacker News article comments, by average ranking"),
          subtitle = "word frequency (size) ~ avg comment ranking (color)") + 
  labs(y = "Word frequency", x = "Avg rank (log scale)") +
  theme_minimal() +
  theme(legend.position=c(.99, .99), 
        legend.justification = c("right","top"),
        panel.grid.major = element_line(colour = "whitesmoke"))


