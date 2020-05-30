
word_counts <- passDf %>% count(Password, sort = T) %>% data.frame(stringsAsFactors = FALSE) 
chatterplotDf <- word_counts %>% rowwise()  %>% mutate(Strength = as.numeric(passMark(Password)))

#write.csv(chatterplotDf,"J:/Personalprojects/Blogsite/sources/content/post/password-etiquette/chattrplotdf.csv")




set.seed(42)

p =  head(chatterplotDf,200)  %>% ggplot(aes( Strength,n, label = Password)) +
   # ggrepel geom, make arrows transparent, color by rank, size by n
   geom_text_repel(segment.alpha = 0,aes(colour = Strength, size=n)) +
#Scale the axes/log transform 
 coord_trans(x="log2", y="log2") +
  # ggrepel geom, make arrows transparent, color by rank, size by n
  #geom_text(aes(colour=Strength, size=n)) + 
  # set color gradient, customize legend
  scale_color_gradient(low="red2", high="green4", 
                       guide = guide_colourbar(direction = "vertical",
                                               title.position = "top",
                                               frame.colour = "black",
                                               ticks.colour = "white",
                                               label.theme = element_text(colour = "white"),
                                               title.theme = element_text(colour = "white"),
                                               barwidth = .2,
                                               barheight = 20, 
                                               )) + 
  # set word size range & turn off legend
  scale_size_continuous(range = c(1, 10),
                        guide = FALSE) + 
  ggtitle(paste0("Top 200 passwords from ",
                 nrow(chatterplotDf),   # dynamically include row count
                 " compromised passwords, by frequency"),
          subtitle = "Password frequency (size) ~ password strength (color)") + 
  labs(y = "Password Frequency  (log scale)", x = "Password Srength (log scale)") 

#theme
p2 <- p + theme(panel.background =  element_rect(fill = "gray4", color = NA),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray4", color = NA),
        panel.grid.major.x = (element_line(color = "grey40",
                                           size = 0.25,
                                           linetype = "dotted")),
        panel.grid.major.y = element_line(color = "grey40",
                                          size = 0.1,
                                          linetype = "dotted"),
        # Change axis line
        axis.line = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(size = 10,
                                  colour = "white"),
        plot.title = element_text(colour = "white",
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(face = "italic",
                                     size = 10,
                                     colour = "white"),
        legend.background = element_blank())
p2

#Save plot
#ggsave(file="ptest.svg", plot=p2, width=10, height=8)
