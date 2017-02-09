## Pie graph

# Simulate data
set.seed(111); value = abs(rnorm(6))
df_pie <- data.frame(industry = paste0("industry",1:6), percent = value/sum(value))

# Animation Piece
fillLegendLabel <- scale_fill_manual(name = " ", # label is matched with color
                                     values =  c("industry1" = "magenta",
                                                 "industry2" = "dodgerblue",
                                                 "industry3" = "gray",
                                                 "industry4" = "khaki", 
                                                 "industry5" = "firebrick", 
                                                 "industry6" = "red"),
                                     labels=c("Bussiness", "Cleaning", 
                                              "Computer", "Education",
                                              "Finance", "Management"))

ggplot(df_pie) +
  aes(x = 1, y = percent, fill = industry) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y",  start = - pi / 3) +
  fillLegendLabel +
  geom_text(aes(x = 1.2, y = percent/2 + c(0, cumsum(percent)[-length(percent)]),  
                label = c("Bussiness", "Cleaning", 
                          "Computer", "Education",
                          "Finance", "Management")),size = 4) + 
  ggtitle("Pie graph test") +
  ylab("") +
  xlab("") + 
  theme(panel.background = element_rect(fill = 'white' ),
        plot.title = element_text(size = 25,colour="black"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()
        #legend.key.size = unit(2, "cm"),
        #legend.text = element_text(size = 20),
        #legend.title = element_text(size = 13)
  )
