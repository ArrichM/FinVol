
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Plot Time series
# png(file = "Time_Series.png", width = 9000, height = 9000, units = "px", res = 1200) # width and height need to be adjusted properly
autoplot(data) + facet_wrap( ~ series, nrow = 3, scale = "free"  ) + 
  scale_colour_manual(values=c("darkgray","darkgray", "darkgray"))+
  guides(colour = FALSE)+
  theme_light() 
# dev.off()


# Plot Variance plots
# png(file = "Variance plots.png", width = 9000, height = 9000, units = "px", res = 1200) # width and height need to be adjusted properly
autoplot(rollapply(data, 12, var, by.column = T)) + facet_wrap( ~ series, nrow = 3, scale = "free") + 
  scale_colour_manual(values=c("darkgray","darkgray", "darkgray"))+
  guides(colour = FALSE)+
  ylab("Variance")+
  theme_light() 
# dev.off()


# Plot Correlation
# png(file = "Correlation plots.png", width = 9000, height = 9000, units = "px", res = 1200) # width and height need to be adjusted properly
autoplot(lapply(combn(1:3,2,simplify = F), 
                FUN = function(col) rollapply(cbind(data[,col[1]],data[,col[2]]), 12, 
                                              FUN = function(x) cor(x[,1],x[,2])  , by.column = F)) %>% do.call(what= cbind) %>% 
           set_colnames(c("GDP-Exports","GDP-Consumption","Exports-Consumption"))) + facet_wrap( ~ series, nrow = 3, scale = "free") + 
  scale_colour_manual(values=c("darkgray","darkgray", "darkgray"))+
  guides(colour = FALSE)+
  ylab("Variance")+
  theme_light() 
# dev.off()