##########################################  Plot Style  #######################################
#                                                                                             #
# Script to set the plot style globally:                                                      #
# "Structural complexity influences Light heterogeneity and Plant Diversity"                  #
#                                                                                             #
###############################################################################################




########################################## Set graph theme ####################################

theme_set(
  theme_bw() +
    theme(
      panel.border = element_rect(fill = NA),
      panel.grid.major = element_blank(),  #remove major-grid labels
      panel.grid.minor = element_blank(),  #remove minor-grid labels
      axis.ticks = element_line(),
      axis.text = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.justification=c(1,1), 
      legend.position=c(1,1),
      legend.background = element_rect(fill = NA)
    )
)


########################################## Colour Palettes #####################################

colour.palette = c("#4d0c7a", "#2891a7", "#e1ae34", "#99d777", "#ee5454") #the old palette
#colour.palette = c("#00008b", "#00cdcd", "#ffa500", "#D60A48", "#749300", "#a200ff")
col.theme <- scale_colour_manual(values = colour.palette)
col.fill <- scale_fill_manual(values = colour.palette)
dots <- "#00008b"
regline <- "#9cc600"

#111a11 	(17,26,17)
#cc0f0f 	(204,15,15)
#28c9c5 	(40,201,197)
#784d4d 	(120,77,77)
#4b1414