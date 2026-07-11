#Instantiating the functions we're graphing below into objects to keep code pretty
packages <- c("astsa", "dplyr", "ggplot2", 
              "timeSeries", "forecast",
              "ggfortify", "grid", 
              "ggthemes", "reshape2", "ggforce", 
              "scales", "lubridate", "tidyquant", 
              "cowplot", "knitr",  "kableExtra")
invisible(lapply(packages, library, character.only = TRUE))

kellyfunc <- function(x){(0.6 * log(1+x)) + (0.4 * log(1-x))}
second <- function(x){(0.7 * log(1+x)) + (0.3 * log(1-x))}
third <- function(x){(0.8 * log(1+x)) + (0.2 * log(1-x))}
jellybeanbet <- function(x){(0.7 * log(1-x)) + (0.2 * log(1+2*x)) + (0.1 * log(1+10*x))}

# Solving for the maximum of the kellyfunc
max <- data.frame(optimise(kellyfunc,c(0,1),maximum = T))
maxsecond <-data.frame(optimise(second,c(0,1),maximum = T))
maxthird <-data.frame(optimise(third,c(0,1),maximum = T))
maxjelly <- data.frame(optimise(jellybeanbet,c(0,1),maximum = T))

# Setting a ggplot theme to make the graph a little prettier by default
theme_set(theme_bw())

# Building the Graph

# Initialize graph
graph <- ggplot() +
  # Add x and y intercepts
  geom_hline(yintercept = 0,linewidth = 1.25) + 
  geom_vline(xintercept = 0,linewidth = 1.25) +
  
  # Adding geometry for our functions we want - the aes piece names them in the legend
  geom_function(fun = kellyfunc, linewidth = 2,aes(col = "p=0.6"))+
  geom_function(fun = second, linewidth = 2,aes(col = "p=0.7")) +
  geom_function(fun = third, linewidth = 2,aes(col = "p=0.8")) +
  #geom_function(fun = jellybeanbet, linewidth = 2,aes(col = "Loss: p=0.7  
  #\n Win 2x pay: p=0.2 \n Win 10x pay: p=0.1")) +
  
  # Adds on the point representing the maximum for the kelly function
  geom_point(data = max,aes(x = maximum, y = objective),size = 2) + 
  geom_point(data = maxsecond,aes(x = maximum, y = objective),size = 2) +
  geom_point(data = maxthird,aes(x = maximum, y = objective),size = 2) +
  #geom_point(data = maxjelly,aes(x = maximum, y = objective),size = 2) +
  # this is made irrelevant by the theme set above,  but I'm leaving it in
  # to save the color codes and syntax in case it's more desirable later
  #scale_colour_manual(values= c('#86A8E7','#5FFBF1'),name = 'Function') +
  
  # Rescales the labels so they are percentages and zooms to the relevant part of the graph
  scale_y_continuous(labels = scales::percent,limits = c(-0.05,0.25)) +
  scale_x_continuous(labels = scales::percent,limits = c(0,1))


# Defines a grob object from the grid package that 
#lets us put an annotation straight onto the graph at a scaled location
kellylabel <- grobTree(textGrob('Max Growth Rate / Ideal Kelly Bet', 
                                x=0.23,  
                                y=0.3,
                                gp=gpar(col='#86A8E7',fontsize=9,fontface="bold")))

# Adding more stuff to the graph...
graph <- graph +
  # Cleaning up X and Y labels
  labs(x = 'Wagered fraction (f)', y = 'Growth Rate (r)')  +
  
  # Adding a title, formatting, and centering it
  ggtitle('Maximizing growth rate given larger payout outcomes') +
  theme(plot.title = element_text(size=12, face='bold',hjust = 0.5)) +
  
  # Add the label defined above onto the graph
  annotation_custom(kellylabel)
# Display it
graph