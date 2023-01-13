library("tidyverse") # Load tidyverse package
library(gtools)
library(ggplot2)

###Tranform .txt into matrix

#Create the dataframe
data_file <- readLines("2023_01_05_relative_5_1.txt")
data_file <- unlist(strsplit(data_file," "))

#Create a matrix with this vector
data_file <- matrix(data_file, nrow = 6, ncol = length(data_file)/6)
data_file

#transpose the matrix
data_file <- as.data.frame(t(data_file))
colnames(data_file) <- c("Region", "Window", "Alpha", "Beta", "Alpha_Error", "Beta_Error")

#Organize data
data_file <- mutate(data_file, Region = as.factor(Region),
                    Window = as.numeric(Window),
                    Alpha = as.numeric(Alpha),
                    Beta = as.numeric(Beta),
                    Alpha_Error = as.numeric(Alpha_Error),
                    Beta_Error = as.numeric(Beta_Error))

data_file <- data_file %>% 
  mutate(Region = fct_relevel(Region, c("upstream", "gene", "downstream"))) 

#str(data_file)

#Calculate methylation levels

total_alpha=sum(data_file$Alpha)
total_beta=sum(data_file$Beta)

data_file <- data_file %>% 
  mutate(Alpha_methylation_level=Alpha/total_alpha)

data_file <- data_file %>% 
  mutate(Beta_methylation_level=Beta/total_beta) 

data_file <- mutate(data_file, 
                    Alpha_methylation_level = as.numeric(Alpha_methylation_level),
                    Beta_methylation_level = as.numeric(Beta_methylation_level))

#Create different data.frames
upstream_data <- filter(data_file, Region=="upstream")

gene_data <- filter(data_file, Region=="gene")

downstream_data <- filter(data_file, Region=="downstream")

complete_data <- mutate(data_file, Window= 0:(nrow(data_file)-1))
complete_data <- mutate(complete_data, Window = as.numeric(Window))


#str(data_file)

#######Plot function
pd <- position_dodge(0.1)

###ALPHA
#Complete
plotting_complete_alpha_methylation<- function(data_input = complete_data)
{
  p<- ggplot(data_input, (aes(x=Window, y=Alpha_methylation_level)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    ggtitle(paste("Methylation Levels (Alpha) - Complete"))+
    xlab("Window (%)")+
    ylab("Methylation level")
  ggsave("Alpha_complete_plot_MethylationLevels.png", scale = 3)
  return(p)
}
plotting_complete_alpha_methylation()

###BETA
#Complete
plotting_complete_beta_methylation<- function(data_input = complete_data)
{
  p<- ggplot(data_input, (aes(x=Window, y=Beta_methylation_level)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    ggtitle(paste("Methylation Levels (Beta) - Complete"))+
    xlab("Window (%)")+
    ylab("Methylation level")
  ggsave("Beta_complete_plot_MethylationLevels.png", scale = 3)
  return(p)
}
plotting_complete_beta_methylation()

###ALPHABETA

#library(patchwork)
ggplot(data=complete_data)+geom_line(data=complete_data, aes(x=Window, y=Alpha_methylation_level, colour="Alpha"))+geom_line(data=complete_data, aes(x=Window, y=Beta_methylation_level, colour="Beta"))+scale_color_manual(name = "Legend", values = c("Alpha" = "blue", "Beta" = "red"))+xlab("Window(%)")+ylab("Methylation Level")+ggtitle("Alpha Beta Methylation Levels- Complete")
ggsave("AlphaBeta_complete_plot_MethylationLevels.png", scale = 3)