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

#Eliminate empty rows
data_file <- data_file %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Calculate methylation levels in gbM genes

data_file <- data_file %>% 
  mutate(Methylation_level_gbM=(Alpha*((1-Alpha)**2 -(1-Beta)**2 -1))/((Alpha+Beta)*((Alpha+Beta-1)**2 -2)))
         
data_file <- mutate(data_file, 
                    Methylation_level_gbM = as.numeric(Methylation_level_gbM))

#Create different data.frames
upstream_data <- filter(data_file, Region=="upstream")

gene_data <- filter(data_file, Region=="gene")

downstream_data <- filter(data_file, Region=="downstream")

complete_data <- mutate(data_file, Window= 0:(nrow(data_file)-1))
complete_data <- mutate(complete_data, Window = as.numeric(Window))


#str(data_file)

#######Plot function
pd <- position_dodge(0.1)

###PLOT
#Complete
plotting_complete_gbM_methylation<- function(data_input = complete_data)
{
  p<- ggplot(data_input, (aes(x=Window, y=Methylation_level_gbM)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    ggtitle(paste("Methylation Level gbM - Complete"))+
    xlab("Window (%)")+
    ylab("Methylation level gbM")
  ggsave("gbM_Complete_plot_MethylationLevels.png", scale = 3)
  return(p)
}
plotting_complete_gbM_methylation()

