library("tidyverse") # Load tidyverse package
library(gtools)
library(ggplot2)

###Tranform .txt into matrix

#Create the dataframe
my_input <- readLines("2023_01_05_relative_5_1.txt")
my_input <- unlist(strsplit(my_input," "))

#Create a matrix with this vector
my_input <- matrix(my_input, nrow = 6, ncol = length(my_input)/6)
my_input

#transpose the matrix
my_input <- as.data.frame(t(my_input))
colnames(my_input) <- c("Region", "Window", "Alpha", "Beta", "Alpha_Error", "Beta_Error")

#Organize data
my_input <- mutate(my_input, Region = as.factor(Region),
                   Window = as.numeric(Window),
                   Alpha = as.numeric(Alpha),
                   Beta = as.numeric(Beta),
                   Alpha_Error = as.numeric(Alpha_Error),
                   Beta_Error = as.numeric(Beta_Error))

my_input <- my_input %>% 
  mutate(Region = fct_relevel(Region, c("upstream", "gene", "downstream"))) 

#str(my_input)

#Create different data.frames
upstream_data <- filter(my_input, Region=="upstream")

gene_data <- filter(my_input, Region=="gene")

downstream_data <- filter(my_input, Region=="downstream")

complete_data <- mutate(my_input, Window= 0:(nrow(my_input)-1))
complete_data <- mutate(complete_data, Window = as.numeric(Window))

#######Plot function
pd <- position_dodge(0.1)

###ALPHA
#Complete
plotting_complete_alpha<- function(data_input = complete_data)
  {
  #png(filename = "Alpha_complete_plot.png")
  p<- ggplot(data_input, (aes(x=Window, y=Alpha)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Window, ymin=Alpha-Alpha_Error, ymax=Alpha+Alpha_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Alpha) - Complete"))+
    xlab("Window (%)")+
    ylab("Alpha")
  #print(p)
  #dev.off()
  return(p)
}
plotting_complete_alpha()


#Upstream
plotting_upstream_alpha<- function(data_input = upstream_data)
{
  #png(filename = "Alpha_upstream_plot.png")
  p<- ggplot(data_input, (aes(x=Window, y=Alpha)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Window, ymin=Alpha-Alpha_Error, ymax=Alpha+Alpha_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Alpha) - upstream"))+
    xlab("Window (%)")+
    ylab("Alpha")
  #print(p)
  #dev.off()
  return(p)
}
plotting_upstream_alpha()


#Gene
plotting_gene_alpha<- function(data_input = gene_data)
{
  #png(filename = "Alpha_gene_plot.png")
  p<- ggplot(data_input, (aes(x=Window, y=Alpha)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Window, ymin=Alpha-Alpha_Error, ymax=Alpha+Alpha_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Alpha) - gene"))+
    xlab("Window (%)")+
    ylab("Alpha")
  #print(p)
  #dev.off()
  return(p)
}
plotting_gene_alpha()

#Downstream
plotting_downstream_alpha<- function(data_input = downstream_data)
{
  #png(filename = "Alpha_downstream_plot.png")
  p<- ggplot(data_input, (aes(x=Window, y=Alpha)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Window, ymin=Alpha-Alpha_Error, ymax=Alpha+Alpha_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Alpha) - downstream"))+
    xlab("Window (%)")+
    ylab("Alpha")
  #print(p)
  #dev.off()
  return(p)
}
plotting_downstream_alpha()

###BETA
#Complete
plotting_complete_beta<- function(data_input = complete_data)
{
  #png(filename = "Beta_complete_plot.png")
  p<- ggplot(data_input, (aes(x=Window, y=Beta)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Window, ymin=Beta-Beta_Error, ymax=Beta+Beta_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Beta) - Complete"))+
    xlab("Window (%)")+
    ylab("Beta")
  #print(p)
  #dev.off()
  return(p)
}
plotting_complete_beta()

#Upstream
plotting_upstream_beta<- function(data_input = upstream_data)
{
  #png(filename = "Beta_upstream_plot.png")
  p<- ggplot(data_input, (aes(x=Window, y=Beta)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Window, ymin=Beta-Beta_Error, ymax=Beta+Beta_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Beta) - Upstream"))+
    xlab("Window (%)")+
    ylab("Beta")
  #print(p)
  #dev.off()
  return(p)
}
plotting_upstream_beta()

#Gene
plotting_gene_beta<- function(data_input = gene_data)
{
  #png(filename = "Beta_gene_plot.png")
  p<- ggplot(data_input, (aes(x=Window, y=Beta)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Window, ymin=Beta-Beta_Error, ymax=Beta+Beta_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Beta) - Gene"))+
    xlab("Window (%)")+
    ylab("Beta")
  #print(p)
  #dev.off()
  return(p)
}
plotting_gene_beta()

#Downstream
plotting_downstream_beta<- function(data_input = downstream_data)
{
  #png(filename = "Beta_downstream_plot.png")
  p<- ggplot(data_input, (aes(x=Window, y=Beta)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Window, ymin=Beta-Beta_Error, ymax=Beta+Beta_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Beta) - Downstream"))+
    xlab("Window (%)")+
    ylab("Beta")
  #print(p)
  #dev.off()
  return(p)
}
plotting_downstream_beta()

###ALPHABETA

#library(patchwork)
ggplot(data=complete_data)+geom_line(data=complete_data, aes(x=Window, y=Alpha, colour="Alpha"))+geom_line(data=complete_data, aes(x=Window, y=Beta, colour="Beta"))+scale_color_manual(name = "Legend", values = c("Alpha" = "blue", "Beta" = "red"))+xlab("Window(%)")+ylab("")+ggtitle("Alpha Beta - Complete")


