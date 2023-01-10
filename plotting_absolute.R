library("tidyverse") # Load tidyverse package
library(gtools)
library(ggplot2)

###Tranform .txt into matrix

#Create the dataframe
data_file <- readLines("2023_01_10_absolute_256_64.txt")
data_file <- unlist(strsplit(data_file," "))

#Create a matrix with this vector
data_file <- matrix(data_file, nrow = 6, ncol = length(data_file)/6)
data_file

#transpose the matrix
data_file <- as.data.frame(t(data_file))
colnames(data_file) <- c("Region", "Basepair_length", "Alpha", "Beta", "Alpha_Error", "Beta_Error")

#Organize data
data_file <- mutate(data_file, Region = as.factor(Region),
                    Basepair_length = as.numeric(Basepair_length),
                    Alpha = as.numeric(Alpha),
                    Beta = as.numeric(Beta),
                    Alpha_Error = as.numeric(Alpha_Error),
                    Beta_Error = as.numeric(Beta_Error))

data_file <- data_file %>% 
  mutate(Region = fct_relevel(Region, c("upstream", "gene", "downstream"))) 

#str(data_file)

#Create different data.frames
upstream_data <- filter(data_file, Region=="upstream")

gene_data <- filter(data_file, Region=="gene")

downstream_data <- filter(data_file, Region=="downstream")

complete_data <- data_file

#######Plot function
pd <- position_dodge(0.1)

###ALPHA
#Complete
plotting_complete_alpha_absolute<- function(data_input = complete_data)
{
  p<- ggplot(data_input, (aes(x=Basepair_length, y=Alpha)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Basepair_length, ymin=Alpha-Alpha_Error, ymax=Alpha+Alpha_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Alpha) - Complete"))+
    xlab("Basepair_length (pb)")+
    ylab("Alpha")
  ggsave("Alpha_complete_plot_absolute.png", scale = 3)
  return(p)
}
plotting_complete_alpha_absolute()


#Upstream
plotting_upstream_alpha_absolute<- function(data_input = upstream_data)
{
  p<- ggplot(data_input, (aes(x=Basepair_length, y=Alpha)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Basepair_length, ymin=Alpha-Alpha_Error, ymax=Alpha+Alpha_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Alpha) - upstream"))+
    xlab("Basepair_length (pb)")+
    ylab("Alpha")
  ggsave("Alpha_upstream_plot_absolute.png", scale = 3)
  return(p)
}
plotting_upstream_alpha_absolute()


#Gene
plotting_gene_alpha_absolute<- function(data_input = gene_data)
{
  p<- ggplot(data_input, (aes(x=Basepair_length, y=Alpha)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Basepair_length, ymin=Alpha-Alpha_Error, ymax=Alpha+Alpha_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Alpha) - gene"))+
    xlab("Basepair_length (pb)")+
    ylab("Alpha")
  ggsave("Alpha_gene_plot_absolute.png", scale = 3)
  return(p)
}
plotting_gene_alpha_absolute()

#Downstream
plotting_downstream_alpha_absolute<- function(data_input = downstream_data)
{
  p<- ggplot(data_input, (aes(x=Basepair_length, y=Alpha)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Basepair_length, ymin=Alpha-Alpha_Error, ymax=Alpha+Alpha_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Alpha) - downstream"))+
    xlab("Basepair_length (pb)")+
    ylab("Alpha")
  ggsave("Alpha_downstream_plot_absolute.png", scale = 3)
  return(p)
}
plotting_downstream_alpha_absolute()

###BETA
#Complete
plotting_complete_beta_absolute<- function(data_input = complete_data)
{
  p<- ggplot(data_input, (aes(x=Basepair_length, y=Beta)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Basepair_length, ymin=Beta-Beta_Error, ymax=Beta+Beta_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Beta) - Complete"))+
    xlab("Basepair_length (pb)")+
    ylab("Beta")
  ggsave("Beta_complete_plot_absolute.png", scale = 3)
  return(p)
}
plotting_complete_beta_absolute()

#Upstream
plotting_upstream_beta_absolute<- function(data_input = upstream_data)
{
  p<- ggplot(data_input, (aes(x=Basepair_length, y=Beta)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Basepair_length, ymin=Beta-Beta_Error, ymax=Beta+Beta_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Beta) - Upstream"))+
    xlab("Basepair_length (pb)")+
    ylab("Beta")
  ggsave("Beta_upstream_plot_absolute.png", scale = 3)
  return(p)
}
plotting_upstream_beta_absolute()

#Gene
plotting_gene_beta_absolute<- function(data_input = gene_data)
{
  p<- ggplot(data_input, (aes(x=Basepair_length, y=Beta)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Basepair_length, ymin=Beta-Beta_Error, ymax=Beta+Beta_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Beta) - Gene"))+
    xlab("Basepair_length (pb)")+
    ylab("Beta")
  ggsave("Beta_gene_plot_absolute.png", scale = 3)
  return(p)
}
plotting_gene_beta_absolute()

#Downstream
plotting_downstream_beta_absolute<- function(data_input = downstream_data)
{
  p<- ggplot(data_input, (aes(x=Basepair_length, y=Beta)))+
    geom_line(position=pd) +
    geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_errorbar(aes(x=Basepair_length, ymin=Beta-Beta_Error, ymax=Beta+Beta_Error), width=0.2, colour="darkblue", alpha=0.9, size=0.6)+
    ggtitle(paste("DNA Methilation (Beta) - Downstream"))+
    xlab("Basepair_length (pb)")+
    ylab("Beta")
  ggsave("Beta_downstream_plot_absolute.png", scale = 3)
  return(p)
}
plotting_downstream_beta_absolute()

###ALPHABETA

#library(patchwork)
ggplot(data=complete_data)+geom_line(data=complete_data, aes(x=Basepair_length, y=Alpha, colour="Alpha"))+geom_line(data=complete_data, aes(x=Basepair_length, y=Beta, colour="Beta"))+scale_color_manual(name = "Legend", values = c("Alpha" = "blue", "Beta" = "red"))+xlab("Basepair_length (pb)")+ylab("")+ggtitle("Alpha Beta - Complete")
ggsave("AlphaBeta_complete_plot_absolute.png", scale = 3)

