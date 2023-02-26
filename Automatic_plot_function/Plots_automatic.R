library("tidyverse") # Load tidyverse package
library("gtools")
library("ggplot2")
#install.packages("ggpubr")
library("ggpubr")
#install.packages("Hmisc")
library(Hmisc)

#This scripts creates plots for every .csv files in a new folder ("Plots")

#temp = list.files(pattern="*.csv")
#myfiles = lapply(temp, read.delim)

setwd("C:/Users/sergi/Documents/Universidad/TUM/1º Semetre/High-Trought Analysis") #directory where the .csv files are
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
chr.list <- lapply(temp, read.csv)
dir.create("Plots")
setwd("Plots")

#Transform Window into bp

  for (i in 1:length(chr.list)) {
        chr.list[[i]] <- chr.list[[i]] %>%
        mutate(
          Window_bp = case_when(
            Window <= 100 ~ Window*(20)-2000,
            Window > 100 & Window <= 200 ~ (Window-100)*60,  
            Window > 200 ~ (Window-200)*(20)+6000
          )
        )
    }


###PLOTS
#Alpha-Beta rate

plot.Alfabeta <- function(chr.list) {
  plot.list <- vector(mode = 'list', length = length(chr.list))
  for (i in 1:length(chr.list)) {
    if("Alpha" %in% colnames(chr.list[[i]]) && "Beta" %in% colnames(chr.list[[i]])){
      plot.list[[i]] <- 
        ggplot(chr.list[[i]])+
        geom_rect(aes(xmin = 0, xmax = 6000, ymin = -Inf, ymax = Inf),
                  fill = "tan", alpha = 0.01)+
        geom_line((aes(x=Window_bp, y=Alpha*100, colour="Alpha"))) +
        geom_errorbar(aes(x=Window_bp, ymin=Alpha*100-Alpha.Error*100, ymax=Alpha*100+Alpha.Error*100, colour="Alpha"), width=0.7, size=0.4, position=position_dodge(0.05))+
        #geom_point((aes(x=Window_bp, y=Alpha*100)), size=2, shape=21, fill="white")+
        geom_line((aes(x=Window_bp, y=Beta*100, colour="Beta"))) +
        geom_errorbar(aes(x=Window_bp, ymin=Beta*100-Beta.Error*100, ymax=Beta*100+Beta.Error*100, colour="Beta"), width=0.7, size=0.4, position=position_dodge(0.05))+
        scale_color_manual(name = "Legend", values = c("Alpha"="turquoise", "Beta" = "violet"))+
        scale_x_continuous(breaks= seq(-2000,8000,by=500),
                           limits = c(-2000, 8000),
                           label = c("-2000bp", "", "-1000bp", "", "0%", "", "16%", "", "33%", "", "50%", "","66%", "", "83%", "", "100%", "", "1000bp", "", "2000bp"))+
        annotate("text", x=-1000, y=0, label= "Upstream", size = 6, colour="darkgrey", alpha = 0.7)+
        annotate("text", x=3000, y=0, label= "Gene", size = 6, colour="darkgrey", alpha = 0.7)+
        annotate("text", x=7000, y=0, label= "Downstream",  size = 6, colour="darkgrey", alpha = 0.7)+
        ggtitle(paste("Alpha-Beta rate", temp[i], sep=" - "))+
        xlab(" ")+
        ylab("Alpha (%)")
      names(plot.list)[i] <- paste0('p', i)
      ggsave(filename = paste("Alpha-Beta_rate_", temp[i], ".png", sep=""), device = "png", plot = plot.list[[i]], scale = 6)
    }
  }
  
  return(plot.list)
}

plot.Alfabeta(chr.list)


#CG steady state methylation levels

plot.steadystate_met <- function(chr.list) {
  plot.list <- vector(mode = 'list', length = length(chr.list))
  for (i in 1:length(chr.list)) {
    if("Observed.Steady.State" %in% colnames(chr.list[[i]]))
    {
      plot.list[[i]] <- 
        ggplot(chr.list[[i]])+
        geom_rect(aes(xmin = 0, xmax = 6000, ymin = -Inf, ymax = Inf),
                  fill = "tan", alpha = 0.01)+
        geom_line((aes(x=Window_bp, y=Observed.Steady.State))) +
        scale_x_continuous(breaks= seq(-2000,8000,by=500),
                           limits = c(-2000, 8000),
                           label = c("-2000bp", "", "-1000bp", "", "0%", "", "16%", "", "33%", "", "50%", "","66%", "", "83%", "", "100%", "", "1000bp", "", "2000bp"))+
        annotate("text", x=-1000, y=0, label= "Upstream", size = 6, colour="darkgrey", alpha = 0.7)+
        annotate("text", x=3000, y=0, label= "Gene", size = 6, colour="darkgrey", alpha = 0.7)+
        annotate("text", x=7000, y=0, label= "Downstream",  size = 6, colour="darkgrey", alpha = 0.7)+
        ggtitle(paste("CG steady state methylation levels", temp[i], sep=" - "))+
        xlab(" ")+
        ylab(" ")
      names(plot.list)[i] <- paste0('p', i)
      ggsave(filename = paste("CG_steady_state_methylation_levels_", temp[i], ".png", sep=""), device = "png", plot = plot.list[[i]], scale = 4)
    }
  }
  return(plot.list)
}

plot.steadystate_met(chr.list)


#Predicted CG steady metylation levels

plot.predicted_met <- function(chr.list) {
  plot.list <- vector(mode = 'list', length = length(chr.list))
  for (i in 1:length(chr.list)) {
    if("Predicted.Steady.State" %in% colnames(chr.list[[i]]))
    {
      plot.list[[i]] <- 
        ggplot(chr.list[[i]])+
        geom_rect(aes(xmin = 0, xmax = 6000, ymin = -Inf, ymax = Inf),
                  fill = "tan", alpha = 0.01)+
        geom_line((aes(x=Window_bp, y=Predicted.Steady.State*100))) +
        scale_x_continuous(breaks= seq(-2000,8000,by=500),
                           limits = c(-2000, 8000),
                           label = c("-2000bp", "", "-1000bp", "", "0%", "", "16%", "", "33%", "", "50%", "","66%", "", "83%", "", "100%", "", "1000bp", "", "2000bp"))+
        annotate("text", x=-1000, y=0, label= "Upstream", size = 6, colour="darkgrey", alpha = 0.7)+
        annotate("text", x=3000, y=0, label= "Gene", size = 6, colour="darkgrey", alpha = 0.7)+
        annotate("text", x=7000, y=0, label= "Downstream",  size = 6, colour="darkgrey", alpha = 0.7)+
        ggtitle(paste("Predicted CG steady metylation levels", temp[i], sep=" - "))+
        xlab("")+
        ylab("(%)")
      names(plot.list)[i] <- paste0('p', i)
      ggsave(filename = paste("Predicted_CG_steady_metylation_levels_", temp[i], ".png", sep=""), device = "png", plot = plot.list[[i]], scale = 4)
    }
  }
  return(plot.list)
}

plot.predicted_met(chr.list)


#1/2*(alpha + beta)

plot.12AlphaBeta <- function(chr.list) {
  plot.list <- vector(mode = 'list', length = length(chr.list))
  for (i in 1:length(chr.list)) {
    if("X1.2....Alpha...Beta." %in% colnames(chr.list[[i]]))
    {
      plot.list[[i]] <- ggplot(chr.list[[i]])+
        geom_rect(aes(xmin = 0, xmax = 6000, ymin = -Inf, ymax = Inf),
                  fill = "tan", alpha = 0.01)+
        geom_line((aes(x=Window_bp, y=X1.2....Alpha...Beta.))) +
        scale_x_continuous(breaks= seq(-2000,8000,by=500),
                           limits = c(-2000, 8000),
                           label = c("-2000bp", "", "-1000bp", "", "0%", "", "16%", "", "33%", "", "50%", "","66%", "", "83%", "", "100%", "", "1000bp", "", "2000bp"))+
        annotate("text", x=-1000, y=0, label= "Upstream", size = 6, colour="darkgrey", alpha = 0.7)+
        annotate("text", x=3000, y=0, label= "Gene", size = 6, colour="darkgrey", alpha = 0.7)+
        annotate("text", x=7000, y=0, label= "Downstream",  size = 6, colour="darkgrey", alpha = 0.7)+
        ggtitle(paste("1/2 * (Alpha + Beta)", temp[i], sep=" - "))+
        xlab(" ")+
        ylab(" ")
      names(plot.list)[i] <- paste0('p', i)
      ggsave(filename = paste("0.5(AlphaBeta)", temp[i], ".png", sep=""), device = "png", plot = plot.list[[i]], scale = 4)
      
    }
  }
  return(plot.list)
}

plot.12AlphaBeta(chr.list)
