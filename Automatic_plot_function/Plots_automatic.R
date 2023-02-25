library("tidyverse") # Load tidyverse package
library("gtools")
library("ggplot2")
#install.packages("ggpubr")
library("ggpubr")

#This scripts creates plots for every .csv files in a new folder ("Plots")

#temp = list.files(pattern="*.csv")
#myfiles = lapply(temp, read.delim)

setwd("C:/Users/sergi/Documents/Universidad/TUM/1º Semetre/High-Trought Analysis") #directory where the .csv files are
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
chr.list <- lapply(temp, read.csv)
dir.create("Plots")
setwd("Plots")

###PLOTS
#Alpha-Beta rate

plot.Alfabeta <- function(chr.list) {
  plot.list <- vector(mode = 'list', length = length(chr.list))
  for (i in 1:length(chr.list)) {
    if("Alpha" %in% colnames(chr.list[[i]]) && "Beta" %in% colnames(chr.list[[i]])){
    plot.list[[i]] <- 
      ggplot(chr.list[[i]])+
      geom_rect(aes(xmin = 100, xmax = 200, ymin = -Inf, ymax = Inf),
                fill = "tan", alpha = 0.01)+
      geom_line((aes(x=Window, y=Alpha*100, colour="Alpha"))) +
      geom_errorbar(aes(x=Window, ymin=Alpha*100-Alpha.Error*100, ymax=Alpha*100+Alpha.Error*100, colour="Alpha"), width=1, size=0.4, position=position_dodge(0.05))+
      #geom_point((aes(x=Window, y=Alpha*100)), size=2, shape=21, fill="white")+
      geom_line((aes(x=Window, y=Beta*100, colour="Beta"))) +
      geom_errorbar(aes(x=Window, ymin=Beta*100-Beta.Error*100, ymax=Beta*100+Beta.Error*100, colour="Beta"), width=1, size=0.4, position=position_dodge(0.05))+
      scale_color_manual(name = "Legend", values = c("Alpha"="turquoise", "Beta" = "violet"))+
      ggtitle(paste("Alpha-Beta rate", temp[i], sep=" - "))+
      xlab("Window")+
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
        geom_rect(aes(xmin = 100, xmax = 200, ymin = -Inf, ymax = Inf),
                  fill = "tan", alpha = 0.01)+
        geom_line((aes(x=Window, y=Observed.Steady.State))) +
        ggtitle(paste("CG steady state methylation levels", temp[i], sep=" - "))+
        xlab("Window")+
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
        geom_rect(aes(xmin = 100, xmax = 200, ymin = -Inf, ymax = Inf),
                  fill = "tan", alpha = 0.01)+
        geom_line((aes(x=Window, y=Predicted.Steady.State*100))) +
        ggtitle(paste("Predicted CG steady metylation levels", temp[i], sep=" - "))+
        xlab("Window")+
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
        geom_rect(aes(xmin = 100, xmax = 200, ymin = -Inf, ymax = Inf),
                  fill = "tan", alpha = 0.01)+
        geom_line((aes(x=Window, y=X1.2....Alpha...Beta.))) +
        ggtitle(paste("1/2 * (Alpha + Beta)", temp[i], sep=" - "))+
        xlab("Window")+
        ylab(" ")
      names(plot.list)[i] <- paste0('p', i)
      ggsave(filename = paste("0.5(AlphaBeta)", temp[i], ".png", sep=""), device = "png", plot = plot.list[[i]], scale = 4)
        
    }
  }
  return(plot.list)
}

plot.12AlphaBeta(chr.list)


