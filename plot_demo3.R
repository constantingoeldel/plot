library("tidyverse") # Load tidyverse package
library(gtools)

#Create a list with the alpha and beta(missing - LATER)

#Alpha data list
Base_alpha <- list.files(path = "/mnt/extStorage/constantin/windows/", pattern="boot_base_alpha", recursive = TRUE)
Base_alpha <- mixedsort(Base_alpha)
Base_alpha

#Alpha-error list
Error_alpha <- list.files(path = "/mnt/extStorage/constantin/windows/", pattern="errors_alpha", recursive = TRUE)
Error_alpha <- mixedsort(Error_alpha)
Error_alpha

#Beta data list
Base_beta <- list.files(path = "/mnt/extStorage/constantin/windows/", pattern="boot_base_beta.txt", recursive = TRUE)
Base_beta <- mixedsort(Base_beta)
Base_beta

#Beta-error list
Error_beta <- list.files(path = "/mnt/extStorage/constantin/windows/", pattern="errors_beta", recursive = TRUE)
Error_beta <- mixedsort(Error_beta)
Error_beta

################################################# Alpha plot
y_list = list()
x_list = list()
y.sd_list = list()
x <- 0

for (i in 1:length(Base_alpha)){
  #print(Base_alpha[i])
  setwd("/mnt/extStorage/constantin/windows")
  y <- read.table(gsub(" ", "/t", Base_alpha[i]))[2]
  y.sd <- read.table(gsub(" ", "/t", Error_alpha[i]))[2]
  x <- x+5
  
  #Transform de frame.value in numeric
  y = as.numeric(as.character(y))
  y.sd = as.numeric(as.character(y.sd))
  
  #print(y)
  #print(y.sd)
  #print(x)
  #Create list
  y_list <- append(y_list, y)
  y.sd_list <- append(y.sd_list, y.sd)
  x_list <- append(x_list, x)
  
}

setwd("/mnt/extStorage/sergio/plot")
pdf(file="saving_plot.pdf")
plot(x, y, xlab="%", ylab="alpha", pch=16, cex=2)
arrows(x0=x, y0=y-y.sd, x1=x, y1=y+y.sd, code=3, angle=0, length=0.1)
dev.off()



#class(x)
#class(y.sd)
