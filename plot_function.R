library("tidyverse") # Load tidyverse package
library(gtools)

#Create a list with the alpha and beta(missing - LATER)

#Alpha data list
Base_alpha <- list.files(path = "/home/constantin/windows/", pattern="boot_base_alpha", recursive = TRUE)
Base_alpha <- mixedsort(Base_alpha)
Base_alpha

#Alpha-error list
Error_alpha <- list.files(path = "/home/constantin/windows/", pattern="errors_alpha", recursive = TRUE)
Error_alpha <- mixedsort(Error_alpha)
Error_alpha

#Beta data list
Base_beta <- list.files(path = "/home/constantin/windows/", pattern="boot_base_beta.txt", recursive = TRUE)
Base_beta <- mixedsort(Base_beta)
Base_beta

#Beta-error list
Error_beta <- list.files(path = "/home/constantin/windows/", pattern="errors_beta", recursive = TRUE)
Error_beta <- mixedsort(Error_beta)
Error_beta

################################################# Alpha plot
y_list = list()
x_list = list()
y.sd_list = list()
x <- (-5)

for (i in 1:length(Base_alpha)){
  #print(Base_alpha[i])
  setwd("/home/constantin/windows")
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


#Create a matrix with alpha data
data_A <- list(x_list, y_list, y.sd_list)
mtrx_A <- matrix(unlist(data_A), ncol = 3, nrow = length(y_list))
mtrx_A

#Create a table
library(MASS)
write.matrix(mtrx_A,file="Mat_A.csv")
Mat_A <- read_table("Mat_A.csv", col_names = FALSE)

#PLOT
pdf(file="saving_plot_alpha.pdf")
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

p<- ggplot(Mat_A, aes(x=X1, y=X2), col="green") +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=X2-X3, ymax=X2+X3), colour="black", width=.1, position=pd)+
  ggtitle("DNA methylation (Alpha)") +
  xlab("%") +
  ylab("Alpha")
  
print(p)

dev.off()

################################################# Beta plot
y_list = list()
x_list = list()
y.sd_list = list()
x <- (-5)

for (i in 1:length(Base_beta)){
  #print(Base_beta[i])
  setwd("/home/constantin/windows")
  y <- read.table(gsub(" ", "/t", Base_beta[i]))[2]
  y.sd <- read.table(gsub(" ", "/t", Error_beta[i]))[2]
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


#Create a matrix with beta data
data_B <- list(x_list, y_list, y.sd_list)
mtrx_B <- matrix(unlist(data_B), ncol = 3, nrow = length(y_list))
mtrx_B

#Create a table
library(MASS)
write.matrix(mtrx_B,file="Mat_B.csv")
Mat_B <- read_table("Mat_B.csv", col_names = FALSE)

#PLOT
pdf(file="saving_plot_beta.pdf")
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

p<- ggplot(Mat_B, aes(x=X1, y=X2), col="green") +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=X2-X3, ymax=X2+X3), colour="black", width=.1, position=pd)+
  ggtitle("DNA methylation (Beta)") +
  xlab("%") +
  ylab("Beta")

print(p)

dev.off()




