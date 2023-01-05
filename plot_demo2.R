install.packages("tidyverse") # Install tidyverse package (if you have not done it yet)
library("tidyverse") # Load tidyverse package

#Create a list with the alpha and beta(missing - LATER)
Boutput_boot <- list.files(path = "/mnt/extStorage/constantin/windows/", pattern="boot_base_alpha", recursive = TRUE)
library(gtools)
Boutput_boot <- mixedsort(Boutput_boot)
Boutput_boot

for (i in 1:length(Boutput_boot)){
  #print(Boutput_boot[i])
  y <- read.table(gsub(" ", "/t", "/mnt/extStorage/constantin/windows/$i"))[2]
  y.sd <- read.table(gsub(" ", "/t", "/mnt/extStorage/constantin/windows/$i"))[2]
  x <- 1
  
  #Transform de frame.value in numeric
  y = as.numeric(as.character(y))
  y.sd = as.numeric(as.character(y.sd))
  
  y
  y.sd
  x
  print(length(Boutput_boot))
}

pdf(file="saving_plot.pdf")
plot(x, y, xlab="%", ylab="alpha", xlim=c(-1, 1), ylim=c(-1, 1))
arrows(x0=x, y0=y-y.sd, x1=x, y1=y+y.sd, code=3, angle=0, length=0.1)
dev.off()

#class(x)
#class(y.sd)

