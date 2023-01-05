install.packages("tidyverse") # Install tidyverse package (if you have not done it yet)
library("tidyverse") # Load tidyverse package

Boutput_boot <- list.files(path = "/mnt/extStorage/constantin/windows/", pattern="boot_base_alpha", recursive = TRUE)
library(gtools)
Boutput_boot <- mixedsort(Boutput_boot)
Boutput_boot



y <- read.table(gsub(" ", "/t", "/mnt/extStorage/constantin/windows/0/Boutput_boot_base_beta.txt"))[2]
y.sd <- read.table(gsub(" ", "/t", "/mnt/extStorage/constantin/windows/0/Boutput_standard_errors_beta.txt"))[2]
x <- 1


y = as.numeric(as.character(y))
y.sd = as.numeric(as.character(y.sd))

pdf(file="saving_plot.pdf")
plot(x, y, xlab="%", ylab="alpha", xlim=c(-1, 1), ylim=c(-1, 1))
arrows(x0=x, y0=y-y.sd, x1=x, y1=y+y.sd, code=3, angle=0, length=0.1)
dev.off()

#class(x)
#class(y.sd)


