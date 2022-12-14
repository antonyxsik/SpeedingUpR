#Libraries (for all code)
suppressMessages(library(fields))
suppressMessages(library(ggplot2))
suppressMessages(library(cowplot))

#Loading in the CO2 data set that comes with fields
data(CO2)
s<- CO2$lon.lat
z<- CO2$y
good<- !is.na( z)
s<- s[good,]
z<- z[good]
N <- length(z)

#setting up our data frame for appending results
#1 run of 1000 to warm up (will be omitted)
datLens <- c(1000, 200, 500, 1000, 2500, 5000, 7500, 10000)
invTimes <- c(NA)
matTimes <- c(NA)
cholTimes <- c(NA)
mKrigTimes <- c(NA)
spatialTimes <- c(NA)

df <- cbind(datLens, invTimes, matTimes, cholTimes, mKrigTimes, spatialTimes)

#setting seed for consistent results 
set.seed(777)

for (i in 1:length(datLens)){
  
  #matrix multiplication timing
  A <- matrix(rnorm((datLens[i])^2), (datLens[i]), (datLens[i]))
  B <- A%*%t(A)
  df[i,2] <- system.time(solve(B))[3]
  df[i,3] <- system.time(B%*%B)[3]
  
  #cholesky timing
  df[i,4] <- system.time(chol(B))[3]
  
  #mKrig timing
  IShuffle <- sample( 1:N, datLens[i], replace=FALSE)
  tempS <- s[IShuffle,]
  tempZ <- z[IShuffle]
  
  #mKrig timing lambda parameter taken from fields documentation
  df[i,5] <- system.time(mKrig(tempS, tempZ, lambda = 0.1))[3]
  
  #spatialProcess timing 
  df[i,6] <- system.time(spatialProcess(tempS, tempZ, lambda = 0.1, smoothness = 0.5))[3]
  
  #idiot numbers 
  cat("Iteration: ", i, "\n")
}

#omitting the first row (warmup run)
df <- df[-1,]
#dfSpeedy <- df

#saving files for different libraries
#save(df, file = "defaultTiming.rda")
#save(dfSpeedy, file = "speedyTiming.rda")

load("defaultTiming.rda")
load("speedyTiming.rda")

print("default times below: ")
df

print("sped up times below: ")
dfSpeedy


dfInv <- data.frame(cbind(df[,1], df[,2], dfSpeedy[,2]))
colnames(dfInv) <- c("Run", "OG", "Fast")

dfMat <- data.frame(cbind(df[,1], df[,3], dfSpeedy[,3]))
colnames(dfMat) <- c("Run", "OG", "Fast")

dfChol <- data.frame(cbind(df[,1], df[,4], dfSpeedy[,4]))
colnames(dfChol) <- c("Run", "OG", "Fast")

dfKrig <- data.frame(cbind(df[,1], df[,5], dfSpeedy[,5]))
colnames(dfKrig) <- c("Run", "OG", "Fast")

dfSpat <- data.frame(cbind(df[,1], df[,6], dfSpeedy[,6]))
colnames(dfSpat) <- c("Run", "OG", "Fast")





pInv <- ggplot(dfInv, aes(x = Run)) +
  geom_line(aes(y = OG), color = "#F8766D") +
  geom_line(aes(y = Fast), color = "#00BFC4") +
  geom_point(aes(y = OG), shape = 19, color = "#F8766D") +
  geom_point(aes(y = Fast), shape = 19, color = "#00BFC4") + 
  ylab("Time (seconds)") + xlab("") + 
  theme(text = element_text(size=rel(2.9)))

pMat <- ggplot(dfMat, aes(x = Run)) +
  geom_line(aes(y = OG), color = "#F8766D") +
  geom_line(aes(y = Fast), color = "#00BFC4") +
  geom_point(aes(y = OG), shape = 19, color = "#F8766D") +
  geom_point(aes(y = Fast), shape = 19, color = "#00BFC4") + 
  ylab("") + xlab("") + 
  theme(text = element_text(size=rel(2.9)))

topRow <- plot_grid(pInv, pMat, 
                    labels = c("Inverse Times", "Matrix Multiplication Times"), 
                    label_size = 9, ncol = 2, label_fontface = "plain", 
                    label_x = 0.1)

pChol <- ggplot(dfChol, aes(x = Run)) +
  geom_line(aes(y = OG), color = "#F8766D") +
  geom_line(aes(y = Fast), color = "#00BFC4") +
  geom_point(aes(y = OG), shape = 19, color = "#F8766D") +
  geom_point(aes(y = Fast), shape = 19, color = "#00BFC4") + 
  xlab("") + ylab("Time (seconds)") + 
  theme(text = element_text(size=rel(2.9)))

pKrig <- ggplot(dfKrig, aes(x = Run)) +
  geom_line(aes(y = OG), color = "#F8766D") +
  geom_line(aes(y = Fast), color = "#00BFC4") +
  geom_point(aes(y = OG), shape = 19, color = "#F8766D") +
  geom_point(aes(y = Fast), shape = 19, color = "#00BFC4") + 
  xlab("Number of Observations") + 
  ylab("") + 
  theme(text = element_text(size=rel(2.9)))

pSpat <- ggplot(dfSpat, aes(x = Run)) +
  geom_line(aes(y = OG), color = "#F8766D") +
  geom_line(aes(y = Fast), color = "#00BFC4") +
  geom_point(aes(y = OG), shape = 19, color = "#F8766D") +
  geom_point(aes(y = Fast), shape = 19, color = "#00BFC4") + 
  xlab("") + ylab("") + 
  theme(text = element_text(size=rel(2.9)))

botRow <- plot_grid(pChol, pKrig, pSpat,
                    labels = c("Cholesky Times", " mKrig Times", "spatialProcess Times"), 
                    label_size = 9, ncol = 3, label_fontface = "plain", 
                    label_x = 0.05)


timingGraphs <- plot_grid(topRow, botRow, ncol = 1)
timingGraphs


#Accuracy code
A <- matrix(rnorm(500^2), 500, 500)
B <- A%*%t(A)
choltest <- chol(B)
cholSpeedy <- as.matrix(choltest)


#saving the file to compare to sped up R 
#save(choltest, file = "defaultChol.rda")
#save(cholSpeedy, file = "speedyChol.rda")

load("defaultChol.rda")
load("speedyChol.rda")

diffMat <- cholSpeedy - choltest
which(diffMat == max(diffMat), arr.ind = TRUE)

#raw maximum error
cat("Maximum Difference between matrices: ", diffMat[500,500], "\n")

#percent error
cat("Maximum Error Percentage: ", diffMat[500,500]/choltest[500,500] * 100, "\n")
#i can live with that