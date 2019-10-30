# Adam Briggs
# CAS 352 Reserach Project Data Analysis

# Import Libraries
library(googlesheets4)
library(googledrive)

data <- read_sheet("https://docs.google.com/spreadsheets/d/1buK0ZxyTiJ1rPrUbCSbql0RezCqn8-XJJ-b40xtPVSw/edit#gid=827708374")
names(data) <- c("Timestamp","Email","Understand Before","Understand After","Varied","Confidence","Conclusion","Teamwork",
                   "Group Work","Satisfaction","Benifit","Additional Thoughts","Item","Leader")

data <- data[-c(1:6),]


# Plotting Number vs String
plot(factor(data$Item),data$Confidence)

# Perform Principal Component Analysis on Data
pca <- prcomp(data[3:11], scale = TRUE)

# Display the results of PCA
pca
summary(pca)

# BiPlot of PCA Data
biplot(pca)

# Append the results of PC1 and PC2 to each variable in main data set
data2 <- cbind(data, pca$x[,1:2])

# Plotting with GGPLOT ---
library(ggplot2)
# ------------------------- Plotting PCA2 v PCA1
# Plots calculated PCA values against each other. The ellipses  are calculated with a multivariate
# t-distribution and the confidance interval represents 1 standard deviation (68%) of the data.
ggplot(data2, aes(PC1,PC2, col = Item, fill = Item)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5, type = "t", level=.68) + 
  geom_point(shape = 21, col = "black") +
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  ggtitle("Principal Component Confidence Interval of Brainstorming", subtitle = "Confidence Interval of One Standard Deviation") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  xlab("PC1 (Arb. Unit)") + ylab("PC1 (Arb. Unit)") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
# -------------------------

# Correlations Between Variables and PCs
corr <- cor(data[,3:11],data2[,15:16])
corr

# Plotting Histogram For Understanding Before
BS <- as.data.frame(data$`Understand Before`[1:10])
colnames(BS) <- c("Before")
BS$type <- 'Solo'

BG <-as.data.frame(data$`Understand Before`[11:20])
colnames(BG) <- c("Before")
BG$type <- 'Group'

groupval <- rbind(BS, BG)
ggplot(groupval, aes(Before, fill = type)) +
  geom_density(alpha = 0.7)

# Plotting Histogram For Understanding After
BS <- as.data.frame(data$`Understand After`[1:10])
colnames(BS) <- c("After")
BS$type <- 'Solo'

BG <-as.data.frame(data$`Understand After`[11:20])
colnames(BG) <- c("After")
BG$type <- 'Group'

groupval <- rbind(BS, BG)
ggplot(groupval, aes(After, fill = type)) +
  geom_density(alpha = 0.5)



# ------------ Saving Images
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="/Users/adambriggs/Dropbox/College/Fall 2019/CAS 352/Research Project/Figures")