# Adam Briggs
# CAS 352 Reserach Project Data Analysis

# Import Libraries
library(googlesheets4)
library(googledrive)
library(corrgram)
library(dplyr)
library(stargazer)
library(data.table)

data <- read_sheet("https://docs.google.com/spreadsheets/d/1buK0ZxyTiJ1rPrUbCSbql0RezCqn8-XJJ-b40xtPVSw/edit#gid=827708374")
names(data) <- c("Timestamp","Email","Understand Before","Understand After","Variance in Answer","Confidence","Conclusion","Teamwork",
                   "Group Work","Satisfaction","Benifit","Additional Thoughts","Item","Leader")
# Removes unused rows from
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
jpeg("PCA_CI.jpeg", units="in", width=11, height=8, res=300)
ggplot(data2, aes(PC1,PC2, col = Item, fill = Item)) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5, type = "t", level=.68) + 
  geom_point(shape = 21, col = "black") +
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  ggtitle("Principal Component Confidence Interval of Brainstorming", subtitle = "Confidence Interval of One Standard Deviation") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  xlab("PC1 (Arb. Unit)") + ylab("PC2 (Arb. Unit)") +
  theme(text = element_text(size=18)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
dev.off()
# -------------------------

# Correlations Between Variables and PCs
# 
corr <- cor(data[3:11],data2[15:16])
corr

# Calculating Means
library(plyr)
mu <- ddply(data2, "Item", summarise, before.mean=mean(`Understand Before`), 
            after.mean = mean(`Understand After`),
            var.mean=mean(`Variance in Answer`),
            confidence.mean=mean(Confidence),
            conclusion.mean=mean(Conclusion),
            teamwork.mean=mean(Teamwork),
            group.mean=mean(`Group Work`),
            satisfaction.mean=mean(Satisfaction),
            benifit.mean=mean(Benifit))


# Histogram Example
# Interleaved histograms

# ------------ Understand Before -----------------------
jpeg("Hist_Before.jpeg", units="in", width=11, height=8, res=300)
ggplot(data2, aes(x=`Understand Before`, fill=Item)) +
  geom_histogram(position='dodge', binwidth = 1, alpha = 0.75, col = "black") +
  geom_vline(data=mu, aes(xintercept=before.mean, color=Item),
             linetype="dashed",show.legend = FALSE)+
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  xlim(0,11) +
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Histogram of Brainstorming Results", subtitle = "Understanding Before Experiment") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()

# ------------ Understand After -----------------------
jpeg("Hist_After.jpeg", units="in", width=11, height=8, res=300)
ggplot(data2, aes(x=`Understand After`, fill=Item)) +
  geom_histogram(position='dodge', binwidth = 1, alpha = 0.75, col = "black") +
  geom_vline(data=mu, aes(xintercept=after.mean, color=Item),
             linetype="dashed",show.legend = FALSE)+
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  xlim(0,11) +
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Histogram of Brainstorming Results", subtitle = "Understanding After Experiment") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()

# ------------ Variance in Answer -----------------------
jpeg("Hist_Variance.jpeg", units="in", width=11, height=8, res=300)
ggplot(data2, aes(x=`Variance in Answer`, fill=Item)) +
  geom_histogram(position='dodge', binwidth = 1, alpha = 0.75, col = "black") +
  geom_vline(data=mu, aes(xintercept=var.mean, color=Item),
             linetype="dashed",show.legend = FALSE)+
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  xlim(0,11) +
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Histogram of Brainstorming Results", subtitle = "Variation of Ideas") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()

# ------------ Confidence in Answer -----------------------
jpeg("Hist_Confidence.jpeg", units="in", width=11, height=8, res=300)
ggplot(data2, aes(x=Confidence, fill=Item)) +
  geom_histogram(position='dodge', binwidth = 1, alpha = 0.75, col = "black") +
  geom_vline(data=mu, aes(xintercept=confidence.mean, color=Item),
             linetype="dashed",show.legend = FALSE)+
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  xlim(0,11) +
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Histogram of Brainstorming Results", subtitle = "Confidence in Answer") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme(text = element_text(size=18)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()

# ------------ Conclusion -----------------------
jpeg("Hist_Conclusion.jpeg", units="in", width=11, height=8, res=300)
ggplot(data2, aes(x=Conclusion, fill=Item)) +
  geom_histogram(position='dodge', binwidth = 1, alpha = 0.75, col = "black") +
  geom_vline(data=mu, aes(xintercept=conclusion.mean, color=Item),
             linetype="dashed",show.legend = FALSE)+
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  xlim(0,11) +
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Histogram of Brainstorming Results", subtitle = "Overall Conclusion") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()

# ------------ Teamwork -----------------------
jpeg("Hist_Teamwork.jpeg", units="in", width=11, height=8, res=300)
ggplot(data2, aes(x=Teamwork, fill=Item)) +
  geom_histogram(position='dodge', binwidth = 1, alpha = 0.75, col = "black") +
  geom_vline(data=mu, aes(xintercept=teamwork.mean, color=Item),
             linetype="dashed",show.legend = FALSE)+
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  xlim(0,11) +
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Histogram of Brainstorming Results", subtitle = "Teamwork") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()

# ------------ Group Work -----------------------
jpeg("Hist_Group.jpeg", units="in", width=11, height=8, res=300)
ggplot(data2, aes(x=`Group Work`, fill=Item)) +
  geom_histogram(position='dodge', binwidth = 1, alpha = 0.75, col = "black") +
  geom_vline(data=mu, aes(xintercept=group.mean, color=Item),
             linetype="dashed",show.legend = FALSE)+
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  xlim(0,11) +
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Histogram of Brainstorming Results", subtitle = "Group Work") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()

# ------------ Satisfaction -----------------------
jpeg("Hist_Satisfaction.jpeg", units="in", width=11, height=8, res=300)
ggplot(data2, aes(x=Satisfaction, fill=Item)) +
  geom_histogram(position='dodge', binwidth = 1, alpha = 0.75, col = "black") +
  geom_vline(data=mu, aes(xintercept=satisfaction.mean, color=Item),
             linetype="dashed",show.legend = FALSE)+
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  xlim(0,11) +
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Histogram of Brainstorming Results", subtitle = "Overall Satisfaction") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()

# ------------ Benifit -----------------------
jpeg("Hist_Benifit.jpeg", units="in", width=11, height=8, res=300)
ggplot(data2, aes(x=Benifit, fill=Item)) +
  geom_histogram(position='dodge', binwidth = 1, alpha = 0.75, col = "black") +
  geom_vline(data=mu, aes(xintercept=benifit.mean, color=Item),
             linetype="dashed",show.legend = FALSE)+
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  xlim(0,11) +
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Histogram of Brainstorming Results", subtitle = "Overall Benifit") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()

# ------------------------ Bar Of Average Questionaire ------------------------------------------
mut <- as.data.frame(t(as.matrix(mu)))
mut <- mut[-c(1),]
mut$Questions<-c("Understand Before","Understand After","Variance in Answer","Confidence","Conclusion","Teamwork",
                 "Group Work","Satisfaction","Benifit")
dfm <- melt(mut[,c('Questions','V1','V2')],id.vars = 1)


jpeg("Questionaire.jpeg", units="in", width=11, height=8, res=300)
ggplot(dfm,aes(x = Questions,y = as.numeric(value))) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge",alpha = 0.75,col = "black")+
  scale_fill_discrete(name = "Brainstorming Type", labels = c("Group", "Individual")) +
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10),expand = c(0,0))+
  xlab("Questions")+
  ylab("Counts")+
  ggtitle("Post Brainstorming Questionaire", subtitle = "Mean Results/Brainstorming Type") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(text = element_text(size=18)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()



