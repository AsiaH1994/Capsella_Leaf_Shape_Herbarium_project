### Title: Statistical Analysis for Herbarium Project V1 
### Date: 11/03/23

setwd("~/Documents/")

#### Packages ####
library(dplyr) 
library(factoextra)
library(tidyverse)
library(ggplot2)
library(emmeans)
library(xlsx)
library(AICcmodavg)
library(ggpubr)
library(sf)
library(mapview)
library(vcd)
library(multigroup)

#### Data Prep ####

total_datadf <- read.csv("~/Documents/leaf_shape_meta_comp.csv")
total_datadf$length <- as.numeric(total_datadf$length)
total_datadf$width <- as.numeric(total_datadf$width)
total_datadf$preimeter <- as.numeric(total_datadf$preimeter)
total_datadf$area <- as.numeric(total_datadf$area)
total_datadf$latitude <- as.numeric(total_datadf$latitude)
total_datadf$longitude <- as.numeric(total_datadf$longitude)
total_datadf$elevation <- as.numeric(total_datadf$elevation)
total_datadf$day <- as.numeric(total_datadf$day)
total_datadf$year <- as.numeric(total_datadf$year)
total_datadf$precp_in <- as.numeric(total_datadf$precp_in)
total_datadf$max_temp_C <- as.numeric(total_datadf$max_temp_C)
total_datadf$min_temp_C <- as.numeric(total_datadf$min_temp_C)
total_datadf$avg_temp_C <- as.numeric(total_datadf$avg_temp_C)
total_datadf$avg_tmax_six_months_prior <- as.numeric(total_datadf$avg_tmax_six_months_prior)
total_datadf$avg_tmin_six_months_prior <- as.numeric(total_datadf$avg_tmin_six_months_prior)
total_datadf$avgtemp_6monthsprior <- as.numeric(total_datadf$avgtemp_6monthsprior)
total_datadf$avg_tmax_previous_.year <- as.numeric(total_datadf$avg_tmax_previous_.year)
total_datadf$avg_tmin_previous_.year <- as.numeric(total_datadf$avg_tmin_previous_.year)
total_datadf$tavg_previous_.year <- as.numeric(total_datadf$tavg_previous_.year)
total_datadf$avg_tmax_.year_DOC <- as.numeric(total_datadf$avg_tmax_.year_DOC)
total_datadf$avg_tmin_.year_DOC <- as.numeric(total_datadf$avg_tmin_.year_DOC)
total_datadf$tavg_.year_DOC <- as.numeric(total_datadf$tavg_.year_DOC)
total_datadf$avgprecp_6monthsprior <- as.numeric(total_datadf$avgprecp_6monthsprior)
total_datadf$avg_precp_previous_year <- as.numeric(total_datadf$avg_precp_previous_year)
total_datadf$avg_precp_year_DOC <- as.numeric(total_datadf$avg_precp_year_DOC)
total_datadf$month <- as.character(total_datadf$month)
total_datadf$precp_in <- (total_datadf$precp_in/10) #data is downloaded in a 1/10 of a degree
total_datadf <- total_datadf[-c(127),]
total_datadf$avgprecp_6monthsprior <- (total_datadf$avgprecp_6monthsprior/10)
total_datadf$avg_precp_previous_year <- (total_datadf$avg_precp_previous_year/10)
total_datadf$avg_precp_year_DOC <- (total_datadf$avg_precp_year_DOC/10)
total_datadf$avgprecp_6monthsprior <- log(total_datadf$avgprecp_6monthsprior)
total_datadf$avg_precp_previous_year <- log(total_datadf$avg_precp_previous_year)
total_datadf$avg_precp_year_DOC <- log(total_datadf$avg_precp_year_DOC)
total_datadf$precp_in <- log(total_datadf$precp_in)
total_datadf$type_ianetta <- as.factor(total_datadf$type_ianetta)
total_datadf$type_shull <- as.factor(total_datadf$type_shull)
total_datadf <- total_datadf[-c(498:533),]

prcp <- total_datadf[,c(3,4,22)]
prcp <- filter(prcp, precp_in > -7)


#climate regions
Southdf <- filter(total_datadf, climate_region == "South") 
Southeastdf <- filter(total_datadf, climate_region == "Southeast") 
Southwestdf <- filter(total_datadf, climate_region == "Southwest") 
Northeastdf <- filter(total_datadf, climate_region == "Northeast")  
Northerrockiesandplainsdf <- filter(total_datadf, climate_region == "Northern_Rockies_and_Plains")
Northwest <- filter(total_datadf, climate_region == "Northwest")
Ohiovalley <- filter(total_datadf, climate_region == "Ohio_Valley") 
Uppermidwest <- filter(total_datadf, climate_region == "Upper_Midwest") 

#month as factor 
total_datadf <- total_datadf %>%
  mutate(month_factor = case_when(
    startsWith(month, "Jan") ~ "january",
    startsWith(month, "Feb") ~ "february",
    startsWith(month, "Mar") ~ "march",
    startsWith(month, "Apr") ~ "april",
    startsWith(month, "May") ~ "may",
    startsWith(month, "June") ~ "june",
    startsWith(month, "Jul") ~ "july",
    startsWith(month, "Aug") ~ "august", 
    startsWith(month, "Sep") ~ "september",
    startsWith(month, "Oct") ~ "october",
    startsWith(month, "Nov") ~ "november", 
    startsWith(month, "Dec") ~ "december"))

#final file 
write.xlsx(total_datadf, file = "total_data_edited.xlsx", sheetName = "Sheet1")

#color pallettes
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")



#### Statistical Analysis ####

##### Linear modeling #####
## exploratory data modeling ##
#climate region
summary(circ_clim <- aov(circularity ~ climate_region, data = total_datadf))
summary(ar_clim <- aov(aspect_ratio ~ climate_region, data = total_datadf))

#year
summary(circ_lm1 <- lm(circularity ~ year, data = total_datadf))
summary(ar_lm1 <- lm(aspect_ratio ~ year, data = total_datadf))

#year and climate region
summary(circ_lm2 <- lm(circularity ~ year + year*climate_region, data = total_datadf))
summary(ar_lm2 <- lm(aspect_ratio ~ year + year*climate_region, data = total_datadf))
summary(circ_lm4 <- lm(circularity ~ year + year*type_ianetta, data = total_datadf))
emmeans(circ_lm4, pairwise ~ type_ianetta)
summary(ar_lm4 <- lm(aspect_ratio ~ year + year*type_ianetta, data = total_datadf))
summary(circ_lm5 <- lm(circularity ~ year + year*type_shull, data = total_datadf))
summary(ar_lm5 <- lm(aspect_ratio ~ year + year*type_shull, data = total_datadf))

#mean annual temp
summary(circ_lm3 <- lm(circularity ~ climate_region + climate_region*tavg_previous_.year, data = total_datadf))
summary(ar_lm3 <- lm(aspect_ratio ~ climate_region + climate_region*avg_precp_previous_year, data = total_datadf))
summary(circ_lm3 <- lm(circularity ~ climate_region + climate_region*tavg_previous_.year, data = total_datadf))

#leaf shape types 
summary(circ_ls <- lm(circularity ~ type_ianetta + type_shull, data = total_datadf))
summary(ar_ls <- lm(aspect_ratio ~ type_ianetta + type_shull, data = total_datadf))

#within two most prevalent leaf shape types
#rhomboidea and Type3
rh_type <- filter(total_datadf, type_shull == "rhomboidea")
type3_type <- filter(total_datadf, type_ianetta == "Type3")

#change by year and mean annual temperature/mean annual precipitation 
summary(rh_circ <- lm(circularity ~ avgtemp_6monthsprior + tavg_previous_.year + year + climate_region, data = rh_type))
summary(rh_ar <- lm(aspect_ratio ~ avgprecp_6monthsprior + avg_precp_previous_year + year + climate_region, data = rh_type))
summary(type3_circ <- lm(circularity ~ avgtemp_6monthsprior + tavg_previous_.year + year + climate_region, data = type3_type))
summary(type3_ar <- lm(aspect_ratio ~ avgprecp_6monthsprior + avg_precp_previous_year + year + climate_region, data = type3_type))

#pc data
pc_data <- read.csv('~/Desktop/desktop_csv_files/herb_pc_circ_data_complete.csv')
summary(pc_lm1 <- lm(circ ~ PC1, data = pc_data))
summary(pc_lm2 <- lm(circ ~ PC2, data = pc_data))
summary(pc_lm3 <- lm(ar ~ PC2, data = pc_data))
summary(pc_lm4 <- lm(ar ~ PC1, data = pc_data))

##### polynomial regression model ##### 
df <- total_datadf[,c(3:4)]
df <- na.omit(df)
#randomly shuffle data
df.shuffled <- df[sample(nrow(df)),]

#define number of folds to use for k-fold cross-validation
K <- 10 

#define degree of polynomials to fit
degree <- 5

#create k equal-sized folds
folds <- cut(seq(1,nrow(df.shuffled)),breaks=K,labels=FALSE)

#create object to hold MSE's of models
mse = matrix(data=NA,nrow=K,ncol=degree)

#Perform K-fold cross validation
for(i in 1:K){
  
  #define training and testing data
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df.shuffled[testIndexes, ]
  trainData <- df.shuffled[-testIndexes, ]
  
  #use k-fold cv to evaluate models
  for (j in 1:degree){
    fit.train = lm(circularity ~ poly(aspect_ratio,j), data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$aspect_ratio)^2) 
  }
}

#find MSE for each degree 
colMeans(mse)

#fit best model
best = lm(circularity ~ poly(aspect_ratio,2, raw=T), data=df)

#view summary of best model
summary(best)

##### ANOVA #####
# Circularity
summary(two.wayC <- aov(circularity ~ avgtemp_6monthsprior + avgprecp_6monthsprior + avg_tmax_six_months_prior
                        + avg_tmin_six_months_prior + factor(climate_region), data = total_datadf))
summary(two.wayddocC <- aov(circularity ~ avg_temp_C + max_temp_C + min_temp_C
                            + precp_in + factor(climate_region), data = total_datadf))
summary(two.waymatC <- aov(circularity ~ tavg_previous_.year + avg_tmax_previous_.year + avg_tmin_previous_.year
                           + avg_precp_previous_year + factor(climate_region), data = total_datadf))
summary(two.way <- aov(circularity ~ avgtemp_6monthsprior + avgprecp_6monthsprior + avg_tmax_six_months_prior
                       + avg_tmin_six_months_prior, data = total_datadf))
summary(two.wayddoc <- aov(circularity ~ avg_temp_C + max_temp_C + min_temp_C
                           + precp_in, data = total_datadf))
summary(two.waymat <- aov(circularity ~ tavg_previous_.year + avg_tmax_previous_.year + avg_tmin_previous_.year
                          + avg_precp_previous_year, data = total_datadf))
summary(interactionmA1 <- aov(circularity ~ avgtemp_6monthsprior*avgprecp_6monthsprior, data = total_datadf))

summary(interactionmA2 <- aov(circularity ~ avg_temp_C*precp_in, data = total_datadf))
summary(interactionmA3 <- aov(circularity ~ tavg_previous_.year*avg_precp_previous_year, data = total_datadf))

## best model - circularity
model.set_all <- list(interactionmA1,two.wayC, two.wayddocC, two.waymatC, interactionmA2,two.way, two.wayddoc, two.waymat, 
                      interactionmA3)
model.names_all <- c("IN_GS","GS_C", "DOC_C", "YL_C", "IN_DOC" ,"GS", "DOC", "YL", "IN_YL")
aicdfh <- aictab(model.set_all, modnames = model.names_all)

# Aspect ratio
summary(gs_ar_C <- aov(aspect_ratio ~ avgtemp_6monthsprior + avgprecp_6monthsprior + avg_tmax_six_months_prior
                       + avg_tmin_six_months_prior + factor(climate_region), data = total_datadf))
summary(doc_ar_C <- aov(aspect_ratio ~ avg_temp_C + max_temp_C + min_temp_C
                        + precp_in + factor(climate_region), data = total_datadf))
summary(MAT_ar_C <- aov(aspect_ratio ~ tavg_previous_.year + avg_tmax_previous_.year + avg_tmin_previous_.year
                        + avg_precp_previous_year + factor(climate_region), data = total_datadf))
summary(gs_ar <- aov(aspect_ratio ~ avgtemp_6monthsprior + avgprecp_6monthsprior + avg_tmax_six_months_prior
                     + avg_tmin_six_months_prior, data = total_datadf))
summary(doc_ar <- aov(aspect_ratio ~ avg_temp_C + max_temp_C + min_temp_C
                      + precp_in, data = total_datadf))
summary(MAT_ar <- aov(aspect_ratio ~ tavg_previous_.year + avg_tmax_previous_.year + avg_tmin_previous_.year
                      + avg_precp_previous_year, data = total_datadf))
summary(int.gs <- aov(aspect_ratio ~ avgtemp_6monthsprior*avgprecp_6monthsprior, data = total_datadf))
summary(int.doc <- aov(aspect_ratio ~ avg_temp_C*precp_in, data = total_datadf))
summary(int.MAT <- aov(aspect_ratio ~ tavg_previous_.year*avg_precp_previous_year, data = total_datadf))

##best model - aspect ratio
model.set_ar <- list(gs_ar_C, doc_ar_C, MAT_ar_C, gs_ar, doc_ar, MAT_ar, 
                     int.gs,  int.doc, int.MAT)
model.names_ar <- c("GS_C", "DOC_C", "YL_C", "GS", "DOC", "YL", 
                    "IN_GS",  "IN_DOC", "IN_YL")
aic_ar <- aictab(model.set_ar, modnames = model.names_ar)

##### Correlations #####
shape_desc <- cor.test(x = total_datadf$aspect_ratio, y = total_datadf$circularity, method = 'spearman')
south_cor <- cor.test(x = Southdf$avgtemp_6monthsprior, y = Southdf$circularity, method = 'pearson')
southeast_cor <- cor.test(x = Southeastdf$avgtemp_6monthsprior, y = Southeastdf$circularity, method = 'pearson')
circ_cor1 <- cor.test(x = total_datadf$avgtemp_6monthsprior,  y = total_datadf$circularity, method = 'pearson')
circ_cor2 <- cor.test(x = total_datadf$tavg_previous_.year, y = total_datadf$circularity, method = 'pearson')
ar_cor1 <- cor.test(x = total_datadf$avgprecp_6monthsprior, y = total_datadf$aspect_ratio, method = 'pearson')
ar_cor2 <- cor.test(x = total_datadf$avg_precp_previous_year, y = total_datadf$aspect_ratio, method = 'pearson')
circ_cor3 <- cor.test(x = total_datadf$avg_precp_previous_year, y = total_datadf$circularity, method = 'pearson')
ar_cor3 <- cor.test(x = total_datadf$tavg_previous_.year, y = total_datadf$aspect_ratio, method = 'pearson')

## Chi squared - shape types ###
cat_vars_nc <- total_datadf[,c(5:6)]
cat_vars <- total_datadf[,c(5:6, 12)]
cat_vars <- cat_vars[-c(498:533), ]
cat_vars_nc <- cat_vars_nc[-c(498:533), ]
multi_var <- total_datadf[,c(3:6)]
multi_var <- multi_var[-c(498:533), ]

cat_vars_nc$type_ianetta <- as.character(cat_vars_nc$type_ianetta)
cat_vars_nc$type_shull <- as.character(cat_vars_nc$type_shull)
cat_vars$type_ianetta <- as.character(cat_vars$type_ianetta)
cat_vars$type_shull <- as.character(cat_vars$type_shull)

#frequencies
s_types <- cat_vars_nc[,c(2)]
i_types <- cat_vars_nc[,c(1)]
s_freq <- as.data.frame(table(s_types))
i_freq <- as.data.frame(table(i_types))
s_freq <- filter(s_freq, Freq > 0)
i_freq <- filter(i_freq, Freq > 0)
total_leaves <- (24+470+3)
percent_r <- (470/total_leaves)*100
percent_3 <- (388/total_leaves)*100

#associations
df_types <- xtabs(~type_shull + type_ianetta, data = cat_vars_nc)
assoc1 <- summary(assocstats(df_types))
df_test1 <- xtabs(~type_shull + climate_region, data = cat_vars)
assoc2 <- summary(assocstats(df_test1))
df_test2 <- xtabs(~type_ianetta + climate_region, data = cat_vars)
assoc3 <- summary(assocstats(df_test2))

#### Figures ####

###### AIC model figure ######
#types
hist1 <- ggplot(rh_type, aes(x = circularity)) + 
  geom_histogram() + theme_classic()
hist2 <- ggplot(type3_type, aes(x = circularity)) + 
  geom_histogram() + theme_classic()
ggarrange(hist1, hist2)
hist3 <- ggplot(rh_type, aes(x = aspect_ratio)) + 
  geom_histogram() + theme_classic()
hist4 <- ggplot(type3_type, aes(x = aspect_ratio)) + 
  geom_histogram() + theme_classic()
ggarrange(hist3, hist4)
summary(rh_type$circularity)
summary(rh_type$aspect_ratio)

#climate region 
ggplot(total_datadf, aes(x = climate_region, 
                         y = circularity)) +
  geom_boxplot() + theme_classic()
ggplot(total_datadf, aes(x = climate_region, 
                         y = aspect_ratio)) +
  geom_boxplot() + theme_classic()

#circ
deltaaicreorder_c <- ggplot(aicdfh, aes(x = reorder(Modnames, Delta_AICc), y = Delta_AICc)) +
  geom_bar(stat="identity", color="white", fill="black") + theme_classic()
png(filename = "~/Desktop/herbarium_paper_figures/deltaaic_circ.png", res = 300, width= 2500, height = 1500)
deltaaicreorder_c + xlab("Model Names") + ylab("Delta AICc") + theme(axis.title = element_text(size = 14)) +
  theme(axis.text = element_text(size = 11))
dev.off()

deltaaicreorder_ar <- ggplot(aic_ar, aes(x = reorder(Modnames, Delta_AICc), y = Delta_AICc)) +
  geom_bar(stat="identity", color="white", fill="black") + theme_classic()
png(filename = "~/Desktop/herbarium_paper_figures/deltaaic_ar.png", res = 300, width= 2500, height = 1500)
deltaaicreorder_ar + xlab("Model Names") + ylab("Delta AICc") + theme(axis.title = element_text(size = 14)) +
  theme(axis.text = element_text(size = 11))
dev.off()
 
####### figure - circ and ar for all models #######
GSavgall <- ggplot(total_datadf,                    
                   aes(x = avgtemp_6monthsprior,
                       y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=25, y=0.7, label=" p = 7.18e-09")
GSavgallplot <- GSavgall + xlim(0, 32) + ylim(0, 0.8) + ggtitle('Growing Season', subtitle='circ deltaAIC = 0.00, ar deltaAIC = 51.22') + theme(plot.title = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Average Growing Season Temp (C) ") + ylab("Circularity") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                      panel.background = element_blank(), axis.line = element_line(colour = "black"))

MATavgall <- ggplot(total_datadf,                    
                   aes(x = tavg_previous_.year,
                       y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=25, y=0.7, label=" p = 0.00104")
MATavgallplot <- MATavgall + xlim(0, 32) + ylim(0, 0.8) + ggtitle('Year Long', subtitle='circ deltaAIC = 62.54, ar deltaAIC = 54.98') + 
  theme(plot.title = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("Average Year Long Temp (C) ") + ylab("Circularity") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

DOCavgall <- ggplot(total_datadf,                    
                   aes(x = avg_temp_C,
                       y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=25, y=0.7, label=" p = 0.98369")
DOCavgallplot <- DOCavgall + xlim(0, 32) + ylim(0, 0.8) + ggtitle('Date of Collection', subtitle='circ deltaAIC = 95.92, ar deltaAIC = 0.00') + 
  theme(plot.title = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("Average Date of Collection Temp (C) ") + ylab("Circularity") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#ar
GSar <- ggplot(total_datadf,                    
                   aes(x = avgtemp_6monthsprior,
                       y = aspect_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=25, y=7, label=" p = 0.2704")
GSarplot <- GSar + xlim(0, 32) + ylim(0, 8) +
  xlab("Average Growing Season Temp (C) ") + ylab("Aspect Ratio") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

MATar <- ggplot(total_datadf,                    
                    aes(x = tavg_previous_.year,
                        y = aspect_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=25, y=7, label=" p = 0.0353")
MATarplot <- MATar + xlim(0, 32) + ylim(0, 8) +
  xlab("Average Year Long Temp (C) ") + ylab("Aspect Ratio") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

DOCar <- ggplot(total_datadf,                    
                    aes(x = avg_temp_C,
                        y = aspect_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=25, y=7, label=" p = 0.8079")
DOCarplot <- DOCar + xlim(0, 32) + ylim(0, 8) +
  xlab("Average Date of Collection Temp (C) ") + ylab("Aspect Ratio") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

## prcp ##
GSprcp <- ggplot(total_datadf,                    
                   aes(x = avgprecp_6monthsprior,
                       y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=0.5, y=0.7, label=" p = 0.0862")
GSprcp_plot <- GSprcp + xlim(-3, 1) + ylim(0, 0.8) +
  xlab("Average Growing Season Precp (log) ") + ylab("Circularity") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

MATprcp <- ggplot(total_datadf,                    
                    aes(x = avg_precp_previous_year,
                        y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=0.5, y=0.7, label=" p = 0.02227")
MATprcp_plot <- MATprcp + xlim(-3, 1) + ylim(0, 0.8) +
  xlab("Average Year Long Precp (log) ") + ylab("Circularity") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

DOCprcp <- ggplot(prcp,                    
                    aes(x = precp_in,
                        y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=-0.5, y=0.7, label=" p = 0.05358")
DOCprcp_plot <- DOCprcp + xlim(-3, 1) + ylim(0, 0.8) +
  xlab("Average Date of Collection Precp (log) ") + ylab("Circularity") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#ar
GSarprcp <- ggplot(total_datadf,                    
               aes(x = avgprecp_6monthsprior,
                   y = aspect_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=0.5, y=7, label=" p = 0.1257")
GSar_prcpplot <- GSarprcp + xlim(-3, 1) + ylim(0, 8) +
  xlab("Average Growing Season Precp (log) ") + ylab("Aspect Ratio") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

MATarprcp <- ggplot(total_datadf,                    
                aes(x = avg_precp_previous_year,
                    y = aspect_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=0.5, y=7, label=" p = 0.1293")
MATar_prcpplot <- MATarprcp + xlim(-3, 1) + ylim(0, 8) +
  xlab("Average Year Long Precp (log) ") + ylab("Aspect Ratio") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

DOCarprcp <- ggplot(prcp,                    
                aes(x = precp_in,
                    y = aspect_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=-0.5, y=7, label=" p = 0.0611")
DOCar_prcpplot <- DOCarprcp + xlim(-3, 1) + ylim(0, 8) +
  xlab("Average Date of Collection Precp (log) ") + ylab("Aspect Ratio") + theme(legend.position="center") + 
  theme(axis.title = element_text(size = 12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#row 1 - all circ temp
#row 3 - all ar temp 
#row 2 - all circ prcp
#row 4 - all ar prcp 

model_selection_scatter <- 
  ggarrange(GSavgallplot,MATavgallplot,DOCavgallplot, 
          GSprcp_plot,MATprcp_plot,DOCprcp_plot, 
          GSarplot,MATarplot,DOCarplot, 
          GSar_prcpplot,MATar_prcpplot, DOCar_prcpplot,  
          ncol = 3, nrow = 4, common.legend = TRUE, legend="bottom", heights = c(1.5,1,1,1,1.5,1,1,1,1.5,1,1,1))

png(filename = "~/Desktop/herbarium_paper_figures/model_selection_scatter_plots.png", res = 300, width= 3200, height = 2700)
ggarrange(GSavgallplot,MATavgallplot,DOCavgallplot, 
          GSarplot,MATarplot,DOCarplot, 
          GSprcp_plot,MATprcp_plot,DOCprcp_plot,
          GSar_prcpplot,MATar_prcpplot, DOCar_prcpplot,  
          ncol = 3, nrow = 4, common.legend = TRUE, legend="bottom", heights = c(1.5,1,1,1,1.5,1,1,1,1.5,1,1,1), align = 'hv')
dev.off()


###### circularity vs. year plot ######
circ_yearall <- ggplot(total_datadf,                    
                       aes(x = year,
                           y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

png(filename = "~/Desktop/herbarium_paper_figures/circ_by_year.png", res = 300, width= 3000, height = 2000)
circ_yearall + xlab("Year ") + ylab("Circularity") + theme(axis.title = element_text(size = 12)) + theme_classic() 
dev.off()

ar_yearall <- ggplot(total_datadf,                    
                       aes(x = year,
                           y = aspect_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

png(filename = "~/Desktop/herbarium_paper_figures/ar_by_year.png", res = 300, width= 3000, height = 2000)
ar_yearall + xlab("Year ") + ylab("Aspect Ratio") + theme(axis.title = element_text(size = 12)) + theme_classic() 
dev.off()

# combined 
circ_year_plot <- circ_yearall + ylab("Circularity") + 
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ar_year_plot <- ar_yearall + ylab("Aspect Ratio") + 
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
year_plot <- ggarrange(circ_year_plot, ar_year_plot, ncol= 2, nrow = 1)

png(filename = "~/Desktop/herbarium_paper_figures/combined_year_plot.png", res = 300, width= 3500, height = 2000)
annotate_figure(year_plot,
                bottom = text_grob("Year", 
                                color = "black", size = 12))
dev.off()

####### Figure  - growing season model #######

GSavgall_fig2 <- ggplot(total_datadf,                    
                   aes(x = avgtemp_6monthsprior,
                       y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x=20, y=0.7, label=" p = 7.15e-09", size = 4)
GSavgallplot_fig2 <- GSavgall_fig2 + xlim(-3, 32) + ylim(0, 0.8) + 
  ggtitle("Continental U.S.") + 
  theme(axis.text.x=element_text(size = 15),
        axis.text.y= element_text(size = 15), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = 'black')) + 
  theme(legend.position = "none")

GSavgsouth <- ggplot(Southdf,                    
                     aes(x = avgtemp_6monthsprior,
                         y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x = 25, y = 0.7, label = "p = 2.72e-06", size = 4)
GSavgsouthplot <- GSavgsouth + xlim(-3, 32) + ylim(0, 0.8) + 
  ggtitle("South") + 
  theme(axis.text.x=element_text(size = 15),
        axis.text.y= element_text(size = 15), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = 'black')) + 
  theme(legend.position = "none")

GSavgsoutheast <- ggplot(Southeastdf,                    
                         aes(x = avgtemp_6monthsprior,
                             y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text(x = 20, y = 0.7, label = "p = 6.63e-06", size = 4)
GSavgsoutheastplot <- GSavgsoutheast + xlim(-3, 32) + ylim(0, 0.8) + 
  ggtitle("Southeast") + 
  theme(axis.text.x=element_text(size = 15),
        axis.text.y= element_text(size = 15), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = 'black')) + 
  theme(legend.position = "none")

GSavgsouthwest <- ggplot(Southwestdf,                    
                         aes(x = avgtemp_6monthsprior,
                             y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
GSavgsouthwestplot <- GSavgsouthwest + xlim(-3, 32) + ylim(0, 0.8) + 
  ggtitle("Southwest") + 
  theme(axis.text.x=element_text(size = 12),
        axis.text.y= element_text(size = 12), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = 'black')) + 
  theme(legend.position = "none")

GSavgnortheast <- ggplot(Northeastdf,                    
                         aes(x = avgtemp_6monthsprior,
                             y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
GSavgnortheastplot <- GSavgnortheast + xlim(-3, 32) + ylim(0, 0.8) + 
  ggtitle("Northeast") + 
  theme(axis.text.x=element_text(size = 12),
        axis.text.y= element_text(size = 12), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = 'black')) + 
  theme(legend.position = "none")

GSavgnorthwest <- ggplot(Northwest,                    
                         aes(x = avgtemp_6monthsprior,
                             y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
GSavgnorthwestplot <- GSavgnorthwest + xlim(-3, 32) + ylim(0, 0.8) + 
  ggtitle("Northwest") + 
  theme(axis.text.x=element_text(size = 12),
        axis.text.y= element_text(size = 12), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = 'black')) + 
  theme(legend.position = "none")

GSavgnorthrockies <- ggplot(Northerrockiesandplainsdf,                    
                            aes(x = avgtemp_6monthsprior,
                                y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
GSavgnorthrockiesplot <- GSavgnorthrockies + xlim(-3, 32) + ylim(0, 0.8) + 
  ggtitle("Northern Rockies and Plains") + 
  theme(axis.text.x=element_text(size = 12),
        axis.text.y= element_text(size = 12), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = 'black')) + 
  theme(legend.position = "none")

GSavgohiovalley <- ggplot(Ohiovalley,                    
                          aes(x = avgtemp_6monthsprior,
                              y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
GSavgohiovalleyplot <- GSavgohiovalley + xlim(-3, 32) + ylim(0, 0.8) + 
  ggtitle("Ohio Valley") + 
  theme(axis.text.x=element_text(size = 12),
        axis.text.y= element_text(size = 12), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = 'black')) + 
  theme(legend.position = "none")

GSavguppermidwest <- ggplot(Uppermidwest,                    
                            aes(x = avgtemp_6monthsprior,
                                y = circularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
GSavguppermidwestplot <- GSavguppermidwest + xlim(-3, 32) + ylim(0, 0.8) + 
  ggtitle("Upper Midwest") + 
  theme(axis.text.x=element_text(size = 12),
        axis.text.y= element_text(size = 12), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = 'black')) + 
  theme(legend.position = "none")

# figure - all 
ggarrange(GSavgallplot_fig2, GSavgsouthplot, GSavgsoutheastplot, GSavgsouthwestplot, 
          GSavgnortheastplot, GSavgnorthwestplot, GSavgnorthrockiesplot, 
          GSavgohiovalleyplot, GSavguppermidwestplot,
          ncol = 3, nrow = 3, common.legend = TRUE, legend="bottom")

figure2 <-ggarrange(GSavgallplot_fig2, 
                    ggarrange(GSavgsouthplot, GSavgsoutheastplot, GSavgsouthwestplot, 
                              GSavgnortheastplot, GSavgnorthwestplot, GSavgnorthrockiesplot, 
                              GSavgohiovalleyplot, GSavguppermidwestplot,
                              ncol = 2, nrow = 4, common.legend = TRUE, legend="bottom"))
png(filename = "~/Desktop/herbarium_paper_figures/Circ_GS_all_plot.png", res = 300, width= 3500, height = 2000)
annotate_figure(figure2,
                bottom = text_grob("Average Growing Season Temp (C)", 
                                color = "black", size = 18), 
                left = text_grob("Circularity", 
                                 color = "black", size = 18, rot = 90))
dev.off()


#figure - Significant

#subset significant points to color 
south_lowest_circ <- subset(Southdf, circularity == 0.09510525) # this blue #003F87
south_second_lowest_circ <- subset(Southdf, circularity == 0.107108697) # this blue #6495ED
south_highest_circ <- subset(Southdf, circularity == 0.761057573) # this red #DC143C
south_second_highest_circ <- subset(Southdf, circularity == 0.592867949) # this red #cd5c5c

southeast_lowest_circ <- subset(Southeastdf, circularity == 0.071067234)
southeast_second_lowest_circ <- subset(Southeastdf, circularity == 0.07889799)
southeast_highest_circ <- subset(Southeastdf, circularity == 0.605709733)
southeast_second_highest_circ <- subset(Southeastdf, sample_number == 384)

test <- filter(Southeastdf, circularity > 0.4 | avgtemp_6monthsprior > 20)
test <- na.omit(test)

GSavgall_sig <- ggplot(total_datadf,                    
                   aes(x = avgtemp_6monthsprior,
                       y = circularity)) +
  geom_point() +
  geom_point(data=south_lowest_circ, colour="#D55E00", size = 3) +
  geom_point(data=south_second_lowest_circ, colour="#cc79a7", size = 3) +
  geom_point(data=south_highest_circ, colour="#0072b2", size = 3) +
  geom_point(data=south_second_highest_circ, colour="#f0e442", size = 3) +
  geom_point(data=southeast_lowest_circ, colour="#D55E00", size = 3) +
  geom_point(data=southeast_second_lowest_circ, colour="#cc79a7", size = 3) +
  geom_point(data=southeast_highest_circ, colour="#0072b2", size = 3) +
  geom_point(data=southeast_second_highest_circ, colour="#f0e442", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +  
  geom_text(x=25, y=0.7, label=" p = 7.15e-09", size = 9) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
GSavgallplot_sig <- GSavgall_sig + xlim(-3, 32) + ylim(0, 0.8) + 
  ggtitle("Continental U.S.") + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 25))

GSavgsouth <- ggplot(Southdf,                    
                     aes(x = avgtemp_6monthsprior,
                         y = circularity)) +
  geom_point() +
  geom_point(data=south_lowest_circ, colour="#D55E00", size = 3) +
  geom_point(data=south_second_lowest_circ, colour="#cc79a7", size = 3) +
  geom_point(data=south_highest_circ, colour="#0072b2", size = 3) +
  geom_point(data=south_second_highest_circ, colour="#f0e442", size = 3) +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_text(x = 25, y = 0.7, label = "p = 2.72e-06", size = 9) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
GSavgsouthplot_sig <- GSavgsouth + xlim(-3, 32) + ylim(0, 0.8) + ggtitle("South") + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 25))

GSavgsoutheast <- ggplot(Southeastdf,                    
                         aes(x = avgtemp_6monthsprior,
                             y = circularity)) +
  geom_point() +
  geom_point(data=southeast_lowest_circ, colour="#D55E00", size = 3) +
  geom_point(data=southeast_second_lowest_circ, colour="#cc79a7", size = 3) +
  geom_point(data=southeast_highest_circ, colour="#0072b2", size = 3) +
  geom_point(data=southeast_second_highest_circ, colour="#f0e442", size = 3) +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_text(x = 25, y = 0.7, label = "p = 6.63e-06", size = 9) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
GSavgsoutheastplot_sig <- 
  GSavgsoutheast + xlim(-3, 32) + ylim(0, 0.8) + ggtitle("Southeast") + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 25))



fig2 <- ggarrange(GSavgallplot_sig, GSavgsouthplot_sig, GSavgsoutheastplot_sig, ncol = 3, nrow = 1)
png(filename = "~/Desktop/herbarium_paper_figures/circ_GS_temp_sig.png", res = 300, width= 5000, height = 3500)
annotate_figure(fig2, 
                bottom = text_grob("Average Temp GS (C)", 
                                   color = "black", size = 25), 
                left = text_grob("Circularity", rot = 90, color = "black", size = 25))
dev.off()

####### circ vs. ar plot #######

circ_ar_plot <- ggplot(total_datadf, aes(x=aspect_ratio, y=circularity)) + 
  geom_point() + stat_smooth(method='lm', formula = y ~ poly(x,2), linewidth = 2) +
  ylim(0,0.8) + xlim(1,7)

png(filename = "~/Desktop/herbarium_paper_figures/circ_by_ar_v3.png", res = 300, width= 2500, height = 1500)
circ_ar_plot + xlab('Aspect ratio') + ylab('Circularity') + geom_text(x = 5, y = 0.7, label = 'r(496) = .302 , p = <0.0001', size = 4) + 
  theme(axis.title = element_text(size = 16)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
dev.off()

?geom_text()

####### mapping locations #######
df <- read.csv("~/Documents/mapping_data_cleaned.csv")
mapping_df <- na.omit(df)
mapview(mapping, xcol = "longitude", ycol = "latitude", zcol = "climate_region", 
        crs = 4269, grid = FALSE, legend = FALSE)

install.packages('rnaturalearth')
install.packages("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthdata")

usa <- ne_states(
  country = 'united states of america', returnclass = 'sf') %>%
  filter(! name %in% c('Alaska', 'Hawaii'))

ggplot(data = usa) +
  geom_sf() +
  geom_point(data = mapping, aes(x = longitude, y = latitude), size = 2, 
             shape = 21, fill = 'blue') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), axis.text = element_blank())

usa <- usa %>%
  mutate(subregion = case_when(
    startsWith(postal, "AL") ~ "southeast",
    startsWith(postal, "AZ") ~ "southwest",
    startsWith(postal, "CT") ~ "northeast",
    startsWith(postal, "DE") ~ "northeast",
    startsWith(postal, "ME") ~ "northeast",
    startsWith(postal, "MD") ~ "northeast", 
    startsWith(postal, "MA") ~ "northeast", 
    startsWith(postal, "NH") ~ "northeast",
    startsWith(postal, "NJ") ~ "northeast",
    startsWith(postal, "NY") ~ "northeast", 
    startsWith(postal, "PA") ~ "northeast",
    startsWith(postal, "RI") ~ "northeast",
    startsWith(postal, "VT") ~ "northeast",
    startsWith(postal, "WV") ~ "northeast",
    startsWith(postal, "IA") ~ "uppermidwest",
    startsWith(postal, "MI") ~ "uppermidwest",
    startsWith(postal, "MN") ~ "uppermidwest",
    startsWith(postal, "WI") ~ "uppermidwest",
    startsWith(postal, "IL") ~ "ohiovalley",
    startsWith(postal, "IN") ~ "ohiovalley",
    startsWith(postal, "KY") ~ "ohiovalley",
    startsWith(postal, "MO") ~ "ohiovalley",
    startsWith(postal, "OH") ~ "ohiovalley",
    startsWith(postal, "TN") ~ "ohiovalley",
    startsWith(postal, "FL") ~ "southeast",
    startsWith(postal, "GA") ~ "southeast",
    startsWith(postal, "NC") ~ "southeast",
    startsWith(postal, "SC") ~ "southeast",
    startsWith(postal, "VA") ~ "southeast",
    startsWith(postal, "MT") ~ "northernrockiesandplains",
    startsWith(postal, "NE") ~ "northernrockiesandplains",
    startsWith(postal, "ND") ~ "northernrockiesandplains",
    startsWith(postal, "SD") ~ "northernrockiesandplains",
    startsWith(postal, "WY") ~ "northernrockiesandplains",
    startsWith(postal, "AR") ~ "south",
    startsWith(postal, "KS") ~ "south",
    startsWith(postal, "LA") ~ "south",
    startsWith(postal, "MS") ~ "south",
    startsWith(postal, "OK") ~ "south",
    startsWith(postal, "TX") ~ "south",
    startsWith(postal, "CO") ~ "southwest",
    startsWith(postal, "NM") ~ "southwest",
    startsWith(postal, "UT") ~ "southwest",
    startsWith(postal, "ID") ~ "northwest",
    startsWith(postal, "OR") ~ "northwest",
    startsWith(postal, "WA") ~ "northwest",
    startsWith(postal, "CA") ~ "west",
    startsWith(postal, "NV") ~ "west"))
  
  
fig_map <- 
  ggplot() + 
  geom_sf(data = usa, mapping = aes(fill = subregion), show.legend = FALSE, alpha = .4) + 
  geom_point(data = mapping_df, mapping = aes(x = longitude, y = latitude), size = 2, shape = 21, colour = 'black', fill = 'blue') + 
  coord_sf()

####### final map #######
png(filename = "~/Desktop/herbarium_paper_figures/region_map_points.png", res = 300, width= 2500, height = 1500)
fig_map + 
  scale_color_manual(values = safe_colorblind_palette) +
  scale_fill_manual(values = safe_colorblind_palette) +  
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_blank(), 
        axis.line=element_blank()) + 
  theme(legend.position = "none")
dev.off()

ggplot(total_datadf,                    
                       aes(x = tavg_previous_.year,
                           y = aspect_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + theme_classic()

####### Figure  - (circ x ar) + time models alt  #######
carplot <- circ_ar_plot + xlab('Aspect ratio') + ylab('Circularity') + geom_text(x = 5, y = 0.7, label = 'r(496) = .302 , p = <0.0001', size = 6) + 
  theme(axis.title = element_text(size = 16)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


deltaaicreorder_c <- ggplot(aicdfh, aes(x = reorder(Modnames, Delta_AICc), y = Delta_AICc)) +
  geom_bar(stat="identity", color="white", fill="black") + theme_classic()
png(filename = "~/Desktop/herbarium_paper_figures/deltaaic_circ.png", res = 300, width= 2500, height = 1500)
d_circ <- deltaaicreorder_c + xlab("Model Names") + ylab("Delta AICc") + theme(axis.title = element_text(size = 14)) +
  theme(axis.text = element_text(size = 16))
dev.off()

deltaaicreorder_ar <- ggplot(aic_ar, aes(x = reorder(Modnames, Delta_AICc), y = Delta_AICc)) +
  geom_bar(stat="identity", color="white", fill="black") + theme_classic()
png(filename = "~/Desktop/herbarium_paper_figures/deltaaic_ar.png", res = 300, width= 2500, height = 1500)
d_ar <- deltaaicreorder_ar + xlab("Model Names") + ylab("Delta AICc") + theme(axis.title = element_text(size = 14)) +
  theme(axis.text = element_text(size = 11))
dev.off()

png(filename = "~/Desktop/herbarium_paper_figures/circ_ar_model_selection.png", res = 300, width= 4800, height = 3000)
ggarrange(carplot, d_circ)
dev.off()
          