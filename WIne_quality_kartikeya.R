#Downloading the data
wine<- read.csv("/Users/tharadawoodjee/Downloads/RedWinesFinal.csv")

install.packages("ggplot2")
library(ggplot2)
library(ggfortify)
library(ggthemes)
library(corrplot)
library(reshape2)
library(dplyr)
library(randomForest)
##################DATA DESCRIPTION###################
#Histogram for each attribute
install.packages("tidyr")
library(tidyr)
install.packages("corrplot")
library(corrplot)
library(gridExtra)
install.packages("GGally")
library(GGally)
library(knitr)
library(dplyr)
wines = wine[, c(2:13)]
wines %>%
  gather(Attributes, value, 1:12) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Wines Attributes - Histograms") +
  theme_bw()

#Boxplot for each attribute  
wines %>%
  gather(Attributes, value,1:12) %>%
  ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Wines Attributes - Boxplots") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  ylim(0, 35) +
  coord_flip()


#Making "goodquality" column
wine$goodquality<-ifelse(wine$quality>6,1,0)
attach(wine)

##Summary statistics
summary(wine)

##Scatterplot matrix
plot(wine,col=ifelse(wine$quality>6, "purple", "grey"))

##Correlation heatmap 
corrplot(cor(wine))

#Collinearity of variables
require(psych)
pairs.panels(wine)

#Physiochemical Properties
#Alcohol 
ggplot(wine,aes(x=alcohol,fill=factor(goodquality)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(alcohol[goodquality==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(alcohol[goodquality==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(8,15,1))+
  xlab(label = "Alcohol Level")+
  ggtitle("Distribution of Alcohol Levels")+
  theme_classic()

#Sulphates
ggplot(wine,aes(x=sulphates,fill=factor(goodquality)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(sulphates[goodquality==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(sulphates[goodquality==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,2,0.25))+
  xlab(label = "Sulphates Level")+
  ggtitle("Distribution of Sulphates Levels")+
  theme_classic()

#Citric acid
ggplot(wine,aes(x=citric.acid,fill=factor(goodquality)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(citric.acid[goodquality==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(citric.acid[goodquality==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  xlab(label = "Citric Acid Level")+
  ggtitle("Distribution of Citric Acid Levels")+
  theme_classic()

#Volatile acidity
ggplot(wine,aes(x=volatile.acidity,fill=factor(goodquality)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(volatile.acidity[goodquality==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(volatile.acidity[goodquality==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,1.6,0.1))+
  xlab(label = "Volatile Acidity Level")+
  ggtitle("Distribution of Volatile Acidity Levels")+
  theme_classic()


##################MODEL SELECTION AND METHODOLOGY###################
####Heteroskedasticity
library(car)
mreg = lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
            chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+
            sulphates+alcohol)
ncvTest(mreg) #p = 2.0419e-06 <- heteroskedasticity

#Correcting for heteroskedasticity
require(lmtest)
require(plm)

coeftest(mreg, vcov=vcovHC(mreg, type="HC1"))

####Predictors with p-value <0.05
#volatile.acidity 3.540e-15
#chlorides 8.170e-05
#free.sulfur.dioxide 0.05016
#total.sulfur.dioxide 1.342e-05
#pH 0.05168
#sulphates 8.264e-12
#alcohol < 2.2e-16

#######Outliers
outlierTest(mreg) #4STD away obs 833

#Removing outlier 833
wine2 = wine[ -c(833) , ]

mreg_noout = lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                  chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+
                  sulphates+alcohol, data=wine2)


summary(mreg_noout)

####Predictors with p-value <0.05
#volatile.acidity < 2e-16
#chlorides 1.46e-05
#free.sulfur.dioxide 0.0252
#total.sulfur.dioxide 4.70e-06
#pH 0.0559
#sulphates 8.38e-16
#alcohol < 2e-16

##PCA without quality
wine_vars = wine[, c(2:12)]
pca = prcomp(wine_vars, scale=TRUE)
library(ggplot2)
library(ggfortify)

autoplot(pca, data = na.omit(wine), loadings = TRUE, col=ifelse(wine$quality>6, "purple", "grey"), loadings.label = TRUE)


##PCA with quality
wine_vars = wine[, c(2:13)]
pca = prcomp(wine_vars, scale=TRUE)
library(ggplot2)
library(ggfortify)

autoplot(pca, data = na.omit(wine), loadings = TRUE, col=ifelse(wine$quality>6, "purple", "grey"), loadings.label = TRUE)

#RF feature selection for classification task
redwineRF = randomForest(factor(goodquality)~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                           chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+
                           sulphates+alcohol)

importance = importance(redwineRF)
varImportance = data.frame(Variables = row.names(importance),
                           Importance = round(importance[,"MeanDecreaseGini"],
                                              2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()

####################CLASSIFICATION TASK**********************************
#####Using predictors from heteroskedastity, outliers tests, pca and random forest feature selection
forest2 = randomForest(factor(goodquality)~volatile.acidity+free.sulfur.dioxide+
                         chlorides+total.sulfur.dioxide+pH+
                         sulphates+alcohol+density+citric.acid, ntree=10000, data=wine2, importance=TRUE)

forest2
#OOB estimate of  error rate: 8.2%
#Accuracy = 91.80%





#####checking OOB on new model
attach(wine2)
forest2 = randomForest(factor(goodquality)~volatile.acidity+free.sulfur.dioxide+
                         chlorides+total.sulfur.dioxide+pH+
                         sulphates+alcohol+density+citric.acid, ntree=10000, do.trace=500, importance=TRUE)
#best OOB score with 10000 trees





##Boosted
library(gbm)
set.seed (1)


forest3 = randomForest(factor(goodquality)~volatile.acidity+free.sulfur.dioxide+
                         chlorides+total.sulfur.dioxide+pH+
                         sulphates+alcohol+density+citric.acid, data=wine2,distribution="bernoulli", ntree=10000,
                       interaction.depth=4)
forest3
#OOB estimate of  error rate: 7.95%
#Accuracy = 92.05%

#####################PREDICTIONS############################
##variant 1
variant1 = data.frame(volatile.acidity=0.182, free.sulfur.dioxide=22, chlorides=0.067,
                      total.sulfur.dioxide=60, pH=2.84, sulphates=0.57, alcohol=9.2000000,
                      density=0.99457, citric.acid=0.59)

predict(forest3, variant1, type = 'prob')


##variant 2
variant2 = data.frame(volatile.acidity=0.410, free.sulfur.dioxide=28, chlorides=0.088,
                      total.sulfur.dioxide=47, pH=3.51, sulphates=0.53, alcohol=9.550000,
                      density=0.99644, citric.acid=0.51)

predict(forest3, variant2, type = 'prob')


##variant 3
variant3 = data.frame(volatile.acidity=0.610, free.sulfur.dioxide=5, chlorides=0.089,
                      total.sulfur.dioxide=28, pH=3.31, sulphates=0.62, alcohol=11.300000,
                      density=0.99382, citric.acid=0.43)

predict(forest3, variant3, type = 'prob')


##variant 4
variant4 = data.frame(volatile.acidity=1.025, free.sulfur.dioxide=15, chlorides=0.097,
                      total.sulfur.dioxide=36, pH=3.64, sulphates=0.68, alcohol=10.450000,
                      density=0.99837, citric.acid=0.10)

predict(forest3, variant4, type = 'prob')


##variant 5
variant5 = data.frame(volatile.acidity=0.254, free.sulfur.dioxide=21, chlorides=0.063,
                      total.sulfur.dioxide=42, pH=3.20, sulphates=0.70, alcohol=11.650000,
                      density=0.99284, citric.acid=0.39)

predict(forest3, variant5, type = 'prob')





