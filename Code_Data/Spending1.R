# family politics # New Zealand

library(haven); library(stargazer); library(ggplot2)
df <- read_sav("dynasties_20180508_(for_Chia-hung_Tsai).sav")
#recode
df$elected<-df$win
df$elected[df$win==0]<-"Not Elected"
df$elected[df$win==1]<-"Elected"
df$family<-df$fam1
df$family[df$fam1==0]<-"No Relative with Office"
df$family[df$fam1==1]<-"Relative with Office"
# win & relative has office
T1<-table(df$elected, df$family)
#graph of political family and winning elections
library(dplyr); library(theme)
dyna.t <- summarize(group_by(df, family, elected), Count=n())
dyna.t <- mutate(dyna.t, Pct=Count/sum(Count))

png('Winplot.png')
ggplot(data=dyna.t, aes(x=family, y=Pct, 
                      fill=elected)) +
  geom_bar(stat="identity") +
  scale_y_continuous(label = scales::percent) +
  labs(x="Candidate's Family")  +
  theme(axis.text.y = element_text(face="bold", 
                                   color="#993333", 
                                   size=14) ) +
  theme_bw()
dev.off()

#only 2016
Yr <- df$year>2015
df2 <- df[Yr, ]
#Any relative won office before
#table(dyna2$fam1)
#View(dyna2)

# Data analysis of family background and vote shares
fd<- read.csv('2016_leg_spent.csv', header=T, sep=",",
          stringsAsFactors = F)
# Removing thousand separater
fd.1<-sapply(fd[, 12:31], function(x) 
          as.numeric(gsub(",", "",x)))
fd<-data.frame(cbind(fd[, 1:11], fd.1))

# Removing percentage
fd$shares <- as.double(gsub("%", "", fd$shares))
# Dummy variables for KMT, DPP, NPP
fd$KMT <- rep(0, nrow(fd))
fd$KMT[fd$Party==1] <-1
fd$DPP <- rep(0, nrow(fd))
fd$DPP[fd$Party==2] <-1
fd$PFP <- rep(0, nrow(fd))
fd$PFP[fd$Party==3] <-1

# remove  NA (spending as the selection criterion)
fd$spending <- fd$支出總計
fd$income <- fd$收入總計

fd$ok <- !is.na(fd$spending)
fd2 <- fd[fd$ok, ]
# non-zero spending
fd2$zero <- fd2$spending > 0
fd3<-fd2[fd2$zero, ]

# Many candidates spend more than their contributions.
hist(fd3$spending-fd3$income, bins=5)

# taking log
fd3$logspending <-log(fd3$spending)

#binary model (family not significant)
GLM1 <- glm(elected ~ Family + logspending + Family:logspending, 
                     family=binomial(logit), data=fd3)
stargazer(GLM1, type='text')

## Adding incumbency to the model
GLM2 <- glm(elected ~ incumbent + Family + logspending + Family:logspending, 
            family=binomial(logit), data=fd3)
stargazer(GLM2, type='text')


# Scatter plot
png('spendingplot.png')
ggplot(fd3, aes(x=logspending, y=shares)) +
  geom_point()  +
  geom_smooth(method='loess', se=T) +
  labs(y="Percent", x="Log of Spending") +
  theme_bw()
dev.off()

#OLS model
fd3$logspending2 <- fd3$logspending^2

OLS0 <- lm(shares ~  Family + logspending + logspending2 ,
           data=fd3)
stargazer(OLS0, type='text')

OLS1 <- lm(shares ~  Family + logspending + logspending2 +
             Family:logspending, data=fd3)
stargazer(OLS1, type='text')


OLS2 <- lm(shares ~ Family  + logspending + logspending2 +
             Family:logspending + KMT + DPP + PFP, 
           data=fd3)
stargazer(OLS0, OLS1, OLS2, type='text')

## Adding incumbency

OLS3 <- lm(shares ~ incumbent + Family  + logspending + logspending2 +
             Family:logspending2 + KMT + DPP + PFP, 
           data=fd3)
stargazer(OLS0, OLS1, OLS2, OLS3, type='text')

# IV model
OLS3.1<-lm(logspending ~ Family, data=fd3)
fd3$predict <- predict(OLS3.1)
OLS3.2 <- lm(shares ~ predict  + logspending  +
             KMT +DPP + PFP, data=fd)
stargazer(OLS3.2, type='text')

