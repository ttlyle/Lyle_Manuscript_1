library(readxl)
library(ggplot2)
library(car)
Untreated_Scores<- read_excel ("C:/Users/ttlyle/Desktop/Untreated_Scores.xlsx", 
                               sheet = "VD_data")
VD <-Untreated_Scores[c(1:771),c(1:12)]
Mouse <- VD[c(1:771), c(2)]
Sex <- VD[c(1:771), c(3)]
Initiation_Latency <- VD[c(1:771), c(10)]
Group <- VD[c(1:771), c(4)]
Group2 <- VD[c(1:771), c(5)]
Day <- VD[c(1:771), c(7)]
Stage <- VD[c(1:771), c(6)]
Response_Latency <- VD[c(1:771), c(11)]
Trials <- VD[c(1:771), c(9)]
Performance <- VD[c(1:771), c(8)]
Collection_Latency <- VD[c(1:771), c(12)]
df1 <- data.frame(Mouse, Sex, Initiation_Latency, Group, Group2, Day, Stage, Response_Latency, Collection_Latency, Performance, Trials)
df2 <- df1[!df1$Group=='Vector', ]
dfVD2 <- drop_na(df2)
rlogits <- dfVD3[dfVD3$Day>9 & dfVD3$Day<11, ]

####Logit-transform_Performance#####
dfVD3 <- GiReversal4 %>%
  mutate(logitIL = car::logit(Performance)) ###Calculating Logits

dd <- lm(logitIL ~ 0 + Mouse, data = dfVD3) ###Calculating Slopes from Logits per Mouse

dd <- lmList(logitIL ~ Group | Mouse, data = dfVD3) ###Calculating Slopes and Intercepts from Logits per Mouse (Different Try_Latest_Effort)
dd$`1`$coefficients

###Reading In Logit Slope Xcel File####
Logit_Transformation <- read_excel ("C:/Users/ttlyle/Desktop/Untreated_Scores.xlsx", 
                               sheet = "Logit_Transformation")
VDLogit <-Logit_Transformation[c(1:73),c(1:12)]
Logit <- dfVD3[!dfVD3$Sex=='Female', ]

####factanova <- aov(VD Slopes from Logits ~ Group + Sex + Group:Sex, data)
anova <- aov(logitIL ~ Group2, data = pbdf.2) %>%
  tukey_hsd()
summary(anova)


factanova <- aov(RPlogit_Slope_Early_Learning ~ Group, Logit)
summary(factanova)
eta_squared(factanova)
leveneTest(Performance ~ Group*Sex, dfR3.2)




####QQPlot_Performance#####
dfVD2 %>%
  ggplot(aes(sample = Performance)) +
  stat_qq(shape = 1, size = 2) + 
  stat_qq_line() + 
  ylab( "Proportion of Correct Choices") + 
  xlab("Normal quantile") + 
  theme_bw()



exp(coef(dd))
##or

dfVD3 <- dfVD2$logitPerformance <- car::logit(dfVD2$Performance)

####factanova <- aov(Performance ~ Group + Sex + Group:Sex, dfR3.2)

factanova <- aov(logitPerformance ~ Group + Sex, dfVD3)
summary(factanova)
eta_squared(factanova)
leveneTest(Performance ~ Group*Sex, dfR3.2)

###QQPlot_Logit_Performance_Transformation###
dfVD2 %>%
  ggplot(aes(sample = logitPerformance)) +
  stat_qq(shape = 1, size =2) + 
  stat_qq_line() +
  ylab( "Proportion of Correct Choices") + 
  xlab("Normal quantile") + 
  theme_bw()

###Normality Checking####
normality <- dfVD3 %>%
  group_by(Group)%>%
  shapiro_test(logitPerformance)
data.frame(normality)

###Sphericity Assumption###
res<-anova_test(data=dfVD3, dv=logitPerformance, wid = Mouse, within = Day)
get_anova_table(res)

asd <- lm(logitIL ~ Day + Group2 + Sex, data = dfVD3)
summary(asd)

###fit logistic regression model##
model <- glm(Performance ~ 0 + Mouse, family = 'binomial', data = dfVD2)
summary(model)
exp(coef(model))
mod <- exp(cbind(Odds_Ratio = coef(model), confint(model)))


####Correlation: Reversal Logits and PNNs######

cor.test(PNNSocial1$Social_Preference_Index...12, PNNSocial1$ave, method = "pearson")


