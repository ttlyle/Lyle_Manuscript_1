library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plyr)
library(ggbeeswarm)
library(rstatix)
library(effsize)
library(lsr)
library(car)
########Reading in the Data###########
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
dfVD4 <- dfVD3[!dfVD3$Sex=='Male', ]
df2.1 <- df2[!df2$Group=='Gq', ]
df4 <- df2.1[!df2.1$Group=='Gi', ]
df5 <- df4[!df4$Group=='Gi', ]
dfVD2 <- drop_na(df2)
Days <- factor(PBdf3.1$Day2, levels = c("Early Learning", "Mid Learning", "Late Learning"))  
Groups <- factor(cumsumplot1.9$Group, levels = c("Untreated", "CNO", "Gq", "Gi"))  
Sexs <- factor(dfVD3$Sex, levels = c('Male', 'Female'))
#######GGPlot Graphing VD Performance with Slopes########

ggplot(pbdf.2, aes(x = Day, y = Performance*100, group = Group, color = Group)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  stat_summary(geom = "line", fun = "mean") + 
  stat_summary(fun.data = mean_se) + #geom = "errorbar") +  
  #geom_boxplot() +
  #geom_jitter(position = position_jitterdodge(jitter.width = 0., dodge.width = .75), size = 1.4)+
  geom_smooth(method = lm, se=F) + 
  stat_regline_equation(label.y = c(100,98), aes(label = ..eq.label..), vjust = .5,) +
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y = c(96, 94), label.x = c(1, 1), dodge.width = 10) + #vjust = 2.5, hjust = 1) +
  #geom_hline(yintercept = 50, linetype = 'dotted', col = 'black',) +
  #annotate("text", x = 3.5, y = 55, label = "Chance Performance", size = 2.5)+
  labs( x = "Day", y = "Correct Choice (%)", title = "Group Performance - VD") +
  #scale_color_discrete(limits = c("Male", "Female")) +
  scale_color_discrete(limits = c("Untreated", "CNO", "Gq", "Gi")) +
  #scale_color_manual(values = c("Male" = "black", "Female" = "red"))+
  scale_color_manual(values=c("Untreated"="black", "CNO" = "dark grey", "Gq" = "forest green", "Gi" = "Magenta")) +
  #scale_x_discrete(limits = c("Early Learning", "Mid Learning", "Late Learning")) +
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0,100) +
  scale_x_continuous(breaks = c(1:10),
                     labels = c(1:10)) +
  facet_grid(.~Sex)
  
stat.test.cumsum <- aov(Performance ~ Group, data = dfVD2) %>%
  tukey_hsd()
  
  
#######GGPlot Graphing VD Performance with no Slopes########
ggplot(dfR2, aes(x = Day, y = Performance*100, group = Group, color = Group)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  stat_summary(geom = "line", fun = "mean") + 
  stat_summary(fun.data = mean_se) + #geom = "errorbar") +
  #stat_compare_means(method = "anova", label = "p.signif", label.y = 100, position = "identity") +
  geom_hline(yintercept = 70, linetype = 'dotted', col = 'black',) +
  #annotate("text", x = 3.5, y = 55, label = "Chance Performance", size = 2.5)+
  labs( x = "Day", y = "Correct Choice (%)", title = "Reveral Performance - Females") +
  #scale_color_discrete(limits = c("Male", "Female")) +
  scale_color_discrete(limits = c("Untreated", "CNO", "Gq", "Gi")) +
  #scale_color_manual(values = c("Male" = "black", "Female" = "red"))+
  scale_color_manual(values=c("Untreated"="black", "CNO" = "grey", "Gq" = "forest green", "Gi" = "Magenta"))+
  theme(plot.title = element_text(hjust = .5)) +
  scale_x_continuous(breaks = c(1:10),
                     labels = c(1:10))+
  ylim(0, 100)+
  xlim(0,40 )+
  facet_wrap(.~Sex)
  



######VisualDiscrimination_Performance_Boxplot##########
  
  ggplot(df2, aes(x = Day, y = Performance, group = Group, color = Group)) +
  theme_bw(base_size = 12, base_family = "serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_boxplot() + stat_compare_means(method = "anova", label.y = 5500) +
  stat_compare_means(label = "p.format", method = "t.test", ref.group = "Untreated") +
  stat_boxplot(geom = "errorbar") +
  geom_jitter()+ geom_beeswarm() +
  labs( x = "Groups", y = "Mean Pixel Intensity", title = "PNN Intensity") + theme(plot.title = element_text(hjust = .5))+
  scale_color_discrete(limits = c("Untreated", "CNO", "Vector", "retro-gi", "Gi", "Gq")) +
  scale_x_discrete(limits = c("Untreated", "CNO", "Vector", "retro-gi", "Gi", "Gq"))

####One-Way_Repeated_Measures_ANOVA_VD#########
library(olsrr)
modelV1 <- aov(Performance ~ Group2 + Error(Mouse), data = dfR3.2) 
summary(modelV1)

mdl <- lm(Performance ~ factor(Group2), data = dfR3.2)
summary(mdl)
ols_test_f(mdl)

ols_test_bartlett(dfR3.2, 'Performance', group_var = 'Group2')


#####Response_Latency_One_Way_Repearted_Measures_ANOVA_VD########

modelV2 <- aov(formula = Performance ~ Sex + Error(Mouse), data = df4)
summary(modelV2)


####One-Way_Repeated_Measures_ANOVA_InitiationLatency_VD#######################
data = df2  
model <- aov(formula = Initiation_Latency ~ Group + Error(Initiation_Latency/Day), data = df2)
summary(model)

######Untreated_Reversal_Performance###############

Untreated_Scores<- read_excel ("C:/Users/ttlyle/Desktop/Untreated_Scores.xlsx", 
                               sheet = "RV_data")
Untreated_Reversal <-Untreated_Scores[c(1:1003),c(1:12)]
Mouse <- Untreated_Reversal[c(1:1003), c(2)]
Sex <- Untreated_Reversal[c(1:1003), c(3)]
Initiation_Latency <- Untreated_Reversal[c(1:1003), c(10)]
Group <- Untreated_Reversal[c(1:1003), c(4)]
Group2 <- Untreated_Reversal[c(1:1003), c(5)]
Day <- Untreated_Reversal[c(1:1003), c(7)]
Stage <- Untreated_Reversal[c(1:1003), c(6)]
Response_Latency <- Untreated_Reversal[c(1:1003), c(11)]
Trials <- Untreated_Reversal[c(1:1003), c(9)]
Performance <- Untreated_Reversal[c(1:1003), c(8)]
Collection_Latency <- Untreated_Reversal[c(1:1003), c(12)]
dfR1 <- data.frame(Mouse, Sex, Initiation_Latency, Group, Group2, Day, Stage, Response_Latency, Performance, Trials, Collection_Latency)
dfR2 <- dfR1[!dfR1$Group=='Vector', ]
dfR3.1 <- dfR2[dfR2$Day>0 & dfR2$Day<11, ]

Male.group.dfR <- dfR2[!dfR2$Sex=='Male', ]
dfR3 <- dfR2[!dfR2$Group=='Gi', ]
dfR4 <- dfR3[!dfR3$Group=='Vector', ]
dfR5 <- dfR4[!dfR4$Group=='Gq', ]
dfR1.1 <- dfR3.1[!dfR3.1$Sex=='male', ]
dfR3.2 <- drop_na(dfR2)
Sexs <- factor(cumsumplot1.9$Sex, levels = c("Male", "Female"))  

####Factorial ANOVA############
aggregate(Trials ~ Group + Sex, dfVD2, mean)

factanova <- aov(cumulative_sum ~ Group2 + Sex + Group2:Sex, cumsumplot1.3)
summary(factanova)
eta_squared(factanova)
leveneTest(Performance ~ Group*Sex, dfR3.2)

##Graphing_Reversal_Performance

ggplot(pbdf.2, aes(x = Day, y = logitIL, group = Sex, color = Sex)) +
  theme_bw(base_size = 12, base_family = "serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  stat_summary(geom = "line", fun = "mean") +
  geom_smooth(method = lm) +
  #geom_hline(yintercept = 70, linetype = 'dotted', col = 'black') +
  labs( x = "Day", y = "Performance", title = "Reversal Performance") +
  #scale_color_discrete(limits = c("Untreated", "CNO", "Vector", "Gi", "Gq"))+
  facet_grid(.~Group)
  theme(plot.title = element_text(hjust = .5))+
  scale_x_continuous(breaks = c(1:10),
                     labels = c(1:10))+
  scale_y_continuous(limits = c(0:10))

######################################################Cumulative_Plot################
ggplot(cumsumplot, aes(x=Day, y = cumulative_sum, group = Group2, color = Group2)) +
  theme_bw(base_size = 12, base_family = "serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  stat_summary(geom = "line", fun = "mean") +
  #scale_color_discrete(limits = c("Untreated", "CNO", "Vector", "Gq", "Gi")) +
  #scale_color_manual(values=c("Untreated"="black", "CNO" = "dark grey", "Gq" = "forest green", "Gi" = "Red")) +
  #stat_pvalue_manual(stat.test.cumsum, label = "p.adj.signif", y.position = 1850) +
  labs( x = "Group", y = "Cumulative Initation Latency (minutes)", title = " Cerebellar Perturbation Reduceds Trial Initiation Latency") +
  #scale_color_discrete(limits = c("Male", "Female")) +
  scale_color_discrete(limits = c("Control", "Gq", "Gi"))+
  #scale_color_manual(values=c("Male"="black", "Female"="red")) +
  scale_color_manual(values=c("Control"="black", "Gq" = "forest green", "Gi" = "Red"))
  theme(plot.title = element_text(hjust = .5)) +
  scale_x_continuous(breaks = c(1:10),
                     labels = c(1:10)) 
  
##########Here
ggplot(cumsumplot1.3, aes(x = Group2, y = cumulative_sum, color = Group2)) +
    theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    geom_boxplot() + 
    #stat_compare_means(method = "anova", label.y = 200) +
    stat_pvalue_manual(stat.test.cumsum, label = "p.adj.signif", y.position = 200) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0., dodge.width = .75), size = 1.4,) +
    scale_color_discrete(limits = c("Male", "Female"))+
    #scale_color_discrete(limits = c("Untreated", "CNO", "Gq", "Gi"))+
    scale_color_manual(values = c("Male" = "black", "Female" = "red"))+
    #scale_color_manual(values=c("Untreated" = "black", "CNO" = "Grey", "Gq" = "forest green", "Gi" = "magenta")) +
    theme(plot.title = element_text(hjust = .5)) +
    #geom_hline(yintercept = 70, linetype = 'dotted', col = 'black',)+
    #scale_x_discrete(limits = c("Early Learning", "Mid Learning", "Late Learning")) +
    labs( x = "Group", y = " Cumulative Collection Latency", title = "VD Collection Latency - Late-Learning") +
    theme(axis.title.x = element_blank()) +
    ylim(0,1) +
    facet_grid(.~Group) 
  
ggsave(filename = "Untreated Cumulative Trials.eps",
       plot = print(cp), 
       device = cairo_ps)

dfR3.3 <- dfR3.2[!dfR3.2$Mouse=='BC26.1 M1', ]
dfR3.4 <- dfR3.3[!dfR3.3$Mouse=='BC26.1 M2', ]
 
cumsumplot <- ddply(pbdf.2[order(pbdf.2$Day),],
                    .(Mouse),
                    .fun = transform,
                    cumulative_sum=(cumsum(Trials)))

cumsumplot1.1 <- cumsumplot[!cumsumplot$Day=='4', ]
cumsumplot1.2 <- cumsumplot1.1[!cumsumplot1.1$Day=='5', ]
cumsumplot1.3 <- cumsumplot1.2[!cumsumplot1.2$Day=='6', ]
cumsumplot1.4 <- cumsumplot1.3[!cumsumplot1.3$Day=='4', ]
cumsumplot1.5 <- cumsumplot1.4[!cumsumplot1.4$Day=='5', ]
cumsumplot1.6 <- cumsumplot1.5[!cumsumplot1.5$Day=='6', ]
cumsumplot1.7 <- cumsumplot1.6[!cumsumplot1.6$Day=='7', ]
cumsumplot1.8 <- cumsumplot1.7[!cumsumplot1.7$Day=='8', ]
cumsumplot1.9 <- cumsumplot1.8[!cumsumplot1.8$Day=='9', ]
cumsumplot2 <- drop_na(cumsumplot1.9)
Groups <- factor(cumsumplot1.9$Group, levels = c("Untreated", "CNO", "Gq", "Gi"))

stat.test.cumsum <- aov(cumulative_sum ~ Group2, data = cumsumplot1.3) %>%
  tukey_hsd()
summary(stat.test.cumsum)
psych::describeBy(cumsumplot1.9, cumsumplot1.9$Group, cumsumplot1.9$cumulative_sum)

cumsumplot3 <- cumsumplot1.2[!cumsumplot1.2$Sex=='Female', ]

PBdf <- dfR3 %>%
  mutate(Day2 = case_when(Day >= 1 & Day <= 10))
                      

PBdf1 <- PBdf %>%
  #select(Mouse, Day2, Sex, Performance) %>%  
  group_by(Mouse,Day2, Sex, Group) %>%
  mutate(Perf = mean(Performance))

PBdf2 <- PBdf1 %>% distinct(Perf, Day2, Sex, Group, .keep_all = TRUE)

PBdf3 <- PBdf2 %>%
  select(Mouse, Day2, Sex, Group, Perf)

PBdf3.1 <- drop_na(PBdf3)

############ScatterPlot_Performance by Trials##################
sp <- ggplot(GiR, aes(x = logitIL, y = ave)) +
  geom_point(alpha=0.5)+
  geom_smooth(method = lm, se = T) +
  geom_beeswarm(groupOnX = F) +
  theme_bw(base_size = 12, base_family = "serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs( x = "Total Trials", y = "Performance", title = "Correlation: Female Performance & Trials - VD") +
  scale_color_discrete(limits = c("Untreated", "CNO", "Vector", "Gq", "Gi")) +
  scale_color_manual(values=c("Untreated"="black", "CNO" = "dark grey", "Gq" = "forest green", "Gi" = "Magenta")) +
  #scale_color_manual(values=c("Male"="black", "Female"="red")) +
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0,100) +
  xlim(0,60) +
  facet_grid(.~Group) 

sp +stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y =99)

stat.test.cumsum <- aov(cumulative_sum ~ Group, data = cumsumplot) %>%
  tukey_hsd()
summary(stat.test.cumsum)

#########################################
#########################################
#######TESTING Things Out Here##########
sp <- ggplot(GiR, aes(x= logitIL, y = ave)) +
  geom_point(alpha=0.5)+
  geom_smooth(method = lm, color = "red", se = F) +
  #stat_regline_equation(label.y = 90, aes(label = ..eq.label..), vjust = -.2) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y= Inf, label.x = Inf, vjust = 2.5, hjust = 1.6) +
  geom_beeswarm(groupOnX = F) +
  theme_bw(base_size = 12, base_family = "serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs( x = "Day 10 Reversal Logits", y = "PNN Pixel Intensity", title = "Pearson Correlation: Reversal Logit & PNN Pixel Intensity") +
  #scale_color_discrete(limits = c("Male", "Female"))+
  #scale_color_manual(values=c("Male"="black", "Female"="red"))+
  theme(plot.title = element_text(hjust = .5)) +
  ylim(1000,5000) +
  facet_grid(.~Sex) 

sp +stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = .9)

###########Regression_Learning&Reversal_Parameters########

modelV1 <- lm(formula = Performance ~ Trials + Day + Group + Sex, data = dfR3.2)
summary(modelV1)


cumsumplot <- ddply(DREADD_cFos1[order(DREADD_cFos1$Sample),],
      .(Mouse),
      .fun = transform,
      cumulative_sum=(mean(cFos_Count)))

######Cohen's D Effect Size#######
cohensD(Logit$logitIL ~ Logit$Group)

cm2 <- cumsumplot1.9[!cumsumplot1.9$Group=='Gq', ]
cm3 <- cm2[!cm2$Group=='Gi', ]
cm4 <- cm3[!cm3$Group=='Untreated', ]

stat.test.cumsum <- aov(logitIL ~ Group, data = dfVD3) %>%
  tukey_hsd()
summary(stat.test.cumsum)


cor.test(dfVD3$logitIL, dfVD3$Collection_Latency...12, method = "pearson")
