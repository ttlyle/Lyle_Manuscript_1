#PNN Analysis
library(ggpubr)
library(ggbeeswarm)
library(dplyr)
library(EnvStats)
library(plotrix)
######Reading in the Data############
PNN_Analysis<- read_excel ("C:/Users/ttlyle/Desktop/Untreated_Scores.xlsx", 
                           sheet = "PNN")
PNN_data <-PNN_Analysis[c(1:281),c(1:7)]

PND <- drop_na(PNN_data)
Sex <- PND[c(1:281), c(5)]
Mouse <- PND [c(1:281), c(1)]
Sample <- PND [c(1:281), c(2)]
Group <- PND[c(1:281), c(3)]
Group2 <- PND[c(1:281), c(4)]
IntensityMean_Cy5 <- PND[c(1:281), c(7)]
dfPNN <- data.frame(Sex, Mouse, Sample, Group, Group2, IntensityMean_Cy5)
dfPNN1 <- drop_na(dfPNN)
dfPNN3 <- PNN1.2[!PNN1.2$Group2=='Vector', ]
dfPNN4 <- PNN1.2[!PNN1.2$Group2=='Retro', ]
dfPNN5 <- dfPNN7[!dfPNN7$Sex=='Male', ]
Groups <- factor(dfPNN7$Group, levels = c("Untreated", "CNO", "Vector", "Gq", "Gi"))
Sexs <- factor(dfPNN7$Sex, levels = c("Male", "Female"))

dfPNN6 <- dfPNN4%>%
  select(Mouse, Sample, Sex, Group, Group2, ave)
dfPNN7 <- dfPNN6[dfPNN6$Sample>0 & dfPNN6$Sample<2, ]

####Averaging Sample per Mouse####

PNN1.1 <- dfPNN1 %>%
  group_by(Mouse) %>%
  mutate(ave = mean(IntensityMean_Cy5))

PNN1.2 <- PNN1.1 %>%
  group_by(Mouse)%>%
  mutate(sderror = std.error(IntensityMean_Cy5))

PNN1.1 <- dfPNN1 %>%
  group_by(Group2) %>%
  summarise(ave = mean(IntensityMean_Cy5, na.rm=TRUE))

factanova <- aov(ave ~ Group2 + Sex + Group2:Sex, dfPNN7)
summary(factanova)


#########Creating new Objects w/ varying Groups############

dfPNN1_1 <- dfPNN7[!dfPNN7$Group2=='Gi', ]
dfPNN1_2 <- dfPNN1_1[!dfPNN1_1$Group2=='Gq', ]
stat.test.RGi <- aov(ave ~ Sex, data = dfPNN1_2) %>%
  tukey_hsd()


dfPNN1.1 <- dfPNN1[!dfPNN1$Group2=='Retro', ]
dfPNN1.2 <- dfPNN1.1[!dfPNN1.1$Group2=='Vector', ]
dfPNN1.3 <- dfPNN1.2[!dfPNN1.2$Group2=='Gq', ]
stat.test.Gq <- aov(IntensityMean_Cy5 ~ Group2, data = dfPNN1.2) %>%
  tukey_hsd()

dfPNN1.3 <- dfPNN1[!dfPNN1$Group2=='Gq', ]
dfPNN1.4 <- dfPNN1.3[!dfPNN1.3$Group=='Retro-Gi', ]
stat.test.Gi <- aov(IntensityMean_Cy5 ~ Group2, data = dfPNN1.4)%>%
  tukey_hsd()

#PNNGGplot_Boxplot

ggplot(dfPNN7, aes(x = Sexs, y = ave, group = Sexs, color = Sexs)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  geom_boxplot() + 
  stat_compare_means(method = "anova", label.y = 5000) +
  geom_point() +
  scale_color_discrete(limits = c("Male", "Female")) +
  scale_color_manual(values = c("Male" = "black", "Female" = "red")) +
  #scale_color_manual(values=c("Untreated" = "black", "CNO" = "light grey", "Vector" = "dark grey", "Gq" = "forest green", "Gi" = "magenta")) +
  labs(y = "Mean Pixel Intensity", title = "Group Difference DCN PNN Intensity") + 
  theme(plot.title = element_text(hjust = .5)) +
  ylim(1000, 5000) +
  facet_grid(.~Group)

stat.test.PNN <- aov(ave ~ Group, data = dfPNN7) %>%
  tukey_hsd()
summary(stat.test.PNN)

###TimelinePNN

PNNTimeline_Analysis<- read_excel ("C:/Users/ttlyle/Desktop/Untreated_Scores.xlsx", 
                           sheet = "Timeline")
PNNT <- PNNTimeline_Analysis[!PNNTimeline_Analysis$Day=='22', ]
PNNT2 <- PNNT[!PNNT$Day=='23', ]
PNNT3 <- PNNT2[!PNNT2$Day=='24', ]


ggplot(PNNT3, aes(x = Day, y = Gray_Value_Mean)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  stat_summary(geom = "line", fun = "mean") +
  stat_summary(fun.data = mean_se) + #geom = "errorbar") +
  geom_smooth(method = lm, se=T, color = "red") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y= Inf, label.x = Inf, vjust = 3, hjust = 4) +
  labs( x = "Postnatal Day", y = "Mean Pixel Intensity", title = "PNN Expression Overtime") + 
  theme(plot.title = element_text(hjust = .5)) +
  scale_x_continuous(breaks = c(15:24),
                     labels = c(15:24))

#####Group PNN Expression ~ Reversal Performance

PNN <- select(dfPNN2, "Mouse", "Group", "IntensityMean_Cy5", "Sex")
PNN1 <- filter(PNN, Group == "Untreated", "CNO", "Vector", "Gq")
Reversal <- select(Untreated_Reversal, "Mouse", "Group", "Day", "Performance")
Reversal1 <- Filter(Reversal, Group == "Untreated", "CNO", "Vector", "Gq")
PNNReversal1 <- merge(Social, dfPNN7)
PNNSocial <- select(PNNReversal1, "Mouse", "Group", "Group2", "Sex", "Social_Preference_Index...12", "ave")
PNNSocial1 <- drop_na(PNNSocial)


View(PNNReversal)

  ggscatter(Social, x = "ave", y = "logitIL", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson") +
  geom_smooth() +
  theme_bw(base_size = 12, base_family = "serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs( x = "Mean Pixel Intensity", y = "Reversal Percentage", title = "Correlate PNN Expresson ~ Reversal Performance") +
    #scale_color_discrete(limits = c("Untreated", "CNO", "Vector", "Gq")) +
    theme(plot.title = element_text(hjust = .5)) +
    xlim(0:1) +
  scale_y_continuous(limits = c(0:5,000))

  x = "Social_Preference_Index...12", y = "IntensityMean_Cy5",
####GQReversalw/PNNExpression
Gqplot <- ggscatter(PNNSocial1, x = "Social_Preference_Index...12", y = "ave",
     add = "reg.line", add.params = list(color = "red", fill = "lightgray"), 
     conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", label.x = "0.00") +
     ylim(1000,5000) +
    xlim(0.25, 1)+
     theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
     labs( x = "Social Preference Index", y = "PNN Intensity", title = "Negative Correlation Between PNN Expresson and Reversal Performance") +
     theme(plot.title = element_text(hjust = .5)) +
    facet_grid(.~Sex)
  

Gqplot +stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = .2)  


==#####JustGq
GqPNN <- dfPNN2 %>%
  select(Mouse, Sex, Group, IntensityMean_Cy5)
  
GqPNN <- select(dfPNN7, "Sex", "Mouse", "Group", "IntensityMean_Cy5")
GiReversal <- select(Social, "Sex", "Mouse", "Group2", "Social_Preference_Index...12")
GiReversal2 <- filter(dfR3.1, Day == "10")
GiReversal3 <- GiR2[!GiR2$Group=='Vector', ]
GiReversal4 <- GiReversal3[!GiReversal3$Group=='CNO', ]
GiReversal5 <- GiReversal4[!GiReversal4$Group=='Gq', ]
GiR <- merge(cumsumplot1.2, cumsumplot1.5) %>%
  distinct()



GiR2 <- merge(dfPNN2, SocialMale)



Initiation_Latency <- Untreated_Reversal[c(1:189), c(9)]
Group <- PNN_data[c(1:189), c(4)]
Stage <- Untreated_Reversal[c(1:189), c(5)]
Response_Latency <- Untreated_Reversal[c(1:189), c(10)]
Trials <- Untreated_Reversal[c(1:189), c(8)]
Performance <- Untreated_Reversal[c(1:189), c(7)]

dfR1 <- data.frame(Sex, Initiation_Latency, Group, Day, Stage, Response_Latency, Trials, Performance)
dfR2 <- dfR1[!dfR1$Sex=='Female', ]    



ggplot(GiR2, aes(x = Group2, y = IntensityMean_Cy5, color = Groups)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  geom_boxplot() + 
  stat_compare_means(method = "anova", label.y = 5200) +
  stat_pvalue_manual(stat.test.Gq, label = "p.adj.signif", y.position = 4850) +
  stat_pvalue_manual(stat.test.Gi, label = "p.adj.signif", y.position = 4600) +
  stat_pvalue_manual(stat.test.RGi, label = "p.adj.signif", y.position = 4200) +
  #stat_compare_means(method = "t.test", ref.group = "Control", label = "p.signif", label.y = 4750) +
  geom_beeswarm() +
  scale_color_manual(values=c("Control" = "black", "Gq" = "forest green", "Gi" = "red")) +
  labs(y = "Mean Pixel Intensity", title = "Female DCN PNN Intensity") + 
  theme(plot.title = element_text(hjust = .5)) 


stat_pvalue_manual(stat.test.Gq, label = "p.adj.signif", y.position = 4850)


model <- lm(ave ~ 0 + Group2 + Sex, data = dfPNN7)
summary(model)  
