
bin1 <- cumsumplot[!cumsumplot$Day=='1', ]
cumsumplot1.2 <- cumsumplot1.1[!cumsumplot1.1$Day=='2', ]
cumsumplot1.3 <- cumsumplot1.2[!cumsumplot1.2$Day=='4', ]
cumsumplot1.4 <- cumsumplot1.3[!cumsumplot1.3$Day=='5', ]
cumsumplot1.5 <- cumsumplot1.4[!cumsumplot1.4$Day=='5', ]
cumsumplot1.6 <- cumsumplot1.5[!cumsumplot1.5$Day=='6', ]
cumsumplot1.7 <- cumsumplot1.6[!cumsumplot1.6$Day=='7', ]
cumsumplot1.8 <- cumsumplot1.7[!cumsumplot1.7$Day=='8', ]
cumsumplot1.9 <- cumsumplot1.8[!cumsumplot1.8$Day=='9', ]

GqPNN <- select(dfPNN7, "Sex", "Mouse", "Group", "IntensityMean_Cy5")
GiReversal <- select(Social, "Sex", "Mouse", "Group2", "Social_Preference_Index...12")
GiReversal2 <- filter(dfR3.1, Day == "10")
GiReversal3 <- GiReversal2[!GiReversal2$Group=='Untreated', ]
GiReversal4 <- GiReversal3[!GiReversal3$Group=='CNO', ]
GiReversal5 <- GiReversal4[!GiReversal4$Group=='Gq', ]
GiR <- merge(pbdf.2, rlogits) %>%
  distinct()

dfVD4 <- dfVD3 %>%
  group_by(Mouse) %>%
  mutate(ave = mean(logitIL))

dfVD5 <- dfVD4 %>%
  group_by(Mouse)%>%
  mutate(sderror = std.error(logitIL))

#####Graphing_Boxplot_LearningbyStage_BarGraph
ggplot(cumsumplot1.9, aes(x = Sexs, y = cumulative_sum, group = Sexs, fill = Sexs)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  stat_summary(geom = "bar", fun = "mean", position = position_dodge()) +
  #geom_beeswarm(color = "dark grey") +  
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
  geom = "errorbar", color = "black", position = position_dodge(), width= .1) +
  stat_compare_means(method = "anova", label.y = 3, dodge.width = .85) +
  geom_jitter(color = "dark grey", position = position_jitterdodge(jitter.width = 0., dodge.width = .75), size = 1.4) +
  #stat_compare_means(label = "p.format", method = "t.test", ref.group = "Untreated") +
  #geom_jitter(position = position_jitterdodge(jitter.width = 0., dodge.width = .75), size = 1.4) +
  #scale_color_discrete(limits = c("Untreated", "CNO", "Gq", "Gi")) +
  #scale_fill_manual(values=c("Untreated" = "black", "CNO" = "Grey", "Gq" = "forest green", "Gi" = "magenta")) +
  scale_color_discrete(limits = c("Male", "Female")) +
  #scale_fill_manual(values = c('#000000', '#ED1C24')) +
  scale_color_manual(values = c("Male" = "black", "Female" = "red")) +
  theme(plot.title = element_text(hjust = .5)) +
  #scale_x_discrete(limits = c("Untreated", "CNO", "Gq", "Gi")) +
  #scale_x_discrete(limits = c("Early Learning", "Mid Learning", "Late Learning")) +
  labs( x = "Groups", y = "Total Trials", title = "Male Cumulative Trials by Group - VD") +
  theme(axis.title.x = element_blank()) +
  ylim(0, 3) +
  facet_grid(.~Group) +
  facet_grid

Days <- factor(dfVD2$Day2, levels = c("Early Learning", "Mid Learning", "Late Learning"))
Groups <- factor(dfVD3$Group, levels = c('Untreated', 'CNO', 'Gq', 'Gi'))
Sexs <- factor(cumsumplot1.2$Sex, levels = c('Male', 'Female'))

model12 <- aov(logitIL ~ Group, data=pbdf.2)
summary(model12)

#####Graphing_Boxplot_LearningbyStage_Boxplot

ggplot(cumsumplot, aes(x = Day2, y = cumulative_sum, group = Groups, color = Groups)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  #stat_summary(geom = "line", fun = "mean") + 
  stat_summary(fun.data = mean_se) + #geom = "errorbar")+
  geom_boxplot(position = "dodge2") +
  stat_compare_means(method = "anova", label.y =  100, dodge.width = .85) +
  #stat_compare_means(label = "p.format", method = "t.test", ref.group = "Control") +
  geom_jitter(position = position_jitterdodge(jitter.width = 0., dodge.width = .75), size = 1.4) +
  #scale_color_discrete(limits = c("Male", "Female"))+
  scale_color_discrete(limits = c("Untreated", "CNO", "Gq", "Gi")) +
  #scale_color_manual(values = c("Male" = "black", "Female" = "red")) +
  scale_color_manual(values=c("Untreated" = "black", "CNO" = "Grey", "Gq" = "forest green", "Gi" = "magenta")) +
  theme(plot.title = element_text(hjust = .5)) +
  #geom_hline(yintercept = 70, linetype = 'dotted', col = 'black',) +
  #scale_x_discrete(limits = c("Control", "Gq", "Gi")) +
  #scale_x_discrete(limits = c("Early Learning", "Mid Learning", "Late Learning")) +
  labs( x = "Learning Stage", y = "Correct Choice (%)", title = "Reversal Group Performance Binned Learning - Females") +
  theme(axis.title.x = element_blank()) +
  ylim(0, 500) +
  facet_grid(.~Day2) +
  facet_grid(.~Sex)
  
  facet_grid(~factor(Group, levels = c('Untreated', 'CNO', 'Gq', 'Gi'))) +
  facet_grid(~factor(Sex, levels = c('Male', 'Female')))

Groups <- factor(cumsumplot$Group, levels = c("Untreated", "CNO", "Gq", "Gi"))
Days <- factor(PBdf3.1$Day2, levels = c("Early Learning", "Mid Learning", "Late Learning"))
  
#####VD_Performance_Boxplot_BinbyDays##########
  
PBdf <- dfR3.1 %>%
    mutate(Day2 = case_when(Day >= 0 & Day <= 3 ~ "Early Learning", 
                             Day >= 4 & Day <= 7 ~ "Mid Learning", 
                             Day >= 8 & Day <= 10 ~ "Late Learning"))  

pbdf.1 <- PBdf[!PBdf$Day2=='Late Learning', ]
pbdf.2 <- pbdf.1[!pbdf.1$Day2=='Early Learning', ]

                   
PBdf.1 <- factor(PBdf$Day2, levels = c("Early Learning", "Mid Learning", "Late Learning"))
  
PBdf1 <- PBdf %>%
  group_by(Mouse, Sex, Day2, Group) %>%
    mutate(Perf = mean(Performance))


PBdf2 <- PBdf1 %>% distinct(Perf, Day2, Sex, Group, .keep_all = TRUE)
              
PBdf3 <- PBdf2 %>%
  select(Mouse, Day2, Sex, Group, Perf)
PBdf3.1 <- drop_na(PBdf3)

#####Pairwise comparisons between time points
pwc1 <- PBdf3.1[!PBdf3.1$Sex=='Female', ]

pwc2 <- pwc1%>%
  group_by(Group)%>%
  pairwise_t_test(Perf ~ Day2, paired = TRUE, p.adjust.method = "bonferroni")
data.frame(pwc2)


####Effect of time at each level of treatment
one.way2 <- PBdf3%>%
  group_by(Group)%>%
  anova_test(dv = Perf, wid = Mouse, within = Day2)%>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
data.frame(one.way2)

###VDPost-Hoc_Tukey-CNOvsUntreated
PBdf3.3 <- PBdf3.1[!PBdf3.1$Group=='Gi', ]
PBdf3.4 <- PBdf3.3[!PBdf3.3$Group=='Gq', ]
PBdf3.5 <- PBdf3.4[!PBdf3.4$Day2=='Late Learning', ]
PBdf3.6 <- PBdf3.5[!PBdf3.5$Day2=='Mid Learning', ]
stat.test.VD <- aov(Perf ~ Group, data = PBdf3.6) %>%
  tukey_hsd()

###VDPost-Hoc_Tukey-ControlvsGq
PBdf3.3 <- PBdf3[!PBdf3$Group2=='Gi', ]
PBdf3.4 <- PBdf3.3[!PBdf3.3$Day2=='Early Learning', ]
PBdf3.5 <- PBdf3.4[!PBdf3.4$Day2=='Mid Learning', ]
stat.test.VD <- aov(Perf ~ Group2, data = PBdf3.5) %>%
  tukey_hsd()

###VDPost-Hoc_Tukey-ControlvsGi
PBdf3.3 <- PBdf3[!PBdf3$Group2=='Gq', ]
PBdf3.4 <- PBdf3.3[!PBdf3.3$Day2=='Mid Learning', ]
PBdf3.5 <- PBdf3.4[!PBdf3.4$Day2=='Early Learning', ]
stat.test.VD <- aov(Perf ~ Group2, data = PBdf3.5) %>%
  tukey_hsd()


###VDPost-Hoc_Tukey-GqvsGi
PBdf3.3 <- PBdf3[!PBdf3$Group2=='Control', ]
PBdf3.4 <- PBdf3.3[!PBdf3.3$Day2=='Early Learning', ]
PBdf3.5 <- PBdf3.4[!PBdf3.4$Day2=='Mid Learning', ]
stat.test.VD <- aov(Perf ~ Group2, data = PBdf3.5) %>%
  tukey_hsd()

###VDPost-Hoc_Tukey-GqvsGi - Males&Females
PBdf <- Male.group.df %>%
  mutate(Day2 = case_when(Day >= 1 & Day <= 3 ~ "Early Learning", 
                          Day >= 4 & Day <= 7 ~ "Mid Learning", 
                          Day >= 8 & Day <= 10 ~ "Late Learning")) 
#######Descriptive_Statistics

psych::describeBy(PBdf3.5, PBdf3.5$Group2, PBdf3.5$Perf)



#####Reversal_Performance_Boxplot_BinbyDays##########

PBdfr <- Male.group.df %>%
  mutate(Day2 = case_when(Day >= 1 & Day <= 3 ~ "Early Learning", 
                          Day >= 4 & Day <= 7 ~ "Mid Learning", 
                          Day >= 8 & Day <= 10 ~ "Late Learning")) 

PBdfr1 <- PBdfr %>%
  group_by(Mouse,Day2, Sex, Group, Group2) %>%
  mutate(Perf = mean(Performance))

PBdfr2 <- PBdfr1 %>% distinct(Perf, Day2, Sex, Group, Group2, .keep_all = TRUE)

PBdfr3 <- PBdfr2 %>%
  select(Mouse, Day2, Sex, Group, Group2, Perf)

PBdfr3.1 <- drop_na(PBdfr3)

###ReversalPost-Hoc_Tukey-CNOvsUntreated
PBdfr3.3 <- PBdfr3.1[!PBdfr3.1$Group=='Gi', ]
PBdfr3.4 <- PBdf3.3[!PBdfr3.3$Group=='Gq', ]
PBdfr3.5 <- PBdfr3.4[!PBdfr3.4$Day2=='Mid Learning', ]
PBdfr3.6 <- PBdfr3.5[!PBdfr3.5$Day2=='Early Learning', ]
stat.test.VD <- aov(Perf ~ Group, data = PBdf3.6) %>%
  tukey_hsd()

###ReversalPost-Hoc_Tukey-ControlvsGq
PBdfr3.3 <- PBdfr3.1[!PBdfr3.1$Group2=='Control', ]
PBdfr3.4 <- PBdfr3.3[!PBdfr3.3$Day2=='Mid Learning', ]
PBdfr3.5 <- PBdfr3.4[!PBdfr3.4$Day2=='Late Learning', ]
stat.test.VD <- aov(Perf ~ Group2, data = PBdfr3.5) %>%
  tukey_hsd()

###ReversalPost-Hoc_Tukey-ControlvsGi
PBdfr3.3 <- PBdfr3[!PBdfr3$Group2=='Gq', ]
PBdfr3.4 <- PBdfr3.3[!PBdfr3.3$Day2=='Early Learning', ]
PBdfr3.5 <- PBdfr3.4[!PBdfr3.4$Day2=='Mid Learning', ]
stat.test.VD <- aov(Perf ~ Group2, data = PBdfr3.5) %>%
  tukey_hsd()

###ReversalPost-Hoc_Tukey-GqvsGi
PBdfr3.3 <- PBdfr3[!PBdfr3$Group2=='Control', ]
PBdfr3.4 <- PBdfr3.3[!PBdfr3.3$Day2=='Late Learning', ]
PBdfr3.5 <- PBdfr3.4[!PBdfr3.4$Day2=='Ealy Learning', ]
stat.test.VD <- aov(Perf ~ Group2, data = PBdfr3.5) %>%
  tukey_hsd()

###ReversalPost-Hoc_Tukey-GqvsGi - Males&Females

PBdf <- dfR3.1 %>%
  mutate(Day2 = case_when(Day >= 1 & Day <= 3 ~ "Early Learning", 
                          Day >= 4 & Day <= 7 ~ "Mid Learning", 
                          Day >= 8 & Day <= 10 ~ "Late Learning")) 

PBdfr1 <- PBdfr %>%
  group_by(Mouse,Day2, Sex, Group, Group2) %>%
  mutate(Perf = mean(Performance))

PBdfr2 <- PBdfr1 %>% distinct(Perf, Day2, Sex, Group, Group2, .keep_all = TRUE)

PBdfr3 <- PBdfr2 %>%
  select(Mouse, Day2, Sex, Group, Group2, Perf)

PBdfr3.1 <- drop_na(PBdfr3)

PBdfr3.3 <- PBdfr3[!pbdf.2$Group2=='Control', ]
PBdfr3.4 <- PBdfr3.3[!PBdfr3.3$Day2=='Late Learning', ]
PBdfr3.5 <- PBdfr3.4[!PBdfr3.4$Day2=='Early Learning', ]
stat.test.VD <- aov(Perf ~ Group2, data = PBdfr3.5) %>%
  tukey_hsd()


######Repeated_Measures_ANOVA#######

modelV1 <- aov(formula = Perf ~ Day2 + Error(Group/Day2), data = PBdf3.1)
summary(modelV1)
           
mdl <- aov(Perf ~ factor(Group) + Error(factor(Mouse)), data = PBdf3.1)
summary(mdl)

res.aov <- anova_test(data= PBdf3.1, dv = Perf, wid = Group, within = Day2)
