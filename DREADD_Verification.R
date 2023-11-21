######Reading in the Data############
DREADD_cFos <- read_excel ("C:/Users/ttlyle/Desktop/Untreated_Scores.xlsx", 
                           sheet = "C-Fos")
DREADD_cFos.9 <-DREADD_cFos[c(1:33),c(1:15)]

Mouse <- DREADD_cFos.9 [c(1:33), c(1)]
Sample <- DREADD_cFos.9 [c(1:33), c(2)]
Side <- DREADD_cFos.9[c(1:33), c(3)]
Group <- DREADD_cFos.9[c(1:33), c(4)]
Sex <- DREADD_cFos.9[c(1:33), c(5)]
Normalized_CellCount<- DREADD_cFos.9[c(1:33), c(6)]
cFos_Count<- DREADD_cFos.9[c(1:33), c(7)]
dfR1 <- data.frame(Mouse, Sex, Group, Sample, Side, Normalized_CellCount, cFos_Count)
DREADD_cFos1 <- drop_na(dfR1)
DREADD_cFos2 <-DREADD_cFos1.1[!DREADD_cFos1.1$Sex=='Female', ]

####Averaging Sample per Mouse####
DREADD_cFos3 <- DREADD_cFos1 %>%
  group_by(Group) %>%
  mutate(ave = mean(cFos_Count))

####Selecting Sample from average####
DREADD_cFos4 <- DREADD_cFos3[DREADD_cFos3$Sample>0 & DREADD_cFos3$Sample<3, ]

Groups <- factor(DREADD_cFos1$Group, levels = c('Untreated', 'ChABC'))
Sexs <- factor(DREADD_cFos1$Sex, levels = c('Male', 'Female'))

#########BoxPlotGraph#########


ggplot(DREADD_cFos1, aes(x = Groups, y = cFos_Count, group = Groups, color = Groups)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  geom_boxplot() + 
  #stat_compare_means(method = "anova", label.y = 250) +
  stat_compare_means(method = "t.test", ref.group = "Untreated", label = "p.signif", label.y = 350) +
  #stat_pvalue_manual(test1, label = "p.adj.signif", y.position = 225) +
  geom_beeswarm() +
  #geom_point() +
  #scale_color_manual(values = c("Male" = "black", "Female" = "red"))+
  #scale_color_discrete(limits = c("Male", "Female")) +
  scale_color_manual(values=c("Untreated" = "Black", "ChABC" = "dark grey")) +
  labs(y = "Normalized_CellCount", title = "Lateral CN cFos following social behavior") + 
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0, 450) +
  facet_grid(.~Sex)

test1<- aov(cFos_Count ~ Group, data = DREADD_cFos1) %>%
  tukey_hsd()
summary(test1)

test2 <- aov(ave ~ Sex, data = DREADD_cFos3) %>%
  tukey_hsd()
summary(test2)

##########################DREADD_Expression_Analysis##########################
DREADD_ex <- read_excel ("C:/Users/ttlyle/Desktop/Untreated_Scores.xlsx", 
                           sheet = "DREADD_Expression")
DREADD_ex1 <-DREADD_ex[c(1:117),c(1:13)]

DREADD_ex2 <-DREADD_ex1[!DREADD_ex1$Group=='Gq', ]
#####Graphing#####

ggplot(DREADD_cFos1, aes(x = Groups, y = Normalized_CellCount, group = Groups, fill = Groups)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  stat_summary(geom = "bar", stat = 'identity', fun = "mean", position = position_dodge()) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
  geom = "errorbar", color = "black", position = position_dodge(), width= .1) +
  stat_compare_means(method = "anova", label.y = 400, dodge.width = .85) +
  #stat_pvalue_manual(test1, label = "p.adj.signif", y.position = 250) +
  geom_jitter(color = "dark grey", position = position_jitterdodge(jitter.width = 0., dodge.width = .75), size = 1.4) +
  scale_color_discrete(limits = c("Untreated", "ChABC")) +
  scale_color_manual(values=c("Untreated" = "black", "ChABC" = "light grey")) +
  #stat_compare_means(label = "p.format", method = "t.test", ref.group = "Background") +
  #scale_color_discrete(limits = c("Background", "Dentate", "Interposed", "Fastigial")) +
  #scale_fill_manual(values=c("Background" = "black", "Dentate" = "Black", "Interposed" = "Black", "Fastigial" = "Black")) +
  theme(plot.title = element_text(hjust = .5)) +
  #scale_x_discrete(limits = c("Untreated", "CNO", "Gq", "Gi")) +
  #scale_x_discrete(limits = c("Early Learning", "Mid Learning", "Late Learning")) +
  labs( x = "Cerebellar Nuclei Regions", y = "cFos_Count", title = "CN DREADD Verification - cFos") +
  theme(axis.title.x = element_blank()) +
  ylim(0, 500) +
  facet_grid(.~Side)
 + 
Groups <- factor(dfVD3$Group, levels = c('Untreated', 'CNO', 'Gq', 'Gi'))
Regions <- factor(DREADD_ex1$Region, levels = c('Background', 'Dentate', 'Interposed', 'Fastigial'))


test1<- aov(Normalization ~ Regions, data = DREADD_ex1) %>%
  tukey_hsd()
summary(test1)

t.test(Normalized_CellCount ~ Group, data = DREADD_cFos1)




