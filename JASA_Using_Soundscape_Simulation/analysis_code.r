# Using Soundscape Simulation to Evaluate Compositions for a 
# Public Space Sound Installation
# 
# Valérian Fraisse
# 
# The present code is the analysis code used on the listening tests reported 
# in the journal article 'Using Soundscape Simulation to Evaluate Compositions for
# a Public Space Sound Installation' by Fraisse V., Schütz N., Wanderley M., 
# Guastavino C., and Misdariis N., under publication to the Journal of the 
# Acoustical Society of America. 
# 
# The code is not functional per se as participants' data from the listening tests
# can't be shared online due to the nature of consent obtained
# from participants as per the ethics approval obtained from the McGill Research
# Ethics Board and the Sorbonne University Research Ethics Commitee. 
# 
# Code sections are ordered according to the paper's sections they are 
# refering to.

#### Initialization ####

# Setting working directories
setwd('XXX')

# Libraries
library(ggplot2)
library(car)
library(ggthemes)
library(forcats)
library(rstatix)
library(chron)
library(viridis)
library(tidyr)
library(reshape2)
library(plotrix)
library(ez)
library(ltm)
library(psych)
library(plyr)
library(corpcor)
library(GPArotation)
library(dplyr)
library(MANOVA.RM)
library(extrafont) 
library(plot3D)
library(cccrm)
library(mvnormtest)
library(mvoutlier)
library(lavaan)
library(corrplot)
library(ape)
library(lme4)
library(scatterplot3d)
library(FactoClass)
library(clValid)
library(dendextend)
library(pvclust)
library(rmcorr)
font_import()
loadfonts(device="win")

# List used for plotting data


plotTheme <- list(
  theme_gdocs(),
  theme(legend.position = 'none',
          text=element_text(family="Helvetica"),
          panel.grid.major.x = element_blank(),
          axis.title.y = element_text(size=14, colour="black"),
          axis.line = element_blank(),
          axis.text.x = element_text(size=14, angle=90, colour="black"),
          axis.text.y = element_text(size=14, colour="black")
    ),
scale_y_continuous(breaks=c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                   labels=c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                   limits = c(0, 100)),
  scale_color_manual(
    "legend",
    values = c("Resonated" = "#117AD7",
               "Referential" = "#00B707",
               "Synthesized" = "#C460E7",
               "Baseline" = "#888888",
               "Ref/Syn Fluctuating" = "#F583FF",
               "Ref/Syn Steady" = "#D9BAE8",
               "Ref/Res Fluctuating" = "#70ebd7"))
)

scalesPlotTheme = list(
  scale_x_discrete(limits=c("sc_agreable", "sc_apaisante", "sc_niveau", 
                            "sc_caractere", "sc_appropriee", "sc_habituelle",
                            "sc_coherente",
                            "sc_variee", "sc_emergence"),
                   labels=c("Pleasant", "Soothing", "Sound Level",
                            "Character", "Appropriate", "Familiar", "Coherent",
                            "Varied", "Emergence"),
  )
)

# Reading and parsing participants data
scales.data <- read.csv(
  "scales.csv", 
  header=T, 
  sep=",", 
  na.strings=c("NA", "Baseline", "O")
  )

# Reading and parsing acoustic measurements data
measurements <- read.csv(
  "acoustic_measurements_excerpts.csv", 
  header=T, 
  sep=",", 
  na.strings=c("NA", "N/A", "O")
  )

# Creating variables associated with the stimuli and stimuli groups nomenclature
scales.data %>% 
  mutate( 
    sound=case_when(
      exc_number == 1 | exc_number == 2 ~ "Baseline",
      exc_number == 3 ~ "O1",
      exc_number == 4 ~ "O1R4F",
      exc_number == 5 ~ "R4",
      exc_number == 6 ~ "O1S2F",
      exc_number == 7 ~ "O1S2S",
      exc_number == 8 ~ "O2",
      exc_number == 9 ~ "O2R4F",
      exc_number == 10 ~ "O2S1F",
      exc_number == 11 ~ "O2S1S",
      exc_number == 12 ~ "O3",
      exc_number == 13 ~ "O3R3F",
      exc_number == 14 ~ "R3",
      exc_number == 15 ~ "O3S1F",
      exc_number == 16 ~ "O3S1S",
      exc_number == 17 ~ "S1",
      exc_number == 18 ~ "O4",
      exc_number == 19 ~ "O4R1F",
      exc_number == 20 ~ "R1",
      exc_number == 21 ~ "O4S2F",
      exc_number == 22 ~ "O4S2S",
      exc_number == 23 ~ "S2",
      exc_number == 24 ~ "O5",
      exc_number == 25 ~ "O5R2F",
      exc_number == 26 ~ "R2",
      exc_number == 27 ~ "O5S2F",
      exc_number == 28 ~ "O5S2S"),
    type=case_when(exc_number %in% c(1,2) ~ "Baseline",
                   exc_number %in% c(3,8,12,18,24) ~ "Referential",
                   exc_number %in% c(5,14,20,26) ~ "Resonated",
                   exc_number %in% c(17,23) ~ "Synthesized",
                   exc_number %in% c(4,9,13,19,25) ~ "Ref/Res Fluctuating",
                   exc_number %in% c(6, 10, 15, 21, 27) ~ "Ref/Syn Fluctuating",
                   exc_number %in% c(7, 11, 16, 22, 28) ~ "Ref/Syn Steady"
                   )) -> scales.data

measurements %>% 
  mutate(
    type=case_when(sound %in% c("N/A") ~ "Baseline",
                   sound %in% c("O1", "O2", "O3", "O4", "O5") ~ "Referential",
                   sound %in% c("R1", "R2", "R3", "R4") ~ "Resonated",
                   sound %in% c("O1R4F", "O2R4F", "O3R3F", "O4R1F", "O5R2F") ~ "Ref/Res Fluctuating",
                   sound %in% c("S1", "S2") ~ "Synthesized",
                   sound %in% c("O1S2F", "O2S1F", "O3S1F", "O4S2F", "O5S2F") ~ "Ref/Syn Fluctuating",
                   sound %in% c("O1S2S", "O2S1S", "O3S1S", "O4S2S", "O5S2S") ~ "Ref/Syn Steady")) -> measurements

scales.data %>% # going from 0 to 99 to 1 to 100
  transform(sc_agreable = sc_agreable +1,
            sc_appropriee = sc_appropriee +1,
            sc_apaisante = sc_apaisante +1,
            sc_variee = sc_variee + 1,
            sc_coherente = sc_coherente + 1,
            sc_caractere = sc_caractere + 1,
            sc_habituelle = sc_habituelle + 1,
            sc_niveau = sc_niveau + 1,
            sc_emergence = sc_emergence + 1) -> scales.data

# Collapsing data from conditions that appear twice by participant
scales.data %>% 
  select(participant, sc_agreable, sc_appropriee, sc_variee, sc_apaisante, 
         sc_coherente, sc_caractere, sc_habituelle, sc_niveau, sc_emergence,
         sound, type, reason, age, familiarity) %>% 
  melt(id.vars=c(
    "type", "sound", "age", "familiarity", "reason", "participant"
    )) %>% 
  ddply(cbind(
    "type", "sound", "age", "familiarity", "reason", "participant", "variable"
    ),
    summarize, mean = mean(value)) %>%
  dcast(
    formula = participant+reason+age+familiarity+sound+type~variable, 
    fun.aggregate=sum, 
    value.var="mean"
    ) -> collapsed.data

# Convert participant data to factor
scales.data$participant <- factor(scales.data$participant, 
                               levels = c("P1", "P2", "P3", "P4", "P5", "P6",
                                          "P7", "P8", "P9", "P10", "P11", "P12",
                                          "P13", "P14", "P15", "P16", "P17",
                                          "P18", "P19", "P20"))


#### II.B: Added sounds ####

# Table I - Mean and SD of acoustic measurements, by composition type
measurements %>% 
  select(type, LAeq1min30s, LAFmax, LCpeakmax) %>% 
  melt("type") %>% 
  ddply(c("type", "variable"), summarize, 
        mean = mean(value), sd = sd(value))


#### II.D: Statistical Assumptions ####

# Scales: Univariate normality

for (col in 3:11) {
  print(colnames(scales.data[col]))
  temp <- as.numeric(unlist(scales.data[col]))
  print(shapiro.test(temp))
}

# Scales: Multivariate normality

scales.data %>%
  select(sc_agreable:sc_emergence) %>% 
  t() %>% 
  mshapiro.test()

# PCA: KMO

collapsed.data %>% 
  select(sc_agreable:sc_emergence) %>% 
  cor(use="complete.obs", method="pearson") -> corrMatrix

KMO(corrMatrix)

#### III.A: Baseline evaluation and methodological validation ####

# Baseline median and IQR, by scale
collapsed.data %>% 
  filter(type == "Baseline") %>% 
  select(sc_agreable:sc_emergence) %>%
  melt() %>% 
  ddply(
    cbind('variable'), 
    summarise, 
    IQR = IQR(value),
    median = median(value)
    ) 

# Test-retest reliability
# Added sounds
scales.data[order(scales.data$participant, scales.data$exc_number),] %>% 
  filter(exc_number %in% c(6, 9, 12, 16, 23, 26)) %>% 
  mutate(ind = rep(c(1, 2), length.out = n())) -> testretest_added.data

testretest_added.data %>% 
  filter(ind==1) -> testretest_added1.data

testretest_added.data %>% 
  filter(ind==2) -> testretest_added2.data

testretest_added_results <- vector( "numeric" , 9)

for (col in 1:9) {
  testretest_added_results[col] <- cor.test(
    as.numeric(unlist(testretest_added1.data[col+2])), 
    as.numeric(unlist(testretest_added2.data[col+2])), 
    method='pearson'
  )$estimate
}

mean(testretest_added_results)

# Baseline
scales.data[order(scales.data$participant, scales.data$exc_number),] %>% 
  filter(exc_number %in% c(1, 2)) %>% 
  mutate(ind = rep(c(1, 2),length.out = n())) -> testretest_baseline.data

testretest_baseline.data %>% 
  filter(ind==1) -> testretest_baseline1.data

testretest_baseline.data %>% 
  filter(ind==2) -> testretest_baseline2.data

testretest_baseline_results <- vector( "numeric" , 9)

for (col in 1:9) {
  testretest_baseline_results[col] <- cor.test(
    as.numeric(unlist(testretest_baseline1.data[col+2])), 
    as.numeric(unlist(testretest_baseline2.data[col+2])), 
    method='pearson'
  )$estimate
}

mean(testretest_baseline_results)

#### III.B: Principal Components Analysis ####
# Initial PCA to determine number of factors to keep
collapsed.data %>% 
  select(sc_agreable:sc_emergence) %>% 
  principal(nfactors = 9, rotate = "none") -> pc1

# Screeplot (three factors have been retained)
pc1$values %>% 
  as.data.frame() %>% 
  ggplot(
    aes(
      x=c(1:9),
      y=.
    )
  ) +
  geom_point(size=4) +
  geom_line() +
  theme_gdocs() +
  ylab('Eigenvalue') +
  xlab('Component') 

# Final PCA with three components according to first PCA
collapsed.data %>% 
  select(sc_agreable:sc_emergence) %>% 
  principal(nfactors = 3, rotate = "oblimin", scores=T) -> pc2

print.psych(pc2, cut = 0, sort = TRUE)

# Cronbach's alpha for each component of the final PCA
collapsed.data %>% 
  select(sc_caractere, sc_appropriee, sc_habituelle, sc_coherente) %>% 
  alpha(check.keys = T)
collapsed.data %>% 
  select(sc_agreable, sc_apaisante, sc_niveau) %>% 
  alpha(check.keys = T)
collapsed.data %>% 
  select(sc_variee, sc_emergence) %>% 
  alpha(check.keys = T)

# Creating new variables from PCA component loadings
collapsed.data$PLEASANT <- vector(mode="numeric", length=540)
collapsed.data$FAMILIAR <- vector(mode="numeric", length=540)
collapsed.data$VARIED <- vector(mode="numeric", length=540)

for (n in c(1:9)) {
  collapsed.data$PLEASANT = collapsed.data$PLEASANT + (pc2$loadings[n, 1] * collapsed.data[, n+6])
  collapsed.data$FAMILIAR = collapsed.data$FAMILIAR + (pc2$loadings[n, 2] * collapsed.data[, n+6])
  collapsed.data$VARIED = collapsed.data$VARIED + (pc2$loadings[n, 3] * collapsed.data[, n+6])
}

collapsed.data$PLEASANT = collapsed.data$PLEASANT / max(collapsed.data$PLEASANT) * 100
collapsed.data$FAMILIAR = collapsed.data$FAMILIAR / max(collapsed.data$FAMILIAR) * 100
collapsed.data$VARIED = collapsed.data$VARIED / max(collapsed.data$VARIED) * 100

# Collapsing data across composition type
collapsed.data %>% 
  select(PLEASANT, FAMILIAR, VARIED, type, participant) %>% 
  melt(id.vars=c('participant', 'type')) %>% 
  ddply(cbind('participant', 'type', 'variable'), summarise, mean = mean(value, na.rm=T)) %>% 
  dcast(formula = participant+type~variable, value.var='mean') -> collapsed.data.type


# Table X - Median and IQR, by component
collapsed.data.type %>% 
  select(PLEASANT, VARIED, FAMILIAR, type) %>%
  melt("type") %>% 
  ddply(
    cbind("type", 'variable'), 
    summarise, 
    median = median(value),
    IQR = IQR(value)
  ) 

# TABLE VII Repeated measures correlations between acoustic measurements and components
data.measurements <- merge(collapsed.data, measurements, by = "sound")

rmcorr(participant, PLEASANT, LAeq1min30s, data.measurements)
rmcorr(participant, FAMILIAR, LAeq1min30s, data.measurements)
rmcorr(participant, VARIED, LAeq1min30s, data.measurements)

rmcorr(participant, PLEASANT, LAFmax, data.measurements)
rmcorr(participant, FAMILIAR, LAFmax, data.measurements)
rmcorr(participant, VARIED, LAFmax, data.measurements)

p.adjust(c(7.18e-06, 7.05e-05, 1.337e-06, 0.00033, 0.0607, 1.704e-11), method="holm")

#### III.C RM MANOVA: Abstract and referential composition strategies ####
# Filtering Baseline, referential, resonated and synthesized compositions
collapsed.data.type.1 = filter(
  collapsed.data.type, 
  type %in% c("Baseline", "Referential", "Synthesized", "Resonated")
)

# 0 - Assumption of Homogeneity of variance
leveneTest(PLEASANT ~ type, data = collapsed.data.type.1)
leveneTest(FAMILIAR ~ type, data = collapsed.data.type.1)
leveneTest(VARIED ~ type, data = collapsed.data.type.1)

# 1 - Assumption of multivariate Normality (rejected)
collapsed.data.type.1 %>% 
  select(PLEASANT, FAMILIAR, VARIED) %>%
  t() %>% 
  mshapiro.test()

# 2 - RM MANOVA
collapsed.data.type.1 %>% 
  multRM(formula = cbind(PLEASANT, FAMILIAR, VARIED) ~ type, 
         subject = "participant", 
         within = "type",
         iter = 10000, 
         resampling = "WildBS", seed = 1234) %>% 
  summary()
  

# 3 - Follow-up semi-parametric RM Anova
collapsed.data.type.1 %>% 
  RM(formula = PLEASANT ~ type, 
         subject = "participant", 
         within = "type",
         iter = 10000, 
         resampling = "WildBS", seed = 1234) %>% 
  summary()

collapsed.data.type.1 %>% 
  RM(formula = FAMILIAR ~ type, 
     subject = "participant", 
     within = "type",
     iter = 10000, 
     resampling = "WildBS", seed = 1234) %>% 
  summary()

collapsed.data.type.1 %>% 
  RM(formula = VARIED ~ type, 
     subject = "participant", 
     within = "type",
     iter = 10000, 
     resampling = "WildBS", seed = 1234) %>% 
  summary()

# 4 - Post-hoc Wilcoxon tests
pairwise.wilcox.test(collapsed.data.type.1$PLEASANT,
                     collapsed.data.type.1$type,
                     paired = TRUE, p.adjust.method = "holm")

pairwise.wilcox.test(collapsed.data.type.1$FAMILIAR,
                     collapsed.data.type.1$type,
                     paired = TRUE, p.adjust.method = "holm")

pairwise.wilcox.test(collapsed.data.type.1$VARIED,
                     collapsed.data.type.1$type,
                     paired = TRUE, p.adjust.method = "holm")

# Effect size
collapsed.data.type.1 %>% 
  wilcox_effsize(PLEASANT ~ type, paired = TRUE)
collapsed.data.type.1 %>% 
  wilcox_effsize(VARIED ~ type, paired = TRUE)
collapsed.data.type.1 %>% 
  wilcox_effsize(FAMILIAR ~ type, paired = TRUE)

# Figure 7 - Left: Mean PCA component ratings by type
collapsed.data.type.1 %>% 
  select(PLEASANT, FAMILIAR, VARIED, type) %>% 
  filter(type %in% c('Baseline', 'Referential', 'Resonated', 'Synthesized')) %>% 
  melt(id.vars=c("type")) %>% 
  ddply(c('type', 'variable'), summarise,
        mean=mean(value, na.rm = T),
        se=std.error(value, na.rm = T)) %>%
  ggplot(aes(
    x=variable,
    y=mean, 
    color=type, 
    group=type,
    shape=type)) +
  ylab("Mean and standard error") +
  xlab("") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, size=0.5, position="dodge") +
  geom_line(position = position_dodge(width = 0.3), size=0.5, linetype = "dashed") +  # geom_line needs group in ggplot
  geom_point(position = position_dodge(width = 0.3), size=3) +
  scale_shape_manual(values = c(17, 15, 16, 18)) +
  plotTheme

# Figure 7 - Right: Mean scale ratings by type
collapsed.data %>% 
  select(sc_agreable:sc_emergence, type) %>% 
  filter(type %in% c('Baseline', 'Referential', 'Resonated', 'Synthesized')) %>% 
  melt(id.vars=c("type")) %>% 
  ddply(c('type', 'variable'), summarise,
        mean=mean(value, na.rm = T),
        se=std.error(value, na.rm = T)) %>%
  ggplot(aes(
    x=variable,
    y=mean, 
    color=type, 
    group=type,
    shape=type)) +
  ylab("Mean and standard error") +
  xlab("") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, size=0.5, position="dodge") +
  geom_line(position = position_dodge(width = 0.3), size=0.5, linetype = "dashed") +  # geom_line needs group in ggplot
  geom_point(position = position_dodge(width = 0.3), size=3) +
  scale_shape_manual(values = c(17, 15, 16, 18)) +
  plotTheme +
  scalesPlotTheme

#### III.C RM MANOVA: Abstract and referential composition strategies ####
# Filtering Baseline, referential, resonated and synthesized compositions
collapsed.data.type.2 = filter(
  collapsed.data.type, 
  type %in% c("Referential", "Synthesized", "Resonated", "Ref/Syn Steady", "Ref/Syn Fluctuating", "Ref/Res Fluctuating")
)

# 0 - Assumption of Homogeneity of variance
leveneTest(PLEASANT ~ type, data = collapsed.data.type.2)
leveneTest(FAMILIAR ~ type, data = collapsed.data.type.2)
leveneTest(VARIED ~ type, data = collapsed.data.type.2)

# 1 - Assumption of multivariate Normality (rejected)
collapsed.data.type.2 %>% 
  select(PLEASANT, FAMILIAR, VARIED) %>%
  t() %>% 
  mshapiro.test()

# 2 - RM MANOVA
collapsed.data.type.2 %>% 
  multRM(formula = cbind(PLEASANT, FAMILIAR, VARIED) ~ type, 
         subject = "participant", 
         within = "type",
         iter = 10000, 
         resampling = "WildBS", seed = 1234) %>% 
  summary()


# 3 - Follow-up semi-parametric RM Anova
collapsed.data.type.2 %>% 
  RM(formula = PLEASANT ~ type, 
     subject = "participant", 
     within = "type",
     iter = 10000, 
     resampling = "WildBS", seed = 1234) %>% 
  summary()

collapsed.data.type.2 %>% 
  RM(formula = FAMILIAR ~ type, 
     subject = "participant", 
     within = "type",
     iter = 10000, 
     resampling = "WildBS", seed = 1234) %>% 
  summary()

collapsed.data.type.2 %>% 
  RM(formula = VARIED ~ type, 
     subject = "participant", 
     within = "type",
     iter = 10000, 
     resampling = "WildBS", seed = 1234) %>% 
  summary()

# 4 - Post-hoc Wilcoxon tests
pairwise.wilcox.test(collapsed.data.type.2$PLEASANT,
                     collapsed.data.type.2$type,
                     paired = TRUE, p.adjust.method = "holm")

pairwise.wilcox.test(collapsed.data.type.2$FAMILIAR,
                     collapsed.data.type.2$type,
                     paired = TRUE, p.adjust.method = "holm")

pairwise.wilcox.test(collapsed.data.type.2$VARIED,
                     collapsed.data.type.2$type,
                     paired = TRUE, p.adjust.method = "holm")

# Effect size
collapsed.data.type.2 %>% 
  wilcox_effsize(PLEASANT ~ type, paired = TRUE)
collapsed.data.type.2 %>% 
  wilcox_effsize(VARIED ~ type, paired = TRUE)
collapsed.data.type.2 %>% 
  wilcox_effsize(FAMILIAR ~ type, paired = TRUE)

# Figure 8 - Left: Mean PCA component ratings by type
collapsed.data.type.2 %>% 
  select(PLEASANT, FAMILIAR, VARIED, type) %>% 
  melt(id.vars=c("type")) %>% 
  ddply(c('type', 'variable'), summarise,
        mean=mean(value, na.rm = T),
        se=std.error(value, na.rm = T)) %>%
  ggplot(aes(
    x=variable,
    y=mean, 
    color=type, 
    group=type,
    shape=type)) +
  ylab("Mean and standard error") +
  xlab("") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, size=0.5, position="dodge") +
  geom_line(position = position_dodge(width = 0.3), size=0.5, linetype = "dashed") +  # geom_line needs group in ggplot
  geom_point(position = position_dodge(width = 0.3), size=3) +
  scale_shape_manual(values = c(8, 18, 25, 15, 16, 18)) +
  plotTheme

# Figure 8 - Right: Mean scale ratings by type
collapsed.data %>% 
  select(sc_agreable:sc_emergence, type) %>% 
  filter( type %in% c("Referential", "Synthesized", "Resonated", "Ref/Syn Steady", "Ref/Syn Fluctuating", "Ref/Res Fluctuating")) %>%
  melt(id.vars=c("type")) %>% 
  ddply(c('type', 'variable'), summarise,
        mean=mean(value, na.rm = T),
        se=std.error(value, na.rm = T)) %>%
  ggplot(aes(
    x=variable,
    y=mean, 
    color=type, 
    group=type,
    shape=type)) +
  ylab("Mean and standard error") +
  xlab("") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, size=0.5, position="dodge") +
  geom_line(position = position_dodge(width = 0.3), size=0.5, linetype = "dashed") +  # geom_line needs group in ggplot
  geom_point(position = position_dodge(width = 0.3), size=3) +
  scale_shape_manual(values = c(8, 18, 25, 15, 16, 18)) +
  plotTheme +
  scalesPlotTheme
