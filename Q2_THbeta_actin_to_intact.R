# Determine if some drugs, and especially Bactin, reduced LID
# Data is from Mitch Bartlett and Torsten Falk 2024
#
# Cowen 2024
#
#
# Goals: Do all 9 (0 7 10 for V K KP, V K KL, V K P ) combos but use 
# kruskal-Wallis followed by Wilcoxon for post-hoc with Holm correction.
#
# Old:
# from Torsten: So, the main prediction for this study was that P will block the
# long-term anti-LID effect of K, but not the immediate anti-LID effect, based
# on literature from K and depression.  After we saw the clear effect of P to
# block the long-term K effect, we wanted to do a second study to test a
# different type of statins (hydrophilic vs hydrophobic) to see if this effect
# is specific to the drug (P) or the class of statin drugs.  Here the prediction
# is less clear.  The second prediction was that L might behave differently and
# not block K, based on its ability to be anti-LID itself in established LID.
# Therefore, you are correct we could focus on day 14, as the main long-term
# time point, which might allow to get significance, that is not lost in a
# million comparisons.  And we could have a separate analysis of just day 0 to
# look at that unexpected sensitization effect of P.
# P = Pravastatine L = lovastatin - so will need 9 graphs.
# Do this for days 0, 7, 10 ONLY

library(tidyverse)
library(readxl)
library(ggpubr)
library(cowplot)
#install.packages("ggsignif")                      # Install ggsignif package
library("ggsignif")  
library('svglite')
theme_set(theme_classic())

# Load data 
path = 'C:/Users/cowen/Documents/GitHub/LID_Ketamine_String_Pulling/Questions_stephen/Drugs_and_LID_2022/'

fname = 'TH Western Blot cleaned.xlsx'  
full_fname = paste0(path,fname)

# Make the table
DL <- read_excel(full_fname)
DL$Hemisphere = factor(DL$Hemisphere)
DL$Cond = factor(DL$Cond, levels = c('V','K','KP','KL','P','L'))
clrs = c('#7B7B7B','#0C65FE','#E10004','#FA6B0C','#688035','#69006B') # 
# Figure out how to get the SEM
DL_summary <- DL %>%
  group_by(Cond, Hemisphere) %>%
  summarize(mean=mean(BActin),
            sd=sd(BActin),
            se=sd(BActin)/sqrt(length(BActin)))


# Generate the figures

txt_size = 16

ggplot(DL_summary, aes(x=Hemisphere, y=mean, fill=Cond)) +
  geom_bar(position=position_dodge(.8), color = 'black', stat="identity", just = .2, width = .5) +
  scale_fill_manual(values=clrs)  +
  geom_errorbar(position=position_dodge(.8), width=.35, aes(ymin=mean-se, ymax=mean+se),linewidth =1.3) +
  geom_point(data = DL, aes(Hemisphere, BActin, fill = Cond, color = 'black'),position=position_dodge(.8), shape=21, color = 'black', size =1.4,stroke = 1.0) + 
  labs(y = "%TH/\u03b2-Actin (Norm to Intact)",size = txt_size)

ggsave('C:\\Temp\\test.pdf', width=5, height=3.5, device=cairo_pdf)
ggsave('C:\\Temp\\test.svg', width=5, height=3.5)

