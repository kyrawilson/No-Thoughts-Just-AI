#### LOAD PACKAGES ####
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(purrr)
library(glmmTMB)
library(performance)
library(grpnet)
library(boot)
library(vip)
library(nptest)
library(bcaboot)
library(moments)
library(glmnet)
library(buildmer)
library(car)
library(emmeans)
library(ggeffects)
library(domir)
library(MuMIn)
library(glmm.hp)
library(randomForest)
library(permimp)
library(tornado)
library(effsize)
library(stringi)
library(DescTools)
library(corrplot)
library(texreg)

#### DATA CLEANING ####
#Read data file from qualtrics
df <- read.csv("STUDY2_responses_clean.csv", header=TRUE)
q_text <- df[1,]
df <- df[c(-1, -2),]

#584 observations before, 575 after filtering for PID and consent
df <- df %>% filter(PROLIFIC_PID != ''
                    & startsWith(Consent.Form., "Yes"))

#How many participants are in each Race condition
df %>% group_by(Group) %>%
  dplyr::summarise(n = n())

#Split conditions into multiple columns
df <- df %>% separate(col = jobs, 
                      into = c("jobs1", "jobs2", "jobs3", "jobs4"), sep = ",", remove=FALSE)  %>% 
            separate(col = expl_block_order, 
                     into = c("expl2", "expl3", "expl4"), sep = ",", remove=FALSE) %>% 
            separate(col = bias_block_order, 
                      into = c("bias2", "bias3", "bias4"), sep = ",", remove=FALSE)
df$expl1 <- 'None'
df$bias1 <- 'None'
df$recs_1 <- 'None,None,None,None,None'

#Make new variables for explicit scores
df <- df %>% mutate_at(vars(Q178_4, Q398_4, Q400_4,
                            Q402_4, Q404_4, Q406_4,  
                            Q408_4, Q410_4, Q397_4, Q399_4, Q401_4, 
                            Q403_4, Q405_4, Q407_4, 
                            Q409_4, Q411_4, Q419_4, Q421_4, Q423_4,  
                            Q425_4, Q427_4, Q429_4,  
                            Q431_4, Q433_4, Q420_4, Q422_4, Q424_4, 
                            Q426_4, Q428_4, Q430_4, 
                            Q432_4, Q434_4, Q438_4, Q440_4, Q442_4,  
                            Q444_4, Q446_4, Q448_4,  
                            Q450_4, Q452_4, Q439_4, Q441_4, Q443_4, 
                            Q445_4, Q447_4, Q449_4, 
                            Q451_4, Q453_4), as.numeric)


df <- df %>% rowwise() %>%
  mutate(BW_explicit = cohen.d(c(Q397_4, Q399_4, Q401_4, 
                       Q403_4, Q405_4, Q407_4, 
                       Q409_4, Q411_4),
                    c(Q178_4, Q398_4, Q400_4,
                       Q402_4, Q404_4, Q406_4,  
                       Q408_4, Q410_4))$estimate,
        AW_explicit = cohen.d(c(Q420_4, Q422_4, Q424_4, 
                                Q426_4, Q428_4, Q430_4, 
                                Q432_4, Q434_4),
                              c(Q419_4, Q421_4, Q423_4,  
                                Q425_4, Q427_4, Q429_4,  
                                Q431_4, Q433_4))$estimate,
        HW_explicit = cohen.d(c(Q439_4, Q441_4, Q443_4, 
                                Q445_4, Q447_4, Q449_4, 
                                Q451_4, Q453_4),
                              c(Q438_4, Q440_4, Q442_4,  
                                Q444_4, Q446_4, Q448_4,  
                                Q450_4, Q452_4))$estimate)

df <- df %>% mutate(explicit = ifelse(!is.na(BW_explicit), BW_explicit, 
                    ifelse(!is.na(AW_explicit), AW_explicit,
                    HW_explicit)))

# Recode variables
df <- df %>% mutate(I_recode = as.factor(ifelse(I==1, "Decision/IAT", "IAT/Decision"))) %>% 
        mutate(PROLFIC_PID = as.factor(PROLIFIC_PID)) %>% 
        mutate(Q307 = as.factor(Q307)) %>% 
         mutate(Q391 = as.factor(Q391))%>% 
        mutate(Q392 = as.factor(Q392))%>% 
         mutate(Q393 = as.factor(Q393))


#Melt df
melt_df <- df %>% dplyr::select(PROLIFIC_PID, Group, R, I_recode, E, B, status_condition,
                         explicit, EndDate, Q307, Q391, Q392, Q393,
                         order_list_1, order_list_2, order_list_3, order_list_4,
                         expl1, expl2, expl3, expl4,
                         bias1, bias2, bias3, bias4,
                         jobs1, jobs2, jobs3, jobs4,
                         recs_1, recs_2, recs_3, recs_4,
                         Scenario.1., Scenario.2., Scenario.3., Scenario.4.) %>%
           pivot_longer(cols = c(-PROLIFIC_PID, -Group, -R, -I_recode, -E, -B, -status_condition,  
                                 -explicit, -Q307, -Q391, -Q392, -Q393,
                                 -EndDate),    # pivot everything except "Name"
               names_to = c(".value", "Trial"),  # .value means split the names into columns "X" and "Y"
               names_pattern = "([A-Za-z_.]+)(\\d+).?$")   # regex to separate into "X"/"Y" and trial number

melt_df$Scenario_list <- lapply(stringr::str_split(melt_df$Scenario., ","), as.numeric)
melt_df$Order_list <- lapply(str_split(melt_df$order_list_, ","), as.numeric)
melt_df$Selected <- Map(function(v, i) v[i], melt_df$Order_list, melt_df$Scenario_list)

#Remove condition where Neutral is spelled wrong so recommendations are not 50/50
melt_df <- melt_df %>% filter(bias != "Netural")

melt_df$Group_recode <- recode_factor(melt_df$Group, 
                                      '1' = "White vs. Black", 
                                      '2' = "White vs. Asian",
                                      '3' = "White vs. Hispanic")
#melt_df$bias <- factor(melt_df$bias, levels = 
#                    c("None", "Neutral", "Sim-Cong", "Ext-Cong", "Sim-Incong", "Ext-Incong"))
melt_df$bias <- factor(melt_df$bias, levels = 
                    c("None", "Neutral", "Sim-Cong", "Sim-Incong", "Sim-Cong-New", "Sim-Incong-New", "Ext-Cong", "Ext-Incong"))
melt_df$Q392 <- factor(melt_df$Q392, levels = 
                         c("Not important", "Moderately important", "Very important"))
melt_df$Q393 <- factor(melt_df$Q393, levels = 
                         c("Poor", "Fair", "Good"))
melt_df$Job_type <- recode_factor(melt_df$status_condition, 
                                      'H.skewed' = "High status", 
                                      'H.balanced' = "High status",
                                      'L.skewed' = "Low status",
                                      'L.balanced' = "Low status")

#Drop rows where there is a 4, add binary response column
melt_df_keep <- melt_df %>%
  mutate(Keep = as.integer(map_lgl(Selected, ~ !(4 %in% .x)))) %>%
  filter(Keep == TRUE)  %>%
  mutate(Response = as.integer(map_lgl(Selected, ~ all(c(2,3) %in% .x)))) %>%
  mutate(Response_fac = as.factor(Response))
melt_df_keep$JOB_ID <- as.factor(paste(melt_df_keep$status_condition, melt_df_keep$jobs, sep="."))


#Merge in IAT scores
iat <- read.csv("STUDY2_NEW-waves1-3-withDscores.csv", header=TRUE)
melt_df_keep <- melt_df_keep %>% merge(iat)

melt_df_keep <- melt_df_keep %>% drop_na()

melt_df_keep_NewTest <- melt_df_keep %>% mutate(Wave = ifelse(startsWith(EndDate, 
                                                                 '2025-04-14'), "New", "Old"))


melt_df_keep <- melt_df_keep %>% filter(bias != "Sim-Cong" & bias != "Sim-Incong")

melt_df_keep <- melt_df_keep %>% 
  mutate(D_split = as.factor(ifelse(D >= 0, "positive", "negative"))) %>%
  mutate(D_split2 = as.factor(ifelse(D >= median(D, na.rm=TRUE), "High", "Low"))) %>%
  mutate(explicit_scale = explicit) %>%
  mutate(explicit_scale_split2 = as.factor(ifelse(explicit_scale >= median(explicit_scale, na.rm=TRUE), "High", "Low")))
  

#Add in raw-coded interactions
melt_df_keep <- melt_df_keep %>% 
  mutate("INT.bias.Job_type" = as.factor(interaction(bias, Job_type))) %>%
  mutate("INT.bias.Job_type.I_recode" = as.factor(interaction(bias, Job_type, I_recode))) %>%
  mutate("INT.bias.I_recode" = as.factor(interaction(bias, I_recode))) %>%
  mutate("INT.Job_type.I_recode" = as.factor(interaction(Job_type, I_recode))) %>%
  mutate("INT.Group_recode.I_recode" = as.factor(interaction(Group_recode, I_recode))) %>%
  mutate("INT.bias.Job_type.D_split2" = as.factor(interaction(bias, Job_type, D_split2))) %>%
  mutate("INT.bias.D_split2" = as.factor(interaction(bias, D_split2))) %>%
  mutate("INT.Job_type.D_split2" = as.factor(interaction(Job_type, D_split2))) %>%
  mutate("INT.Group_recode.D_split2" = as.factor(interaction(Group_recode, D_split2))) %>%
  mutate("INT.bias.Job_type.explicit_scale_split2" = as.factor(interaction(bias, Job_type, explicit_scale_split2))) %>%
  mutate("INT.Job_type.explicit_scale_split2" = as.factor(interaction(Job_type, explicit_scale_split2))) %>%
  mutate("INT.bias.explicit_scale_split2" = as.factor(interaction(bias, explicit_scale_split2))) %>%
  mutate("INT.Group_recode.explicit_scale_split2" = as.factor(interaction(Group_recode, explicit_scale_split2))) %>%
  mutate("INT.bias.Job_type.Q307" = as.factor(interaction(bias, Job_type, Q307))) %>%
  mutate("INT.Job_type.Q307" = as.factor(interaction(Job_type, Q307))) %>%
  mutate("INT.bias.Q307" = as.factor(interaction(bias, Q307))) %>%
  mutate("INT.bias.Job_type.Q391" = as.factor(interaction(bias, Job_type, Q391))) %>%
  mutate("INT.Job_type.Q391" = as.factor(interaction(Job_type, Q391))) %>%
  mutate("INT.bias.Q391" = as.factor(interaction(bias, Q391))) %>%
  mutate("INT.bias.Job_type.Q392" = as.factor(interaction(bias, Job_type, Q392))) %>%
  mutate("INT.Job_type.Q392" = as.factor(interaction(Job_type, Q392))) %>%
  mutate("INT.bias.Q392" = as.factor(interaction(bias, Q392))) %>%
  mutate("INT.bias.Job_type.Q393" = as.factor(interaction(bias, Job_type, Q393))) %>%
  mutate("INT.Job_type.Q393" = as.factor(interaction(Job_type, Q393))) %>%
  mutate("INT.bias.Q393" = as.factor(interaction(bias, Q393))) 

#Save cleaned files
melt_df_keep_save <- melt_df_keep
melt_df_keep_save$Order_list <- vapply(melt_df_keep_save$Order_list, paste, collapse = ", ", character(1L))
melt_df_keep_save$Scenario_list <- vapply(melt_df_keep_save$Scenario_list, paste, collapse = ", ", character(1L))
melt_df_keep_save$Selected <- vapply(melt_df_keep_save$Selected, paste, collapse = ", ", character(1L))
write.csv(melt_df_keep_save, file = "hiring_behavioral.csv", row.names = FALSE)

demo_df <- df[df$PROLIFIC_PID %in% melt_df_keep$PROLIFIC_PID, ]
demo_df <- select(demo_df, c("Q388", "Q387", "Q389", "Q307", "Q396", "Q391", "Q312", "PROLIFIC_PID", "Group"))
write.csv(demo_df, file = "hiring_demographics.csv", row.names = FALSE)



