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


#### LOAD CLEANED DATA ####
melt_df_keep <- read.csv("hiring_behavioral.csv")
demo_df <- read.csv("hiring_demographics.csv")


#### DEMOGRAPHIC STATISTICS ####

#Q388 - Gender
demo_df %>% group_by(Group, Q388) %>%
  dplyr::summarise(n= n())

demo_df %>% group_by(Q388) %>%
  dplyr::summarise(n= n()) %>%
  mutate(freq = n / sum(n))

#Q387 - Race
race_demo <- demo_df %>% group_by(Group, Q387) %>%
  dplyr::summarise(n= n())
print(c("White: ", sum(ungroup(filter(race_demo, grepl("White", Q387)))$n)))
print(c("Black: ", sum(ungroup(filter(race_demo, grepl("Black", Q387)))$n)))
print(c("Hispanic: ", sum(ungroup(filter(race_demo, grepl("Hispanic", Q387)))$n)))
print(c("Asian: ", sum(ungroup(filter(race_demo, grepl("Asian", Q387)))$n)))
print(c("Other: ", sum(ungroup(filter(race_demo, !grepl("Asian|White|Hispanic|Black", Q387)))$n)))

#Q389 - Age
mean(as.integer(demo_df$Q389))
sd(as.integer(demo_df$Q389))

#Q307 - Hiring Experience
demo_df %>% group_by(Q307) %>%
  dplyr::summarise(n= n())

#Q396 - Hiring Experience w/ AI
demo_df %>% group_by(Q396) %>%
  dplyr::summarise(n= n())

#Q391 - AI experience
demo_df %>% group_by(Q391) %>%
  dplyr::summarise(n= n())

#Q312 - IAT experience
demo_df %>% group_by(Q312) %>%
  dplyr::summarise(n= n())





#### Exploratory plots ####

#### MODEL 1 ####
### Experimental analysis - Recommendation and Task Order

contrasts(melt_df_keep$bias) <- contr.sum
contrasts(melt_df_keep$Job_type) <- contr.sum
contrasts(melt_df_keep$Group_recode) <- contr.sum
contrasts(melt_df_keep$I_recode) <- contr.sum

m1a <- glmmTMB(Response ~ bias*Job_type*Group_recode +
                        bias*Job_type*I_recode + Group_recode*I_recode +
                     (1|PROLIFIC_PID) + (1|JOB_ID),
                   data=melt_df_keep,
                   family = binomial('logit'),
                   na.action='na.fail')

summary(m1a)
Anova(m1a, type=3)

texreg(m1a, "Model1_GLMM.tex", single.row = TRUE, booktabs=TRUE)
xtable(Anova(m1a, type=3))

emmeans(m1a, pairwise ~ bias:Job_type, adjust="holm", type="response")

emm <- emmeans(m1a, ~ Group_recode:Job_type:I_recode, type="response")
simp <- pairs(emm, simple = "each")
test(simp[[3]], by = NULL, adjust = "mvt")

emm2 <- emmeans(m1a, ~ Job_type:bias)
simp2 <- pairs(emm2, simple = "each", adjust="holm")

summary(emmeans(m1a, ~ Job_type:bias, type = "response"), null=logit(0.5), infer=c(T, T), adjust="holm")
summary(emmeans(m1a, ~ Job_type:bias, type = "response"), null=logit(1), infer=c(T, T), adjust="holm")
summary(emmeans(m1a, ~ Job_type:bias, type = "response"), null=logit(0), infer=c(T, T), adjust="holm")

# Plot bias x job type interaction
g <- ggemmeans(m1a, terms = c("bias", "Job_type", "Group_recode")) 

# Make table of differences 
m1a_diffs <- g %>% group_by(facet, group) %>%
  mutate(x_value = predicted[x == "None"]) %>%  # Extract X value within group
  filter(x != "None") %>%                   # Keep only rows other than X
  mutate(diff_from_None = predicted - x_value) %>% # Subtract
  mutate(x_value = predicted[x == "Neutral"]) %>%  # Extract X value within group
  filter(x != "Neutral") %>%                   # Keep only rows other than X
  mutate(diff_from_Neutral = predicted - x_value) %>% # Subtract
  select(facet, group, x, predicted, diff_from_None, diff_from_Neutral)

g2 <- g

g2$rec <- case_when(
  g2$x=="Neutral" ~ 0.5,
  g2$x=="Ext-Cong" & g2$group=="High status"~ 1,
  g2$x=="Ext-Incong" & g2$group=="Low status"~ 1,
  g2$x=="Ext-Cong" & g2$group=="Low status"~ 0,
  g2$x=="Ext-Incong" & g2$group=="High status"~ 0,
  g2$x=="Ext-Incong" & g2$group=="High status"~ 0,
  g2$x=="Sim-Cong-New" & g2$group=="High status" & g2$facet=="White vs. Black" ~ 0.835, 
  g2$x=="Sim-Cong-New" & g2$group=="Low status" & g2$facet=="White vs. Black" ~ 0.83, 
  g2$x=="Sim-Cong-New" & g2$group=="High status" & g2$facet=="White vs. Asian" ~ 0.765, 
  g2$x=="Sim-Cong-New" & g2$group=="Low status" & g2$facet=="White vs. Asian" ~ 0.695, 
  g2$x=="Sim-Cong-New" & g2$group=="High status" & g2$facet=="White vs. Hispanic" ~ 0.61, 
  g2$x=="Sim-Cong-New" & g2$group=="Low status" & g2$facet=="White vs. Hispanic" ~ 0.77,
  g2$x=="Sim-Incong-New" & g2$group=="High status" & g2$facet=="White vs. Black" ~ 1-0.835, 
  g2$x=="Sim-Incong-New" & g2$group=="Low status" & g2$facet=="White vs. Black" ~ 1-0.83, 
  g2$x=="Sim-Incong-New" & g2$group=="High status" & g2$facet=="White vs. Asian" ~ 1-0.765, 
  g2$x=="Sim-Incong-New" & g2$group=="Low status" & g2$facet=="White vs. Asian" ~ 1-0.695, 
  g2$x=="Sim-Incong-New" & g2$group=="High status" & g2$facet=="White vs. Hispanic" ~ 1-0.61, 
  g2$x=="Sim-Incong-New" & g2$group=="Low status" & g2$facet=="White vs. Hispanic" ~ 1-0.77,
)

g2$rec_x <- case_when(
  g2$group=="Low status" ~ 0.12,
  g2$group=="High status" ~ -0.12
)

rq1_plot <- g2 %>%
  ggplot(aes(x = x,
             y = predicted,
             ymin = conf.low,
             ymax = conf.high,
             color = group)) +
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  geom_point(aes(x=as.integer(x)+rec_x, y=rec), shape=4, size=3, show.legend = F) + 
  labs(x = "AI Recommendation",
       y = "Probability of picking >50% white candidates",
       color = "Job Status") +
  scale_x_discrete(labels = c("None", "Neutral", "Congruent/\nModerate", 
                            "Incongruent/\nModerate", "Congruent/\nSevere",
                            "Incongruent/\nSevere")) + 
  theme_light() + facet_wrap(~ facet, ncol=1) +
  font("xy.text", size = 10, color = "black") + 
  font("ylab", size = 12, color = "black", face="bold")+
  font("xlab", size = 12, color = "black", face="bold")+
  font("legend.title", size=12, color = "black", face="bold")+
  font("legend.text", size=10, color = "black") + 
  theme(strip.text = element_text(
    size = 12, color = "black")) +
  theme(legend.position = "top", 
        legend.background = element_rect(size=0.5, 
        linetype="solid",colour ="gray")) + 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

ggsave(file="Model1_BiasxJobType.svg", plot=rq1_plot, width=4.1, height=5.5)

rq1a_plot <- g2 %>% group_by(x, group) %>% 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  ggplot(aes(x = x,
             y = predicted,
             ymin = conf.low,
             ymax = conf.high,
             color = group)) +
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  geom_point(aes(x=as.integer(x)+rec_x, y=rec), shape=4, size=5, show.legend = F) + 
  labs(x = "AI Recommendation",
       y = "Probability of picking >50% \nwhite candidates",
       color = "Job Status") +
  scale_x_discrete(labels = c("None", "Neutral", "Congruent/\nModerate", 
                              "Incongruent/\nModerate", "Congruent/\nSevere",
                              "Incongruent/\nSevere")) + 
  theme_light() + 
  font("xy.text", size = 10, color = "black") + 
  font("ylab", size = 12, color = "black", face="bold")+
  font("xlab", size = 12, color = "black", face="bold")+
  font("legend.title", size=12, color = "black", face="bold")+
  font("legend.text", size=10, color = "black") + 
  theme(strip.text = element_text(
    size = 12, color = "black")) +
  theme(legend.position = c(0.15, 0.8), 
        legend.background = element_rect(size=0.5, 
                                         linetype="solid",colour ="gray"))
ggsave(file="Model1_BiasxJobTypeAvg.svg", plot=rq1a_plot, width=6, height=3.5)

ggpredict(m1a, terms=c('bias', 'Job_type'))
z <- ggemmeans(m1a, terms=c('bias', 'Job_type'))

#Plot bias x job type x I order interaction
g3 <- ggemmeans(m1a, terms = c("I_recode", "Job_type", "Group_recode"))

rq2_plot <- g3 %>%
  ggplot(aes(x = x,
             y = predicted,
             ymin = conf.low,
             ymax = conf.high,
             color = group)) +
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  labs(x = "Task Order",
       y = "Probability of picking >50% white candidates",
       color = "Job Status") + 
  theme_light() + facet_wrap(~ facet, ncol=1) +
  font("xy.text", size = 10.5, color = "black") + 
  font("ylab", size = 14, color = "black", face="bold")+
  font("xlab", size = 14, color = "black", face="bold")+
  font("legend.title", size=14, color = "black", face="bold")+
  font("legend.text", size=12, color = "black") + 
  theme(strip.text = element_text(
    size = 14, color = "black")) +
  theme(legend.position = "top", 
        legend.background = element_rect(size=0.5, 
                                         linetype="solid",colour ="gray"))

ggsave(file="Model1_TaskOrder.svg", plot=rq2_plot, width=4.1, height=4.6)




#### MODEL 2 DREDGE ####

m_dredge <- buildglmmTMB(Response ~  bias*Job_type*Group_recode +
                           bias*Job_type*I_recode + Group_recode*I_recode + 
                    bias*Job_type*D + Group_recode*D + 
                    bias*Job_type*explicit_scale + Group_recode*explicit_scale + 
                    bias*Job_type*Q307 + 
                    bias*Job_type*Q391 +  
                    bias*Job_type*Q392 +  
                    bias*Job_type*Q393 + (1|PROLIFIC_PID) + (1|JOB_ID), 
                  data=melt_df_keep, 
                  buildmerControl=buildmerControl(calc.anova = TRUE),
                  family = binomial('logit'))

summary(m_dredge)
Anova(m_dredge @ model, type=3)
g4 <- ggemmeans(m_dredge @ model, terms = c("Job_type", "Q392", "bias"))
summary(g4)

ggemmeans(m_dredge @ model, terms = c("Q392", "Job_type", "bias"))

options(scipen=999)
m_dredge_Q392_diffs <- g4 %>% group_by(x, group) %>%
  mutate(x_value = predicted[facet == "None"]) %>%  # Extract X value within group
  filter(facet != "None") %>%                   # Keep only rows other than X
  mutate(diff_from_None = predicted - x_value) %>% # Subtract
  select(facet, x, group, predicted, diff_from_None)


g4$rec <- case_when(
  g4$facet=="Neutral" ~ 0.5,
  g4$facet=="Ext-Cong" & g4$x=="High status"~ 1,
  g4$facet=="Ext-Incong" & g4$x=="Low status"~ 1,
  g4$facet=="Ext-Cong" & g4$x=="Low status"~ 0,
  g4$facet=="Ext-Incong" & g4$x=="High status"~ 0,
  g4$facet=="Sim-Cong-New" & g4$x=="High status"~ 0.736,
  g4$facet=="Sim-Incong-New" & g4$x=="Low status"~ 0.765,
  g4$facet=="Sim-Cong-New" & g4$x=="Low status"~ 1-0.736,
  g4$facet=="Sim-Incong-New" & g4$x=="High status"~ 1-0.765,
)


cond.labs <- c("None", "Neutral", "Congruent/Moderate", 
               "Incongruent/Moderate", "Congruent/Severe",
               "Incongruent/Severe")
names(cond.labs) <- c("None", "Neutral", "Sim-Cong-New", 
                      "Sim-Incong-New", "Ext-Cong",
                      "Ext-Incong")

rq3_plot <- g4 %>%
  ggplot(aes(x = x,
             y = predicted,
             ymin = conf.low,
             ymax = conf.high,
             color = group)) +
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  geom_point(aes(x=as.integer(x), y=rec), shape=4, size=5, 
             show.legend = F, color='black') + 
  labs(x = "Job Status",
       y = "Probability of picking >50% white candidates",
       color = "Importance of AI Recs.") + 
  theme_light() + facet_wrap(~ facet, labeller = 
        labeller(facet = cond.labs))+
  font("xy.text", size = 10.5, color = "black") + 
  font("ylab", size = 14, color = "black", face="bold")+
  font("xlab", size = 14, color = "black", face="bold")+
  font("legend.title", size=14, color = "black", face="bold")+
  font("legend.text", size=12, color = "black") + 
  theme(strip.text = element_text(
    size = 14, color = "black")) +
  theme(legend.position = "top", 
        legend.background = element_rect(size=0.5, 
                                         linetype="solid",colour ="gray"))

ggsave(file="Model2_Importance.svg", plot=rq3_plot, width=8, height=6)

rq3_plot2 <- m_dredge_Q392_diffs %>%
  ggplot(aes(facet, diff_from_None, fill=group)) + 
  geom_bar(stat="identity", position='dodge') + 
  facet_wrap(~x, strip.position = "bottom", scales = "free_x") + theme_light() +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.text = element_text(color='black', size=12),
        strip.placement = "outside") +
  font("xy.text", size = 10.5, color = "black") + 
  font("ylab", size = 13, color = "black", face="bold")+
  font("xlab", size = 13, color = "black", face="bold")+
  font("legend.title", size=12, color = "black", face="bold")+
  font("legend.text", size=11, color = "black") +
  scale_x_discrete(labels = c("Neutral", "Congruent/\nModerate", 
                              "Incongruent/\nModerate", "Congruent/\nSevere",
                              "Incongruent/\nSevere")) + 
  scale_fill_discrete(labels = c("Not", "Moderately", "Very")) + 
  labs(x = "Job Status / AI Recommendation",
       y = "Δ Predicted Prob. of Majority-White \nResponse from No AI Rec.",
       fill = "AI Recs. Importance") + 
  theme(legend.position = "top", 
        legend.background = element_rect(size=0.5, 
                                         linetype="solid",colour ="gray")) + 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
  

g5 <- ggemmeans(m_dredge @ model, terms = c("Job_type", "Q393", "bias"))

m_dredge_Q393_diffs <- g5 %>% group_by(x, group) %>%
  mutate(x_value = predicted[facet == "None"]) %>%  # Extract X value within group
  filter(facet != "None") %>%                   # Keep only rows other than X
  mutate(diff_from_None = predicted - x_value) %>% # Subtract
  select(facet, x, group, predicted, diff_from_None)

g5$rec <- case_when(
  g5$facet=="Neutral" ~ 0.5,
  g5$facet=="Ext-Cong" & g5$x=="High status"~ 1,
  g5$facet=="Ext-Incong" & g5$x=="Low status"~ 1,
  g5$facet=="Ext-Cong" & g5$x=="Low status"~ 0,
  g5$facet=="Ext-Incong" & g5$x=="High status"~ 0,
  g5$facet=="Sim-Cong-New" & g5$x=="High status"~ 0.736,
  g5$facet=="Sim-Incong-New" & g5$x=="Low status"~ 0.765,
  g5$facet=="Sim-Cong-New" & g5$x=="Low status"~ 1-0.736,
  g5$facet=="Sim-Incong-New" & g5$x=="High status"~ 1-0.765,
)

cond.labs <- c("None", "Neutral", "Congruent/Moderate", 
               "Incongruent/Moderate", "Congruent/Severe",
               "Incongruent/Severe")
names(cond.labs) <- c("None", "Neutral", "Sim-Cong-New", 
                      "Sim-Incong-New", "Ext-Cong",
                      "Ext-Incong")

rq4_plot <- g5 %>%
  ggplot(aes(x = x,
             y = predicted,
             ymin = conf.low,
             ymax = conf.high,
             color = group)) +
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  geom_point(aes(x=as.integer(x), y=rec), shape=4, size=5, 
             show.legend = F, color='black') + 
  labs(x = "Job Status",
       y = "Probability of picking >50% white candidates",
       color = "Quality of AI Recs.") + 
  theme_light() + facet_wrap(~ facet, labeller = 
                               labeller(facet = cond.labs))+
  font("xy.text", size = 10.5, color = "black") + 
  font("ylab", size = 14, color = "black", face="bold")+
  font("xlab", size = 14, color = "black", face="bold")+
  font("legend.title", size=14, color = "black", face="bold")+
  font("legend.text", size=12, color = "black") + 
  theme(strip.text = element_text(
    size = 14, color = "black")) +
  theme(legend.position = "top", 
        legend.background = element_rect(size=0.5, 
                                         linetype="solid",colour ="gray"))
ggsave(file="Model2_Quality.svg", plot=rq4_plot, width=8, height=6)

rq4_plot2 <- m_dredge_Q393_diffs %>%
  ggplot(aes(facet, diff_from_None, fill=group)) + 
  geom_bar(stat="identity", position='dodge') + 
  facet_wrap(~x, strip.position = "bottom", scales = "free_x") + theme_light() +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.text = element_text(color='black', size=12),
        strip.placement = "outside") +
  font("xy.text", size = 10.5, color = "black") + 
  font("ylab", size = 13, color = "black", face="bold")+
  font("xlab", size = 13, color = "black", face="bold")+
  font("legend.title", size=12, color = "black", face="bold")+
  font("legend.text", size=11, color = "black") +
  scale_x_discrete(labels = c("Neutral", "Congruent/\nModerate", 
                              "Incongruent/\nModerate", "Congruent/\nSevere",
                              "Incongruent/\nSevere")) + 
  labs(x = "Job Status / AI Recommendation",
       y = "Δ Predicted Prob. of Majority-White\n Response from No AI Rec.",
       fill = "AI Recs. Quality") + 
  theme(legend.position = "top", 
        legend.background = element_rect(size=0.5, 
                                         linetype="solid",colour ="gray")) + 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

rq34 <- ggarrange(rq3_plot2, rq4_plot2, 
          labels = c("A", "B"),
          font.label = list(size = 20, color = "gray50", face = "bold"),
          ncol = 2)
ggsave(file="Model2_Quality+Importance.svg", plot=rq34, width=12, height=5)


#### MODEL 2 GLM IMPORTANCE ####

full_model <- glmmTMB(Response ~ bias*Job_type*Group_recode +
                        bias*Job_type*I_recode + Group_recode*I_recode + 
                        bias*Job_type*D + Group_recode*D + 
                        bias*Job_type*explicit_scale + Group_recode*explicit_scale + 
                        bias*Job_type*Q307 + 
                        bias*Job_type*Q391 +  
                        bias*Job_type*Q392 +  
                        bias*Job_type*Q393 + (1|PROLIFIC_PID) + (1|JOB_ID),
                      data=filter(melt_df_keep, is.finite(melt_df_keep$explicit_scale)),
                      family = binomial('logit'),
                      na.action='na.fail')
icc(full_model, tolerance=1e-10)

full_model_glm <- glm(Response ~ bias*Job_type + Group_recode + 
                        bias*Job_type + Group_recode*I_recode + 
                        bias*Job_type + Group_recode*D + 
                        Job_type*explicit_scale + Group_recode*explicit_scale + 
                        Job_type*Q307 + 
                        Job_type*Q391 +  
                        Job_type*Q392 +  
                        Job_type*Q393,
                      data=filter(melt_df_keep, is.finite(melt_df_keep$explicit_scale)),
                      family = binomial('logit'))
reduced_glm <- glm(Response ~ 1, 
                   data=filter(melt_df_keep, is.finite(melt_df_keep$explicit_scale)),
                   family = binomial('logit'))

dict <- list(old = c("bias:Job_type", "bias", "Q392", "Job_type:Q392",
                     "Group_recode:I_recode", "Group_recode:D", "Q393",
                     "Group_recode:explicit_scale", "Q307", "Job_type:Q391",
                     "Job_type:Q393", "Job_type:Q307", "Job_type", "Group_recode",
                     "I_recode", "explicit_scale", "D", "Q391", "Job_type:explicit_scale"),
             new = c("AI Recommendation:Job Status", "AI Recommendation", "Importance", 
                     "Job Status:Importance", "Race:Task Order", "Race:IAT Score", 
                     "Quality", "Race:Explicit Score", "Hiring Experience", 
                     "Job Status:AI Experience", "Job Status:Quality", 
                     "Job Status:Hiring Experience", "Job Status", "Race",
                     "Task Order", "Explicit Score", "IAT Score", "AI Experience", 
                     "Job Status:Explicit Score"))
imp <- importance(full_model_glm, reduced_glm, dict = dict)
p <- plot(imp, plot=FALSE)
ggsave("Model2_VarImportance.svg", p)
plot(imp)

#### MODEL 2 GROUP ELASTIC NET ####

set.seed(1)
train = melt_df_keep %>%
  filter(is.finite(explicit_scale)) %>%
  sample_frac(size=0.8)

test = melt_df_keep %>%
  filter(is.finite(explicit_scale)) %>%
  setdiff(train)


#Which penalty leads to the least amount of overfitting - LASSO, SCAD, or MCP?
#LASSO: misclassification error=0.333 / r-squared=0.2452389,0.1642498
#MCP = misclassification error=0.339 / r-squared=0.1982803,0.1982803
#SCAD = misclassification error=0.339233 / r-squared=0.2167077, 0.1878902

params_LASSO <- cv.grpnet(Response ~ bias*Job_type + Group_recode + 
                      bias*Job_type*I_recode + Group_recode*I_recode + 
                      bias*Job_type*D + Group_recode*D + 
                      bias*Job_type*explicit_scale + Group_recode*explicit_scale + 
                      bias*Job_type*Q307 + 
                      bias*Job_type*Q391 +  
                      bias*Job_type*Q392 +  
                      bias*Job_type*Q393, 
                    data=train, penalty="LASSO",
                    family = "binomial", parallel=TRUE)

probabilities <- predict.cv.grpnet(params_LASSO, newdata=as.data.frame(test), type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
mis_error <- 1-mean(predicted.classes == test$Response)
print(mis_error)
rsq_LASSO <- params_LASSO$grpnet.fit$dev.ratio[params_LASSO$index]

#Compare to glmmTMB prediction performance
full_model <- glmmTMB(Response ~ bias*Job_type*Group_recode +
                        bias*Job_type*I_recode + Group_recode*I_recode + 
                        bias*Job_type*D + Group_recode*D + 
                        bias*Job_type*explicit_scale + Group_recode*explicit_scale + 
                        bias*Job_type*Q307 + 
                        bias*Job_type*Q391 +  
                        bias*Job_type*Q392 +  
                        bias*Job_type*Q393 + (1|PROLIFIC_PID) + (1|JOB_ID),
                      data=train,
                      family = binomial('logit'),
                      na.action='na.fail')

probabilities <- predict(full_model, newdata = test, type="response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
mis_error <- 1-mean(predicted.classes == test$Response)
print(mis_error)

#Investigate importance in the LASSO model

imp_LASSO <- as.data.frame(predict(params_LASSO, newdata = as.data.frame(test), 
                          type = "imp") * 100)
imp_LASSO <- cbind(rownames(imp_LASSO), imp_LASSO)
colnames(imp_LASSO) <- c("Variable", "Importance")
imp_LASSO <- imp_LASSO %>% filter(Importance != 0) %>% arrange(desc(Importance))

print(xtable(imp_LASSO[,1:2], booktabs=TRUE), include.rownames=FALSE)
  
ggplot(imp_LASSO, aes(x=Variable, y=Importance)) + geom_bar(stat='identity')
