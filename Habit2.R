#Project: Early Tashelhiyt Berber Word Segmentation: The Role of The PWC
#Script Author: Abdellah Elouatiq
#Date:  01/09/2021 - 31-01-2022

#############################################################################
############################# Packages ######################################
#############################################################################


library(tidyverse) #ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats #
library(lme4)
library(lmerTest)
library(readxl)
library(sjPlot)
library(cowplot)
library(effects)
library(sjmisc)
library(sjstats)
library(brms)
library(broom)
library(papaja)
library(rio)
library(report)
library(ggpubr)
library(ggthemes)
library(ggside)
library(WRS2)
library(afex)
library(ggeffects)
library(ggstatsplot)
library(nortest)
library(emmeans)
library(plotly)
library(effectsize)
library(MuMIn)
library(flexplot)
library(performance)
library(lattice)
library(rms)
library(stringr)

## additional ##
library(quantmod)
library(multilevelTools)
library(reshape2)
library(HLMdiag)
library(sjlabelled)
library(ggeffects)
library(RColorBrewer)
library(multcomp)
library(interactions)
library(jtools)
library(webshot)



#############################################################################
########################## Set Directories ##################################
#############################################################################

file_paths = fs::dir_ls("C:/Users/frog-/OneDrive/Documents/habit/results/all data/data")
bio_paths = fs::dir_ls("C:/Users/frog-/OneDrive/Bureau/Radboud study/S4/Lab Rotation (MPI)/Documents and forms/fill after session/Pps_Overview")

#############################################################################
######################### Import Habit2 Data ################################
#############################################################################


Raw_Data = file_paths %>%
  map_df(function(path){read_csv(path)}) 

Bio_Data_Raw = bio_paths %>%
  map_df(function(path){readxl::read_xlsx(path)})

Bio_Data = Bio_Data_Raw %>%
  dplyr::select(SubjectID, Age_In_Days, Gender) %>%
  filter(!is.na(SubjectID), !is.na(Gender))


# substitute " " with "_" so it can be easily called for select function #
names(Raw_Data) <- gsub( " ", "_", names(Raw_Data))


Raw_Data$Order = substr(Raw_Data$`Order_(randomization)`, 1, 6)

# drop NA values from subject ID, and select relevant variables #
All_Data = filter(Raw_Data, !is.na(Raw_Data$SubjectID))  %>%
  dplyr::select(SubjectID, Order, Phase, Trial, StimName, TotalCenter) %>% 
  filter(Phase == "TEST")%>%      #filter out Familiarization phase trials
  filter(TotalCenter!=0)%>%       #Remove 0 values
  merge(Bio_Data)              #Add Age and Gender variables
  



All_Data$Context = (substr(All_Data$StimName, 3, 3))   # change "C" and "V" to "Consonant" and "Vowel"
All_Data$Context [All_Data$Context == "C"] = "Consonant"
All_Data$Context [All_Data$Context == "V"] = "Vowel"
All_Data$Context = as.factor(All_Data$Context)        # convert into a factor #
All_Data$Familiarity = substr(All_Data$StimName, 1, 1)  # create new variable 'Familiarity condition' using 1st string of 'stimuli name'
All_Data$Familiarity [All_Data$Familiarity == "F"] = "Familiar"     # change "N" and "F" to "Novel" and "Familiar" #
All_Data$Familiarity [All_Data$Familiarity == "N"] = "Novel"
All_Data$Familiarity = as.factor(All_Data$Familiarity)      # convert into a factor
All_Data$Item = substr(All_Data$StimName, 5, 8)   # create new variable 'item' using last three/four strings of 'stimuli name'
All_Data$Item = as.factor(All_Data$Item)
All_Data$Gender = as.factor(All_Data$Gender)
All_Data$SubjectID = as.factor(All_Data$SubjectID)
All_Data$Order = as.factor(All_Data$Order)
All_Data$Log_Total_Center = log(All_Data$TotalCenter)   # Log transform looking times #
All_Data$Log_Age_In_Days = log(All_Data$Age_In_Days)   # Log transform Age in days #
All_Data$Trial = as.numeric(All_Data$Trial)




# rename #
All_Data = rename(All_Data, Total_Looks = TotalCenter)
All_Data = rename(All_Data, Log_Total_Looks = Log_Total_Center)
 



### Center and Total_Looks & Center Age_In_Days ###  
All_Data = mutate(All_Data, 
                  Log_Total_Looks_C = Log_Total_Looks - mean(Log_Total_Looks),
                  Log_Total_Looks_Z = Log_Total_Looks_C/sd(Log_Total_Looks_C),
                  Age_In_Days_C = Age_In_Days - mean(Age_In_Days),
                  Age_In_Days_Z = Age_In_Days_C/sd(Age_In_Days_C),
                  Log_Age_In_Days_C = Log_Age_In_Days - mean(Log_Age_In_Days),
                  Log_Age_In_Days_Z = Log_Age_In_Days_C/sd(Log_Age_In_Days_C))

  
All_Data = All_Data %>%
  dplyr::select(SubjectID, Order, Trial, Item, Age_In_Days, Age_In_Days_C, Age_In_Days_Z, Log_Age_In_Days, Log_Age_In_Days_C, Log_Age_In_Days_Z, Gender, Context, Familiarity, Total_Looks,
         Log_Total_Looks, Log_Total_Looks_C, Log_Total_Looks_Z)


Test_Data = All_Data %>%
  filter(str_detect(SubjectID, "E"))

Test_Data$Total_Looks_Sec= Test_Data$Total_Looks/1000

ID_Data = Test_Data %>%
  group_by(SubjectID, Context, Familiarity)%>%
  summarise(Total_Looks = mean(Total_Looks)) %>%
  merge(Bio_Data)%>%
  mutate(Log_Total_Looks = log(Total_Looks), 
         Log_Age_In_Days = log(Age_In_Days))


ID_Vowel = Test_Data %>%
  group_by(SubjectID, Context, Familiarity)%>%
  summarise(Total_Looks = mean(Total_Looks)) %>%
  filter(Context == "Vowel")%>%
  merge(Bio_Data)%>%
  mutate(Log_Total_Looks = log(Total_Looks), 
         Log_Age_In_Days = log(Age_In_Days))

ID_Consonant = Test_Data %>%
  group_by(SubjectID, Context, Familiarity)%>%
  summarise(Total_Looks = mean(Total_Looks)) %>%
  filter(Context == "Consonant")%>%
  merge(Bio_Data)%>%
  mutate(Log_Total_Looks = log(Total_Looks), 
         Log_Age_In_Days = log(Age_In_Days))
  


Pilot_Data = All_Data %>%
  filter(str_detect(SubjectID, "P"))

ID_Pilot = Pilot_Data %>%
  group_by(SubjectID, Context, Familiarity)%>%
  summarise(Total_Looks = mean(Total_Looks)) %>%
  merge(Bio_Data)%>%
  mutate(Log_Total_Looks = log(Total_Looks), 
         Log_Age_In_Days = log(Age_In_Days))

Vowel_Data = Test_Data %>%
  filter(Context == "Vowel")

Consonant_Data = Test_Data %>%
  filter(Context == "Consonant")

Novel_Data = Test_Data %>%
  filter(Familiarity == "Novel")

Familiar_Data = Test_Data %>%
  filter(Familiarity == "Familiar")

Age_325_Data = Test_Data %>%
  filter(Age_In_Days < 325 )

Age_343_Data = Test_Data %>%
  filter(Age_In_Days < 343 )


Trials_12_Age_325_Data = Test_Data %>%
  filter(Age_In_Days < 325 ) %>%
  filter(Trial <= 12 )

Trials_8_Data = Test_Data  %>%
  filter(Trial <= 8 )

Trials_12_Data = Test_Data  %>%
  filter(Trial <= 12 )

Trials_Strict_8 = Test_Data  %>%
  filter(SubjectID != "E004")   %>%
  filter(Trial <= 24 )

Age_343_E004 = Test_Data %>%
  filter(Age_In_Days < 343 )



Trials_Strict_8 = Test_Data  %>%
  filter(Trial >= 8 )

Trials_Strict_8_343 = Test_Data  %>%
  filter(Trial >= 8 )%>%
  filter(Age_In_Days < 343 ) 
  

ID_Trials_12 = Trials_12_Data %>%
  group_by(SubjectID, Context, Familiarity) %>%
  summarise(Total_Looks = mean(Total_Looks))  %>%
  merge(Bio_Data) %>%
  mutate(Log_Total_Looks = log(Total_Looks), 
         Log_Age_In_Days = log(Age_In_Days))

ID_Vowel_12 = Trials_12_Data %>%
  group_by(SubjectID, Context, Familiarity)%>%
  summarise(Total_Looks = mean(Total_Looks)) %>%
  filter(Context == "Vowel")%>%
  merge(Bio_Data)%>%
  mutate(Log_Total_Looks = log(Total_Looks), 
         Log_Age_In_Days = log(Age_In_Days))

ID_Consonant_12 = Trials_12_Data %>%
  group_by(SubjectID, Context, Familiarity)%>%
  summarise(Total_Looks = mean(Total_Looks)) %>%
  filter(Context == "Consonant")%>%
  merge(Bio_Data)%>%
  mutate(Log_Total_Looks = log(Total_Looks), 
         Log_Age_In_Days = log(Age_In_Days))


Trials_12_Data_V = Vowel_Data  %>%
  filter(Trial <= 12 )


Trials_12_Data_C = Consonant_Data  %>%
  filter(Trial <= 12 )


Trials_20_Data = Test_Data  %>%
  filter(Trial <= 20 )

Trials_13_32_Data = Test_Data  %>%
  filter(Trial >= 13 )

Ratings = Test_Data  %>%
  dplyr::select(SubjectID, Total_Looks, Familiarity, Context)


###Test_Data$Trial = as.factor(Test_Data$Trial)

By_ID_Diff = Test_Data  %>%
  filter(SubjectID != "E004") %>%
  dplyr::group_by(SubjectID, Familiarity)   %>%
  dplyr::summarise(Total_Looks = mean(Total_Looks), 
                   Trial = dplyr::n())  %>%
  dplyr::mutate(Trial_N = Trial[Familiarity == "Familiar"] + Trial[Familiarity == "Novel"]) %>%
  dplyr::mutate(Difference = Total_Looks[Familiarity == "Familiar"] - Total_Looks[Familiarity == "Novel"]) %>% 
  dplyr::mutate(Difference_by_Trial_N = Difference/Trial_N)  %>%
  dplyr::group_by(Difference_by_Trial_N) %>%
  merge(Bio_Data) 


Individual = By_ID_Diff  %>%
  dplyr::select(SubjectID, Difference_by_Trial_N) %>%
  dplyr::group_by(SubjectID)   %>%
  dplyr::summarise(Total_Looks = mean(Total_Looks), 
                   Trial = dplyr::n())  %>%
  dplyr::mutate(Trial_N = Trial[Familiarity == "Familiar"] + Trial[Familiarity == "Novel"]) %>%
  dplyr::mutate(Difference = Total_Looks[Familiarity == "Familiar"] - Total_Looks[Familiarity == "Novel"]) %>% 
  dplyr::mutate(Difference_by_Trial_N = Difference/Trial_N)  %>%
  merge(Bio_Data) %>%
  dplyr::ungroup()


ggplot(data = By_ID_Diff,
       aes(y = Difference, x = 0))+
  geom_point() + 
  geom_abline() +
  ylab("Individual Difference In Total Looking Times To Familiar and Novel Targets") +
  xlab(" ") +
  scale_y_continuous(breaks=seq(-3000, 3000, by = 500))+
  scale_x_continuous(breaks = 0, labels = "")
  
  
  
ggplot(data = By_ID_Diff,
       aes(y = Difference_by_Trial_N, x = 0))+
  geom_point() + 
  geom_abline() +
  ylab("Individual Difference (Familiar = 1, Novel = -1) Averaged by Number of Trials") + 
  xlab(" ") +
  scale_y_continuous(breaks=seq(-300, 300, by = 50))+
  scale_x_continuous(breaks = 0, labels = " ")


###################################


ggboxplot(Test_Data, x = "Context", y = "Total_Looks", color = "Familiarity",
          add = c("mean_se", "jitter"), bxp.errorbar = TRUE, 
          bxp.errorbar.width = 0.4, width = 0.7, xlab = "Context",
          ylab = "Total Looking Times (in ms)", legend = "right") +
  theme_classic()

ggboxplot(Test_Data, x = "Familiarity", y = "Total_Looks",
          add = c("mean_se", "jitter"), legend = "right")




ggboxplot(Test_Data, x = "Context", y = "Total_Looks",
          add = c("mean_se", "jitter"), legend = "right")


#############################################################################
########################### Data Cleaning ###################################
#############################################################################

###data accuracy###








###NA values###








###Outliers###







#############################################################################
######################### Linear Mixed Model ################################
#############################################################################

ANOVA = aov(Log_Total_Looks ~ Familiarity*Context*Log_Age_In_Days + Trial + Order, data = Test_Data)
anova(ANOVA)
summary(ANOVA)
report(ANOVA)
##########################
######Baseline Model######
##########################


Baseline= lmer(Log_Total_Looks ~ 1 + (1|SubjectID), data = Test_Data)

summary(Baseline)
anova(Baseline)
performance::icc(Baseline)
check_model(Baseline)


Two_way_fe = lmer(Log_Total_Looks ~ Familiarity*Context + (1|SubjectID), Test_Data)
summary(Two_way_fe)
anova(Two_way_fe)
performance::icc(Two_way_fe)
check_model(Two_way_fe)
report(Two_way_fe)


############################
##### Expanded Models ######
############################

Constrained = lmer(Log_Total_Looks ~ Familiarity+Context+Log_Age_In_Days_C+ 
                     Trial + Order +  (1|SubjectID), Test_Data)

Intermediate1 =  lmer(Log_Total_Looks ~ Familiarity*Context + Log_Age_In_Days_C 
                      + Trial + Order +
                             (1|SubjectID), Test_Data)

Intermediate2 =  lmer(Log_Total_Looks ~ Familiarity*Context+ 
                        Familiarity*Log_Age_In_Days_C + Trial + Order +
                             (1|SubjectID), Test_Data)
   


Final= lmer(Log_Total_Looks ~ Familiarity*Context*Log_Age_In_Days_C + 
             Trial + Order + (1|SubjectID), Test_Data)           



Final_N= lmer(Total_Looks_Sec ~ Familiarity*Context*Age_In_Days + 
              Trial + Order + (1|SubjectID), Test_Data)   

Final12_N= lmer(Total_Looks_Sec ~ Familiarity*Context*Age_In_Days + 
              Trial + Order + (1|SubjectID), Trials_12_Data) 

Full= lmer(Log_Total_Looks ~ Familiarity*Context*Log_Age_In_Days_C + 
                Trial + Order + (1+Familiarity|SubjectID) + (1|Item), Test_Data)

Final325= lmer(Log_Total_Looks ~ Familiarity*Context*Log_Age_In_Days_C + 
                    Trial + Order + (1|SubjectID), Age_325_Data)

Final12_325= lmer(Log_Total_Looks ~ Familiarity*Context*Log_Age_In_Days_C + 
                    Trial + Order + (1|SubjectID), Trials_12_Age_325_Data) 

cohens_f(Final12)

Test_Data$Trial = as.factor(Test_Data$Item)

compare_performance(Baseline, Constrained, Intermediate1, Intermediate2, Final, rank = T)

anova(Baseline, Constrained, Intermediate1, Intermediate2, Final)

summary(Constrained)
anova(Constrained)
icc(Constrained)
check_model(Constrained)


plot(allEffects(Final))

### Comparing models (325) with Log LTs###
Model_Comparison = tab_model(Baseline, Constrained, Intermediate1, Intermediate2, Final, 
          dv.labels = c("Base", "Constrained", "Int1", "Int2", "Final"), 
          p.style = "numeric", show.p = TRUE, show.re.var = FALSE, file = "ModelComp.html",
          show.icc = TRUE, show.loglik = TRUE,
          title = "Table 3. Linear Mixed Effects Models Ouput Comparison",
          rm.terms = c("Order [order2]", "Order [order3]", "Order [order4]","Order [order5]",
                       "Order [order6]","Order [order7]","Order [order8]", "Gender [M]"))

webshot("ModelComp.html", "ModelComp.png", zoom = 10)

Table3 = tab_model(Final, 
          p.style = "numeric", show.p = TRUE, show.re.var = FALSE,file = "ModelOut.html",
          show.icc = F, show.loglik = TRUE, 
          rm.terms = c("Order [order2]", "Order [order3]", "Order [order4]","Order [order5]",
                       "Order [order6]","Order [order7]","Order [order8]", "Gender [M]"), 
          title = "Table 3. Linear Mixed Effects Model Ouput. Beta estimates, confidence intervals (IC), and p-values of all predictors included in the final model.
          Significant results are in bold.")

tab_model(Final)

webshot("ModelOut.html", "ModelOut.png", zoom = 10)



#############################################################################
############################ Assumptions ####################################
#############################################################################




######## Normality of Residuals ############


All_Assumptions = check_model(Final, panel = F)

Heteroskedasticity = plot(check_heteroscedasticity(Final))+
  theme(plot.title = element_text(size = 24), 
        axis.title = element_text(size = 24), 
        plot.subtitle = element_text(size = 18))

collinearity = plot(check_collinearity(Final))+
  theme(plot.title = element_text(size = 24), 
        axis.title = element_text(size = 24), 
        plot.subtitle = element_text(size = 18))


Normality = plot(check_normality(Final))+
  theme(plot.title = element_text(size = 24), 
        axis.title = element_text(size = 24), 
        plot.subtitle = element_text(size = 18))


collinearity = plot(check_collinearity(Final))+
  theme(plot.title = element_text(size = 24), 
        axis.title = element_text(size = 24), 
        plot.subtitle = element_text(size = 18))



#############################################################################
############################ Bayesian LMM ###################################
#############################################################################

### We use brms default priors ###



Bayesian_Model1 <- brm(Log_Total_Looks ~ Familiarity*Context*Age_In_Days_Z + Trial + Order + 
                        (1|SubjectID),
                      data = Test_Data, iter = 4000, chains = 3, family = gaussian(link = "log"))

summary(Bayesian_Model1)


Bayes_Final = brm(Log_Total_Looks ~ Familiarity*Context*Log_Age_In_Days_C + 
                    Trial + Order + (1|SubjectID), Test_Data, iter = 6000)

summary(Bayes_Final)


get_prior(Total_Looks ~ Familiarity + Context + Familiarity*Context +
            Trial + Order + (1 + Familiarity | SubjectID) + (1 | Item),
          data = Test_Data,family = poisson(link = "log"), iter = 4000)



posterior <- as.matrix(Bayes_Final)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("b_FamiliarityNovel", "b_ContextVowel", "b_FamiliarityNovel:ContextVowel"),
           prob = 0.70) + plot_title


#############################################################################
########################## Data Visualization ###############################
#############################################################################

Figure_Age_Distribution = hist(Test_Data$Age_In_Days, 
                               breaks = 6, xlab = "Age In Days",
                               main = "Figure 1. Age Distribution of Participants")

Figure_Age_Distribution = hist(ID_Pilot$Age_In_Days, 
                               xlab = "Age In Days",
                               main = "Figure 1. Age Distribution of Participants")


ggplot(Test_Data, aes(x = Familiarity, y = Total_Looks)) +
  facet_wrap(~SubjectID) + 
  geom_boxplot()

ggplot(Pilot_Data, aes(x = Familiarity, y = log(Total_Looks))) +
  facet_wrap(~SubjectID) + 
  geom_boxplot()

### ALL DATA FAMILIARITY ###
Figure1=ggbetweenstats(data = Test_Data,
               x = Familiarity, plot.type = "box",
               centrality.type = "parametric", 
                y = Total_Looks, results.subtitle = F, 
               ylab = "Total Looking Times (Log seconds)", xlab = "(a) Familiarity Condition") 
               #subtitle = "Figure 1. Difference in Total Looking Times Towards Familiar and Novel Trials")
Figure1


### ALL DATA CONTEXT ###
Figure2=ggbetweenstats(data = Test_Data,
               x = Context, plot.type = "box",
               centrality.type = "parametric",
               y = Total_Looks, results.subtitle = F,
               ylab = "Total Looking Times (Log seconds)", xlab = "(b) Context Condition")
               #subtitle = "Figure 2. Difference in Total Looking Times Towards Consonant and Vowel Trials")
Figure2


Figure_1_2 = ggarrange(Figure1, Figure2)
Figure_1_2


### Violin plot Familiarity x Context ###
ggplot(
  data = Test_Data, mapping = aes(x = Context, y = Log_Total_Looks_C, 
                                  fill = Familiarity))+
  geom_violin(trim = FALSE) + 
  geom_jitter()


### Total LTs by Participant ###
flexplot(Total_Looks~SubjectID,data=Test_Data)



### Familiarity by Participant ###
flexplot(Log_Total_Looks_Z~Familiarity | SubjectID,data=Test_Data)


### Context effects by Item ###
flexplot(Log_Total_Looks~Context | Item, jitter = c(.3, 0), data=Test_Data)


### Familiarity effects by Item ###
flexplot(Log_Total_Looks~Familiarity | Item, data=Test_Data)


### 2x2 interaction PLOT ###
INTERACTION = flexplot(Log_Total_Looks ~ Context+Familiarity, data=Test_Data) +
  #labs(title = "Figure 3. Familiarity by Context Interaction Plot") +
  ylab("Total Looking Times (Log seconds)")+
  xlab("Context Condition") + 
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 10))
INTERACTION

INTERACTION_12 = flexplot(Log_Total_Looks ~ Context+Familiarity, data=Trials_12_Data) +
  #labs(title = "Figure 8. Familiarity by Context Interaction Plot (First 12 trials)") +
  ylab("Total Looking Times (Log seconds)")+
  xlab("Context Condition") + 
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 10))
INTERACTION_12



### Is familiarity moderated by age? ###
flexplot(Log_Total_Looks_C~Age_In_Days_Z|Familiarity,method = "lm", data=Test_Data)

flexplot(Log_Total_Looks_C~Age_In_Days_Z|Context,method = "lm", data=Test_Data)

flexplot(Log_Total_Looks~Log_Age_In_Days|Familiarity,method = "lm", data=ID_Data)

flexplot(Log_Total_Looks~Log_Age_In_Days|Context,method = "lm", data=ID_Data)


### Looking times Fam Vs Novel in Vowel Condition ###
Figure4 = ggbetweenstats(data = Vowel_Data,
               x = Familiarity, 
               y = Log_Total_Looks,
               plottype = "box", results.subtitle = F, ylab = "Total Looking Times (Log seconds)",
               xlab = "(a) Vowel Condition") 
               #subtitle = "Figure 4. Difference in Total Looking Times Towards Familiar and Novel Words in Vowel Trials") 


### Looking times Fam Vs Novel in Consonant Condition ###
Figure5 = ggbetweenstats(data = Consonant_Data,
               x = Familiarity, 
               y = Log_Total_Looks,
               plottype = "box", results.subtitle = F, ylab = "Total Looking Times (Log seconds)",
               xlab = "(b) Consonant Condition") 
               #subtitle = "Figure 5. Difference in Total Looking Times Towards Familiar and Novel Words in Consonant Trials") 


ggarrange(Figure4, Figure5)


FAM_CON_interaction_plot = cat_plot(Final, pred = Context, 
                                    modx = Familiarity, geom = "line", 
                                    plot.points = T, point.shape = T)


FAM_AGE_interaction_plot = interact_plot(Final, pred = Log_Age_In_Days_C, 
                                         modx = Familiarity, centered = "none", 
                                         plot.points = T, interval = T, 
                                         int.width = 0.8)

FAM_AGE_interaction_plot

CON_AGE_interaction_plot = interact_plot(Final, pred = Log_Age_In_Days_C, 
                                         modx = Context, centered = "none", 
                                         plot.points = T, interval = T, 
                                         int.width = 0.8)+
  xlab("Age (Centred)") + ylab("Total Looking Times (Log seconds)")
  #labs(subtitle = "Figure 6. Changes in Differences In Looking Timess Between Context Conditions Over Age")

Threeway_interaction_plot = interact_plot(Final, pred = Log_Age_In_Days_C, 
                                         modx = Familiarity, mod2 = Context,
                                         centered = "none", 
                                         plot.points = T, interval = T, 
                                         int.width = 0.8, x.label = "Age (Centered)",
                                         y.label = "Total Looking Times (Log seconds)", 
                                         mod2.labels = c("Consonant Condition", "Vowel Condition"))+
                                        labs(subtitle = "Figure 6. A Threeway Plot of Total Looking Time by Age, Familiarity, and Context")

Threeway_interaction_plot 


### Subjects LTs declination over trials ###
Figure7 = ggplot(data = Test_Data, mapping = aes(x = Trial, y = Log_Total_Looks))+
  geom_smooth(size = 0.5, se = T, level = 0.95, method = "loess")+
  geom_point(alpha = 0.4, jitter = T, size = 0.4)+
  facet_wrap(~SubjectID)+
  xlab("Trial Number") +
  ylab("Total Looking Times (Log seconds)")+
  #labs(title = "Figure 7. Individual Total Looking Times Decline Over Trials")+
  theme(plot.title = element_text(size = 10))


FAM_CON_interaction_plot_12 = cat_plot(Final12, pred = Context, 
                                    modx = Familiarity, geom = "line", 
                                    plot.points = T, point.shape = T)


FAM_AGE_interaction_plot_12 = interact_plot(Final12, pred = Log_Age_In_Days_C, 
                                         modx = Familiarity, centered = "none", 
                                         plot.points = T, interval = T, point.alpha = 0.2,
                                         int.width = 0.8)

CON_AGE_interaction_plot_12 = interact_plot(Final12, pred = Log_Age_In_Days_C, 
                                            modx = Context, centered = "none", 
                                            plot.points = T, interval = T, 
                                            int.width = 0.8)+
  xlab("Age (Centred)") + ylab("Total Looking Times (Log seconds)")
  #labs(subtitle = "Figure 9. Changes in Differences In Looking Times Between Context Conditions Over Age")


Threeway_interaction_plot_12 = interact_plot(Final12, pred = Log_Age_In_Days_C, 
                                          modx = Familiarity, mod2 = Context,
                                          centered = "none", 
                                          plot.points = T, interval = T, 
                                          int.width = 0.8, x.label = "Age (Centered)",
                                          y.label = "Total Looking Times (Log seconds)", 
                                          mod2.labels = c("Consonant Condition", "Vowel Condition"))

Threeway_interaction_plot_12 +
  xlab("Age (Centered)")+ ylab("Total Looking Times (Log seconds)")


### Looking times Fam Vs Novel in Consonant Condition ###
ggbetweenstats(data = Consonant_Data,
               x = Familiarity, 
               y = Log_Total_Looks, results.subtitle = F)


### Looking times Fam Vs Novel in Consonant Condition ###
ggbetweenstats(data = Vowel_Data,
              x = Familiarity, 
              y = Log_Total_Looks, results.subtitle = F)

ggbetweenstats(data = Test_Data,
               x = Context, 
               y = Log_Total_Looks)

#### Familiarity by item boxplots #####
ggstatsplot::ggbetweenstats(data = Test_Data,
                                    x = Item, 
                                    y = Log_Total_Looks_Z,
                                    plottype = "box",
                                    type = "p", 
                                    pairwise.display = "s",
                                    outlier.tagging = TRUE,
                                    outlier.color = "red",
                                    conf.level = 0.95)



ggplot(data = Trials_12_Data,
               aes(x = Log_Age_In_Days_C, y = Log_Total_Looks_C, col = Context))+
  geom_point(alpha = 0.3, position = "jitter", )+
  geom_smooth(method = "lm")+
  theme_minimal()+
  scale_color_manual(name = "Context",
                     labels = c("Consonant", "Vowel"),
                     values = c("darkgreen", "purple"))+
  facet_wrap(~Familiarity)


ggplot(Test_Data, aes(Age_In_Days, Log_Total_Looks, colour = Context)) +
  geom_point(size = 2, alpha = 0.4, position = "jitter") +
  theme_bw() +
  labs(x = "Age In Days") +
  labs(y = "Total Looking Times (Log seconds)") +
  geom_smooth(formula = y~x, method = "loess")+
  facet_wrap(~Familiarity)+
  scale_color_colorblind()


#### Boxplot LTs by Context x Familiarity with Stats ####
ggstatsplot::grouped_ggbetweenstats(data = Test_Data,
                            x = Context, 
                            y = Log_Total_Looks,
                            grouping.var = Familiarity,
                            plottype = "boxplot",
                            type = "p")


grouped_ggbetweenstats(data = ID_Data, 
                      x = Familiarity, 
                      y = Log_Total_Looks, 
                      grouping.var = Context,
                      plottype = "boxplot", 
                      type = "p", plot.type = "box", 
                      p.adjust.methods = "none", 
                      centrality.type = "parametric", 
                      k = 2, 
                      xlab = c("Familiarity Condition", "Familiarity Condition"),
                      ylab = "Total Looking Times (Log seconds)")



#### Boxplot LTs by Familiarity x Context with Stats ####
ggstatsplot::grouped_ggwithinstats(data = Test_Data,
                                    x = Familiarity, 
                                    y = Log_Total_Looks_Z,
                                    grouping.var = Context,
                                    plottype = "boxplot",
                                    type = "p",
                                    pairwise.display = "all",
                                    p.adjust.method = "bonferroni",
                                    outlier.tagging = TRUE,
                                    outlier.color = "red",
                                    conf.level = 0.95)



#### Boxplot LTs by Context (between) with Stats ####
ggbetweenstats(data = Test_Data,
                            x = Context, 
                            y = Log_Total_Looks_Z,
                            plottype = "boxplot",
                            type = "p",
                            p.adjust.method = "bonferroni")

#### Boxplot LTs by Familiarity (within) with Stats ####
ggstatsplot::ggwithinstats(data = Test_Data, 
                           x = Familiarity, 
                           y = Log_Total_Looks, 
                           type = "p", 
                           pairwise.display = "all")



### 
ggstatsplot::grouped_ggdotplotstats(data = Test_Data,
                                    x = Familiarity,
                                    y = Log_Total_Looks,
                                    grouping.var = SubjectID)  

### Scatterplot with regression line Context x Age with Stats ###
ggstatsplot::grouped_ggscatterstats(data = Test_Data,
                       x = Log_Age_In_Days,
                       y = Log_Total_Looks_Z,
                       colour = Context) 
 
### Scatterplot with regression line Familiarity x Age with Stats ###
ggstatsplot::grouped_ggscatterstats(data = Test_Data,
                                    x = Log_Age_In_Days,
                                    y = Log_Total_Looks_Z,
                                    grouping.var = Familiarity) 



ggstatsplot::grouped_ggwithinstats(
  data    = Test_Data,
  x       = Familiarity,
  y       = Log_Total_Looks,
  grouping.var = Context)

  
  

#### ES from Johnson et al ####

Johnsonetal_Interaction_Cohen_EXP1 = F_to_d(f = 8.24, df= 1, df_error = 30, paired = T)
Johnsonetal_FAM_Cohen_EXP1 = F_to_d(f = 4.18, df= 1, df_error = 30, paired = T)
Johnsonetal_Interaction_Cohen_EXP2 = F_to_d(f = 3, df= 1, df_error = 30, paired = T)
Johnsonetal_FAM_Cohen_EXP2 = F_to_d(f = 4.3, df= 1, df_error = 30, paired = T)
Johnsonetal_FAM_Cohen_EXP3 = F_to_d(f = 4.69, df= 1, df_error = 38, paired = T)
Johnsonetal_Interaction_Cohen_EXP3 = F_to_d(f = 6.97, df= 1, df_error = 38, paired = T)







#### Visualizations with untransformed variables ###

Figure1N=ggbetweenstats(data = Test_Data,
                       x = Familiarity, plot.type = "box",
                       centrality.type = "parametric", 
                       y = Total_Looks_Sec, results.subtitle = F, 
                       ylab = "Total Looking Times in seconds", xlab = "(a) Familiarity Condition") 
#subtitle = "Figure 1. Difference in Total Looking Times Towards Familiar and Novel Trials")
Figure1N


### ALL DATA CONTEXT ###
Figure2N=ggbetweenstats(data = Test_Data,
                       x = Context, plot.type = "box",
                       centrality.type = "parametric",
                       y = Total_Looks_Sec, results.subtitle = F,
                       ylab = "Total Looking Times in seconds", xlab = "(b) Context Condition")
#subtitle = "Figure 2. Difference in Total Looking Times Towards Consonant and Vowel Trials")
Figure2N


Figure_1_2N = ggarrange(Figure1N, Figure2N)
Figure_1_2N


INTERACTION_N = flexplot(Total_Looks_Sec ~ Context+Familiarity, data=Test_Data) +
  #labs(title = "Figure 3. Familiarity by Context Interaction Plot") +
  ylab("Total Looking Times in seconds")+
  xlab("Context Condition") + 
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 10))
INTERACTION_N

INTERACTION_12_N = flexplot(Total_Looks_Sec ~ Context+Familiarity, data=Trials_12_Data) +
  #labs(title = "Figure 8. Familiarity by Context Interaction Plot (First 12 trials)") +
  ylab("Total Looking Times in seconds")+
  xlab("Context Condition") + 
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 10))
INTERACTION_12_N




Figure4N = ggbetweenstats(data = Vowel_Data,
                         x = Familiarity, 
                         y = Total_Looks_Sec,
                         plottype = "box", results.subtitle = F, ylab = "Total Looking Times in seconds",
                         xlab = "(a) Vowel Condition") 
#subtitle = "Figure 4. Difference in Total Looking Times Towards Familiar and Novel Words in Vowel Trials") 


### Looking times Fam Vs Novel in Consonant Condition ###
Figure5N = ggbetweenstats(data = Consonant_Data,
                         x = Familiarity, 
                         y = Total_Looks_Sec,
                         plottype = "box", results.subtitle = F, ylab = "Total Looking Times in seconds",
                         xlab = "(b) Consonant Condition") 
#subtitle = "Figure 5. Difference in Total Looking Times Towards Familiar and Novel Words in Consonant Trials") 


ggarrange(Figure4N, Figure5N)


CON_AGE_interaction_plot_N = interact_plot(Final_N, pred = Age_In_Days, 
                                         modx = Context, centered = "none", 
                                         plot.points = T, interval = T, 
                                         int.width = 0.8)+
  xlab("Age in days") + ylab("Total Looking Times in seconds")
#labs(subtitle = "Figure 6. Changes in Differences In Looking Timess Between Context Conditions Over Age")


Figure7N = ggplot(data = Test_Data, mapping = aes(x = Trial, y = Total_Looks_Sec))+
  geom_smooth(size = 0.5, se = T, level = 0.95, method = "loess")+
  geom_point(alpha = 0.4, jitter = T, size = 0.4)+
  facet_wrap(~SubjectID)+
  xlab("Trial Number") +
  ylab("Total Looking Times in seconds")+
  #labs(title = "Figure 7. Individual Total Looking Times Decline Over Trials")+
  theme(plot.title = element_text(size = 10))

CON_AGE_interaction_plot_12N = interact_plot(Final12_N, pred = Age_In_Days, 
                                            modx = Context, centered = "none", 
                                            plot.points = T, interval = T, 
                                            int.width = 0.8)+
  xlab("Age (Centred)") + ylab("Total Looking Times (Log seconds)")
#labs(subtitle = "Figure 9. Changes in Differences In Looking Times Between Context Conditions Over Age")







write.csv(Test_Data, "C:/Users/frog-/OneDrive/Bureau/Radboud study/Thesis MA/Test_Data.csv")
write.csv(Trials_12_Data, "C:/Users/frog-/OneDrive/Bureau/Radboud study/Thesis MA/Trial_12_Data.csv")
