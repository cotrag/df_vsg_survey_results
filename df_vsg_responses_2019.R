df_vsg_2019 <- read_csv("VOTER_Survey_Jan217_Release1-csv.csv")

# test=> predicting ft trump based on general views then label conservative,
# more predictive?

# respondent's views on econ trends
df_vsg_2019$econtrend_2019
# 1==better, 3==worse, 4==don't know, 8==skipped

# decribed as a conservative
df_vsg_2019$selfdescr_ccap_7_baseline
# 1== yes, 2==no


# break up big banks-- POLCY
df_vsg_2019$policy3_2019
# 1== strong sup, 5==strong opp, remove 8 for skip


# tax credits low income-- POLICY
df_vsg_2019$policy6_2019
# 1== strong sup, 5==strong opp, remove 8 for skip


# trumpapproval (outcome variable)
df_vsg_2019$trumpapp_2019


# filter nonsubstantive results
df_vsg_2019 <- df_vsg_2019 %>%
  filter(econtrend_2019 != 4)  %>%
  filter(econtrend_2019 != 8) %>%
  filter(policy3_2019 != 8) %>%
  filter(policy6_2019 != 8) %>%
  filter(trumpapp_2019 != 5) %>%
  filter(trumpapp_2019 != 8)

typeof(df_vsg_2019$trumpapp_2019)

df_vsg_2019$trumpapp_2019_factor <-factor(df_vsg_2019$trumpapp_2019)


# Ensure non-substantive results are filtered out
ggplot(df_vsg_2019, aes(econtrend_2019)) +
  geom_bar(fill = "#0073C2FF") +
  theme_minimal()

ggplot(df_vsg_2019, aes(selfdescr_ccap_7_baseline)) +
  geom_bar(fill = "#0073C2FF") +
  theme_minimal()

ggplot(df_vsg_2019, aes(policy3_2019)) +
  geom_bar(fill = "#0073C2FF") +
  theme_minimal()

ggplot(df_vsg_2019, aes(policy6_2019)) +
  geom_bar(fill = "#0073C2FF") +
  theme_minimal()

ggplot(df_vsg_2019, aes(trumpapp_2019)) +
  geom_bar(fill = "#0073C2FF") +
  theme_minimal()

# run multinomiam regression. The response variable is ordered from 1 to 4
# strong approve, somewhat approve, somewhat disapprove, strongly disappprove.
# Must use this model because linear regression cannot be used on categorical
# outcome surveys, distance from one level to another may not be equal.
# Model 1 is nested into model 2. 


model <- polr(formula = trumpapp_2019_factor ~ econtrend_2019 + 
                policy3_2019 + policy6_2019, data = df_vsg_2019)

model_2 <- polr(formula = trumpapp_2019_factor ~ econtrend_2019 + 
                  policy3_2019 + policy6_2019 + selfdescr_ccap_7_baseline, 
                data = df_vsg_2019)

summary(model)
summary(model_2)
stargazer(model, type = "html", out = "model.html")
stargazer(model_2, type = "html", out = "model_2.html")


# ALL EXPLANATORY VARIABLES ARE SIGNIFICANT
# EACH COEFFICICENT CAN BE INTERPRETED AS THE ODDS OF INCREASING FROM ONE LEVEL
# TO THE NEXT, IN THIS CASE, LOWERING A LEVEL OF TRUMP'S APPROVAL, BY A FACTOR
# OFEXP(BETA). ONE CAN USE INVLOGIT(INTERCEPT VALUE) TO FIND THE ODDS OF HAVING
# A CERTAIN LEVEL OF SUPPORT FOR TRUMP GIVEN THE EXPLANATORY VARIABLES ARE 0.
# AIC IS
# BECAUSE OF THE LOWER AIC, MODEL 2 IS A STRONGER MODEL.
# THUS, SELF IDENTIFICATION IS AN IMPORTANT ASPECT OF PRESIDENTIAL APPROVAL,
# EVEN WHEN PENALIZING FOR ADDING AN EXTRA EXPLANATORY VARIABLE.







