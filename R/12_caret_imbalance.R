source(here::here("R", "utilities.R"))
library(caret)
library(gbm)
library(fontcm)

# Load and setup for filling NA values =========================================
orig <- loadRData(here("data", "tidy", "multiclass_county_collapsed.Rda"))
prop(orig, "gen2020")
# gen2020
#    Mail In person Not voted 
#    83.0       5.3      11.7

orig %>% map_dbl(~ sum(is.na(.x)))
#                voter_id                 gen2020                 pri2020 
#                       0                       0                       0 
#                 gen2018                 pri2018                 gen2016 
#                       0                       0                       0 
#                 pri2016                 gen2014                 pri2014 
#                       0                       0                       0 
# permanent_mail_in_voter     in_person_vote_date       registration_date 
#                     386                 3515093                     386 
#           status_reason                  status    residential_zip_code 
#                     386                     386                     386 
#       residential_state        residential_city     residential_address 
#                     386                     386                     386 
#             state_house            state_senate           congressional 
#                     386                     386                     386 
#             county_full             vote_method                   party 
#                       0                  425478                       0 
#                  gender                     yob               last_name 
#                   63619                  224905                       0 
#              first_name         residential_zip            ballot_style 
#                      95                     266                  224905 
#                   phone              preference             middle_name 
#                     173                     383                     162 
#              party_hist             county_hist               party_all 
#                       0                       0                    2253 
#                     age              age_groups             reg_gen2020 
#                  224905                  224906                     386 
#             reg_gen2016             reg_gen2018             reg_gen2014 
#                     386                     386                     386 
#             reg_pri2020             reg_pri2016             reg_pri2018 
#                     386                     386                     386 
#             reg_pri2014                 reg_bin      county_designation 
#                     386                       0                       0 
#             voted_party   received_party_ballot           reject_reason 
#                 3457075                 2605175                 3672127 
#                rejected                  county 
#                       0                       0 
df <- orig %>% select(-county, -in_person_vote_date)
assert_that(!any(is.na(df)))

# Parameters ===================================================================
## dp = how much to downsample the majority class?
dp <- c(0.05, 0.1, 0.2)
metric <- "prAUC"
alg <- c("gbm", "ranger")
yvar <- "gen2020"

# Setup and run downsampling ===================================================
perf_list <- vector("list", length = length(alg))
names(perf_list) <- alg
perf_list <- perf_list %>%
  imap(~ set_names(vector("list", length = length(dp)), nm = dp))

for (algx in alg) {
  for (dpx in dp) {
    ## Data prep
    t1 <- multiclass_train_prep(df)
    t2 <- downSample_custom(t1, p = dpx)
    dim(t2$traind)
    prop(t2$traind, yvar)
    fname <- here(
      "output",
      paste0(algx, "_caret_", metric, "_downsample_", dpx * 100, ".Rda")
    )
    
    ## Run and save
    if (!file.exists(fname)) {
      model_down <- train(
        gen2020 ~ .,
        data = t2$traind, trControl = t2$tc, method = algx, metric = metric
      )
      save(model_down, file = fname)
    } else {
      load(fname)
    }
    
    gc(reset = TRUE)
    
    # Performance summary
    temp2 <- pred_df(t2, model_down)
    perf_list[[algx]][[dpx]] <- 
      multiClassSummary(temp2, lev = levels(t2$test[[yvar]]))
    save(perf_list, file = here("output", "perf_list.Rda"))
    
    message(paste0("Task finished: ", algx, ", ", dpx, "."))
  }
}

# Importance ===================================================================
varImp(model_down)

## Export; manual label added
pdf_varimp(
  model_down,
  filename = here(
    "fig", 
    paste0(alg, "_caret_", metric, "_downsample_", dp * 100, "_varimp.pdf")
  ),
  font = "CM Roman",
  # labels = rev(
  #   c(
  #     "Reg. date",
  #     "Voted by mail at pri. 2020",
  #     "Inactive",
  #     "Did not vote at gen. 2016",
  #     "Voted by mail at gen. 2018",
  #     "Did not vote at pri. 2020",
  #     "Voted by mail at gen. 2016",
  #     "Did not vote at gen. 2018",
  #     "Year of birth",
  #     "Republican"
  #   )
  # ),
  width = 5, height = 3
)

## yardstick figures
x <- roc_curve(temp2, obs, In_person, Mail, Not_voted)
x <- x %>% mutate(.level = gsub("_", " ", .level))
pdf(
  here(
    "fig", 
    paste0(alg, "_caret_", metric, "_downsample_", dp * 100, "_roc_curve.pdf")
  ),
  width = 6, height = 3
)
print(pdf_default(autoplot(x)))
dev.off()

x <- pr_curve(temp2, obs, In_person, Mail, Not_voted)
x <- x %>% mutate(.level = gsub("_", " ", .level))
pdf(
  here(
    "fig", 
    paste0(alg, "_caret_", metric, "_downsample_", dp * 100, "_pr_curve.pdf")
  ),
  width = 6, height = 3
)
print(pdf_default(autoplot(x)))
dev.off()
