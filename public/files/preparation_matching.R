# load all packages
detach(package:lubridate)
library(tidyverse)
library(here) # see https://www.tidyverse.org/blog/2017/12/workflow-vs-script/
library(xlsx)
library(stringr)
library(numbers)
library(dplyr)
library(hms)
library(anytime) # for converting Epoch times from oTree Page Time 
here <- here::here # because lubridate also has a here function

# overwrite select and filter functions 
select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename

df1 <- read.csv(here("input", "1st_batch.csv"), sep = ',', header=TRUE) %>% 
  filter(participant._current_app_name == "matching") %>% 
  mutate(session = "batch1")
df7 <- read.csv(here("input", "7th_batch.csv"), sep = ',', header=TRUE) %>% 
  filter(participant._current_app_name == "matching") %>% 
  select(-ends_with("redistribution_complete")) %>% 
  mutate(session = case_when(session.code == "y766tppf" ~ "batch7",
                             session.code == "u91wjnvr" ~ "batch6",
                             session.code == "6rvn5xrc" ~ "batch5",
                             session.code == "ef6q9r58" ~ "batch4",
                             session.code == "2bhmbf38" ~ "batch3",
                             session.code == "7a2m0dy4" ~ "batch2",
                             TRUE ~ "unknown session"))

df <- rbind(df1, df7) #2nd, 3rd, 4th, 5th and 6th batch are included in 7th batch datafile


groups <- df %>% 
  filter(matching.1.player.id_in_group == 1) %>% 
  select(matching.1.group.group_id, matching.1.group.treatment) %>% 
  group_by(matching.1.group.treatment) %>% 
  tally()


timespent1 <- read.csv(here("input", "PageTimes_1st_batch.csv"), sep = ',', header=TRUE)
timespent7 <- read.csv(here("input", "PageTimes_7th_batch.csv"), sep = ',', header=TRUE)

timespent <- rbind(timespent1, timespent7) 

rm(df1, df7, timespent1, timespent7)
# check redistribution -> payment extra
# < 0.50 minutes -> set some data to NA

timespent <- timespent %>%
  rename(participant.code = participant_code) %>%
  mutate(timestamp = anytime(epoch_time)) %>%
  add_count(participant.code, sort=TRUE) %>%
  select(c(participant.code, page_index, timestamp)) %>%
  arrange(timestamp)

timespent <- pivot_wider(timespent, names_from = page_index, names_prefix = "page", values_from = timestamp)
timespent <- timespent %>%
  mutate(time_total = difftime(page56, page1, units = "secs"),
         time_instr = difftime(page4, page1, units = "secs"),
         time_practice = difftime(page6, page4, units = "secs"),
         time_check = difftime(page10, page6, units = "secs"),
         time_decision1 = difftime(page29, page10, units = "secs"),
         time_decision2 = difftime(page31, page29, units = "secs"),
         time_strategy = difftime(page32, page31, units = "secs"),
         time_bayesian = difftime(page33, page32, units = "secs"),
         time_box1 = difftime(page53, page33, units = "secs"),
         time_box2 = difftime(page54, page53, units = "secs"),
         time_demographics = difftime(page55, page54, units = "secs"),
         time_redistribution = difftime(page56, page55, units = "secs"),
         time_total = hms(seconds = as.numeric(time_total)),
         time_instr = hms(seconds = as.numeric(time_instr)),
         time_practice = hms(seconds = as.numeric(time_practice)),
         time_check = hms(seconds = as.numeric(time_check)),
         time_decision1 = hms(seconds = as.numeric(time_decision1)),
         time_decision2 = hms(seconds = as.numeric(time_decision2)),
         time_strategy = hms(seconds = as.numeric(time_strategy)),
         time_bayesian = hms(seconds = as.numeric(time_bayesian)),
         time_box1 = hms(seconds = as.numeric(time_box1)),
         time_box2 = hms(seconds = as.numeric(time_box2)),
         time_demographics = hms(seconds = as.numeric(time_demographics)),
         time_redistribution = hms(seconds = as.numeric(time_redistribution)),
         date = page1
         ) %>%
  select(-starts_with("page"))

df <- merge(df, timespent, by="participant.code", all.x = TRUE)  %>%
  select(starts_with(c("matching", "participant", "session", "time", "date"))) 
rm(timespent)

thenames <- names(df)
function.clean_varnames <-  function(text){
  if(grepl("player", text, perl=TRUE | grepl("matching", text, perl=TRUE))){
    if(grepl("matching", text, perl=TRUE)){text <- sub(pattern = "matching.", replacement = "", x = text)}
    if(grepl(".player", text, perl=TRUE)){text <- sub(pattern = ".player", replacement = "", x = text)} 
    text <- strsplit(text, '[.]')
    text <- text[[1]][c(2,1)]
    text <- paste(text, collapse='_')
  }
  else {text <- text}} 
thenames <- lapply(thenames, function.clean_varnames)
names(df) <- thenames 
rm(thenames, function.clean_varnames)

df <- df %>% 
  rename(id = prolific_id_1,
         id_otree = participant.code,
         treatment = matching.3.group.treatment,
         check1 = uq_1_1,
         check2 = uq_2_1,
         check3 = uq_3_1,
         check4 = uq_4_1,
         id_in_group = id_in_group_1,
         browser = browser_1,
         instr_opened = total_opened_2,
         decision0 = pref0_1, # practice round decision
         decision1 = pref_1,
         decision2 = pref_2,
         rank1 = position1_1,
         rank2 = position2_1,
         allocation1 = allocation_1,
         allocation2 = allocation_2,
         success1 = successful_1,
         success2 = successful_2,
         payoff1 = payoff_1,
         payoff2 = payoff_2,
         bayesian = bayesian_2,
         box1 = boxes_collected_2,
         box2 = boxes_collected_3,
         bomb1 = bomb_2,
         bomb2 = bomb_3,
         edu = edu_3,
         gender = gender_3,
         strategy = strategy_2,
         feedback = feedback_3,
         work_other = other_text_3,
         bonus_task = selected_3,
         bonus_payoff = bonus_payoff_3,
         group_id = matching.1.group.group_id,
         redistribution = redistribution_3,
         payoff_extra = payoff_extra_3,
         attention = attention_3) %>% 
  mutate(browser = case_when(grepl("Pad", browser, perl = TRUE) ~ "Tablet",
                             grepl("Mobile", browser, perl = TRUE) ~ "Mobile",
                             grepl("Phone", browser, perl = TRUE) ~ "Mobile",
                             TRUE ~ "Desktop"),
         rank0 = 3, # by definition
         valuationsb0 = as.numeric(substr(valuations0_1,6,7)),
         valuationsb1 = as.numeric(case_when(substr(valuations1_1,7,9) == "0]" ~ "0",
                                  TRUE ~ substr(valuations1_1,7,9))),
         valuationsb2 = as.numeric(case_when(substr(valuations2_1,7,9) == "0]" ~ "0",
                                  TRUE ~ substr(valuations2_1,7,9))),
         hint0 = case_when(treatment == "Partial Info" ~ substr(hint_1, 3,3), TRUE ~ NA_character_),
         hint1 = case_when(treatment == "Partial Info" ~ substr(hint_1, 8,8), TRUE ~ NA_character_),
         hint2 = case_when(treatment == "Partial Info" ~ substr(hint_1, 13,13), TRUE ~ NA_character_),
         gender = factor(gender, levels = c(0,1,2,3),
                         labels = c("Male", "Female", "Non-binary", "Rather not say")),
         bonus_payoff = case_when(bonus_task > 2 ~ bonus_payoff,
                                  bonus_task == 1 ~ 0.0032 * payoff1,
                                  bonus_task == 2 ~ 0.0032 * payoff2),         
         bonus_task = factor(bonus_task, levels = c(1:5),
                             labels = c("Decision 1", "Decision 2", "Cookie question", "Bomb task 1", "Bomb task 2")),
         work_fulltime = case_when(grepl("[0]", edu, perl = TRUE) ~ 1, TRUE ~ 0),
         work_parttime = case_when(grepl("[1]", edu, perl = TRUE) ~ 1, TRUE ~ 0),
         work_student = case_when(grepl("[2]", edu, perl = TRUE) ~ 1, TRUE ~ 0),
         work_retired = case_when(grepl("[3]", edu, perl = TRUE) ~ 1, TRUE ~ 0),
         work_care = case_when(grepl("[4]", edu, perl = TRUE) ~ 1, TRUE ~ 0)) %>%
  group_by(group_id) %>% 
  mutate(group_payoff1 = sum(payoff1),
         group_payoff2 = sum(payoff2),
         group_completed = case_when(group_payoff1 == 0 ~ 0,
                                     TRUE ~ 1),
         group_completed = factor(group_completed, levels = c(0,1), labels = c("Not yet", "Completed"))) %>% 
  rename(rank1_alt2 = position1_alt2_1,
         rank1_alt3 = position1_alt3_1,
         rank1_alt4 = position1_alt4_1,
         rank1_alt5 = position1_alt5_1,
         rank1_alt6 = position1_alt6_1,
         rank1_alt7 = position1_alt7_1,
         rank1_alt8 = position1_alt8_1,
         rank1_alt9 = position1_alt9_1,
         rank1_alt10 = position1_alt10_1,
         rank1_alt11 = position1_alt12_1,
         rank1_alt12 = position1_alt12_1,
         rank1_alt13 = position1_alt13_1,
         rank1_alt14 = position1_alt14_1,
         rank1_alt15 = position1_alt15_1,
         rank1_alt16 = position1_alt16_1,
         rank1_alt17 = position1_alt17_1,
         rank1_alt18 = position1_alt18_1,
         rank1_alt19 = position1_alt19_1,
         rank1_alt20 = position1_alt20_1,
         rank1_alt21 = position1_alt21_1,
         rank1_alt22 = position1_alt22_1,
         rank1_alt23 = position1_alt23_1,
         rank1_alt24 = position2_alt24_1,
         rank2_alt2 = position2_alt2_1,
         rank2_alt3 = position2_alt3_1,
         rank2_alt4 = position2_alt4_1,
         rank2_alt5 = position2_alt5_1,
         rank2_alt6 = position2_alt6_1,
         rank2_alt7 = position2_alt7_1,
         rank2_alt8 = position2_alt8_1,
         rank2_alt9 = position2_alt9_1,
         rank2_alt10 = position2_alt10_1,
         rank2_alt11 = position2_alt12_1,
         rank2_alt12 = position2_alt12_1,
         rank2_alt13 = position2_alt13_1,
         rank2_alt14 = position2_alt14_1,
         rank2_alt15 = position2_alt15_1,
         rank2_alt16 = position2_alt16_1,
         rank2_alt17 = position2_alt17_1,
         rank2_alt18 = position2_alt18_1,
         rank2_alt19 = position2_alt19_1,
         rank2_alt20 = position2_alt20_1,
         rank2_alt21 = position2_alt21_1,
         rank2_alt22 = position2_alt22_1,
         rank2_alt23 = position2_alt23_1,
         rank2_alt24 = position2_alt24_1,
         payoff1_alt2 = payoff_alt2_1,
         payoff1_alt3 = payoff_alt3_1,
         payoff1_alt4 = payoff_alt4_1,
         payoff1_alt5 = payoff_alt5_1,
         payoff1_alt6 = payoff_alt6_1,
         payoff1_alt7 = payoff_alt7_1,
         payoff1_alt8 = payoff_alt8_1,
         payoff1_alt9 = payoff_alt9_1,
         payoff1_alt10 = payoff_alt10_1,
         payoff1_alt11 = payoff_alt11_1,
         payoff1_alt12 = payoff_alt12_1,
         payoff1_alt13 = payoff_alt13_1,
         payoff1_alt14 = payoff_alt14_1,
         payoff1_alt15 = payoff_alt15_1,
         payoff1_alt16 = payoff_alt16_1,
         payoff1_alt17 = payoff_alt17_1,
         payoff1_alt18 = payoff_alt18_1,
         payoff1_alt19 = payoff_alt19_1,
         payoff1_alt20 = payoff_alt20_1,
         payoff1_alt21 = payoff_alt21_1,
         payoff1_alt22 = payoff_alt22_1,
         payoff1_alt23 = payoff_alt23_1,
         payoff1_alt24 = payoff_alt24_1,
         payoff2_alt2 = payoff_alt2_2,
         payoff2_alt3 = payoff_alt3_2,
         payoff2_alt4 = payoff_alt4_2,
         payoff2_alt5 = payoff_alt5_2,
         payoff2_alt6 = payoff_alt6_2,
         payoff2_alt7 = payoff_alt7_2,
         payoff2_alt8 = payoff_alt8_2,
         payoff2_alt9 = payoff_alt9_2,
         payoff2_alt10 = payoff_alt10_2,
         payoff2_alt11 = payoff_alt11_2,
         payoff2_alt12 = payoff_alt12_2,
         payoff2_alt13 = payoff_alt13_2,
         payoff2_alt14 = payoff_alt14_2,
         payoff2_alt15 = payoff_alt15_2,
         payoff2_alt16 = payoff_alt16_2,
         payoff2_alt17 = payoff_alt17_2,
         payoff2_alt18 = payoff_alt18_2,
         payoff2_alt19 = payoff_alt19_2,
         payoff2_alt20 = payoff_alt20_2,
         payoff2_alt21 = payoff_alt21_2,
         payoff2_alt22 = payoff_alt22_2,
         payoff2_alt23 = payoff_alt23_2,
         payoff2_alt24 = payoff_alt24_2,
         timeout_decision1 = timeout1_1,
         timeout_decision2 = timeout2_2,
         timeout_redistr = timeout_redistribution_3) %>%
select(c("session.code", "date",
  "id", "id_otree", "time_total", "group_id", "treatment", "bonus_task",
  "bonus_payoff", "group_completed", "group_payoff1", "group_payoff2", "id_in_group", starts_with("check"), "instr_opened",
  "rank1", "hint1", "decision1", "allocation1", "success1", "valuationsb1", "payoff1",
  "rank2", "hint2", "decision2", "allocation2", "success2", "valuationsb2", "payoff2", "redistribution", "payoff_extra",
  "bayesian", "box1", "box2", "bomb1", "bomb2", "gender", starts_with("work"),
  "strategy", "feedback", "browser", "attention", "session",
  starts_with("time_"), starts_with("payoff1"), starts_with("payoff2"), starts_with("rank1"), starts_with("rank2")
  ))  %>% 
  arrange(date) %>% 
  ungroup() %>% 
  # mutate_at(vars("bonus_payoff"), case_when(time_total < 0:00:30 ~ NA, TRUE ~ .))
  # mutate_at(
  # vars("bonus_payoff"),
  # funs(case_when(time_total < 0:00:30 ~ NA, TRUE ~ .)))
  # mutate_at(vars(c("bonus_payoff")), ~ case_when(time_total < 0:00:30 ~ `is.na<-`(., TRUE), TRUE ~ .))
  mutate_at(vars(c("bayesian", "box1", "box2", "bomb1", "bomb2", "gender", starts_with("work"), "strategy", "feedback")), 
            ~ case_when(time_total < 30 ~ `is.na<-`(., TRUE), TRUE ~ .))


# calculate theoretical maximum of each group
max1 <- df %>% 
  group_by(group_id) %>% 
  arrange(desc(valuationsb1), .by_group = TRUE) %>% 
  do(head(.,2)) %>% 
  mutate(maximum1 = sum(valuationsb1) + 2 * 300) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  select(group_id, maximum1) 

max2 <- df %>% 
  group_by(group_id) %>% 
  arrange(desc(valuationsb2), .by_group = TRUE) %>% 
  do(head(.,2)) %>% 
  mutate(maximum2 = sum(valuationsb2) + 2 * 300) %>%
  filter(row_number()==1) %>% 
  ungroup() %>% 
  select(group_id, maximum2) 

df <- merge(df, max1, by = "group_id")
df <- merge(df, max2, by = "group_id")
rm(max1, max2)


# indicate all duplicates as duplicate
df <- df %>% 
  arrange(date) %>% 
  group_by(id) %>% 
  mutate(attempt = case_when(id == "" ~ as.integer(1), 
                             TRUE ~ as.integer(row_number()))) %>% 
  ungroup()

# check duplicates session 3 and up
duplicates <- df %>% 
  mutate(duplicate = case_when(id == "" ~ "False", 
                               duplicated(id) ~ "True", TRUE ~ "False")) %>% 
  select(date, duplicate, id, time_total, session) %>% 
  filter(duplicate == "True") 
# %>% 
#   filter(session != "batch2")

duplicates2 <- df %>% 
  group_by(id) %>% 
  mutate(n = n(),
         treatments_duplicates = paste(treatment, collapse = ""),
         treatments_dup_equal = case_when(treatments_duplicates == "Partial InfoPartial Info" ~ 1,
                                          treatments_duplicates == "Full InfoFull Info" ~ 1,
                                          treatments_duplicates == "No InfoNo Info" ~ 1, 
                                          TRUE ~ 0)) %>% 
  ungroup() %>% 
  mutate(duplicate = case_when(id == "" ~ "False",
                               n > 1 ~ "True", 
                               TRUE ~ "False"),
         auto_advanced = case_when(time_instr > 23*60*60 ~ "auto_advanced",
                                   time_practice > 23*60*60 ~ "auto_advanced",
                                   time_check > 23*60*60 ~ "auto_advanced",
                                   time_decision1 > 23*60*60 ~ "auto_advanced",
                                   time_decision2 > 23*60*60 ~ "auto_advanced", 
                                   TRUE ~ "human")) %>% 
  filter(duplicate == "True") %>% 
  select(date, id, id_otree, attempt, treatments_duplicates, treatments_dup_equal, auto_advanced, session)

# write.xlsx(as.data.frame(duplicates2), here("clean data", "sensitive", "check_duplicates.xlsx"), row.names=FALSE)

rm(duplicates2)

#### keep the first one if both scenario decisions are made 
#### keep the first completed one (i.e., second or third attempt) if up until that point all treatments are the same 
#### if in the incomplete attempts somebody got past the first scenario decision, delete then all attempts
df2 <- df %>% 
  mutate(invalid = case_when(id_otree %in% c("7sd66cxz", "mjckfxz9", "3xxmsash", "ictleetp",
                                             "u17e54pg", "x9dazp9y") ~ "duplicate",               # remove second if both are human  
                             id_otree %in% c("miywiunt", "dwrgkr7o", "9mtk3mru",                  # remove first if treatments are equal
                                             "4h67g4gx", "nedqe4pu", "qye2jbuo", "ic9l5aq7") ~ "duplicate", 
                             id_otree %in% c("p0zq42bg", "ifuhtj20", "apxo83ho", "39uljbgp",      # remove both
                                             "o1z37p01", "596gklxs", "y27ckrq2", "y68s5e71",
                                             "mu433xpb", "2bhgswyi", "rna7dl5w", "ch49qso7",
                                             "yh8aimtt", "10m09kfa", "qhhm9cqi", "ojqprehy",
                                             "hdol39a8", "ws5qlgl2", "yfid2gl7", "sdx99ev2",
                                             "j5zqcz7g", "vhqkp60o", "lubtx3zb", "yf77nxqd",
                                             "757f9qw5", "89df62cx", "d8idffyt", "njad7ldj",
                                             "xf24on6o", "ln9nvrii", "qkjtbq1u", "4c0mts0w",
                                             "y4dd1r4x", "71jg72fg", "x2zkxisr", "hckjo00f", 
                                             "a5i0wvup", "09qdumtd", "u4ksah5h", "eorfs0va") ~ "duplicate",
                             id == "" ~ "auto_advanced",                                          # auto advanced after 24h
                             time_instr > 23*60*60 ~ "auto_advanced",
                             time_practice > 23*60*60 ~ "auto_advanced",
                             time_check > 23*60*60 ~ "auto_advanced",
                             time_decision1 > 23*60*60 ~ "auto_advanced",
                             time_decision2 > 23*60*60 ~ "auto_advanced",
                             TRUE ~ ""))

# delete all <- with only bots or duplicates
df3 <- df2 %>% 
  mutate(valid = case_when(invalid == "" ~ "yes", TRUE ~ "")) 

groups <- df3 %>% 
  mutate(duplicate = case_when(invalid == "duplicate" ~ 1, TRUE ~ 0),
         bot = case_when(invalid == "auto_advanced" ~ 1, TRUE ~ 0)) %>% 
  select(valid, group_id, treatment, invalid, session, bot, duplicate) %>% 
  group_by(group_id)  

write.csv(as.data.frame(groups), here("clean data", "group_counts.csv"), row.names=FALSE)

df3 <- df3 %>% 
  group_by(group_id) %>% 
  mutate(ids_group = paste(valid, collapse = ""),
         all_auto_advanced = case_when(ids_group == "" ~ 0, TRUE ~ 1)) %>% 
  filter(all_auto_advanced == 1) %>% 
  select(-all_auto_advanced, -ids_group) %>%
  filter(!is.na(time_total))


df <- df3
rm(df2, df3)

# set all human-input variables of duplicates and auto-advanced to missing
df <- df %>% 
  mutate_at(vars(c("attention", "bonus_task", "payoff_extra", "bonus_payoff",
                   "bayesian", "box1", "box2", "bomb1", "bomb2", 
                   "gender", starts_with("work"), starts_with("check"), "instr_opened",
                   "strategy", "feedback")), 
          ~ case_when(invalid != "" ~ `is.na<-`(., TRUE), TRUE ~ .))

# change order of columns
df <- df[c(sort(names(df)))]
firstcols = c("session", "date", "group_id", "id_otree", "time_total", "invalid", "treatment", "bonus_task", "bonus_payoff", "payoff_extra", "feedback")
df <- df[,c(firstcols, setdiff(names(df), firstcols))]

# manual fixes after message on prolific
df <- df %>% 
  mutate(gender = case_when(id_otree == 'fpmutoza' ~ "Male", TRUE ~ as.character(gender)),
         gender = as.factor(gender),
         bayesian = case_when(id_otree == 'i2n29b3e' ~ 60, TRUE ~ bayesian))

# write.xlsx(as.data.frame(df), here("clean data", "sensitive", "all_batches_up_to_5.xlsx"), row.names=FALSE)

batch4 <- df %>% 
  filter(session == "batch4")
# write.xlsx(as.data.frame(batch4), here("clean data", "sensitive", "batch4_clean.xlsx"), row.names=FALSE)

batch6 <- df %>% 
  filter(session == "batch6")
# write.xlsx(as.data.frame(batch6), here("clean data", "sensitive", "batch6_clean.xlsx"), row.names=FALSE)

batch7 <- df %>% 
  filter(session == "batch7")
# write.xlsx(as.data.frame(batch7), here("clean data", "sensitive", "batch7_clean.xlsx"), row.names=FALSE)

rm(batch4, batch6, batch7)
# update number of groups per treatment

# save all data together

write.csv(as.data.frame(df), here("clean data", "sensitive", "all_batches.csv"), row.names=FALSE)


groups <- df %>% 
  select(group_id, treatment, valid, session, invalid) %>% 
  group_by(group_id) %>% 
  mutate(all_human_count = paste(valid, collapse = ""),
         num_human = case_when(all_human_count == "yesyesyesyes" ~ 4, 
                               all_human_count == "yesyesyes" ~ 3, 
                               all_human_count == "yesyes" ~ 2, 
                               all_human_count == "yes" ~ 1, 
                               TRUE ~ 0),
         no_duplicates = paste(invalid, collapse = ""),
         num_duplicates = case_when(no_duplicates == "auto_advancedduplicate" ~ 1,
                                    no_duplicates == "auto_advancedduplicateauto_advanced" ~ 1,
                                    no_duplicates == "duplicateauto_advancedauto_advanced" ~ 1,
                                    no_duplicates == "auto_advancedauto_advancedduplicate" ~ 1,
                                    no_duplicates == "duplicate" ~ 1,
                                    no_duplicates == "duplicateduplicate" ~ 2,
                                    no_duplicates == "duplicateauto_advanced" ~ 1,
                                    no_duplicates == "duplicateduplicateauto_advanced" ~ 2,
                                    TRUE ~ 0)) %>% 
  group_by(group_id, num_duplicates, treatment, session, num_human) %>% 
  tally()

table(groups$num_duplicates, groups$treatment, groups$session)
table(groups$num_human, groups$treatment)
table(groups$treatment)


rm(duplicates, groups, firstcols)

df <- df %>% 
  mutate(payoff_total = bonus_payoff + payoff_extra) # this excludes the 1.50 showup fee

# Preparing data for publication: remove all private or irrelevant info
df <- df %>% 
  select(-c("bonus_task", "bonus_payoff", "payoff_extra", "group_completed", "id"))

write.csv(as.data.frame(df), here("clean data", "all_batches_public.csv"), row.names=FALSE)

