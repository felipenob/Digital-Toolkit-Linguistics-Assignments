#felipe_nobrega

library(tidyverse)

# Homework ----------------------------------------------------------------
# Task 1.1: Calculate the percentage of "correct", "incorrect", 
# and "don't know" answers in the two critical conditions.

moses <- read_csv("moses_clean.csv")
questions <- read_csv("questions.csv")

moses.preprocessed <-
  moses |> 
  inner_join(questions, by=c("ITEM", "CONDITION", "LIST")) |>
  select(ID, ITEM, CONDITION, QUESTION, ANSWER, CORRECT_ANSWER) |>
  mutate(ACCURATE = ifelse(CORRECT_ANSWER == ANSWER,
                           yes = "correct",
                           no = ifelse(ANSWER == "dont_know",
                                       yes = "dont_know",
                                       no = "incorrect")),
         CONDITION = case_when(CONDITION == '1' ~ 'illusion',
                               CONDITION == '2' ~ 'no illusion',
                               CONDITION == '100' ~ 'good filler',
                               CONDITION == '101' ~ 'bad filler'))  

moses.preprocessed |>
  filter(CONDITION %in% c('illusion', 'no illusion')) |>
  group_by(CONDITION, ACCURATE) |>
  summarise(count = n()) |>
  mutate(percentage = count / sum(count) * 100)

# Task 1.2: Of all the questions in all conditions, which 
# question was the easiest and which was the hardest?

minmax <- 
  moses.preprocessed |>
  group_by(ITEM, QUESTION, ACCURATE) |>
  summarise(count = n()) |>
  mutate(CORRECT_ANSWERS = count / sum(count) * 100) |>
  arrange(CORRECT_ANSWERS) |>
  filter(ACCURATE == "correct")

head(minmax, 2)
tail(minmax, 2)

# Task 1.3: Of the Moses illusion questions, which question fooled most people?

fool <-
moses.preprocessed |>
  group_by(ITEM, CONDITION, QUESTION, ACCURATE) |>
  summarise(count = n()) |>
  mutate(CORRECT_ANSWERS = count / sum(count) * 100) |>
  filter(CONDITION == 'illusion', 
         ACCURATE == "incorrect") |>
  arrange(CORRECT_ANSWERS) |>
  print(n=Inf)

# Task 1.4: Which participant was the best in answering questions? 
# Who was the worst?

best <-
moses.preprocessed |>
  group_by(ID, ACCURATE) |>
  summarise(count = n()) |>
  mutate(CORRECT_ANSWERS = count / sum(count) * 100) |>
  filter(ACCURATE == "correct") |>
  arrange(CORRECT_ANSWERS) |>
  print(n=Inf)

# Task 2.1
noisy_aj <- read.csv("noisy_aj.csv") 
noisy_aj |>
  group_by(CONDITION) |>
  summarise(MEAN_RATING = mean(RATING),
            SD = sd(RATING))

# Task 2.2 
noisy_rt <- read.csv("noisy_rt.csv") 
noisy_rt |>
  group_by(IA, CONDITION) |>
  summarise(MEAN_RT = mean(RT),
            SD = sd(RT))

# Task 2.3

noisy <- noisy_aj |>
  full_join(noisy_rt)
# full_join(noisy_rt, by=c("ID", "ITEM", "CONDITION")) |> head()

# Lecture 13 May 2024 ------------------------------------------------------

# Noisy data preparation
noisy <- read_csv("noisy.csv")
noisy.rt <- 
  noisy |>
  rename(ID = "MD5.hash.of.participant.s.IP.address",
         SENTENCE = "Sentence..or.sentence.MD5.") |>
  mutate(RT = as.numeric(Reading.time)) |>
  filter(Label == "experiment",
         PennElementType != "Scale",
         PennElementType != "TextInput",
         Reading.time != "NULL",
         RT > 80 & RT < 2000) |>
  select(ID, ITEM, CONDITION, SENTENCE, RT, Parameter) |>
  na.omit()

# Plotting
# Data summary with 1 row per observation
noisy.summary <- 
  noisy.rt |>
  group_by(ITEM, CONDITION, Parameter) |>
  summarise(RT = mean(RT)) |>
  group_by(CONDITION, Parameter) |>
  summarise(MeanRT = mean(RT),
            SD = sd(RT)) |>
  rename(IA = Parameter)

# Plot object
noisy.summary |>
  ggplot() +
  aes(x=as.numeric(IA), y=MeanRT, colour=CONDITION) +
  geom_line() +
  geom_point() +
  facet_wrap(.~CONDITION) +
  stat_sum() +
  # geom_errorbar(aes(ymin=MeanRT-2*SD, ymax=MeanRT+2*SD)) +
  coord_cartesian() +
  theme_classic() +
  labs(x = "Interest area",
       y = "Mean reading time in ms",
       title = "Noisy channel data",
       subtitle = "Reading times only",
       caption = "Additional caption",
       colour="Condition",
       size = "Count")

# Esquisse and Patchwork
library(esquisse)
set_i18n("en") # Set language to English
library(patchwork)

esquisser()

#Chart 1 - Noisy Summary 1
  ggplot(noisy.summary) +
  aes(x = IA, y = MeanRT, fill = SD) +
  geom_col() +
  scale_fill_gradient() +
  labs(
    x = "Interest area",
    y = "Mean reading time in ms",
    title = "Summary - Noisy Channel 1"
  ) +
  theme_void() +
  facet_wrap(vars(CONDITION))

#Chart 2 - How many people answered correctly question 1 from Moses dataset?
  ggplot() +
  aes(x = ACCURATE, y = ITEM) +
  geom_col(fill = "#FF8C00") +
  labs(
    x = "Responses",
    y = "Number of responses",
    title = "Rtesponses from Question 1 - Moses dataset",
    subtitle = "\"According to the Bible,
    how many animals of each kind did Moses take on the ark?\""
  ) +
  theme_minimal()

#Chart 3 - Mean reading times per Interest Area - Noisy Dataset
  ggplot(noisy.summary) +
  aes(x = IA, weight = MeanRT) +
  geom_bar(fill = "#228B22") +
  coord_polar() +
  labs(
    x = "Interest Area",
    y = "Mean reading times in ms",
    title = "Mean reading times for each Interest Area"
  ) +
  theme_light()

#Chart 4 - Ratings by condition
plot4 <- ggplot(noisy_aj) +
  aes(x = CONDITION, weight = RATING) +
  geom_bar(fill = "#FF69B4") +
  labs(title = "Ratings by condition") +
  theme_minimal()

#Chart 5 - Number of responses per type - Moses Dataset
plot5 <- ggplot(moses.preprocessed) +
  aes(x = CONDITION) +
  geom_bar(fill = "#B22222") +
  labs(
    x = "Type of responses",
    y = "Number",
    title = "Type of responses in Moses Dataset"
  ) +
  theme_minimal()

#Chart 6 - Answers for the question 101 - What is the capital of Spain?
plot6 <- moses.preprocessed %>%
  filter(ITEM >= 101L & ITEM <= 101L) %>%
  ggplot() +
  aes(x = ITEM) +
  geom_histogram(bins = 30L, fill = "#B22222") +
  labs(
    x = "Answers",
    y = "Count",
    title = "Answers for the question:",
    subtitle = "\"What is the capital of Spain?\""
  ) +
  theme_minimal() +
  facet_wrap(vars(ANSWER))

#Chart 7 - Responses for the question: What is the name of the semiaquatic, venomous, egg-laying mammal endemic to eastern Australia?
plot7 <- moses %>%
  filter(ITEM >= 104L & ITEM <= 104L) %>%
  ggplot() +
  aes(x = ANSWER) +
  geom_bar(fill = "#A11C5F") +
  labs(title = "Responses for the question: What is the name of the semiaquatic, venomous, egg-laying mammal endemic to eastern Australia?") +
  theme_minimal()

#Chart 8 - Reading times for each Interest Rate
plot8 <- ggplot(noisy_rt) +
  aes(x = RT) +
  geom_histogram(bins = 30L, fill = "#009F00") +
  labs(title = "Reading times for each Interest Rate") +
  theme_minimal() +
  facet_wrap(vars(IA))

#Chart 9 - Accuracy of answers from Moses Dataset
plot9 <- ggplot(moses.preprocessed) +
  aes(x = ACCURATE) +
  geom_bar(fill = "#72493E") +
  labs(title = "Accuracy of answers from Moses Dataset") +
  theme_minimal()

#Chart 10 - Ugly and bad graph
plot10 <- ggplot(noisy.summary) +
  aes(x = MeanRT, y = IA) +
  geom_point(shape = "circle", size = 1.5, colour = "#F80000") +
  theme_minimal()

combined_plot <- (plot1 | plot2 | plot3) / (plot4 | plot5 | plot6) / (plot7 | plot8 | plot9)

print(combined_plot)

ggsave("combined_plots_digital_toolkit_felipe_nobrega.pdf", combined_plot, width = 40, height = 35)

