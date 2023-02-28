library(tidyverse)
library(lme4)
library(sjPlot)

gpt_responses_categorized_tsv <- "data/output/2022_10_02_13_26_48_results_categorized.tsv"
gpt_responses_categorized <- read_tsv(gpt_responses_categorized_tsv)

# delete prompt and response text columns, as they are not needed for the analysis
# for this round we'll also get rid of logprobs because we're not using them
# and in this case it's all using davinci-002
gpt_responses_categorized <- gpt_responses_categorized %>% select(-prompt, -response, -logprobs, -gtp3_model)

# most importantly "null" should be the first level, the reference.
gpt_responses_categorized$modifier <- factor(gpt_responses_categorized$modifier, levels = c("null", "truthful", "friendly", "agreeable", "agreeable_b"))
gpt_responses_categorized$question <- as.factor(gpt_responses_categorized$question)
gpt_responses_categorized$question_wrapper <- as.factor(gpt_responses_categorized$question_wrapper)
gpt_responses_categorized$interaction_type <- factor(gpt_responses_categorized$interaction_type, levels = c("zero_shot", "single_shot"))


# reorder response_type so the order is "affirmation", "noncomittal", "negation" (worst to best outcome)
gpt_responses_categorized$response_type <- factor(gpt_responses_categorized$response_type, levels = c("affirmation", "noncomittal", "negation"))


# add column for question_wrappers that contain the phrase "That's so cool!"
gpt_responses_categorized$question_wrapper_type <- ifelse(grepl("That's so cool!", gpt_responses_categorized$question_wrapper), "positive", "standard")

gpt_responses_categorized$question_wrapper_type <- factor(gpt_responses_categorized$question_wrapper_type, levels = c("positive", "standard"))

# now do regression analysis
m1 <- glmer(response_type ~
              modifier +
              interaction_type +
              question_wrapper_type +
              (1 | question),
              data = gpt_responses_categorized,
              family = binomial(link = "logit"),
              glmerControl(optimizer = "bobyqa",
                optCtrl = list(maxfun = 100000)))
print(summary(m1))
plot(m1)
plot_model(m1,
            type = "std2",
            title = "Both zero-shot and single-shot",
            show.intercept = TRUE)
# now do regression analysis only on single shot
m2 <- glmer(response_type ~
              modifier +
              question_wrapper_type +
              (1 | question),
              data = gpt_responses_categorized[
                gpt_responses_categorized$interaction_type == "single_shot", ],
              family = binomial(link = "logit"),
              glmerControl(optimizer = "bobyqa",
                optCtrl = list(maxfun = 100000)))
print(summary(m2))
plot(m2)
plot_model(m2, type = "std2", title = "Single-shot only", show.intercept = TRUE)

# plot overall response type distribution
gpt_responses_categorized %>%
  ggplot(aes(x = response_type)) +
  geom_bar() +
  labs(
    title = "Overall response type distribution",
    x = "Response type",
    y = "Count"
  )

# plot response type distribution by modifier

gpt_responses_categorized %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar() +
  labs(
    title = "Response type distribution by modifier",
    x = "Response type",
    y = "Count"
  )


# plot response type distribution by modifier

gpt_responses_categorized %>%
  ggplot(aes(x = modifier, fill = response_type)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Response type distribution by modifier",
    x = "Response type",
    y = "Count"
  )

# plot to compare response types for "null" and "truthful" modifiers

gpt_responses_categorized %>%
  filter(modifier %in% c("null", "truthful")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Null vs truthful response type distribution",
    x = "Modifier",
    y = "Count"
  )

# save plot as png
ggsave("data/output/null_vs_truthful_response_type_distribution.png")

# plot to compare response types for "truthful" and "agreeable" modifiers

gpt_responses_categorized %>%
  filter(modifier %in% c("truthful", "agreeable")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + agreeable response type distribution",
    x = "Modifier",
    y = "Count"
  )

# save plot as png
ggsave("data/output/truthful_vs_truthful_agreeable_response_type_distribution.png")


# plot to compare response types for "truthful" and "friendly" modifiers

gpt_responses_categorized %>%
  filter(modifier %in% c("truthful", "friendly")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + friendly response type distribution",
    x = "Modifier",
    y = "Count"
  )

# save plot as png
ggsave("data/output/truthful_vs_truthful_friendly_response_type_distribution.png")

# plot to compare response types for interaction types
gpt_responses_categorized %>%
  ggplot(aes(x = response_type, fill = interaction_type)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Response type distribution by interaction type",
    x = "Response type",
    y = "Count"
  )

# save plot as png
ggsave("data/output/response_type_distribution_by_interaction_type.png")

# plot to compare response types by question_wrapper_type
gpt_responses_categorized %>%
  ggplot(aes(x = response_type, fill = question_wrapper_type)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Response type distribution by question wrapper type",
    x = "Response type",
    y = "Count"
  )

# save plot as png
ggsave("data/output/response_type_distribution_by_question_wrapper_type.png")

# plot to compare response types for "truthful" and "agreeable" modifiers

gpt_responses_categorized %>%
  filter(modifier %in% c("truthful", "agreeable")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + agreeable response type distribution",
    x = "Modifier",
    y = "Count"
  ) +
  facet_wrap(~interaction_type)

# save plot as png
ggsave("data/output/truthful_vs_truthful_agreeable_response_type_distribution_by_interaction_type.png")


# plot to compare truthful and agreeable modifiers by question_wrapper_type
gpt_responses_categorized %>%
  filter(modifier %in% c("truthful", "agreeable")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + agreeable response type distribution by question wrapper type",
    x = "Response type",
    y = "Count"
  ) +
  facet_wrap(~question_wrapper_type)

# save plot as png
ggsave("data/output/truthful_vs_agreeable_response_type_distribution_by_question_wrapper_type.png")
