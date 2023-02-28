library(tidyverse)
library(lme4)
library(sjPlot)

gpt_responses_categorized_tsv <- "data/output/results_merged_old_new_last2_categorized.tsv"
gpt_responses_categorized <- read_tsv(gpt_responses_categorized_tsv)

# delete prompt and response text columns, as they are not needed for the analysis
# for this round we'll also get rid of logprobs because we're not using them
# and in this case it's all using davinci-002
gpt_responses_categorized <- gpt_responses_categorized %>% select(-prompt, -response, -logprobs, -gtp3_model)

# most importantly "null" should be the first level, the reference.
gpt_responses_categorized$modifier <- factor(gpt_responses_categorized$modifier, levels = c("null", "truthful", "friendly", "agreeable", "agreeable_b", "creative", "warm", "outgoing", "humorous", "intellectual", "skilful", "attentive", "withdrawn", "neutral", "ambitious", "outspoken", "frank", "straightforward", "skeptical", "upfront"))
gpt_responses_categorized$question <- as.factor(gpt_responses_categorized$question)
gpt_responses_categorized$question_wrapper <- as.factor(gpt_responses_categorized$question_wrapper)
gpt_responses_categorized$interaction_type <- factor(gpt_responses_categorized$interaction_type, levels = c("zero_shot", "single_shot"))


# reorder response_type so the order is "affirmation", "noncomittal", "negation" (worst to best outcome)
gpt_responses_categorized$response_type <- factor(gpt_responses_categorized$response_type, levels = c("affirmation", "noncomittal", "negation"))


# add column for question_wrappers that contain the phrase "That's so cool!"
gpt_responses_categorized$question_wrapper_type <- ifelse(grepl("That's so cool!", gpt_responses_categorized$question_wrapper), "positive", "standard")

gpt_responses_categorized$question_wrapper_type <- factor(gpt_responses_categorized$question_wrapper_type, levels = c("positive", "standard"))


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
ggsave("data/output/by_modifier.png", width = 20, height = 20)


# plot response type distribution by modifier (only single_shot)

gpt_responses_categorized %>%
  filter(interaction_type %in% c("single_shot")) %>%
  ggplot(aes(x = modifier, fill = response_type)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Response type distribution by modifier",
    x = "Response type",
    y = "Count"
  )
ggsave("data/output/by_modifier_single_shot.png", width = 20, height = 20)


# plot response type distribution by modifier (only some)

gpt_responses_categorized %>%
  filter(modifier %in% c("truthful", "creative", "skilful", "attentive")) %>%
  ggplot(aes(x = modifier, fill = response_type)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Response type distribution for Truthful, vs Truthful + (Creative, Skilful, Attentive)",
    x = "Response type",
    y = "Count"
  )
ggsave("data/output/by_modifier_relevant.png", width = 8)


# plot response type distribution by modifier (only some)

gpt_responses_categorized %>%
  filter(modifier %in% c("truthful", "agreeable", "attentive", "friendly")) %>%
  ggplot(aes(x = modifier, fill = response_type)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Response type distribution for Truthful, vs Truthful + (Agreeable, Attentive, Friendly)",
    x = "Response type",
    y = "Count"
  )
ggsave("data/output/by_modifier_relevant2.png", width = 8)

# plot response type distribution by modifier (only some)

gpt_responses_categorized %>%
  filter(modifier %in% c("truthful", "outgoing", "withdrawn")) %>%
  ggplot(aes(x = modifier, fill = response_type)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Response type distribution for Truthful, vs Truthful + (Agreeable, Attentive, Friendly)",
    x = "Response type",
    y = "Count"
  )
ggsave("data/output/by_modifier_relevant2.png", width = 8)



# plot to compare response types for "truthful" and "agreeable" modifiers

gpt_responses_categorized %>%
  #filter(interaction_type %in% c("single_shot")) %>%  
  filter(modifier %in% c("truthful", "agreeable")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + agreeable response type distribution",
    x = "Modifier",
    y = "Count"
  )

# plot to compare response types for "truthful" and "outgoing" modifiers

gpt_responses_categorized %>%
  #filter(interaction_type %in% c("single_shot")) %>%  
  filter(modifier %in% c("truthful", "outgoing")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + outgoing response type distribution",
    x = "Modifier",
    y = "Count"
  )


# save plot as png
ggsave("data/output/aaagaaa.png")

# plot to compare response types for "truthful" and "attentive" modifiers

gpt_responses_categorized %>%
  #filter(interaction_type %in% c("single_shot")) %>%  
  filter(modifier %in% c("truthful", "attentive")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + attentive response type distribution",
    x = "Modifier",
    y = "Count"
  )


# save plot as png
ggsave("data/output/aaagaaa.png")

# plot to compare response types for "truthful" and "warm" modifiers

gpt_responses_categorized %>%
  #filter(interaction_type %in% c("single_shot")) %>%  
  filter(modifier %in% c("truthful", "warm")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + warm response type distribution",
    x = "Modifier",
    y = "Count"
  )


# save plot as png
ggsave("data/output/aaagaaa.png")



# plot to compare response types for "truthful" and "skeptical" modifiers

gpt_responses_categorized %>%
  #filter(interaction_type %in% c("single_shot")) %>%  
  filter(modifier %in% c("truthful", "skeptical")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + skeptical response type distribution",
    x = "Modifier",
    y = "Count"
  )


# save plot as png
ggsave("data/output/truthful_vs_truthful_agreeable_response_type_distribution.png")






# plot to compare response types for "truthful" and "creative" modifiers

gpt_responses_categorized %>%
  #filter(interaction_type %in% c("single_shot")) %>%  
  filter(modifier %in% c("truthful", "creative")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + creative response type distribution",
    x = "Modifier",
    y = "Count"
  )

# save plot as png
ggsave("data/output/creative.png")



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

# plot to compare response types for "truthful" and "skeptical" modifiers

gpt_responses_categorized %>%
  filter(modifier %in% c("truthful", "skeptical")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + skeptical response type distribution",
    x = "Modifier",
    y = "Count"
  ) +
  facet_wrap(~interaction_type)

# save plot as png
ggsave("data/output/truthful_vs_truthful_skeptical_response_type_distribution_by_interaction_type.png")


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



# plot to compare truthful and agreeable modifiers by question_wrapper_type
gpt_responses_categorized %>%
  filter(modifier %in% c("truthful", "outgoing")) %>%
  ggplot(aes(x = response_type, fill = modifier)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Truthful vs Truthful + outgoing response type distribution by question wrapper type",
    x = "Response type",
    y = "Count"
  ) +
  facet_wrap(~question_wrapper_type)

# save plot as png
ggsave("data/output/tsdsdsd.png")
