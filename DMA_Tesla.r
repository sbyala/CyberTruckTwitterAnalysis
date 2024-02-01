#1
#Top US Locations Interested in Cybertruck: Total Like Count
# library(plotly)
# library(viridisLite)
# library(sf)
# library(leaflet)
# library(tidyverse)
# library(gt)
# library(tslatwtr)
# library(plotly)
# library(RColorBrewer)
# source("01-clean-tweet.R")

# Extract data for top US locations interested in Cybertruck
us_location_like_data <- list_member |>
  left_join(select(list, list_name, list_id), by = join_by(list_id)) |>
  filter(list_name == "💯 Tesla Creators") |>
  arrange(desc(tweet_count)) |>
  mutate(username = str_c("@", username)) |>
  left_join(tweet, by = c("user_id" = "user_id")) |>
  filter(str_detect(text, regex("cybertruck", ignore_case = TRUE))) |>
  group_by(location) |>
  summarise(total_likes = sum(like_count, na.rm = TRUE)) |>
  filter(str_detect(location, "USA")) |>
  arrange(desc(total_likes)) |>
  slice(1:50)


us_location_like_data <- us_location_like_data |>
  mutate(percentage_contribution = sprintf("%.0f%%", (total_likes / sum(total_likes)) * 100))

#Pie Chart
pastel_colors <- brewer.pal(length(us_location_like_data$location), "Pastel1")

fig <- plot_ly(
  labels = us_location_like_data$location,
  values = us_location_like_data$total_likes,
  type = "pie",
  marker = list(colors = pastel_colors)
) |>
  layout(
    annotations = list(
      title = "Top US Locations Interested in Cybertruck: Total Like Count",
      showlegend = FALSE
    ),
    paper_bgcolor = "rgba(0,0,0,0)",  # Set the background color to transparent
    plot_bgcolor = "rgba(0,0,0,0)"    # Set the plot area background color to transparent
  )
fig

# gt Table
us_location_like_data |>
  gt() |>
  tab_spanner(
    label = md("**Top US Locations Interested in Cybertruck: Total Like Count**"),
    columns = c("location", "total_likes", "percentage_contribution")
  ) |>
  tab_header(
    title = md("**Top US Locations Interested in Cybertruck: Total Like Count**"),
    subtitle = md("Based on Tweets from the 💯 Tesla Creators list")
  ) |>
  cols_label(
    location = md("**Location**"),
    total_likes = md("**Total Like Count**"),
    percentage_contribution = md("**Percentage Contribution**")
  ) |>
  tab_spanner(
    label = md("**Likes**"),
    columns = c("total_likes", "percentage_contribution")
  ) |>
  tab_style(
    style = cell_text(color = "#1F9BF0"),
    locations = cells_body(columns = location)
  ) |>
  fmt_number(columns = c("total_likes", "percentage_contribution"), decimals = 0, suffixing = TRUE) |>
  tab_source_note(
    source_note = md("**Source**: The Twitter API and the {tslatwtr} R package (see \"01-top-ten.R\")")
  ) |>
  tab_options(
    source_notes.font.size = 7,
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.left.color = "transparent",
    table.border.right.color = "transparent"
  )


#2
#Filtering tweets with relevant keywords to filter 

#install.packages("tidytext")
# library(tidyverse)
library(tidytext)

rev_tweet <- tweet |> 
  left_join(list |> select(list_name, list_id, list_name), join_by(list_id)) |> 
  relocate(list_name, .before = list_id) |> 
  left_join(list_member |> select(username, user_id, followers_count, following_count, location, verified), join_by(user_id)) |> 
  relocate(username, .before = user_id) |> 
  filter(is_auto_related & is_tesla_related)

relevant_keywords <- c("cybertruck", "elon", "tesla", "ev", "musk", "elonmusk", "model", "tsla")

highly_engaged_tweets <- rev_tweet |> 
  filter(str_detect(text, paste(relevant_keywords, collapse = "|", sep = "|")))

highly_engaged_tweets <- highly_engaged_tweets |> 
  unnest_tokens(word, text)

word_frequencies <- highly_engaged_tweets |> 
  filter(word %in% relevant_keywords) |> 
  count(word, sort = TRUE)

word_frequencies <- word_frequencies %>%
  mutate(percentage = round(n / sum(n) * 100, 0))

word_frequencies <- word_frequencies[order(word_frequencies$percentage, decreasing = TRUE), ]

#Bar Chart
save2 <- ggplot(word_frequencies, aes(x = reorder(word, percentage), y = percentage, fill = percentage)) +
  geom_col(alpha = 0.8) +
  labs(
    title = "Key Terms in Highly-Engaged Tweets",
    subtitle = "Exploring Keywords Impacting Engagement",
    x = "Key Term",
    y = "Percentage",
    caption = "Data source: tslatwtr"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "top",
  ) +
  scale_fill_gradient(low = "lightcoral", high = "darkred") +
  geom_text(aes(label = sprintf("%.0f%%", percentage)), 
            vjust = 0.5, color = "black", size = 4, position = position_dodge(width = 1)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format())

ggsave("key_terms_in_tweets.png", save2, width = 13.33, height = 7.5, units = "in")

#3
# Visualization 3: Engagement Over Different Periods of a Day

tweet |>
  mutate(hour_category = case_when(
    hour(created_at) %in% 0:5 ~ "Night",
    hour(created_at) %in% 6:11 ~ "Morning",
    hour(created_at) %in% 12:17 ~ "Afternoon",
    TRUE ~ "Evening"
  )) |>
  group_by(hour_category) |>
  summarise(total_retweets = sum(retweet_count, na.rm = TRUE),
            total_replies = sum(reply_count, na.rm = TRUE),
            total_quotes = sum(quote_count, na.rm = TRUE),
            total_likes = sum(like_count, na.rm = TRUE)) |>
  pivot_longer(cols = c(total_retweets, total_replies, total_quotes, total_likes),
               names_to = "Engagement Type", values_to = "Count") |>
  mutate(Log_Count = log1p(Count)) |>
  
  # Plotting log-transformed bar chart for different engagement types over different periods of a day
  ggplot(aes(x = hour_category, y = Log_Count, fill = `Engagement Type`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Log-Transformed Engagement Over Different Periods of a Day",
       x = "Hour Category",
       y = "Log(Count)",
       fill = "Engagement Type") +
  theme_minimal()
ggsave("timeofday.png", time, width = 10, height = 6, units = "in")


#4
# Visualization 4: Engagement Over Different Periods of a Day (Line Chart)

tweet |>
  mutate(hour_category = case_when(
    hour(created_at) %in% 0:5 ~ "Night",
    hour(created_at) %in% 6:11 ~ "Morning",
    hour(created_at) %in% 12:17 ~ "Afternoon",
    TRUE ~ "Evening"
  )) |>
  group_by(hour_category) |>
  summarise(total_retweets = sum(retweet_count, na.rm = TRUE),
            total_replies = sum(reply_count, na.rm = TRUE),
            total_quotes = sum(quote_count, na.rm = TRUE),
            total_likes = sum(like_count, na.rm = TRUE)) |>
  pivot_longer(cols = c(total_retweets, total_replies, total_quotes, total_likes),
               names_to = "Engagement Type", values_to = "Count") |>
  mutate(Log_Count = log1p(Count)) |>
  
  ggplot(aes(x = hour_category, y = Log_Count, color = `Engagement Type`, group = `Engagement Type`)) +
  geom_line() +
  labs(title = "Log-Transformed Engagement Over Different Periods of a Day",
       x = "Hour Category",
       y = "Log(Count)",
       color = "Engagement Type") +
  theme_minimal()


#5
## visulization 5: Likes vs Retweets
# Filter tweets containing 'cybertruck'
tweet1 <- tweet |> filter(str_detect(text, regex("cybertruck", ignore_case = TRUE)))
tweet1

# Mutate date_of_created
tweet2 <- tweet1 |> mutate(date_of_created = as_date(created_at))
tweet2

# Select relevant columns
tweet3 <- tweet2 |> select(text, like_count, retweet_count, reply_count, date_of_created)
tweet3

# Reorder columns
tweet4 <- tweet3[,c(5,1,2,3,4)]
tweet4

# Arrange by date_of_created
tweet5 <- tweet4 |> arrange(date_of_created)
tweet5

# Filter rows with non-NA like_count, retweet_count, and reply_count
tweet6 <- tweet5 |> filter(!is.na(like_count) & !is.na(retweet_count) & !is.na(reply_count))
tweet6

# Summarize max_likes
most_likes <- tweet6 |> summarize(max_likes = max(like_count))
most_likes

# Filter rows with max_likes
max_likes1 <- tweet6 |> filter(like_count == "178228")
max_likes1

# Create a line plot
p <- ggplot(tweet6, aes(x=date_of_created)) +
  geom_line(aes(y=like_count, color = "like_count")) +
  geom_line(aes(y=retweet_count, color = "retweet_count")) +
  geom_line(aes(y = reply_count, color = "reply_count")) +
  labs(x="Date",y="Count", color="Interactions", title="Cybertruck Twitter Traffic", subtitle = "Every Tesla's post on Cybertruck was a hot topic, and people don't seem to lose interest over time.",caption = "Data from tesla twitter package")
p
ggsave(file = "Plot for Cybertruck.png", width = 13.33, height = 7.5, units = "in")

# Create a customized line plot
p <- ggplot(tweet6, aes(x=date_of_created)) +
  geom_line(aes(y=like_count, color = "like_count")) +
  geom_line(aes(y=retweet_count, color = "retweet_count")) +
  geom_line(aes(y = reply_count, color = "reply_count")) +
  labs(x="Date", y="Count", color="Interactions", title="Cybertruck Twitter Traffic", subtitle = "Traffic Analysis for April 2021", caption = "Data from tesla twitter package") +
  scale_color_manual(values = c("like_count" = "blue", "retweet_count" = "red", "reply_count" = "green")) +
  xlim(as.Date("2021-04-01"), as.Date("2021-04-30"))
