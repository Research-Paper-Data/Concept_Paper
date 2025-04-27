# R Analysis for structural topic modelling (STM) and word embedding (WE) analysis of biodiversity financing-related literature


###------Description--------:  
#  With the data (Concept Paper_file/Data/Academic_literature/WoS/D_groups), we applied the STM and WE in data analysis for the paper: Navigating the conceptual jungle: Unpacking biodiversity, conservation,and nature finance". 
#  The paper aims to answering the following two research Questions: 
#    RQ1: Are different concepts preferred in different research communities?
#    RQ2: Which topics underpin these concepts across different research communities, and how have they evolved over time?
#    RQ3: In what semantic contexts are these concepts used across research communities? 



## Install and load packages
required_packages <- c("tidyverse", "revtools", "here", "readxl", "openxlsx", "stm", "LDAvis", "stringr", "bibliometrix", "tokenizers", "text2vec", "tm", "Rtsne", "ggplot2","patchwork")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)


getwd()

# Set working directory
setwd("/Users/username/Desktop/Concept Paper_file") 

# Load necessary library
#library(dplyr)

# Function to load and preprocess data
load_and_preprocess <- function(file_path, source_name) {
  read_bibliography(file_path) %>%
    rename(RA = "research_areas") %>%
    filter(!is.na(abstract), !is.na(journal)) %>%
    mutate(source = source_name)
}

# Load data separately using the function
data_bio <- load_and_preprocess("Data/Academic_literature/WoS/D_groups/savedrecs_tit_biofin.bib", "Bio")
data_con <- load_and_preprocess("Data/Academic_literature/WoS/D_groups/savedrecs_tit_confin.bib", "Con")
data_nat <- load_and_preprocess("Data/Academic_literature/WoS/D_groups/savedrecs_tit_natfin.bib", "Nat")

# write.xlsx(data_bio, "Output/data_bio_list.xlsx")
# search_results <- read_excel("Output/data_bio_list.xlsx")
# 
# write.xlsx(data_con, "Output/data_con_list.xlsx")
# search_results <- read_excel("Output/data_con_list.xlsx")
# 
# write.xlsx(data_nat, "Output/data_nat_list.xlsx")
# search_results <- read_excel("Output/data_nat_list.xlsx")

# Merge the datasets
combined_data <- bind_rows(data_bio, data_con, data_nat)

####STEP 1: Descriptive analysis on publication landscapes___________________________

# Group and summarize the data, and plot
summary_data <- combined_data %>% 
  group_by(year, source) %>% 
  summarize(n = n()) %>% 
  drop_na()

custom_colors <- c("Bio" = "#1f77b4", "Con" = "darkgreen", "Nat" = "pink")

ggplot(summary_data, aes(x = year, y = n, color = source, group = source)) + 
  geom_line(size = 1.2) + 
  scale_color_manual(values = custom_colors) + 
  theme_classic() + 
  labs(title = "Publications per Year by Source", x = "Year", y = "Number of Publications", color = "Source")

# Remove duplicates
cleaned_data <- distinct(combined_data, title, .keep_all = TRUE)

# Save the cleaned dataset to Excel
write.xlsx(cleaned_data, "Output/cleaned_data_list.xlsx")


#Define the function to preprocess the data (this function will be used again later)
preprocess_data <- function(data) {
  data %>%
    select(abstract, year, journal, author, title, RA) %>%
    mutate(RA1 = str_extract(RA, "^[^,;&-]+"),
           RA1 = str_replace(RA1, "[\\\\-]+$", "")) %>%
    mutate(RC = case_when(
      str_detect(RA1, "Business") ~ "G1:Business",
      str_detect(RA1, "Arts|Cultural Studies|Development Studies|Geography|International Relations|Literature|Philosophy|Psychology|Urban Studies") ~ "G2:Social Sciences and Humanities",
      str_detect(RA1, "Biodiversity|Environmental Sciences|Life Sciences|Meteorology|Oceanography|Physics|Plant Sciences|Water Resources|Science") ~ "G3:Natural Sciences",
      str_detect(RA1, "Agriculture|Computer Science|Energy|Engineering|Forestry|Thermodynamics") ~ "G4:Applied Sciences and Engineering",
      TRUE ~ "Other"  # Add a default case
    ))
}

# Preprocess the dataset: sub_cleaned_data01
sub_cleaned_data01 <- preprocess_data(cleaned_data)

# Define to create plot for publications over time 
# Define custom colors for each research discipline group
custom_colors <- c("G1:Business" = "#F8766D", "G2:Social Sciences and Humanities" = "#00BA38", "G3:Natural Sciences" = "#00BFC4", "G4:Applied Sciences and Engineering" = "#C77CFF")

# Publications over time by research discipline group
create_publication_plot <- function(data, title) {
    data %>%
    group_by(year, RC) %>%
    summarize(n = n(), .groups = 'drop') %>%
    drop_na() %>%
    ggplot(aes(y = n, x = year, fill = RC, group = RC)) +
    geom_area(alpha = 0.6) +
    theme_minimal() +
    scale_fill_manual(values = custom_colors) +
    labs(title = title, x = "Year", y = "Number of Publications", fill = "Research Community")
}

# Create plots for the dataset
plot_claened_data <- create_publication_plot(sub_cleaned_data01, "Publications over Time by Research Community")

# Display the combined plot
print(plot_claened_data )

## Data preprocessing_for three concept paper groups

# Preprocess each datasets
sub_data_bio01 <- preprocess_data(data_bio)
sub_data_con01 <- preprocess_data(data_con)
sub_data_nat01 <- preprocess_data(data_nat)

# Create plots for each dataset
plot_bio <- create_publication_plot(sub_data_bio01, "Publications over Time by Research Community_Biodiversity Finance")
plot_con <- create_publication_plot(sub_data_con01, "Publications over Time by Research Community_Conservation Finance")
plot_nat <- create_publication_plot(sub_data_nat01, "Publications over Time by Research Community_Nature Finance")

# Combine the plots into one figure
combined_plot <- plot_bio / plot_con / plot_nat

# Display the combined plot
print(combined_plot)

#write.xlsx(sub_cleaned_data01, "Output/sub_data_bio01.xlsx")

####STEP 2: Fit STM model for each concepts groups (bio-con-nat)__________________

# Define function to process text data and prepare documents for STM
process_and_prepare_stm <- function(data) {
  processed <- textProcessor(data$abstract, metadata = data %>% select(year, journal, author, title, abstract, RA, RC))
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
  out$meta <- out$meta %>%
    mutate(year = as.numeric(year),
           RC = as.factor(RC))
  return(out)
}

# Define function to fit STM models
fit_stm_model <- function(docs, vocab, meta, K) {
  model <- stm(documents = docs, vocab = vocab, K = K, prevalence = ~ year + RC, max.em.its = 500, data = meta, init.type = "Spectral")
  prep <- estimateEffect(1:K ~ year + RC, model, metadata = meta)
  labels <- labelTopics(model, n = 10)$prob
  list(model = model, prep = prep, labels = labels)
}

# Define function to plot summary using base R
plot_summary <- function(model, title) {
  plot(model, type = "summary", xlim = c(0, 0.5), main = title)
}

# Define function to plot effect estimates using base R
plot_effect_estimates <- function(prep, model, labels, title) {
  par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
  for (i in 1:3) {
    plot(prep, "year", method = "continuous", topics = i, model = model, printlegend = FALSE, xlab = "year", main = paste(labels[i, 1:3], collapse = " "))
    plot(prep, "RC", method = "pointestimate", topics = i, model = model, printlegend = FALSE, xlab = "RC", main = paste(labels[i, 1:3], collapse = " "))
  }
  title(main = title, outer = TRUE, line = -1)
}


# Set the number of topics
K <- 3

# Process and fit STM models for each dataset
bio_out <- process_and_prepare_stm(sub_data_bio01)
bio_results <- fit_stm_model(bio_out$documents, bio_out$vocab, bio_out$meta, K)

con_out <- process_and_prepare_stm(sub_data_con01)
con_results <- fit_stm_model(con_out$documents, con_out$vocab, con_out$meta, K)

nat_out <- process_and_prepare_stm(sub_data_nat01)
nat_results <- fit_stm_model(nat_out$documents, nat_out$vocab, nat_out$meta, K)


# Visualization with LDAvis
toLDAvis(mod = bio_results$model, docs = bio_out$documents)
toLDAvis(mod = con_results$model, docs = con_out$documents)
toLDAvis(mod = nat_results$model, docs = nat_out$documents)

#Generate the HTML Files in R

# Generate JSON data for LDAvis using toLDAvis()
bio_json <- toLDAvis(mod = bio_results$model, docs = bio_out$documents)
con_json <- toLDAvis(mod = con_results$model, docs = con_out$documents)
nat_json <- toLDAvis(mod = nat_results$model, docs = nat_out$documents)

# Use serVis() to save the visualizations to the correct directories
serVis(bio_json, out.dir = "bio_vis", open.browser = FALSE) 
serVis(con_json, out.dir = "con_vis", open.browser = FALSE)
serVis(nat_json, out.dir = "nat_vis", open.browser = FALSE)


# Plot summaries for each dataset
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
plot_summary(bio_results$model, "Biodiversity Finance")
plot_summary(con_results$model, "Conservation Finance")
plot_summary(nat_results$model, "Nature Finance")

# Print the top 10 words in each topic for each dataset
cat("Biodiversity Finance Labels:\n")
print(bio_results$labels)
cat("Conservation Finance Labels:\n")
print(con_results$labels)
cat("Nature Finance Labels:\n")
print(nat_results$labels)

## Create a table to present the topics in the paper
# Function to process label data
process_labels <- function(labels, label_name) {
  # Convert the labels to a data frame
  labels_df <- as.data.frame(labels)
  # Add a topic column to identify which topic each row corresponds to
  labels_df$Topic <- paste(label_name, "Topic", 1:nrow(labels_df))
  # Reorder columns to display the topic as the first column
  labels_df <- labels_df[, c("Topic", names(labels_df)[-ncol(labels_df)])]
  return(labels_df)
}

# Apply the function to each dataset
bio_labels_df <- process_labels(bio_results$labels, "Biodiversity Finance")
con_labels_df <- process_labels(con_results$labels, "Conservation Finance")
nat_labels_df <- process_labels(nat_results$labels, "Nature Finance")
# Combine the data frames into one if you want to analyze them together
all_labels_df <- rbind(bio_labels_df, con_labels_df, nat_labels_df)
# Print each individual table (or all of them together)
print(bio_labels_df)
print(con_labels_df)
print(nat_labels_df)
# Or print the combined table
print(all_labels_df)


# Plot effect estimates for each dataset
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
plot_effect_estimates(bio_results$prep, bio_results$models, bio_results$labels, "Effects on Biodiversity Finance Topics")
plot_effect_estimates(con_results$prep, con_results$models,con_results$labels, "Effects on Conservation Finance Topics")
plot_effect_estimates(nat_results$prep, nat_results$models,nat_results$labels, "Effects on Nature Finance Topics")


# Find and save top documents per topic

# Define the function to find and save top documents per topic
find_save_top_docs <- function(model, texts, data, file_name = "Top30Articles.xlsx") {
  thoughts <- findThoughts(model, texts = texts, n = 10, topics = 1:model$settings$dim$K)
  top_docs <- as.data.frame(t(as.data.frame(thoughts$index))) %>% as_tibble()
  
  labels <- labelTopics(model, n = 4)$prob
  topic_names <- matrix(apply(labels[, 1:3], 1, paste, collapse = ', '), ncol = 1) %>%
    cbind(number = 1:model$settings$dim$K, .) %>%
    as_tibble() %>%
    mutate(number = as.numeric(number)) %>%
    rename(name = V2)
  
  wb <- createWorkbook()
  for (i in topic_names$number) {
    df <- data[top_docs[i, ] %>% as.numeric, c("author", "year", "journal", "title", "abstract", "RA", "RC")]
    sheet_name <- paste(topic_names$name[which(as.numeric(as.character(topic_names$number)) == i)])
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, df)
  }
  save_path <- paste(here(), "Analysis", file_name, sep = "/")
  saveWorkbook(wb, save_path, overwrite = TRUE)
}

# Fit STM models for each dataset
fit_and_save_model <- function(data, K, file_name) {
# Process the data for STM
out <- process_and_prepare_stm(data)
  
# Fit the STM model
model <- fit_stm_model(out$documents, out$vocab, out$meta, K)$model
  
# Get the text data
texts <- data$abstract
  
# Save top documents
find_save_top_docs(model, texts, data, file_name)
}

# Number of topics
K <- 3

# Create workbooks for each dataset
fit_and_save_model(sub_data_bio01, K, "Top30Articles_Bio.xlsx")
fit_and_save_model(sub_data_con01, K, "Top30Articles_Con.xlsx")
fit_and_save_model(sub_data_nat01, K, "Top30Articles_Nat.xlsx")


###STEP3 Analysis: Word Embeddings ###------------------------------------------------------------------

# Define a helper function to check if a word is numeric
is_not_numeric <- function(word) {
  return(!grepl("^[0-9]+$", word))
}

# Define the function to process data for a given RC value
process_data_for_RC <- function(data, RC_value, stop_words) {
  filtered_data <- data %>% 
    filter(RC == RC_value) %>% 
    select(abstract)
  
# Tokenize text data
  tokens <- tokenize_words(filtered_data$abstract)
  
# Prepare data for text2vec
  it <- itoken(tokens, progressbar = FALSE)
  vocab <- create_vocabulary(it)
  vectorizer <- vocab_vectorizer(vocab)
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
  
# Train GloVe model
  glove_model <- GlobalVectors$new(rank = 100, x_max = 10)
  word_vectors <- glove_model$fit_transform(tcm, n_iter = 10)
  word_vectors <- word_vectors + t(glove_model$components)
  
# Find closest words for phrases
  find_closest_words <- function(word_vectors, target_phrase, stop_words, n = 30) {
    words <- strsplit(target_phrase, " ")[[1]]
    if (any(!words %in% rownames(word_vectors))) return(NULL)
    
    phrase_vector <- colMeans(word_vectors[words, , drop = FALSE])
    similarities <- sim2(word_vectors, matrix(phrase_vector, nrow = 1), method = "cosine")
    closest <- sort(similarities[, 1], decreasing = TRUE)
    
    closest_words <- names(closest)[!names(closest) %in% stop_words]
    closest_words <- closest_words[sapply(closest_words, is_not_numeric)]  # Exclude numeric words
    return(closest_words[1:n])
  }
  
  closest_to_biodiversity_finance <- find_closest_words(word_vectors, "biodiversity finance", stop_words)
  closest_to_conservation_finance <- find_closest_words(word_vectors, "conservation finance", stop_words)
  closest_to_nature_finance <- find_closest_words(word_vectors, "nature finance", stop_words)
  
  create_closest_words_df <- function(closest_words, phrase) {
    data.frame(word = closest_words, rank = 1:length(closest_words), phrase = phrase, RC = RC_value)
  }
  
  df_biodiversity_finance <- create_closest_words_df(closest_to_biodiversity_finance, "Biodiversity Finance")
  df_conservation_finance <- create_closest_words_df(closest_to_conservation_finance, "Conservation Finance")
  df_nature_finance <- create_closest_words_df(closest_to_nature_finance, "Nature Finance")
  
  bind_rows(df_biodiversity_finance, df_conservation_finance, df_nature_finance)
}

# Define the function to process the entire dataset without distinguishing RCs
process_data_general <- function(data, stop_words) {
  filtered_data <- data %>% 
    select(abstract)
  
# Tokenize text data
  tokens <- tokenize_words(filtered_data$abstract)
  
# Prepare data for text2vec
  it <- itoken(tokens, progressbar = FALSE)
  vocab <- create_vocabulary(it)
  vectorizer <- vocab_vectorizer(vocab)
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
  
# Train GloVe model
  glove_model <- GlobalVectors$new(rank = 100, x_max = 10)
  word_vectors <- glove_model$fit_transform(tcm, n_iter = 10)
  word_vectors <- word_vectors + t(glove_model$components)
  
# Find closest words for phrases
  find_closest_words <- function(word_vectors, target_phrase, stop_words, n = 30) {
    words <- strsplit(target_phrase, " ")[[1]]
    if (any(!words %in% rownames(word_vectors))) return(NULL)
    
  phrase_vector <- colMeans(word_vectors[words, , drop = FALSE])
    similarities <- sim2(word_vectors, matrix(phrase_vector, nrow = 1), method = "cosine")
    closest <- sort(similarities[, 1], decreasing = TRUE)
    
    closest_words <- names(closest)[!names(closest) %in% stop_words]
    closest_words <- closest_words[sapply(closest_words, is_not_numeric)]  # Exclude numeric words
    return(closest_words[1:n])
  }
  
  closest_to_biodiversity_finance <- find_closest_words(word_vectors, "biodiversity finance", stop_words)
  closest_to_conservation_finance <- find_closest_words(word_vectors, "conservation finance", stop_words)
  closest_to_nature_finance <- find_closest_words(word_vectors, "nature finance", stop_words)
  
  create_closest_words_df <- function(closest_words, phrase) {
    data.frame(word = closest_words, rank = 1:length(closest_words), phrase = phrase, RC = "General")
  }
  
  df_biodiversity_finance <- create_closest_words_df(closest_to_biodiversity_finance, "Biodiversity Finance")
  df_conservation_finance <- create_closest_words_df(closest_to_conservation_finance, "Conservation Finance")
  df_nature_finance <- create_closest_words_df(closest_to_nature_finance, "Nature Finance")
  
  bind_rows(df_biodiversity_finance, df_conservation_finance, df_nature_finance)
}

# List of stop words
stop_words <- c(stopwords("en"), "an", "a", "for", "on", "in", "of", "to", "at", "the", "by", "and", "also", "but", "will", "however", "based", "can", "may")

# Apply the function to each RC group
RC_values <- c("G1:Business", "G2:Social Sciences and Humanities", "G3:Natural Sciences", "G4:Applied Sciences and Engineering")
all_closest_words <- lapply(RC_values, function(RC) process_data_for_RC(sub_cleaned_data01, RC, stop_words))

# Apply the function to the entire dataset without distinguishing RCs
general_closest_words <- process_data_general(sub_cleaned_data01, stop_words)

# Combine results
combined_closest_words_df <- bind_rows(all_closest_words)
general_closest_words_df <- bind_rows(general_closest_words)

# Split the data into two parts
split_index <- nrow(combined_closest_words_df) %/% 2
first_half <- combined_closest_words_df[1:split_index, ]
second_half <- combined_closest_words_df[(split_index + 1):nrow(combined_closest_words_df), ]

# Define custom colors
custom_colors <- c("Biodiversity Finance" = "#1f77b4", "Conservation Finance" = "darkgreen", "Nature Finance" = "pink")

# Plotting the first half of the data
plot1 <- ggplot(first_half, aes(x = reorder(word, -rank), y = rank, fill = phrase)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ RC + phrase, scales = "free_y", ncol = 3) +  # Set ncol to 3 to have 3 columns
  coord_flip() +
  scale_fill_manual(values = custom_colors) + 
  labs(title = "Closest Words to the Concepts (G1 and G2)", x = "Words", y = "Rank (Lower is More Similar)") +
  theme_minimal() +
  theme(legend.position = "none")

# Plotting the second half of the data
plot2 <- ggplot(second_half, aes(x = reorder(word, -rank), y = rank, fill = phrase)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ RC + phrase, scales = "free_y", ncol = 3) +  # Set ncol to 3 to have 3 columns
  coord_flip() +
  scale_fill_manual(values = custom_colors) + 
  labs(title = "Closest Words to the Concepts (G3 and G4)", x = "Words", y = "Rank (Lower is More Similar)") +
  theme_minimal() +
  theme(legend.position = "none")

# Plotting the general result
plot_general <- ggplot(general_closest_words_df, aes(x = reorder(word, -rank), y = rank, fill = phrase)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ phrase, scales = "free_y", ncol = 3) +  # Set ncol to 1 to have 1 column
  coord_flip() +
  scale_fill_manual(values = custom_colors) + 
  labs(title = "Closest Words to the Concepts", x = "Words", y = "Rank (Lower is More Similar)") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plots
print(plot1)
print(plot2)
print(plot_general)
