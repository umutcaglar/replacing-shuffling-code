# Libraries
require("tidyverse")
require("Matrix")

# Functions
# 1. shuffle vector
vector_shuffle_function = function(n_length, n_shuffle, keep_first_last_letters = FALSE)
{
  #browser()
  initial_vec = seq(1:n_length)
  initial_vec_arch = initial_vec
  
  if(keep_first_last_letters)
  {initial_vec = initial_vec %>% tail(.,-1) %>% head(.,-1)}
  
  n_shuffle = ifelse(test = n_shuffle > length(initial_vec), yes = length(initial_vec), no = n_shuffle)
  
  selected_elements = sample(x = initial_vec, size = n_shuffle, replace = FALSE) %>% sort()
  
  if(keep_first_last_letters)
  {initial_vec = c(head(initial_vec_arch, 1), initial_vec, tail(initial_vec_arch,1))}
  
  if(n_shuffle > 1)
  {
    accidental_correct_place = 1
    while(accidental_correct_place != 0)
    {
      mixed_selected_elements = sample(x = selected_elements)
      accidental_correct_place = sum(mixed_selected_elements == selected_elements)
    }
  }else{
    mixed_selected_elements = selected_elements
  }
  
  
  final_vector = initial_vec
  final_vector[selected_elements] = mixed_selected_elements
  
  return(final_vector)
}

# shuffled_vec = vector_shuffle_function(n_length = 10, n_shuffle = 3, keep_first_last_letters = TRUE)
# shuffled_vec


# Letters Dictionary Vector
letters_vec = c(letters[1:26], LETTERS[1:26], seq(from = 0, to = 9))

# 2 Function to convert letters to vector
convert_word_to_numvec = function(word_ex, letters_vec_ = letters_vec)
{
  word_ex %>% 
    strsplit(., "") %>% 
    .[[1]] %>% 
    match(x = .,  letters_vec_) -> numeric_vector
  
  return(numeric_vector)
}

#convert_word_to_numvec(word_ex = "hello")



# 3 Function to convert num vector to letters
convert_numvec_to_letters = function(num_vec, letters_vec_ = letters_vec)
{
  num_vec %>% 
    letters_vec[.] %>% 
    paste(., collapse = "") -> word_
  
  return(word_)
}

convert_numvec_to_letters(num_vec = c(8, 5, 12, 12, 15))

# 4 shuffle word function
shuffle_word = function(word_, n_shuffle_a, keep_first_last_letters = FALSE)
{
  # browser()
  word_ %>% strsplit(., "") %>% .[[1]] %>% length() -> word_length
  
  if(word_length == 1)
  {
    mixed_word_ = word_
  }else{
    n_shuffle_a = ifelse(test = n_shuffle_a > word_length, yes = word_length, no = n_shuffle_a)
    
    word_vec = convert_word_to_numvec(word_ex = word_)
    
    vec0_unshuffled = seq(1:word_length)
    vec_mixed = vector_shuffle_function(n_length = word_length, 
                                        n_shuffle = n_shuffle_a, 
                                        keep_first_last_letters)
    
    sparseMatrix(i = vec0_unshuffled, j = vec_mixed) %>% 
      as.matrix() -> matrix_out
    matrix_out = matrix_out + 0
    
    word_vec_mixed = word_vec %*% matrix_out
    
    mixed_word_ = convert_numvec_to_letters(num_vec = word_vec_mixed)
  }
  
  return(mixed_word_)
}

#shuffle_word(word_ = "0123", n_shuffle_a = 3, keep_first_last_letters = TRUE)




# 5 vector replace function
vector_replace_function = function(n_length, n_replace = n_replace_a, keep_first_last_letters = FALSE)
{
  initial_vec = seq(1:n_length)
  initial_vec_arch = initial_vec
  
  if(keep_first_last_letters)
  {initial_vec = initial_vec %>% tail(.,-1) %>% head(.,-1)}
  
  n_replace = ifelse(test = n_replace > length(initial_vec), yes = length(initial_vec), no = n_replace)
  
  selected_elements = sample(x = initial_vec, size = n_replace, replace = FALSE) %>% sort()
  
  if(keep_first_last_letters)
  {initial_vec = c(head(initial_vec_arch, 1), initial_vec, tail(initial_vec_arch,1))}
  
  accidental_correct_place = 1
  while(accidental_correct_place != 0)
  {
    replaced_selected_elements = sample(x =  length(letters_vec), size = n_replace, replace = TRUE)
    accidental_correct_place = sum(replaced_selected_elements == selected_elements)
  }
  
  final_list = list(selected_elements, replaced_selected_elements)
  
  return(final_list)
}



# 6 Replace letters in word function

replace_word = function(word_, n_replace_a, keep_first_last_letters = FALSE)
{
  word_ %>% strsplit(., "") %>% .[[1]] %>% length() -> word_length
  n_replace_a = ifelse(test = n_replace_a > word_length, yes = word_length, no = n_replace_a)
  
  word_vec = convert_word_to_numvec(word_ex = word_)
  
  vec_replace_list = vector_replace_function(n_length = word_length, 
                                             n_replace = n_replace_a, 
                                             keep_first_last_letters)
  
  vec_replaced = word_vec
  vec_replaced[ vec_replace_list[[1]] ] = vec_replace_list[[2]]
  
  replaced_word_ = convert_numvec_to_letters(num_vec = vec_replaced)
  
  return(replaced_word_)
}










# 7. Paragraph to Words Function

paragraph_to_word_vector = function(paragraph_) 
{
  paragraph_ %>% strsplit(x = ., split = " ") -> paragraph_vector 
  return(paragraph_vector)
}

paragraph_to_word_vector(paragraph_ = "hello world") 

# Key words list
key_words_list = c("is", "are", "am", "will", "was", "were", "the", "that", "those", "these", "a", "there")

# 9 Shuffle and Replace Words in Paragraph Function
shuffle_replace_words_in_paragraph = function(paragraph_, 
                                              n_shuffle_letters, n_replace_letters, 
                                              keep_first_last_letters = FALSE,
                                              keep_keywords_as_is = FALSE)
{
  paragraph_vector = paragraph_to_word_vector(paragraph_ = paragraph_)
  n_words = length(paragraph_vector[[1]])
  
  shuffled_replaced_paragraph_vector = c()
  for(counter01 in 1: n_words)
  {
    word_under_consideration = paragraph_vector[[1]][[counter01]]
    
    if(keep_keywords_as_is & word_under_consideration %in% key_words_list)
    {
      replaced_word = word_under_consideration
    } else{
      shuffled_word = shuffle_word(word_ = word_under_consideration, 
                                   n_shuffle_a = n_shuffle_letters, 
                                   keep_first_last_letters = keep_first_last_letters)
      
      replaced_word = replace_word(word_ = shuffled_word, 
                                   n_replace_a = n_replace_letters, 
                                   keep_first_last_letters = keep_first_last_letters)
    }
    
    shuffled_replaced_paragraph_vector[counter01] = replaced_word
  }
  
  shuffled_replaced_paragraph = paste(shuffled_replaced_paragraph_vector, collapse = " ")
  return(shuffled_replaced_paragraph)
}




story = "In the frozen lands of Antarctica, there lived a small arctic fox named Frost. Frost was a curious and adventurous fox, always eager to explore the snowy wilderness. But one day, Frost noticed that something was different. The snow and ice that he had always known were starting to melt, and the temperature was getting warmer. Frost knew that this was a problem, and he set out to find a solution. He searched the snowy wilderness for a way to stop the melting, but no matter what he tried, the ice continued to melt. As the days went by, Frost realized that the problem was global warming. He knew that he had to do something to help, so he set out on a mission to spread awareness about the dangers of climate change. Frost traveled far and wide, telling everyone he met about the importance of taking care of the planet. And eventually, his message was heard, and people began to take action to protect the earth."
story = gsub('[[:punct:] ]+', ' ', story)

shuffle_replace_words_in_paragraph(paragraph_ = story, 
                                   n_shuffle_letters = 0, 
                                   n_replace_letters = 40,
                                   keep_first_last_letters = FALSE,
                                   keep_keywords_as_is = FALSE)
