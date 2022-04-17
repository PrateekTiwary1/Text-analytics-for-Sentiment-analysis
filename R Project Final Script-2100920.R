#SENTIMENT ANALYSIS AND NARRATIVE ON TWO BOOKS ALICE ADVENTURE'S IN WONDERLAND AND MOBY DICK

#Loading all the necessary Library

library(dplyr)  #used for manipulation of data
library(tidytext) #to convert data in tidy format
library(stringr) # contain an overall 
library(tidyr) # used for restructuring of data
library(ggplot2) #used for plotting of data
library(ggthemes) # Used for some extra themes for gg plot.
library("wordcloud") #Used for a visual representation of text.
library('ngram') # It is used for identifying sequence of words.
library(gutenbergr) # a library of many texts
library(janeaustenr)
library(knitr) # used for creating a report in formats such as HTML,PDF or word.

# Filtering impurities from the dataset

rm(list=ls())  # remove all environment constraint



#Downloading books used for reference

#statistical analysis
#Hypothesis [queries/emotions]

#------------------------------------------------------------------------------------------------------------------------#

# [n] Section#
# In this section, two books are dowloaded using gutenberg library and making the use of tidy


Alice_Wonderland<- gutenberg_download(c(11))  # downloaded by number,details through website
Alice_Wonderland 

data(stop_words)  # a reference tibble of stop words is used in tidy format 
stop_words

Alice_Wonderland_tidy <- Alice_Wonderland %>%  # unnest:used to convert to tidy format 
  unnest_tokens(word, text) %>%    #it is used for removal of punctuation marks, numbers,spaces,convert text to lower case and one word per row format
  anti_join(stop_words) # Stop words can be removed by this
Alice_Wonderland_tidy

Alice_Wonderland_tidy %>%count(word, sort = TRUE)  # for counting most repeating words

# decide to add "custom words to be removed from the text" as a stop word as its exceptionally common
custom_stop_words <- bind_rows(tibble(word = c("time", "forms"),  # add your own extra stop words
                                      lexicon = c("custom")), 
                               stop_words)
custom_stop_words

Alice_Wonderland_tidy <- Alice_Wonderland_tidy %>%
  anti_join(custom_stop_words) # removal of custom stop words in Alice_Wonderland  here
Alice_Wonderland_tidy %>%count(word, sort = TRUE)  # count most repeating words

Alice_Wonderland_tidy

#removing digits/numbers from the code
Alice_Wonderland_tidy <- Alice_Wonderland_tidy %>%
  filter(!grepl('[0-9]', word))    # regular expression used to remove numbers

Alice_Wonderland_tidy

Alice_Wonderland_tidy %>%count(word, sort = TRUE)  # counting of most repeating words after removing numbers



Moby_Dick  <- gutenberg_download(c(15))  # downloaded by using number,details through website
Moby_Dick

Moby_Dick_tidy <- Moby_Dick %>%  # unnest:used to convert to tidy format 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) # antijoin: used for removal of stop words.
Moby_Dick_tidy

tempMb <- Moby_Dick_tidy

#Displaying most frequent words of the book Moby Dick
Moby_Dick_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#Displaying first 50 words of Moby Dick using wordcloud
Moby_Dick_tidy %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))


#remove_reg <- "&amp;|&lt;|&gt;": it is used for removal of short form such as @ # <|>

MOby_Dick_tidy1 <- Moby_Dick_tidy %>% 
  mutate(text = str_remove_all(word, remove_reg)) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))



#ANALYSIS

# 1-> number of words difference between two books before and after removal of white spaces, stop words, numbers/digits


# [n] Section#


#Moby Dick chapters information

Wholesome_Moby_Dick <- Moby_Dick %>%
  mutate(linenumber = row_number(),  # add column with line and chapter 
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE))))

Wholesome_Moby_Dick


cumsum(str_detect(Wholesome_Moby_Dick$text, 
                  regex("^chapter [\\divxlc]", 
                        ignore_case = TRUE)))  # chapter numbers for each line of text 


table(Wholesome_Moby_Dick$chapter) # tables numbers of lines per chapter per book

Moby_Dick_tidy <- Wholesome_Moby_Dick %>%  # unnest:used to convert to tidy format  
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) # Anti_join is used here for removal of customised Stop words.
Moby_Dick_tidy %>%count(word, sort = TRUE) # It is used for sorting words in descending order.

Mobyd_Back_to_untidy <- Moby_Dick_tidy %>% 
  group_by(chapter, linenumber) %>% 
  summarize(text = str_c(word, collapse = " ")) %>%
  ungroup()
Mobyd_Back_to_untidy

# "^chapter [\\divxlc]" regex: used for locating chapter headings 

    str_detect(Moby_Dick$text,regex("^chapter [\\divxlc]", ignore_case = TRUE)) # chapter detection
    Moby_Dick$text%>%str_subset(regex("^chapter [\\divxlc]",ignore_case = TRUE)) #using pipes 
    sum(str_detect( # "^chapter [\\divxlc]" regex to locate chapter headings 
  str_detect(Moby_Dick$text,regex("^chapter [\\divxlc]", ignore_case = TRUE)) # chapter detection
  Moby_Dick$text%>%str_subset(regex("^chapter [\\divxlc]",ignore_case = TRUE)) #using pipes 
  sum(str_detect(Moby_Dick$text,regex("^chapter [\\divxlc]", ignore_case = TRUE)))
  str_subset(Moby_Dick$text,regex("^chapter [\\divxlc]", ignore_case = TRUE))
  
  Moby_Dick$text%>%str_detect(regex("^chapter [\\divxlc]",ignore_case = TRUE))%>%table

  
  #table(Wholesome_Moby_Dick$linenumber, Wholesome_book$chapter) # tables number of line per chapter and book
  
  #table(wholesome_books$book,Wholesome_books$chapter) # tables no of lines per chapter and book
  #table(Moby_Dick_tidy$text,Moby_Dick_tidy$chapter) # tables no of words per chapter and book
  
  Moby_Dick_tidy$word%>%str_detect(regex("^chapter",ignore_case = TRUE))%>%table # count of chapter word
  
  
  wordcounts <- MOby_Dick_tidy %>%  # how many words are in each chapter
    summarize(words = n())
  head(wordcounts)
  
#Alice's Adventures in Wonderland chapter's information
  
# working with sentences and sections
  
  p_and_p_sentences <- tibble(text = Alice_Wonderland$text) %>% 
    unnest_tokens(sentence, text, token = "sentences")
  
  tibble(text = Alice_Wonderland$text) %>%  # tibble:latest data frames used for removal of unused words and keeping the key ones.
    unnest_tokens(line, text, token = "lines")
  
  tibble(text = Alice_Wonderland$text) %>%
    unnest_tokens(chapter, text, token = "regex", pattern = "Chapter [\\\\d]")
  
  tibble(text = Alice_Wonderland$text) %>% 
    unnest_tokens(character, text, token = "characters")
  
     tibble(text = Alice_Wonderland$text) %>%
    unnest_tokens(chapter, text, token = "regex", 
                  pattern = "PART|Chapter|CHAPTER  [\\dIVXLC]") %>%
    ungroup()
  
    Alice_Wonderland %>% 
    filter(book == Alice_Wonderland$text)
  
  
  common_words <- inner_join(Moby_Dick_tidy,Alice_Wonderland_tidy,by="word")
  common_words%>%count(word, sort = TRUE)  # most common common words
  
  
  # using antijoin to find the words exclusive to each book
  uncommon_words1 <- anti_join(Moby_Dick_tidy,Alice_Wonderland_tidy,by="word")
  uncommon_Moby_Dick <-  uncommon_words1%>%count(word, sort = TRUE)
  uncommon_Moby_Dick  # words not in the Alice_World book
  uncommon_words2 <- anti_join(Alice_Wonderland_tidy,Moby_Dick_tidy,by="word")
  uncommon_Alice_Wonderland <- uncommon_words2%>%count(word, sort = TRUE)
  uncommon_Alice_Wonderland # words not in the Moby Dick book

  
  get_sentiments("afinn")  # sentiment reference
  exc_Alice_words <- uncommon_Alice_Wonderland %>%inner_join(get_sentiments("afinn"),"word")
  exc_Alice_words
  exc_Alice_words <- exc_Alice_words%>%
    mutate(weighted=n*value)

  exc_Alice_words  # calculates the total sentiment contribution of each word
  str(exc_Alice_words) # note 78 words exc to Alice book 
  exc_Alice_sentiments <- as.numeric(exc_Alice_words$weighted)
  hist(exc_Alice_sentiments)  # distribution of the Alice exc word sentiments  

  # the following does exactly as above for the Moby Dick book
  get_sentiments("afinn")  # sentiment reference
  exc_Moby_Dick_words <- uncommon_Moby_Dick %>%inner_join(get_sentiments("afinn"),"word")
  exc_Moby_Dick_words <- exc_Moby_Dick_words%>%
    mutate(weighted=n*value)
  exc_Moby_Dick_words
  str(exc_Moby_Dick_words)
  exc_Moby_Dick_sentiments <- as.numeric(exc_Moby_Dick_words$weighted)
  hist(exc_Moby_Dick_sentiments) # distribution of the Moby Dick exc word sentiments 
  
  # t test difference of means test for the two distributions 
  t.test(exc_Moby_Dick_sentiments,exc_Alice_sentiments) 
  
  library("dgof")  # Always look for culmulative dist 
  cul_exc_Moby_Dick_sentiments <- ecdf(exc_Moby_Dick_sentiments) 
  plot(cul_exc_Moby_Dick_sentiments,main="Culmulative plots",pch = c(17),ylab="",xlab="sentiment")
  cul_exc_Moby_Dick_sentiments <- ecdf(exc_Moby_Dick_sentiments) # CDF for Alice values
  lines(cul_exc_Moby_Dick_sentiments,col="red",pch = c(19)) # combine in one plot 
  abline(v=mean(exc_Moby_Dick_sentiments), col="blue") # vertical line  at Alice mean 
  legend("bottomleft", 
         legend = c("Moby_Dick","Alice"), 
         col = c("black","red"), 
         pch = c(17,19), 
         bty = "n", 
         pt.cex = 2, 
         cex = 1.2, 
         text.col = "black", 
         horiz = F , 
         inset = c(0.1, 0.1)) # legend added to plot
  
  # kolmogorov-smirnov test for a difference in distributions
  ks.test(exc_Alice_sentiments,cul_exc_great_exp_sentiments) # p value is sign 
  # very low p value shows that the distributions differ
  
  get_sentiments("afinn")  # sentiment reference
  get_sentiments("bing")
  get_sentiments("nrc")
  
  
  nrc_joy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")  # select joy sentiment words
  nrc_joy
  
  Moby_Dick_tidy <- Moby_Dick_tidy %>%
    inner_join(nrc_joy)   # joy words in Emma
  
  
  
 Moby_Dick_tidy %>%
    inner_join(nrc_joy) %>%
    count(word, sort = TRUE)  # counts the most prominant joy words
  
  
  bing_sentiment <- Moby_Dick_tidy %>%
    inner_join(get_sentiments("bing"),"word")
  bing_sentiment
  
  tail(bing_sentiment)
  
  #Displaying first 50 words of T.I using wordclound
  bing_sentiment %>%
    anti_join(custom_stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 50))
  
  
  
  moby_dick_sentiment <- tempMb %>%
    inner_join(get_sentiments("bing")) %>%
    count(gutenberg_id, index = row_number() %/% 80, sentiment)%>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)%>% 
    mutate(sentiment = positive - negative)  # add to above with a pos-neg diff
  moby_dick_sentiment

  
  ggplot(moby_dick_sentiment, aes(index, sentiment, fill = gutenberg_id )) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~gutenberg_id , ncol = 2, scales = "free_x")  
  
  
  Alice_Wonderland_bigrams <- tibble(text = Alice_Wonderland$text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) # using the bigram option 
  Alice_Wonderland_bigrams
  
  
  
  Alice_Wonderland_bigrams %>%count(bigram, sort = TRUE) # most popular bigrams are stop word pairs
  
  bigrams_separated <- Alice_Wonderland_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ") # separates the bigram in 2 cols
  bigrams_separated
  
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%  # removes via single stop words
    filter(!word2 %in% stop_words$word)
  bigrams_filtered
  
  # new bigram counts:
  bigram_counts <- bigrams_filtered %>%count(word1, word2, sort = TRUE)
  bigram_counts
  
  bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ")
  bigrams_united  # can be used to recombine into a bigram
  
  
  bigrams_separated %>%
    filter(word1 == "not") %>%  # count cases of negation , changing the sentiment
    count(word1, word2, sort = TRUE)
  
  AFINN <- get_sentiments("afinn")
  AFINN
  
  not_words <- bigrams_separated %>%
    filter(word1 == "not") %>%
    inner_join(AFINN, by = c(word2 = "word")) %>%
    count(word2, value, sort = TRUE)
  not_words
  
  
  library(ggplot2)
  
  not_words %>%
    mutate(contribution = n * value) %>%  # these sentiments are faulty
    arrange(desc(abs(contribution))) %>%
    head(20)
  
  not_words %>%
    mutate(contribution = n * value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%  # can pipe above to ggplot
    ggplot(aes(n * value, word2, fill = n * value > 0)) +
    geom_col(show.legend = FALSE) +
    labs(x = "Sentiment value * number of occurrences",
         y = "Words preceded by \"not\"")
  
  negation_words <- c("not", "no", "never", "without") # more negation words
  
  negated_words <- bigrams_separated %>%
    filter(word1 %in% negation_words) %>%  # filter for the set of negation words
    inner_join(AFINN, by = c(word2 = "word")) %>%
    count(word1, word2, value, sort = TRUE)
  boxplot(negated_words$value)  # what does this boxplot suggest ?
  
  
  
  #[n] Section#
  
  
  # Alices Adventures in Wonderland  -- 11 [guten_berg id] 2) Moby Dick -- 15 [guten_berg id]
  
  AliceMoby <- gutenberg_download(c(11, 15))
  tidy_AliceMoby <- AliceMoby %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  tidy_AliceMoby %>%  count(word, sort = TRUE)

  
  frequency <- bind_rows(mutate(Moby_Dick_tidy, author = "A"), 
                         mutate(Alice_Wonderland_tidy, author = "B")) %>% 
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(author, word) %>%
    group_by(author) %>%
    mutate(proportion = n / sum(n)) %>% 
    select(-n) %>% 
    pivot_wider(names_from = author, values_from = proportion) %>%
    pivot_longer('A':'B', # addition to the pipe selects two authors
                 names_to = "author", values_to = "proportion")
  
  
  bingnegative <- get_sentiments("bing") %>%   # list of negative words from the Bing lexicon. 
    filter(sentiment == "negative")
  head(bingnegative)
  table(bingnegative$sentiment)
  
  
  
 afinn_temp1 <- tidy_AliceMoby %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(index = row_number() %/% 100) %>% 
    summarise(sentiment = sum(value)) %>% 
    mutate(method = "AFINN")
  
  bing_and_nrc <- bind_rows(
    tidy_AliceMoby %>% 
      inner_join(get_sentiments("bing")) %>%
      mutate(method = "Bing"), tidy_AliceMoby %>% 
      inner_join(get_sentiments("nrc") %>% 
                   filter(sentiment %in% c("positive", "negative"))) %>%
      mutate(method = "NRC")) %>%
    count(method, index = row_number() %/% 100, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
    mutate(sentiment = positive - negative)
  
  bind_rows(afinn_temp1, 
            bing_and_nrc) %>%
    ggplot(aes(index, sentiment, fill = method)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")
  
 library(ggplot2)
  