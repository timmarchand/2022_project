### load libraries

library(tidyverse)
library(stringi)
library(quanteda)
library(qdapRegex)
library(tidylog) # to check on results of dplyr functions


### load data
## This data comes directly from the Wordpress export function as a csv

wp_2022 <- read_csv("/Users/timmarchand/Downloads/WP-comments-export-2022_09_27_18_29_28.csv")


## remove unnecessary columns
## these can be removed at the exporting stage too
wp_2022 <- wp_2022 %>%
  select(-c(comment_approved, comment_type, user_id, comment_alter_id))


## extracting semester and cohort details

## set spring semester months between April ~ August
spring <- 4:8

wp_2022 <- wp_2022 %>%
  mutate(year= lubridate::year(comment_date_gmt),
         month = lubridate::month(comment_date_gmt),
         semester = case_when(month %in% spring ~ "spring",
                              TRUE ~ "fall"),
         cohort = ifelse(month < 3, year - 2012, year -2011))


# LEARNER DETAILS ----



## checking for learners with 2+ usernames
wp_2022 %>%
  distinct(comment_author, comment_author_email) %>%
  arrange(comment_author)

## defining users with > 1 usernames
## create variable called double_users

double_users <-
wp_2022 %>%
  count(comment_author, comment_author_email) %>%
  count(comment_author_email) %>%
  filter(n >1) %>%
  pull(comment_author_email)

## add username which combines double users to most freq used comment_author
wp_2022 <- wp_2022 %>%
 # filter(comment_author_email %in% double_users) %>%
  add_count(comment_author) %>%
    arrange(comment_author_email, desc(n)) %>%
  group_by(comment_author_email) %>%
  mutate(username = first(comment_author), .before = comment_author) %>%
  ungroup() %>%
  select(-n) %>%
  arrange(comment_ID)


## create anonymous usernames based on cohort

wp_2022 <- wp_2022 %>%
  distinct(cohort,username) %>%
  group_by(cohort) %>%
  mutate(base = cohort + 11,
         row = row_number()-1,
         learner_id = str_c(base,str_pad(row,2,"left", pad = "0"))) %>%
  ungroup() %>%
  select(username,learner_id) %>%
  inner_join(wp_2022) %>%
  arrange(comment_ID)


## TEXT DETAILS ----

# add reply details
wp_2022 <- wp_2022 %>%
  mutate(reply = comment_parent > 0)

## add how many times each comment was replied to
wp_2022 <- wp_2022 %>%
  filter(reply) %>%
  count(comment_parent) %>%
  rename(comment_ID = comment_parent, replied_by_count = n) %>%
  left_join(wp_2022,.) %>%
  mutate(replied_by_count = replace_na(replied_by_count, 0))

## nest all the replies to new column
wp_2022 <- wp_2022 %>%
  select(comment_ID, comment_parent) %>%
  filter(comment_parent > 0) %>%
  rename(comment_child = comment_ID, comment_ID = comment_parent) %>%
  left_join(wp_2022,.) %>%
  nest(comment_children = comment_child)


## cleaning comment texts ----


## Deailing with encoding and character issues
### get replacement vector from fullwidth table (including common Japanese chrs)
## create df called fullwidth
## create named vector called replace_vect

fullwidth <- tibble::tribble(
  ~pattern, ~replacement,
  "！",          "!",
  "＂",         "\"",
  "＃",           "#",
  "＄",          "$",
  "％",          "%",
  "＆",          "&",
  "＇",          "'",
  "（",          "(",
  "）",          ")",
  "＊",          "*",
  "＋",          "+",
  "，",          ",",
  "－",          "-",
  "。",          ".",
  "、",          ",",
  "．",          ".",
  "`",          "'",
  "／",          "/",
  "０",          "0",
  "１",          "1",
  "２",          "2",
  "３",          "3",
  "４",          "4",
  "５",          "5",
  "６",          "6",
  "７",          "7",
  "８",          "8",
  "９",          "9",
  "：",          ":",
  "；",          ";",
  "＜",          "<",
  "＝",          "=",
  "＞",          ">",
  "？",          "?",
  "＠",          "@",
  "Ａ",          "A",
  "Ｂ",          "B",
  "Ｃ",          "C",
  "Ｄ",          "D",
  "Ｅ",          "E",
  "Ｆ",          "F",
  "Ｇ",          "G",
  "Ｈ",          "H",
  "Ｉ",          "I",
  "Ｊ",          "J",
  "Ｋ",          "K",
  "Ｌ",          "L",
  "Ｍ",          "M",
  "Ｎ",          "N",
  "Ｏ",          "O",
  "Ｐ",          "P",
  "Ｑ",          "Q",
  "Ｒ",          "R",
  "Ｓ",          "S",
  "Ｔ",          "T",
  "Ｕ",          "U",
  "Ｖ",          "V",
  "Ｗ",          "W",
  "Ｘ",          "X",
  "Ｙ",          "Y",
  "Ｚ",          "Z",
  "［",          "[",
  "＼",         "\\",
  "］",          "]",
  "＾",          "^",
  "＿",          "_",
  "｀",          "`",
  "ａ",          "a",
  "ｂ",          "b",
  "ｃ",          "c",
  "ｄ",          "d",
  "ｅ",          "e",
  "ｆ",          "f",
  "ｇ",          "g",
  "ｈ",          "h",
  "ｉ",          "i",
  "ｊ",          "j",
  "ｋ",          "k",
  "ｌ",          "l",
  "ｍ",          "m",
  "ｎ",          "n",
  "ｏ",          "o",
  "ｐ",          "p",
  "ｑ",          "q",
  "ｒ",          "r",
  "ｓ",          "s",
  "ｔ",          "t",
  "ｕ",          "u",
  "ｖ",          "v",
  "ｗ",          "w",
  "ｘ",          "x",
  "ｙ",          "y",
  "ｚ",          "z",
  "\\p{hyphen}",          "-",
  "\\p{dash}",          "-",
  "\\p{quotation_mark}",          "'"
)

## convert to named vector to input str_replace_all
replace_vect <- fullwidth %>% deframe



## cleaning text by replacing all fullwidth chrs and converting to UTF-8
## create df called raw_text
raw_text <- wp_2022 %>%
transmute(comment_ID,
          clean_text = str_replace_all(comment_content, replace_vect) %>%
           str_squish) %>%
  mutate(enc = stri_enc_mark(clean_text)) %>%
## convert to UTF-8 encoding
mutate(UTF8 = ifelse(enc == "ASCII",
                     stri_encode(clean_text, "ASCII", "UTF-8"),
                     clean_text))  %>%
  select(comment_ID, UTF8)

## replace usernames with anon ids in comments
## create named vector called anon_vect

anon_vect <- wp_2022 %>%
  select(username, learner_id) %>%
  mutate(learner_id = paste0("ID",learner_id)) %>%
  deframe

raw_text <- raw_text %>%
  mutate(UTF8 = str_replace_all(UTF8, anon_vect))


## spellchecking

## Steps:
### use hunspell function to identify words not matched in dictionary
### use hunspell_suggest to generate suggestions for output file
### use str_extract_all to define context around search word typo
### split concatenated suggestions and add to output file with cSplit

## create dfs  spell_df and spell_df_unnested of texts, typos and suggestions
## create df spell_output for exporting to spreadsheet

(spell_df <- raw_text %>%
 mutate(typo = hunspell::hunspell(UTF8, dict = c("en_GB","en_US")),
suggestions = map(typo,
                  ~hunspell::hunspell_suggest(.x, dict = c("en_GB","en_US")) %>%
                    map_chr(
                      ~paste(.x, collapse = ","))))
)

 # unnest for later join and to create typos vector
(spell_df_unnested <- spell_df %>%
    unnest(c(typo,suggestions), keep_empty = TRUE))

spell_output <-
  spell_df_unnested %>%
  drop_na %>%
  # define context window as 30 chars either side and grab typo
  mutate(context = str_extract_all(UTF8,paste0("([^ ]+\\s){0,5}-?\\b",typo,"\\b(\\s[^ ]+){0,5}"))) %>%
  unnest(cols = context) %>%
  # emphasise typo as search word in context
  mutate(context = str_replace(context,typo,paste0("<<",toupper(typo),">>"))) %>%
  mutate(change_to = "") %>%
  # split concatenated suggestions
  splitstackshape::cSplit("suggestions",",") %>%
  tibble() %>%
  select(comment_ID,context,change_to,typo,contains("suggestions")) %>%
  # remove duplicates
  distinct() %>%
  # drop comments with no typos
  filter(!is.na(typo))

## save to online source
# googlesheets4::gs4_create(name = "wp_2022_typos", sheets = spell_output)
# 2

## save to file
read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQPZ2R927p-UqN792kwIFLuZjNAwsbuKODoW1jyTeMUkqANfBR0h8lj95PzimJbOzmz7oxTunwMhOvG/pub?gid=2070656198&single=true&output=csv") %>%
write_csv(wp_2022_typos,"/Users/timmarchand/Desktop/2022_project/data/JSC/2022_typos.csv")


## new from file with corrections ----

## corrections manually checked and coded

## load df called corrected_typos
corrected_typos <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT5k9_BokwZ4FfuttJzwAYTqmVyWIbfedpyUhya0aXW9CixsP5eCSm9FqtX-sFnoYegoeZV9jPa0fMI/pub?gid=0&single=true&output=csv")

## how many typos went uncorrected?
corrected_typos %>%
  filter(change == typo) %>% nrow

## how many corrections were the first suggestion?
corrected_typos %>%
  filter(change == suggestions_01) %>%  nrow

## how many typos had multiple corrections?
corrected_typos %>%
  distinct(change,typo) %>%
  add_count(typo) %>%
  filter(n>1) %>%  nrow
  arrange(typo) %>%
  print(n = Inf)

## Which typos went uncorrected
  ## create vector no_change_typos
  (no_change_typos <-
      corrected_typos %>%
      filter(change == typo) %>%
      pull(typo) %>% unique)

## create edits_tbl for later str_replace_all

  edits_tbl <- corrected_typos %>%
    select(comment_ID,typo, change) %>%
    filter(change != typo) %>%
    distinct()


 comment_IDs <- raw_text %>% pull(comment_ID)

 # tokenise
corrected_text <-  raw_text %>%
   pull(UTF8) %>%
    tokenize_word() %>%
   set_names(comment_IDs) %>%
   enframe(name = "comment_ID", value = "typo") %>%
   unnest(cols = typo) %>%
   mutate(comment_ID = parse_number(comment_ID)) %>%
   left_join(edits_tbl) %>%
   mutate(token = case_when(is.na(change) ~ typo,
                            TRUE ~ paste0(change,"_<",typo,">"))) %>%
   mutate(token = str_replace_all(token, "(\\*\\*name\\*\\*)[^ ]+","\\1")) %>%
   group_by(comment_ID) %>%
   summarise(text = paste(token, collapse = ""))

## some corrections involve typos over 2 words (missed by tokenisation)
## add edits with spaces
## create vector of comment_IDs of edits_with_spaces_cols for later filtering
edits_with_spaces_cols <-
 edits_tbl %>%
  filter(str_detect(typo, " ")) %>%
  pull(comment_ID)


## create df of edits required for typos with spaces
edits_with_spaces <- edits_tbl %>%
  filter(str_detect(typo, " ")) %>%
   left_join(corrected_text, .) %>%
  drop_na() %>%
   mutate(text = str_replace_all(text,typo,change))

## create df of
## completed corrections tbl
text_with_corrections <-
corrected_text %>%
  filter(!comment_ID  %in% edits_with_spaces_cols) %>%
  bind_rows(edits_with_spaces) %>%
  select(comment_ID, text) %>%
  arrange(comment_ID)

 ## corrected text tbl for tagging etc
corrected_text <-
text_with_corrections %>%
  mutate(text = str_remove_all(text, "_<.+?>"))


## word count
corrected_text %>%
  mutate(wc = str_count(text, "\\w+")) %>%
 inner_join(wp_2022) %>%
  group_by(cohort) %>%
  summarise(comments = n(),
            wc = sum(wc),
            users = unique(username) %>% length)


## Save data to file
wp_2022 %>% write_csv(file ="/Users/timmarchand/Desktop/2022_project/data/JSC/wp_comments.csv")

text_with_corrections %>% write_csv(file ="/Users/timmarchand/Desktop/2022_project/data/JSC/test_w_corrections.csv")

corrected_text %>% write_csv(file ="/Users/timmarchand/Desktop/2022_project/data/JSC/corrected_text.csv")

corrected_typos %>%  write_csv(file ="/Users/timmarchand/Desktop/2022_project/data/JSC/corrected_typos.csv")

read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQPZ2R927p-UqN792kwIFLuZjNAwsbuKODoW1jyTeMUkqANfBR0h8lj95PzimJbOzmz7oxTunwMhOvG/pub?gid=2070656198&single=true&output=csv") %>%
  write_csv("/Users/timmarchand/Desktop/2022_project/data/JSC/2022_typos.csv")

