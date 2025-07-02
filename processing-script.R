###
# Processing Lisa's activity text
# @author Zane Billings
# @date 2020-10-22
###

# Load libraries I'm sure I'll need
library(stringi)
library(stringr)
library(rebus)
library(dplyr)
library(tm)

# Load completely raw copy-pasted file
raw <- readLines("raw.txt")

# Remove the little circle dot from everything
txt1 <- str_replace(raw, "·", "")

# Remove non-ASCII lines
txt1 <- suppressWarnings(str_conv(txt1, "ASCII"))
txt1 <- txt1[which(stri_enc_isascii(txt1))]

# remove blank lines
txt2 <- txt1[txt1 != ""]

# remove 1st 4 lines
txt3 <- txt2[-c(1:4)]

# Remove all lines that say "The Mean Admin says BLM"
txt4 <- txt3[str_which(txt3, regex("The Mean Admin says BLM"), TRUE)]

# Remove all lines that are "comment" and "# days ago" via regex and dates.
cl_reg <- regex(START %R% "Comment" %R% SPACE %R% or(DIGIT, SPACE))
txt5 <- txt4[str_which(txt4, cl_reg, TRUE)]

days_reg <- regex(START %R% "[1-9]" %R% optional("[1-9]") %R% SPACE %R% or("days", "hours", "weeks", "seconds"))
txt6 <- txt5[str_which(txt5, days_reg, TRUE)]

dy_reg <- regex(START %R% or1(month.name))
txt7 <- txt6[str_which(txt6, dy_reg, TRUE)]

# Remove all lines that are just links
link_reg <- regex(or("http", "www"))
txt8 <- txt7[str_which(txt7, link_reg, TRUE)]

# Remove all lines containing "ago"
txt9 <- txt8[str_which(txt8, "ago", TRUE)]

# Remove certain punctuation types
punct_reg <- regex(or("-", ":", PLUS))
txt10 <- txt9[str_which(txt9, punct_reg, TRUE)]

# Convert to lower case
txt11 <- tolower(txt10)

# Split by word
txt12 <- as.character(str_split(txt11, " ", simplify = TRUE))

# Replace punctuations
punct2 <- regex(or(STAR, BACKSLASH, "\"", ",", "/", QUESTION, "!", DOT, ";", ":",
                   "\\(", "\\)", '"', "#", "~", "\\[", "\\]", "&", "@", "="))
txt13 <- str_replace_all(txt12, punct2, "")

# Remove anything that doesn't contain a word.
words <- regex(one_or_more(or1(letters)))
txt14 <- txt13[str_which(txt13, words)]

# Remove anything starting with a number.
txt14 <- txt14[str_which(txt14, regex(START %R% DIGIT), TRUE)]

# Remove anything too long or too short
txt15 <- txt14[str_length(txt14) <= 15]
txt16 <- txt15[str_length(txt15) >= 4]

# Remove all stopwords with the help of tm package
txt17 <- txt16[!(txt16 %in% stopwords())]

processed <- txt17
one_string <- paste(processed, collapse = " ")

# Outputs
writeLines(processed, "processed.txt")
writeLines(one_string, "one_string.txt")
writeLines(unique(processed), "unique.txt")

# playing around
this <- table(processed)
that <- as.data.frame(this)
names(that) <- c("word", "count")
that2 <- that %>% top_n(10, count) %>% arrange(desc(count))
pander::pander(that2)
