library(magrittr)
M19 <-
  readxl::read_excel(".\\data\\data_raw\\Copy of 2019 Karluk Sampling.xlsx",
                     range = "Tagged Steelhead!A2:S140",
                     col_names = c("mark_date", "card", "fish", "tag", "recap", "sex", "spawner", "length", "scales", "finclip",
                                   "age", "age_f", "age_s", "age_t", "comment"),
                     col_types = c("date", rep("numeric", 3), "text", "numeric", "text", "numeric", rep("text", 2), "skip",
                                   rep("text", 4), rep("skip", 3), "text"),
                     na = c("N/A"))

lapply(M19, table, useNA = "ifany")
#standardize data
M19$recap <- toupper(M19$recap)
M19$sex <- factor(M19$sex, levels = 1:2, labels = c("M", "F"))
M19$age <- ifelse(M19$age == "r", NA, M19$age)
M19$age_f <- ifelse(M19$age_f == "x", NA, M19$age_f)
M19$spawn <- as.character(stringr::str_count(M19$age, "s"))
#Check total ages
table(M19$age, M19$age_t, useNA = "ifany")
#check salt ages
table(M19$age_s, M19$spawn, useNA = "ifany")
#Check comments
M19[M19$comment %in% "Dead", ]
M19[M19$tag == 95, ] #Delete tag number since not a valid mark but valid for composition sample
M19[M19$comment %in% "Dead", "tag"] = NA
M19[M19$comment %in% "Dead", ]

M19[M19$comment %in% "Possible RBT", ]
M19[M19$tag %in% 83, ] #OK

M19[M19$tag %in% c(87, 16, 77, 85, 142), ] # Delete recap record (4 same event recaps and one 2018 recap)
M19[grepl("Recap", M19$comment), ]
#Per Tyler: "Pink tag #87" and tag # 116 represent the same fish, which was retagged with a Yellow (current year) tag. Update comment for tag #116.
M19$comment[M19$tag == 116] <- "Pink tag #87, retagged Yellow #116"
M19[M19$tag == 116, ]
M19 <- M19[!grepl("Recap", M19$comment),  ]
dim(M19) #OK

#define strata
Mbreaks <- quantile(M19$mark_date, probs = c(0, .5, 1))
Mbreaks
lubridate::day(Mbreaks[2]) <- 9
M19$strata <- cut(M19$mark_date, Mbreaks, include.lowest = TRUE, labels = FALSE)
#check
lapply(M19, table, useNA = "ifany")



#Second event fish examined
weir_daily19 <-
  readxl::read_excel(".\\data\\data_raw\\Copy of 2019 Karluk Sampling.xlsx",
                     range = "Emmigration!A3:C121") %>%
  setNames(tolower(names(.)))
weir_daily19
Cbreaks <- quantile(rep(weir_daily19$date, times = weir_daily19$daily)); Cbreaks
plot(weir_daily19$date, weir_daily19$daily)
abline(v = Cbreaks)
lubridate::day(Cbreaks[5]) <- 3 #make last strata include last fish
plot(weir_daily19$date, weir_daily19$daily)
abline(v = Cbreaks)
plot(weir_daily19$date, weir_daily19$cum)
abline(v = Cbreaks)
weir_daily19$strata <- cut(weir_daily19$date, Cbreaks, labels = FALSE)
tail(weir_daily19, 20)



R19 <-
  readxl::read_excel(".\\data\\data_raw\\Copy of 2019 Karluk Sampling.xlsx",
                     range = "Tags recovered!B2:G35",
                     col_names = c("date", "tag_text", "Ytag", "Ptag", "finclip", "comment"),
                     col_types = c("date", "text", rep("numeric", 2), rep("text", 2)))
lapply(R19, table, useNA = "ifany")
R19$tag_text[grepl("109", R19$comment)] <- "yellow 109" #tag number in comments but not in tag field
dim(R19)
R19 <- R19[!(R19$tag_text %in% "yellow 128"), ] #"yellow 128" #Mort wash-up not recap
dim(R19)

#Get tag numbers and colors
R19$tag_color <-  tolower(gsub("(.*)\\s(\\d+)", "\\1", R19$tag_text))
R19$tag <- as.numeric(gsub("(.*)\\s(\\d+)", "\\2", R19$tag_text, ignore.case = T))
table(R19$tag_text, R19$tag_color, useNA = "ifany")
table(R19$tag_text, R19$tag, useNA = "ifany")
table(R19$tag_text, R19$Ytag, useNA = "ifany")
table(R19$tag_text, R19$Ptag, useNA = "ifany")

#Drop pink tag records (2018 recaps)
dim(R19)
R19 <- R19[!((R19$tag_color == "pink") & (is.na(R19$Ytag))), ]
dim(R19)
R19[!is.na(R19$Ytag), ]
R19[is.na(R19$Ytag), ]

#Merge ASL
R19 <- R19 %>%
  dplyr::select(date, Ytag, finclip, tag) %>%
  dplyr::left_join(M19[!is.na(M19$tag), c("mark_date", "tag", "length", "sex", "age", "age_s", "age_t", "age_f", "spawn")], "tag") %>%
  dplyr::mutate(strata = cut(date, Cbreaks, labels = FALSE),
                mark_strata = cut(mark_date, Mbreaks, include.lowest = TRUE, labels = FALSE))
lapply(R19, table, useNA = "ifany")


#Second event ASL
C19 <-
readxl::read_excel(".\\data\\data_raw\\Copy of 2019 Karluk Sampling.xlsx",
                   range = "WeirASL!A2:M262",
                   col_names = c("date", "card", "fish", "lg_type", "tag_color", "tag", "finclip", "sex", "length", "age", "age_f", "age_s", "age_t"),
                   col_types = c("date", rep("numeric", 2), rep("text", 4), rep("numeric", 2), rep("text", 4))) %>%
  dplyr::mutate(strata = cut(date, Cbreaks, labels = FALSE))
lapply(C19, table, useNA = "ifany")
C19$tag <- ifelse(C19$tag == "n", NA, C19$tag)
C19$sex <- factor(C19$sex, levels = 1:2, labels = c("M", "F"))
hist(C19$length)
  C19$length[C19$length %in% 69] <- 690 #Per Tyler
  hist(C19$length)
C19$age <- ifelse(C19$age == "r", NA, C19$age)
C19$age_f <- ifelse(C19$age_f == "x", NA, C19$age_f)
C19$spawn <- as.character(stringr::str_count(C19$age, "s"))
#Check total ages
lapply(C19, table, useNA = "ifany")
table(C19$age, C19$age_t, useNA = "ifany")
C19$age_t <- ifelse(C19$age == "2.2s" & is.na(C19$age_t), 5, C19$age_t) #missing a total age
table(C19$age, C19$age_t, useNA = "ifany")
#check salt ages
table(C19$age_s, C19$spawn, useNA = "ifany")
C19[(C19$age_s < C19$spawn) %in% 1, ] #impossible salt age
C19$age_s[(C19$age_s < C19$spawn) %in% 1] <- 2
table(C19$age_s, C19$spawn, useNA = "ifany")
#drop rows with no asl
dim(C19)
C19 <- C19[!is.na(C19$sex) | !is.na(C19$length) | !is.na(C19$age), ]
dim(C19)
lapply(C19, table, useNA = "ifany")
C19[is.na(C19$sex), ]
C19[is.na(C19$length), ]
C19[is.na(C19$age), ] %>% print(n = 100)

#A few Yellow tags listed in C19 but missing from R19!
C19[!(C19$tag %in% unique(R19$tag)) & C19$tag_color != "pink",]
R19
R19 <-
  C19[!(C19$tag %in% unique(R19$tag)) & C19$tag_color != "pink",] %>%
  dplyr::mutate(Ytag = 1,
                tag = as.numeric(tag)) %>%
  dplyr::select(date, Ytag, finclip, tag, strata) %>%
  dplyr::left_join(M19[!is.na(M19$tag), c("mark_date", "tag", "length", "sex", "age", "age_s", "age_t", "age_f", "spawn")], "tag") %>%
  dplyr::mutate(mark_strata = cut(mark_date, Mbreaks, include.lowest = TRUE, labels = FALSE)) %>%
  rbind(R19)



dat19 <- list(M = M19, C = C19, R = R19, weir = weir_daily19)
save(dat19, file = ".\\data\\dat19.rda")

temp <-
  weir_daily19 %>%
  dplyr::select(-strata) %>%
  dplyr::mutate(year = format(date, "%Y"),
                date = as.Date(paste0(format(date, "%m"), "-", format(date, "%d"), "-", year), "%m-%d-%Y"),
                jday = as.numeric(format(date, "%j")),
                week = format(date, "%U"))
load(".\\data\\weir_daily.rda")
weir_daily <- rbind(weir_daily[!weir_daily$year == 2019,], temp) %>% dplyr::arrange(date)
save(weir_daily, file = ".\\data\\weir_daily.rda")
