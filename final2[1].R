dataset <- "E:/ISI/Semester_2/DARP - Data Analysis using R and Python/Projects/Group__5/New folder/attacks.csv"
#Cleaning:-

##1)Removing unnecessary columns and chose to keep only case number 1
df <- read.csv(dataset, stringsAsFactors = FALSE)
df_updated <- df[, !(names(df) %in% c("href","href.formula","pdf","Date","Case.Number.2","Case.Number.3","original.order"))]
##2)Fixing column name inconsistencies:-
names(df_updated) <- tolower(names(df_updated))
names(df_updated) <- gsub("\\.+", "_", names(df_updated))
names(df_updated) <- trimws(names(df_updated))       

##3)Removing Invalid or Unreliable Records
df_updated <- df_updated[!is.na(df_updated$year) & df_updated$year >= 1900, ]
summary(df_updated$year)

##4)Clean entries of sex column:-
table(df_updated$sex)
df_updated$sex <- trimws(df_updated$sex)
df_updated$sex <- tolower(df_updated$sex)
df_updated$sex[df_updated$sex %in% c("m", "male")] <- "Male"
df_updated$sex[df_updated$sex %in% c("f", "female")] <- "Female"
df_updated$sex[df_updated$sex == "" | is.na(df_updated$sex)] <- "Unknown"
df_updated$sex[df_updated$sex %in% c(".", "n", "lli")] <- "Unknown"

##5)Conversion of Fatal Variable to Numeric Format
table(df_updated$fatal_y_n)
df_updated$fatal_y_n <- trimws(df_updated$fatal_y_n)
df_updated$fatal_y_n <- toupper(df_updated$fatal_y_n)
df_updated$fatal_y_n[df_updated$fatal_y_n %in% c("UNKNOWN", "F", "2017")] <- NA
df_updated$fatal_y_n[df_updated$fatal_y_n == "Y"] <- "1"
df_updated$fatal_y_n[df_updated$fatal_y_n == "N"] <- "0"
df_updated$fatal_y_n <- as.numeric(df_updated$fatal_y_n)
table(df_updated$fatal_y_n, useNA = "ifany")

##6)Age entries fix
table(df_updated$age, useNA = "ifany")
df_updated$age <- iconv(df_updated$age, from = "UTF-8", to = "ASCII", sub = "")
df_updated$age <- gsub("[^0-9A-Za-z ]", "", df_updated$age)
df_updated$age <- trimws(df_updated$age)
df_updated$age_num <- suppressWarnings(as.numeric(sub(".*?(\\d+).*", "\\1", df_updated$age)))
df_updated$age_group <- cut(
  df_updated$age_num,
  breaks = c(0, 12, 19, 59, 120),
  labels = c("Child", "Teen", "Adult", "Old")
)
df_updated$age_group <- as.character(df_updated$age_group)
df_updated$age_group[is.na(df_updated$age_group)] <- "Unknown"

##7)Country Fix:-
###Since it has regions and not only countries:-
names(df_updated)[names(df_updated) == "country"] <- "regions"
table(df_updated$regions, useNA = "ifany")
df_updated$regions <- trimws(df_updated$regions)
df_updated$regions <- toupper(df_updated$regions)
df_updated$regions[df_updated$regions %in% c("UNITED STATES", "US", "USA")] <- "USA"
df_updated$regions[df_updated$regions %in% c("UNITED KINGDOM", "ENGLAND", "SCOTLAND")] <- "UK"
df_updated$regions[df_updated$regions %in% c("MEXICO ")] <- "MEXICO"
df_updated$regions[df_updated$regions %in% c("YEMEN ")] <- "YEMEN"

##8)Species:-
table(df_updated$species, useNA = "ifany")
df_updated$species <- iconv(df_updated$species, from = "UTF-8", to = "ASCII", sub = "")
df_updated$species <- gsub("[^0-9A-Za-z ]", "", df_updated$species)
df_updated$species <- tolower(trimws(df_updated$species))

df_updated$species_group <- "Other Shark"


df_updated$species_group[is.na(df_updated$species) | df_updated$species == ""] <- "Unknown"

df_updated$species_group[grepl("white", df_updated$species)] <- "White Shark"
df_updated$species_group[grepl("tiger", df_updated$species)] <- "Tiger Shark"
df_updated$species_group[grepl("bull", df_updated$species)] <- "Bull Shark"

df_updated$species_group[grepl("reef", df_updated$species)] <- "Reef Shark"

df_updated$species_group[grepl("hammerhead", df_updated$species)] <- "Hammerhead"

df_updated$species_group[grepl("mako", df_updated$species)] <- "Mako Shark"

df_updated$species_group[grepl("blue", df_updated$species)] <- "Blue Shark"

df_updated$species_group[grepl("nurse", df_updated$species)] <- "Nurse Shark"

df_updated$species_group[grepl("raggedtooth|sand tiger|sandtiger", df_updated$species)] <- "Sand Tiger Shark"

df_updated$species_group[grepl("spinner", df_updated$species)] <- "Spinner Shark"

df_updated$species_group[grepl("blacktip|black tip", df_updated$species)] <- "Blacktip Shark"

df_updated$species_group[grepl("porbeagle", df_updated$species)] <- "Porbeagle Shark"

df_updated$species_group[grepl("lemon", df_updated$species)] <- "Lemon Shark"

df_updated$species_group[grepl("dusky", df_updated$species)] <- "Dusky Shark"

df_updated$species_group[grepl("whaler|bronze", df_updated$species)] <- "Whaler Shark"

df_updated$species_group[grepl("sevengill|7-gill", df_updated$species)] <- "Sevengill Shark"

df_updated$species_group[grepl("wobbegong", df_updated$species)] <- "Wobbegong"

df_updated$species_group[grepl("thresher", df_updated$species)] <- "Thresher Shark"

df_updated$species_group[grepl("dogfish", df_updated$species)] <- "Dogfish"

df_updated$species_group[grepl("angel", df_updated$species)] <- "Angel Shark"

df_updated$species_group[grepl("small|unknown|unidentified|shark", df_updated$species) & 
                           df_updated$species_group == "Other Shark"] <- "Unspecified Shark"
df_updated$species_group[df_updated$species_group == "Unspecified Shark"] <- "Unknown"


write.csv(df_updated, dataset, row.names = FALSE)


##9)Activity column fixes:-
table(df_updated$activity, useNA = "ifany")
df_updated$activity <- iconv(df_updated$activity, from = "UTF-8", to = "ASCII", sub = "")
df_updated$activity <- gsub("[^0-9A-Za-z ]", "", df_updated$activity)
df_updated$activity <- tolower(trimws(df_updated$activity))

df_updated$activity_group <- "Other"

df_updated$activity_group[grepl("swim|bathing|floating|wading", df_updated$activity)] <- "Swimming"

df_updated$activity_group[grepl("surf|board|boogie|paddle", df_updated$activity)] <- "Surfing"

df_updated$activity_group[grepl("diving|scuba|free diving|snorkel", df_updated$activity)] <- "Diving"

df_updated$activity_group[grepl("fish|net|catch|harpoon", df_updated$activity)] <- "Fishing"

df_updated$activity_group[grepl("boat|ship|raft|kayak|canoe", df_updated$activity)] <- "Boating"

df_updated$activity_group[grepl("sank|wreck|crash|torpedo|disaster", df_updated$activity)] <- "Disaster"

df_updated$activity_group[is.na(df_updated$activity) | df_updated$activity == ""] <- "Unknown"
table(df_updated$activity_group)
df_updated$activity_group[df_updated$activity_group %in% c("Other", "Unknown")] <- "Unknown"


# df_updated[grepl("^male$|^female$", tolower(trimws(df_updated$name))), c("name","sex")]

# This checks if the column exists before trying to print it
if("name" %in% names(df_updated)) {
  print(df_updated[grepl("^male$|^female$", tolower(trimws(df_updated$name))), c("name","sex")])
}
#REMOVED NAME COLUMN SINCE ITS USELESS
df_updated$name <- NULL
colnames(df_updated)

write.csv(df_updated, dataset, row.names = FALSE)

##10)Time column fixes:-
table(df_updated$time, useNA = "ifany")
df_updated$time <- iconv(df_updated$time, from = "UTF-8", to = "ASCII", sub = "")
df_updated$time <- gsub("[^0-9A-Za-z: ]", "", df_updated$time)
df_updated$time <- tolower(trimws(df_updated$time))
df_updated$hour <- as.numeric(substr(df_updated$time, 1, 2))
df_updated$hour[grepl("morning|early morning|midmorning|late morning|dawn|daybreak", df_updated$time)] <- 9
df_updated$hour[grepl("afternoon|early afternoon|late afternoon|midday|noon|lunchtime|after noon|after lunch", df_updated$time)] <- 14
df_updated$hour[grepl("evening|early evening|dusk|sunset|sundown", df_updated$time)] <- 18
df_updated$hour[grepl("night|late night|midnight|after midnight|dark|after dark", df_updated$time)] <- 22
df_updated$time_group <- "Unknown"

df_updated$time_group[df_updated$hour >= 5 & df_updated$hour < 12] <- "Morning"
df_updated$time_group[df_updated$hour >= 12 & df_updated$hour < 17] <- "Afternoon"
df_updated$time_group[df_updated$hour >= 17 & df_updated$hour < 21] <- "Evening"
df_updated$time_group[df_updated$hour >= 21 | df_updated$hour < 5] <- "Night"
df_updated$time_group[is.na(df_updated$time) | df_updated$time == "" | df_updated$time == "--"] <- "Unknown"
df_updated$hour <- NULL

table(df_updated$time_group)
###cleaning finished:-
write.csv(df_updated, dataset, row.names = FALSE)
