# R tests for TTY slides

library("dplyr")
library("ggplot2")
theme_set(theme_bw(base_size = 16))
library("lubridate")
library("tidyr")
## Read data from the spreadsheet #######

# Following http://blog.revolutionanalytics.com/2014/06/reading-data-from-the-new-version-of-google-spreadsheets.html

# Sourcing gists does not work with two files...
# library("devtools")
# source_gist("https://gist.github.com/andrie/a9ff69a274963c70b72f#file-readgooglesheet-r")
# source_gist("https://gist.github.com/andrie/a9ff69a274963c70b72f#file-cleangoogletable-r")

source("read_google_spreadsheet.R")
gdoc.url <- "https://docs.google.com/spreadsheets/d/19DWN0n1GBOLRpttgt9oXhObs58XxlXVW7FO72t-82TQ/pubhtml"
temp <- readGoogleSheet(gdoc.url)
survey.res <- cleanGoogleTable(temp, table=1)

# # Alternative, mentioned in comments
# gdoc.key <- "19DWN0n1GBOLRpttgt9oXhObs58XxlXVW7FO72t-82TQ"
# require(RCurl)
# url <- paste0("https://docs.google.com/spreadsheets/d/",gdoc.key,"/export?format=csv&id=",gdoc.key)
# # url <- paste0("https://docs.google.com/spreadsheets/d/",gdoc.key,"/export?format=csv")
# # url <- paste0("https://docs.google.com/spreadsheets/d/",gdoc.key,"/export?format=csv&id=KEY")
# myCsv <- getURL(url,.opts=list(ssl.verifypeer=FALSE))
# test <- read.csv(textConnection(myCsv))


## Wordcloud of data science #########

library("gdata")
library("wordcloud")
library("tm")

# Parse keywords
keywords <- survey.res[,3] %>%
  tolower %>%
  gsub(";", ",", x=.) %>%
  strsplit(split=",") %>%
  unlist %>%
  trim %>%
  table
  
# Word cloud
png("wordcloud1.png")
set.seed(123)
wordcloud(words=names(keywords), freq=keywords, scale=c(4,0.7), min.freq = 1,
          random.order=FALSE, random.color=FALSE, rot.per=0.0, 
          use.r.layout=FALSE, colors=brewer.pal(6, "Dark2"), fixed.asp=FALSE)
dev.off()

## Distribution of number of lecture participants #######

guesses <- as.numeric(survey.res[,4])
qplot(guesses)


## fillarilaskennat #########

# Preparation:
# * clone this repo: https://github.com/apoikola/fillarilaskennat
# * run source/plot_model.R
# * save your workspace as an image with 'save_image()'

# Load image
load("data/bikemodel_plots.RData")

ggplot(bike.dat, aes(x=Day, y=Count, colour=WeekEnd)) + 
  geom_point(size=1) + facet_grid(Year ~ LocationName) + 
  labs(y="Määrä", x="Päivä", colour="Viikonloppu") +
  scale_y_log10() + 
  theme(legend.position="top")

bike.dat %>%
  filter(LocationName == "Eläintarhanlahti" & Year == 2007) %>%
  ggplot(data =., aes(x=Day, y=Count, colour=WeekEnd)) + 
  geom_point(size=1) + facet_grid(Year ~ LocationName) + 
  labs(y="Määrä", x="Päivä", colour="Viikonloppu") +
  scale_y_log10() + 
  theme(legend.position="none") + 
  ggsave(width=5, height=3, file=file.path(fig.folder, "raw_data_subset.png"))

## 3 sample of weather data
weather.df %>%
  mutate(date=ymd(date), Year = year(date), Day=yday(date)) %>%
  gather(Measurement, Value, tday:tmin) %>%
  mutate(Measurement = factor(Measurement, levels=c("tmax", "tday", "tmin"))) %>%
  filter(name == "Helsinki Kaisaniemi" & Year==2007) %>%
  ggplot(data =., aes(x=Day, y=Value, colour=Measurement)) + 
  geom_path() + facet_grid(Year ~ name) +
  theme(legend.position="none") + labs(x="Päivä", y="Lämpötila")
ggsave(width=5, height=3, file=file.path(fig.folder, "weather_data_subset.png"))

## 4 model + data example
d2.subset <- droplevels(subset(d2, main.site.named=="1070\nEläintarhanlahti" & year %in% 2007))
levels(d2.subset$variable) <- c("raakadata", "sovitettu malli")
ggplot(d2.subset, aes(x=yday(date), y=Count, colour=variable)) + 
  geom_line(alpha=0.8) + 
  scale_y_log10() + theme(legend.position="bottom") +
  ggtitle("Mallinnettu data") +
  labs(x="Päivä", y="Määrä", colour="Data") +
  ggsave(width=10, height=4, file=file.path(fig.folder, "data+model.png"))


## 5 temperature
tday.df <- droplevels(subset(smooths, x.var == "tday\n(mean temperature for day)")) %>%
  ggplot(data = ., aes(x=x.val, y=value, colour=x.val)) + 
  geom_line(size=1.5) + 
  geom_line(aes(y=value + 2*se)) + #, linetype="dashed") + 
  geom_line(aes(y=value - 2*se)) +#, linetype="dashed") +
  scale_colour_gradient2(low=muted("blue"), mid="gray", high=muted("red")) + 
  scale_y_continuous(breaks=y.vals, labels=percent.vals-100, limits=range(smooths$value)) +
  geom_hline(y=0, linetype="dashed") + 
  labs(x="Lämpötila", y="Vaikutus (%) ± keskivirhe") +
  theme(legend.position="none") + 
  ggtitle("Lämpötilan vaikutus") +
  ggsave(width=4, height=4, file=file.path(fig.folder, "temperature.png"))

## 6 main effects: weather and time

percent.vals4 <- c(67, 80, 90, 100, 110, 125)
y.vals4 <- log(percent.vals4/100)

# Discrete vars
factors.discrete <- c("julyTRUE", "AnySnow", "holidayTRUE", "AnyRain")
main.discrete.df <- droplevels(subset(cm.df, Factor %in% factors.discrete))
levels(main.discrete.df$Factor) <- c("heinäkuu", "lunta maassa", "juhlapyhä", "sataa")
main.discrete.df$Group <- c("Aika", "Aika", "Sää", "Sää")
# levels(main.discrete.df$Factor) <- gsub(" \\(kyllä/ei\\)", "", levels(main.discrete.df$Factor))
main.discrete.df$Factor <- factor(main.discrete.df$Factor, levels=rev(main.discrete.df$Factor[order(main.discrete.df$Coefficient)]))
main.discrete.df$Group <- factor(main.discrete.df$Group, levels=c("Sää", "Aika"))
p.main.discrete <- ggplot(main.discrete.df, aes(y=Factor, x=Coefficient, xmin=Coefficient-SE, xmax=Coefficient+SE)) + 
  geom_point(size=3) + 
  geom_errorbarh(height=0) + 
  ggtitle("Sään ja ajankohdan vaikutus") + 
  labs(y=NULL, x="Vaikutus (%)") +
  scale_x_continuous(breaks=y.vals4, labels=percent.vals4-100) +
  geom_vline(x=0, linetype="dashed") +
  facet_grid(Group ~ ., scales="free_y", space="free_y") +
  theme(strip.text.y=element_text(angle=0)) +
  ggsave(width=4, height=4, file=file.path(fig.folder, "main_effects.png"))


# 7 weekdays
weekday.df <- droplevels(subset(cw.df, Factor %in% c("baseline", "AnyRain")))
levels(weekday.df$Weekday) <- c("sunnuntai", "lauantai", "perjantai", "torstai", 
                                "keskiviikko", "tiistai", "maanantai")
levels(weekday.df$Factor) <- c("muutos\nsateella", "lähtötaso")
ggplot(weekday.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE, colour=Weekday)) + 
  geom_point(size=3, position=position_dodge(width=0.7)) + 
  geom_errorbar(width=0, position=position_dodge(width=0.7)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals4, labels=percent.vals4-100) +  
  labs(x=NULL, y="Vaikutus (%)", colour="viikonpäivä") +
  ggtitle("Viikonpäivien vaikutus") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE)) + 
  ggsave(width=8, height=6, file=file.path(fig.folder, "weekday.png"))



# 8 location/site

site.df <- droplevels(subset(cms.df, Factor %in% c("baseline", "year", "holidayTRUE") &
                               main.site.named != "Tuntematon"))
site.df[site.df$Factor=="year", "Coefficient"] <- 10*site.df[site.df$Factor=="year", "Coefficient"]
site.df[site.df$Factor=="year", "SE"] <- 10*site.df[site.df$Factor=="year", "SE"]
levels(site.df$Factor)[3] <- "years_10"
levels(site.df$Factor) <- c("lähtötaso", "muutos\njuhlapyhänä", "muutos 10\nvuoden aikana")
site.df$Factor <- factor(site.df$Factor, levels=c("muutos 10\nvuoden aikana", "muutos\njuhlapyhänä", "lähtötaso"))

ggplot(site.df, aes(x=Factor, y=Coefficient, colour=main.site.named)) + 
  geom_point(size=3, position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymin=Coefficient-SE, ymax=Coefficient+SE), width=0, position=position_dodge(width=0.7)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3-100) +  
  labs(x=NULL, y="Vaikutus (%)", colour="sijainti") +
  ggtitle("Sijainnin vaikutus") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE)) + 
  ggsave(width=8, height=6, file=file.path(fig.folder, "location.png"))


## election machine classification ####



