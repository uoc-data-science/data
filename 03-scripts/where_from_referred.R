library(ggplot2)
library(ggthemes)
library(stringr)
library(plyr)

cleanclickstream <- read.csv("02-clean-data/clickstream/clickstream_data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

a <- cleanclickstream$`Request.Referrer`
notreferred <- (a[str_detect(a, "NULL")])
notreferred <- gsub("NULL", "Direct Visits", notreferred)

# Clean Domain:
a <- gsub('\\', "", a, fixed = TRUE)
a <- a[grepl("http",a)]
a <- sub("^http://(?:www[.])?([^/]*).*$", "\\1", a)
a <- sub("^https://(?:www[.])?([^/]*).*$", "\\1", a)
# Leave out IP-addresses
a <- a[!str_detect(a, "[0-9]*[.][0-9]*[.][0-9]*")]
# Remove Subdomains:
b <- a[str_detect(a, "[A-z0-9]*[.][A-z0-9]*[.][A-z]*")]
b <- sub('[A-z0-9]*[.]', '', b)
a <- a[!str_detect(a, "[A-z0-9]*[.][A-z0-9]*[.][A-z]*")]
a <- c(a,b)
# Remove Port numbers:
b <- a[str_detect(a, "[A-z0-9]*[.][A-z0-9]*[:][0-9]*")]
b <- sub('[:][A-z0-9]*', '', b)
a <- a[!str_detect(a, "[A-z0-9]*[.][A-z0-9]*[:][0-9]*")]
a <- c(a,b,notreferred)

a <- data.frame(a)
colnames(a) <- "Referrers"

n <- nrow(a)
referrers <- count(a$Referrers)
colnames(referrers) <- c("Referrers", "Prevalence")
referrers <- transform(referrers, PrevRatio = Prevalence*100/sum(referrers$Prevalence))
referrers <- referrers[referrers$PrevRatio >= 1.5 , ]

referrers <- rbind(referrers, data.frame("Referrers" = "Others", "Prevalence" = n - sum(referrers$Prevalence), "PrevRatio" = 100 - sum(referrers$PrevRatio)))
referrers <- referrers[order(referrers$Prevalence, decreasing = TRUE),]
referrers$Referrers <- factor(referrers$Referrers, levels = referrers$Referrers)

#ggplot(referrers, aes(x = "", y = PrevRatio, fill = Referrers)) +
#  geom_bar(width = 1, stat = "identity", color = "black") +
#  labs(title="Site visit referrences", subtitle= paste("Percental, Out of", n, "site visits"), caption = "Referrers with less than 1% prevalence are added up to \"Others\"") +
#  coord_polar("y", start = 0)+
#  scale_fill_grey(start = 0, end = 1)+
#  theme_void()