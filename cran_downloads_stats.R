library(rvest)
library(lubridate)
library(cranlogs)
# package = "RJDemetra"
for(package in c("RJDemetra", "ggdemetra", "rjdqa", "rjdmarkdown", "rjwsacruncher")){
  url = sprintf("https://cran.r-project.org/src/contrib/Archive/%s/", package)
  tab = data.frame(html_table(html_node(read_html(url), "table"))[,2:3])
  tab[tab == ""] <- NA
  tab[,2] = lubridate::ymd_hm(tab[,2])
  tab[,1] <- gsub(".tar.gz","", tab[,1])
  tab <- na.omit(tab)

  text_rvest <- (read_html(sprintf("https://CRAN.R-project.org/package=%s", package)) %>%
                   html_text() %>%
                   strsplit("\n"))[[1]]
  last_release <- text_rvest[grep("^Version", text_rvest)+1]
  date_last_release <- text_rvest[grep("^Published", text_rvest)+1]
  tab = rbind(tab, NA)
  tab[nrow(tab),1] = sprintf("%s_%s", package,last_release)
  tab[nrow(tab),2] <- ymd(date_last_release)
  # remove hour
  tab[,2] <- ymd(as.Date(tab[,2]))

  pkg_stats <- cran_downloads(package, from = as.character(tab[1,2]))
  pkg_stats$version = ""
  pkg_stats$date <- ymd(pkg_stats$date)
  for(i in seq_len(nrow(tab))){
    pkg_stats[pkg_stats$date >= tab[i,2],"version"] <- tab[i,1]
  }
  write.csv2(pkg_stats,
             file = sprintf("data/full_table/%s.csv", package),
             fileEncoding = "UTF-8",
             row.names = FALSE)
  sum_stats <- aggregate(count ~ version, data = pkg_stats, FUN = sum)
  sum_stats
  summary_table <- cbind(tab, sum_stats$count, cumsum(sum_stats$count))
  colnames(summary_table) <- c("Name", "Date release", "Total downloads",
                               "Cumulative downloads")
  write.csv2(summary_table,
             file = sprintf("data/summary/%s.csv", package),
             fileEncoding = "UTF-8",
             row.names = FALSE)
}



