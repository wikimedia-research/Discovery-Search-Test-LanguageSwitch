library(readr)
library(data.table)
library(wmf)
library(ggplot2)
library(BCDA)
options(scipen=500)
main <- function(){
  
  # Get
  retrieve_data <- function(){
    
    files <- list.files("/a/mw-log/archive/", pattern = "CirrusSearchUserTesting\\.log-201511", full.names = TRUE)
    files <- files[1:7]
    
    results <- lapply(files, function(x){
      file <- tempfile()
      system(paste("gunzip -c ", x, ">", file))
      
      data <- readr::read_tsv(file,
                              col_names = c("wiki", "group", "queries", "results", "source", "time_taken", "ip",
                                            "user_agent", "query_metadata"),
                              col_types = "ccciciccc")
      file.remove(file)
      data <- as.data.table(data[grepl(x = data$query_metadata, pattern = "full_text", fixed = TRUE), ])
      data <- data[!data$user_agent == "",]
      data$results <- ifelse(data$results == 0, 0, 1)
      data$wiki <- as.Date(substring(data$wiki, 0, 10))
      data <- data[,j=list(events=.N), by = c("wiki","group", "source", "results")]
      gc()
      return(data)
    })
    return(do.call("rbind", results))
  }
  
  # Explore
  eda <- function(data){
    
    data <- data[data$wiki %in% seq(as.Date("2015-11-03"), as.Date("2015-11-09"), "day"),]
    per_group <- data[, j = list(events = sum(events)), by = c("wiki", "group")]
    ggsave(file = "events_by_group_summary.png",
           plot = ggplot(per_group, aes(wiki, events, group = group, type = group, colour = group)) + geom_line() +
             theme_fivethirtynine() + labs(title = "Events by day and group, Language switching test",
                                           x = "Date", y = "Events"))
    
    per_platform <- data[, j = list(events = sum(events)), by = c("source", "group")]
    ggsave(plot = ggplot(per_platform, aes(source, events, group = group, type = group, fill = group)) + 
      geom_bar(stat = "identity", position = "dodge") + theme_fivethirtynine() +
      labs(title = "Distribution of events per group and source, Language switching test",
           x = "Source", y = "Events"),
      file = "events_by_source_summary.png")
    
    return(data)
  }
  
  data <- eda(retrieve_data())
  data$group <- ifelse(data$group == "multilang-b", "Test", "Control")
  # Generate high-level aggregate
  high_level <- data[, j = sum(events), by = c("results", "group")]
  high_level <- high_level[order(high_level$results, high_level$group, decreasing = T),]
  count_table <- matrix(data = high_level$V1, nrow = 2, byrow = FALSE, dimnames=list("Group" = unique(high_level$group),
                                                                                "Outcome" = c("Results", "Zero results")))
  ci_prop_diff_tail(count_table)
  ci_relative_risk(count_table)
  web <- data[data$source == "web", j = sum(events), by = c("results", "group")]
  web <- web[order(web$results, web$group, decreasing = T),]
  count <- matrix(data = web$V1, nrow = 2, byrow = FALSE, dimnames=list("Group" = unique(web$group),
                                                                               "Outcome" = c("Results", "Zero results")))
  ci_prop_diff_tail(count)
  ci_relative_risk(count)

  
}
