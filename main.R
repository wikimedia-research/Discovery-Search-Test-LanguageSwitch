library(readr)
library(data.table)
library(wmf)
library(ggplot2)
library(BCDA)
options(scipen=500)
main <- function(){
  
  # Get
  retrieve_data <- function(){
    
    files <- list.files("/a/mw-log/archive/CirrusSearchUserTesting", pattern = "CirrusSearchUserTesting\\.log-201511", full.names = TRUE)
    # files <- files[1:7]
    
    results <- lapply(files, function(x){
      file <- tempfile()
      system(paste("gunzip -c ", x, ">", file))
      
      data <- readr::read_tsv(file,
                              col_names = c("date", "group", "queries", "results", "source", "time_taken", "ip",
                                            "user_agent", "query_metadata"),
                              col_types = "ccciciccc")
      file.remove(file)
      data <- as.data.table(data[grepl(x = data$query_metadata, pattern = "full_text", fixed = TRUE), ])
      data <- data[!data$user_agent == "",]
      data$results <- ifelse(data$results == 0, 0, 1)
      data$date <- as.Date(substring(data$date, 0, 10))
      data <- data[,j=list(events=.N), by = c("date","group", "source", "results")]
      gc()
      return(data)
    })
    return(do.call("rbind", results))
  }
  
  # Explore
  eda <- function(data){
    
    data <- data[data$date %in% seq(as.Date("2015-11-03"), as.Date("2015-11-09"), "day"),]
    per_group <- data[, j = list(events = sum(events)), by = c("date", "group")]
    ggsave(file = "events_by_group_summary.png",
           plot = ggplot(per_group, aes(date, events, group = group, type = group, colour = group)) + geom_line() +
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
  ci_prop_diff_tail(count_table) # -0.04325084 -0.04270128
  ci_relative_risk(count_table) # 0.9460554 1.0413982
  
  # Overall the test group is neither more nor less likely to get results.
  
  web <- data[data$source == "web", j = sum(events), by = c("results", "group")]
  web <- web[order(web$results, web$group, decreasing = T),]
  count_web <- matrix(data = web$V1, nrow = 2, byrow = FALSE, dimnames=list("Group" = unique(web$group),
                                                                               "Outcome" = c("Results", "Zero results")))
  ci_prop_diff_tail(count_web) # 0.002660514 0.003836064
  ci_relative_risk(count_web) # 1.003332 1.004799
  # Queries made via web using language detection were more likely to get results. 
  
  api <- data[data$source == "api", j = sum(events), by = c("results", "group")]
  api <- api[order(api$results, api$group, decreasing = T),]
  count_api <- matrix(data = api$V1, nrow = 2, byrow = FALSE, dimnames=list("Group" = unique(api$group),
                                                                               "Outcome" = c("Results", "Zero results")))
  ci_prop_diff_tail(count_api) # -0.05580459 -0.05518439
  ci_relative_risk(count_api) # 0.9304791 1.0243534
  # Queries made via api using language detection were neither more nor less likely to get results. 
  
  # Out of curiosity...
  sources <- data[, j = sum(events), by = c("results", "source")]
  sources <- sources[order(sources$results, sources$source, decreasing = T),]
  count_sources <- matrix(data = sources$V1, nrow = 2, byrow = FALSE, dimnames=list("Source" = unique(sources$source),
                                                                               "Outcome" = c("Results", "Zero results")))
  ci_prop_diff_tail(count_sources) # 0.02286115 0.02352183
  ci_relative_risk(count_sources) # 1.029438 1.030292
  # Queries made via web were more likely to get results.
  
  # Queries coming from API make up such a large group that they actually overpower the effect that is present
  # within the 'queries via web' group.
  
}
