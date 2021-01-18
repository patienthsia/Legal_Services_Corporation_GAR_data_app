data <- read.csv("reveune_wide2.CSV", header = T)
a <- subset(data, grant_year == 2008 & state == "AL")


df <- structure(list(x = rep(c(1:5),2), time = c(0.5, 0.5, 1, 2, 3, 0.5, 0.5, 
                                        1, 2, 3)), 
                .Names = c("x", "time"), row.names = c(NA, -10L), class = "data.frame")

subset(df, x == c(1, 4) & time == c(1, 0.5))
df[df$x %in% c(1, 4) & df$time %in% c(1, 0.5), ]

library(rlist)


intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

List_PC <- list("Blank"= c("Total_PAI", "Total_Staff", "Total_h_agency_decision"),
                "Consumer" = c("cases_closed_extended_Consumer_Staff", "Total_Staff", "Total_PAI"))

List_PAI_cat <- list("Blank" = c("Total_h_agency_decision", "Total_l_extensive_services"),
                     "PAI" = c("Total_PAI"))

list.cbind(List_PC)
list.cases(List_PC)
list.flatten(List_PC)
list.take(List_PC, 2)
list.ungroup(List_PC[c("Blank","Consumer")], level = 1)

List(List_PC)

List_PC[c("Blank","Consumer")]

intersect_all(Blank,Consumer,PAI)
