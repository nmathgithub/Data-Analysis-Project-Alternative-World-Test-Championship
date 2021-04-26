# Nitesh Mathur 
# 23 April 2021 
# World Test Championship 
# Goal: Convert World Test Championship proposal into an actual algorithm 


library(cricketr)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringi)
library(htmltab)

# getWTCData = function(PlayerID){
#   #url = paste("http://stats.espncricinfo.com/ci/engine/player/",PlayerID,".html?class=",MatchType,";template=results;type=batting;;view=dismissal_summary",sep="");
#   url = paste("https://stats.espncricinfo.com/ci/engine/records/team/match_results/", PlayerID,".html?id=13202;type=tournament", sep ="")
#   x = htmltab(url, rm_nodata_rows=TRUE)
# }
# WTC = getWTCData(13202)

url = paste("https://stats.espncricinfo.com/ci/engine/records/team/match_results.html?id=13202;type=tournament")
x = htmltab(url, rm_nodata_rows=TRUE)
winner = x[[3]][[1]]

scorecard = readLines('https://www.espncricinfo.com/series/australia-in-eng-2019-1144422/england-vs-australia-1st-test-1152846/full-scorecard')

#Match Result Data
out0 = grep('won', scorecard)
result0 = gregexpr('won', scorecard[out0])
print(substring(scorecard[out0], result0[[1]][[1]]-15, result0[[1]][[1]]+15))


#Lunch Data
out1 = grep('Lunch:', scorecard)
# Output: 158 
lunch0 = gregexpr('Lunch:', scorecard[out1])
# Output: 132187 134475 136295 138130 140704 548512 550220 551501 552780 554750
lunch = rep(NULL)
for(x in 1:(0.5*length(lunch0[[1]]))){
  print(substring(scorecard[out1],lunch0[[1]][[x]], lunch0[[1]][[x]]+36 ))
  #lunch[[x]] = substring(scorecard[out],lunch0[[1]][[x]] +7, lunch0[[1]][[x]]+30)
  lunch11 = gsub('-', '\\1', substring(scorecard[out1],lunch0[[1]][[x]] +7, lunch0[[1]][[x]]+31))
  lunch11 = gsub('o', '\\1', lunch11)
  lunch11 = gsub('v', '\\1', lunch11)
  lunch11 = gsub('in', '\\1', lunch11)
  lunch11 = gsub('/', ' ', lunch11)
  lunch12 = strsplit(lunch11," ")
  lunchday = stri_remove_empty(lunch12[[1]])
  #lunchday = strsplit(lunchday, split = "/")
  #lunch2 = c(lunch2, lunchday)
  lunch = append(lunch, lunchday)
}
# print(lunch) 
testlunch = as.data.frame(matrix(lunch, ncol=4, byrow = TRUE))
names(testlunch) = c('Team', 'Runs', 'Wickets', 'Overs')
print(testlunch)

#Tea Data
out2 = grep('Tea:', scorecard)
tea0 = gregexpr('Tea:', scorecard[out2])
tea = rep(NULL)
for(x in 1:(0.5*length(tea0[[1]]))){
  print(substring(scorecard[out2],tea0[[1]][[x]], tea0[[1]][[x]]+36 ))
  # tea[[x]] = substring(scorecard[out2],tea0[[1]][[x]]+5, tea0[[1]][[x]]+32)
  tea11 = gsub('-', '\\1', substring(scorecard[out2],tea0[[1]][[x]] +5, tea0[[1]][[x]]+30))
  tea11 = gsub('overs', '\\1', tea11)
  tea11 = gsub('o', '\\1', tea11)
  tea11 = gsub('v', '\\1', tea11)
  tea11 = gsub('in', '\\1', tea11)
  tea11 = gsub('ul><', '',tea11)
  tea11 = gsub('<', '',tea11)
  tea11 = gsub('li>', '',tea11)
  #tea11 = gsub('ul', '',tea11)
  tea11 = gsub('/', ' ', tea11)
  tea12 = strsplit(tea11," ")
  teaday = stri_remove_empty(tea12[[1]])
  tea = append(tea,teaday)
}
# print(tea) 
# tea = append(tea[[12]],'0.0')
tea = append(tea, '0.0', 11)
testtea = as.data.frame(matrix(tea, ncol=4, byrow = TRUE))
names(testtea) = c('Team', 'Runs', 'Wickets', 'Overs')
print(testtea)


#EndofDay
out3 = grep('End Of Day:', scorecard)
end0 = gregexpr('End Of Day:', scorecard[out3])
end = rep(NULL)
for(x in 1:(0.5*length(end0[[1]]))){
  print(substring(scorecard[out3],end0[[1]][[x]], end0[[1]][[x]]+36 ))
  # end[[x]] = substring(scorecard[out3],end0[[1]][[x]]+11, end0[[1]][[x]]+36 )
  end11 = gsub('-', '\\1', substring(scorecard[out3], end0[[1]][[x]] + 11, end0[[1]][[x]]+36))
  end11 = gsub('o', '\\1', end11)
  end11 = gsub('v', '\\1', end11)
  end11 = gsub('e', '\\1', end11)
  end11 = gsub('in', '\\1', end11)
  end11 = gsub('/', ' ', end11)
  end12 = strsplit(end11," ")
  endday = stri_remove_empty(end12[[1]])
  #lunchday = strsplit(lunchday, split = "/")
  #lunch2 = c(lunch2, lunchday)
  end = append(end, endday)
}
testend = as.data.frame(matrix(end, ncol=4, byrow = TRUE))
names(testend) = c('Team', 'Runs', 'Wickets', 'Overs')
print(testend)
    
  


