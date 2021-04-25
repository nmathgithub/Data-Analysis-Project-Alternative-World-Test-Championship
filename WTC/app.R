# Nitesh Mathur 
# 23 April 2021 
# World Test Championship 
# Goal: Convert World Test Championship proposal into an actual algorithm 


library(cricketr)
library(shiny)
library(shinydashboard)
library(tidyverse)

scorecard = readLines('https://www.espncricinfo.com/series/australia-in-eng-2019-1144422/england-vs-australia-1st-test-1152846/full-scorecard')
#Lunch Data
out1 = grep('Lunch:', scorecard)
# Output: 158 
lunch0 = gregexpr('Lunch:', scorecard[out])
# Output: 132187 134475 136295 138130 140704 548512 550220 551501 552780 554750
for(x in 1:(0.5*length(lunch0[[1]]))){
  print(substring(scorecard[out],lunch0[[1]][[x]], lunch0[[1]][[x]]+36 ))
}

#Tea Data
out2 = grep('Tea:', scorecard)
tea0 = gregexpr('Tea:', scorecard[out2])
for(x in 1:(0.5*length(tea0[[1]]))){
  print(substring(scorecard[out2],tea0[[1]][[x]], tea0[[1]][[x]]+36 ))
}

#EndofDay
out3 = grep('End Of Day:', scorecard)
end0 = gregexpr('End Of Day:', scorecard[out3])
for(x in 1:(0.5*length(end0[[1]]))){
  print(substring(scorecard[out3],end0[[1]][[x]], end0[[1]][[x]]+36 ))
}
    
  


