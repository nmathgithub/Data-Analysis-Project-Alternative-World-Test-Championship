# Nitesh Mathur 
# 3 May 2021 
# World Test Championship 
# India-Australia Series 
# 37, 39, 43, 45 on WTC List 


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
res = htmltab(url, rm_nodata_rows=TRUE)
winner = res[[3]][[45]]
home = res[[1]][[45]]; #home = gsub(" ", "", home, fixed = TRUE); #Removes spaces in Sri Lanka, New Zealand, South Africa, West Indies, etc.
away = res[[2]][[45]]; #away = gsub(" ", "", away, fixed = TRUE); 

# matches = c(1152846)
# for(x in 1:length(matches)) {
# scorecard = readLines("https://espncricinfo.com/ci/engine/match/matches[[x]].html")
# out0 = grep('won', scorecard)
# result0 = gregexpr('won', scorecard[out0])
# print(substring(scorecard[out0], result0[[1]][[1]]-15, result0[[1]][[1]]+15))
# }

#1st Test Adelaide 
# scorecard = readLines("https://espncricinfo.com/ci/engine/match/1223869.html")
#2nd Test Melbourne
# scorecard = readLines("https://espncricinfo.com/ci/engine/match/1223870.html")
#3rd Test Sydney  
#scorecard = readLines("https://espncricinfo.com/ci/engine/match/1223871.html")
#4th Test Brisbane 
scorecard = readLines("https://espncricinfo.com/ci/engine/match/1223872.html")


#Match Result Data
out0 = grep('won', scorecard)
result0 = gregexpr('won', scorecard[out0])
print(substring(scorecard[out0], result0[[1]][[1]]-15, result0[[1]][[1]]+15))


#Lunch Data
out1 = grep('Lunch:', scorecard)
# Output: 158 
lunch0 = gregexpr('Lunch:', scorecard[out1])
# Output: 132187 134475 136295 138130 140704 548512 550220 551501 552780 554750
lunch = rep(NULL)  # 0.5 taken out
for(x in 1:(0.5*length(lunch0[[1]]))){
  print(substring(scorecard[out1],lunch0[[1]][[x]], lunch0[[1]][[x]]+39 ))
  #lunch[[x]] = substring(scorecard[out],lunch0[[1]][[x]] +7, lunch0[[1]][[x]]+30)
  lunch11 = gsub('-', '\\1', substring(scorecard[out1],lunch0[[1]][[x]] +7, lunch0[[1]][[x]]+32))
  lunch11 = gsub('(', '\\1', lunch11, fixed = TRUE)
  lunch11 = gsub('overs', '\\1', lunch11)
  lunch11 = gsub('over', '\\1', lunch11)
  lunch11 = gsub('ove', '\\1', lunch11)
  lunch11 = gsub('ov', '\\1', lunch11)
  lunch11 = gsub('er', '\\1', lunch11)
  #lunch11 = gsub('r', '\\1', lunch11)
  lunch11 = gsub('o', '\\1', lunch11)
  lunch11 = gsub('v', '\\1', lunch11)
  # = gsub('v', '\\1', lunch11)
  # lunch11 = gsub('e', '\\1', lunch11)
  # lunch11 = gsub('r', '\\1', lunch11)
  lunch11 = gsub('in', '\\1', lunch11)
  lunch11 = gsub('li>', '\\1', lunch11)
  lunch11 = gsub('<li', '\\1', lunch11)
  lunch11 = gsub('/', ' ', lunch11)
  lunch11 = gsub('<', ' ', lunch11)
  # lunch11 = gsub('ain', ' ', lunch11)
  #lunch11 = gsub('Rai', ' ', lunch11)
  lunch12 = strsplit(lunch11," ")
  lunchday = stri_remove_empty(lunch12[[1]])
  if(anyNA(as.numeric(lunchday[[2]]))==TRUE)
  {
    lunchday = lunchday[2: length(lunchday)]
  }
  #lunchday = strsplit(lunchday, split = "/")
  #lunch2 = c(lunch2, lunchday)
  lunch = append(lunch, lunchday)
}
# print(lunch) 
# if(anyNA(as.numeric(lunch[[4]]))==TRUE)
# {
#   lunch = append(lunch,'0.0', 3)
# }
for(i in 1: length(tea)){
  if((i%%4 == 0) && (anyNA(as.numeric(tea[[i]]))==TRUE))
  {
    lunch = append(lunch, '0.0', i-1)
    # tea[[4]] = 0 # lunch = append(lunch,'0.0', 3)  
  }
}
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
  tea11 = gsub('over', '\\1', tea11)
  tea11 = gsub('ove', '\\1', tea11)
  tea11 = gsub('ov', '\\1', tea11)
  tea11 = gsub('er', '\\1', tea11)  
  tea11 = gsub('o', '\\1', tea11)
  tea11 = gsub('v', '\\1', tea11)
  tea11 = gsub('in', '\\1', tea11)
  tea11 = gsub('ul><', '',tea11)
  tea11 = gsub('<', '',tea11)
  tea11 = gsub('li>', '',tea11)
  #  tea11 = gsub('e', '',tea11)
  #tea11 = gsub('ul', '',tea11)
  tea11 = gsub('/', ' ', tea11)
  tea12 = strsplit(tea11," ")
  teaday = stri_remove_empty(tea12[[1]])
  if(anyNA(as.numeric(teaday[[2]]))==TRUE)
  {
    teaday = teaday[2: length(teaday)]
  }
  tea = append(tea,teaday)
}
for(i in 1: length(tea)){
if((i%%4 == 0) && (anyNA(as.numeric(tea[[i]]))==TRUE))
    {
      tea = append(tea, '0.0', i-1)
      # tea[[4]] = 0 # lunch = append(lunch,'0.0', 3)
    }
}
# print(tea) 
# tea = append(tea[[12]],'0.0')
# if(tea0[[1]][[1]] > lunch0[[1]][[2]])
# {tea = prepend(tea, rep(0,4))}
# tea = append(tea, '0.0', 7)
testtea = as.data.frame(matrix(tea, ncol=4, byrow = TRUE))
names(testtea) = c('Team', 'Runs', 'Wickets', 'Overs')
# testtea[[2,4]] = 0; 
print(testtea)


#EndofDay
out3 = grep('End Of Day:', scorecard)
end0 = gregexpr('End Of Day:', scorecard[out3])
end = rep(NULL) 
for(x in 1:(0.5*length(end0[[1]]))){
  print(substring(scorecard[out3],end0[[1]][[x]], end0[[1]][[x]]+36 ))
  # end[[x]] = substring(scorecard[out3],end0[[1]][[x]]+11, end0[[1]][[x]]+36 )
  end11 = gsub('-', '\\1', substring(scorecard[out3], end0[[1]][[x]] + 11, end0[[1]][[x]]+36))
  end11 = gsub('overs', '\\1', end11)
  end11 = gsub('over', '\\1', end11)
  end11 = gsub('ove', '\\1', end11)
  end11 = gsub('ov', '\\1', end11)
  end11 = gsub('er', '\\1', end11)
  # end11 = gsub('es', '\\1', end11)
  end11 = gsub('o', '\\1', end11)
  end11 = gsub('v', '\\1', end11)
  # end11 = gsub('s', '\\1', end11)
  # end11 = gsub('r', '\\1', end11)
  end11 = gsub('in', '\\1', end11)
  end11 = gsub('<', '\\1', end11)
  end11 = gsub('li>', '\\1', end11)
  end11 = gsub('ul>', '\\1', end11)
  end11 = gsub('/', ' ', end11)
  end12 = strsplit(end11," ")
  endday = stri_remove_empty(end12[[1]])
  if(anyNA(as.numeric(endday[[2]]))==TRUE)
  {
    endday = endday[2: length(endday)]
  }
  #lunchday = strsplit(lunchday, split = "/")
  #lunch2 = c(lunch2, lunchday)
  end = append(end, endday)
}
# if(anyNA(as.numeric(lunch[[4]]))==TRUE)
# {
#   lunch = append(lunch,'0.0', 3)
# }
for(i in 1: length(tea)){
  if((i%%4 == 0) && (anyNA(as.numeric(tea[[i]]))==TRUE))
  {
    end = append(end, '0.0', i-1)
    # tea[[4]] = 0 # lunch = append(lunch,'0.0', 3)
  }
}
testend = as.data.frame(matrix(end, ncol=4, byrow = TRUE))
names(testend) = c('Team', 'Runs', 'Wickets', 'Overs')
print(testend)


#Innings Breaks
out4 = grep('Innings Break:', scorecard)
inn0 = gregexpr('Innings Break:', scorecard[out4])
inn = rep(NULL)
for(x in 1:(0.5*length(inn0[[1]]))){
  print(substring(scorecard[out4],inn0[[1]][[x]], inn0[[1]][[x]]+45 ))
  # end[[x]] = substring(scorecard[out3],end0[[1]][[x]]+11, end0[[1]][[x]]+36 )
  inn11 = gsub('-', '\\1', substring(scorecard[out4], inn0[[1]][[x]] + 15, inn0[[1]][[x]]+41))
  inn11 = gsub('overs', '\\1', inn11)
  inn11 = gsub('over', '\\1', inn11)
  inn11 = gsub('ove', '\\1', inn11)
  inn11 = gsub('ers', '\\1', inn11)
  inn11 = gsub('ov', '\\1', inn11)
  inn11 = gsub('er', '\\1', inn11)
  inn11 = gsub('o', '\\1', inn11)
  # inn11 = gsub('v', '\\1', inn11)
  # inn11 = gsub('e', '\\1', inn11)
  # inn11 = gsub('s', '\\1', inn11)
  inn11 = gsub('in', '\\1', inn11)
  inn11 = gsub('<', '\\1', inn11)
  inn11 = gsub('li>', '\\1', inn11)
  inn11 = gsub('/', ' ', inn11)
  inn12 = strsplit(inn11," ")
  innday = stri_remove_empty(inn12[[1]])
  if(anyNA(as.numeric(innday[[2]]))==TRUE)
  {
    innday = innday[2: length(innday)]
  }
  #lunchday = strsplit(lunchday, split = "/")
  #lunch2 = c(lunch2, lunchday)
  inn = append(inn, innday)
}
testinn = as.data.frame(matrix(inn, ncol=4, byrow = TRUE))
names(testinn) = c('Team', 'Runs', 'Wickets', 'Overs')
# 2nd Test Melbourne (Day 3 and Day 4 lunch collide with Innings Break)
# testinn = rbind(testinn[1,], testlunch[3,], testlunch[4,])
# 3rd Test Sydney (Day 4 Tea, Australia declare second innings)
# testinn = rbind(testinn[1:2,], testtea[4,])

#EndofMatch
out5 = grep('end of match', scorecard)
final = gregexpr('end of match', scorecard[out5])
final1 = rep(NULL)
for(x in 1:(0.5*length(final[[1]]))){
  print(substring(scorecard[out5],final[[1]][[x]]-19, final[[1]][[x]]-4 ))
  # end[[x]] = substring(scorecard[out3],end0[[1]][[x]]+11, end0[[1]][[x]]+36 )
  fin11 = gsub('-', '\\1', substring(scorecard[out5], final[[1]][[x]] - 17, final[[1]][[x]] - 4))
  fin11 = gsub('o', '\\1', fin11)
  fin11 = gsub('v', '\\1', fin11)
  fin11 = gsub('e', '\\1', fin11)
  lunch11 = gsub('ov', '\\1', lunch11)
  lunch11 = gsub('er', '\\1', lunch11)
  fin11 = gsub('s', '\\1', fin11)
  fin11 = gsub('in', '\\1', fin11)
  fin11 = gsub('g', '\\1', fin11)
  fin11 = gsub("(", ' ', fin11, fixed = TRUE)
  fin11 = gsub(")", ' ', fin11, fixed = TRUE)
  fin11 = gsub('/', ' ', fin11)
  fin12 = strsplit(fin11," ")
  finnday = stri_remove_empty(fin12[[1]])
  # if(anyNA(as.numeric(finnday[[2]]))==TRUE)
  # {
  #   finnday = finnday[2: length(finnday)]
  # }
  #lunchday = strsplit(lunchday, split = "/")
  #lunch2 = c(lunch2, lunchday)
  final1 = append(final1, finnday)
}
testfinn = as.data.frame(matrix(final1, ncol=3, byrow = TRUE))
testfinn = cbind(testinn[2,1], testfinn)
names(testfinn) = c('Team', 'Runs', 'Wickets', 'Overs')
#testfinn[[1]] = home; 
print(testfinn)
testinn[length(testinn[[1]])+1,] = testfinn[1,]


# ___________________________________________________________________________
# Algorithm Starts Here
# Step I: Allocate Home and Away Points 

# home = "England"
# away = "Australia"
HPoints = rep(NULL)
APoints = rep(NULL)

if(winner == home)
{ HPoints = 16; APoints = 0; loser = away;

} else if (winner == 'drawn')
{ HPoints = 8; APoints = 12

} else {
  HPoints = 0; APoints = 24; loser = home;
}

# HPoints1 = rep(NULL)
# APoints1 = rep(NULL)

# Step II: Session By Session Data 
days = max(length(testlunch[[1]]), length(testtea[[1]]), length(testend[[1]])) # Iterate over maximum number of days played
# index = c(1); # Counter for index in case of innings break
# for(i in 1:days){
#   if(i==1){
#     S1wickets = as.numeric(testlunch[i,3]);
#     S1runs = as.numeric(testlunch[i,2]);
#     S1RR = as.numeric(testlunch[i,2])/as.numeric(testlunch[i,4])
#     # Sessions 2 and Sessions 3 
#     if(testtea[i,1]==testlunch[i,1]) {
#     S2wickets = as.numeric(testtea[i,3])-as.numeric(testlunch[i,3]);
#     S3wickets = as.numeric(testend[i,3])-as.numeric(testtea[i,3]);
#     S2Runs = as.numeric(testtea[i,2])-as.numeric(testlunch[i,2]);
#     S2RR = as.numeric(testtea[i,2])/as.numeric(testtea[i,4]);
#     S3RR = as.numeric(testend[i,2])/as.numeric(testend[i,4])
#     } else{
#     S2wickets1 = 10 - as.numeric(testlunch[i,3]); 
#     S2wickets2 = as.numeric(testtea[i,3]);
#     S2Runs1 = as.numeric(testinn[index,2])-as.numeric(testlunch[i,3]);
#     S2Runs2 = as.numeric(testtea[i,2]);
#     S2Overs1 = as.numeric(testinn[index,4])-as.numeric(testlunch[i,4]);
#     S2Overs2 = as.numeric(testtea[i,4]);
#     S2RR1 = S2Runs1/S2Overs1; S2RR2 = S2Runs2/S2Overs2
#     # S2Overs2 = as.numeric(testtea[i,4])-as.numeric(testlunch[i,4]);
#     index = index + 1;
#     }
#   }else{
#     #Wickets Per Session
#     S1wickets = as.numeric(testlunch[i,3])-as.numeric(testend[i-1,3]);
#     S2wickets = as.numeric(testtea[i,3])-as.numeric(testlunch[i,3]);
#     S3wickets = as.numeric(testend[i,3])-as.numeric(testtea[i,3]);
#     #Runs Per Session
#     #RunRate Per Session
#     S1RR = as.numeric(testlunch[i,2])/as.numeric(testlunch[i,4])
#     S2RR = as.numeric(testtea[i,2])/as.numeric(testtea[i,4])
#     S3RR = as.numeric(testend[i,2])/as.numeric(testend[i,4])
#   }
# }
session = list(testlunch, testtea, testend, testinn)
mapply(write.table, x = session, file = c("testlunch.txt", "testtea.txt", "testend.txt", "testinn.txt"))

# index = 1;
# innings = 1;
session_data <- function(day, sess_start, sess_end, innings) {
  #If Session is Day 1 Lunch
  if(day == 1 & sess_start==0 & sess_end ==1 ){
    Swickets = as.numeric(testlunch[1,3]);
    Sruns = as.numeric(testlunch[1,2]);
    Sovers = as.numeric(testlunch[1,4])
    SRR = Sruns/Sovers
    Team = testlunch[1,1]
  }
  else if(sess_start == 0) {
    Swickets = as.numeric(testlunch[day,3])-as.numeric(testend[day-1,3])
    Sruns = as.numeric(testlunch[day,2])-as.numeric(testend[day-1,2])
    Sovers = as.numeric(testlunch[day,4])-as.numeric(testend[day-1,4])
    SRR = Sruns/Sovers
    Team = testlunch[day,1]
    #print(Swickets)
    #print('bob')
  }
  # else if (sess == 3){
  #   Swickets = as.numeric(testlunch[day+1,3])-as.numeric(testend[day,3])
  #   Sruns = as.numeric(testlunch[day+1,2])-as.numeric(testend[day,2])
  #   Sovers = as.numeric(testlunch[day+1,4])-as.numeric(testend[day,4])
  #   SRR = Sruns/Sovers
  # }
  else {
    Swickets = as.numeric(session[[sess_end]][day,3])-as.numeric(session[[sess_start]][day,3])
    Sruns = as.numeric(session[[sess_end]][day,2])-as.numeric(session[[sess_start]][day,2])
    Sovers = as.numeric(session[[sess_end]][day,4])-as.numeric(session[[sess_start]][day,4])
    SRR = Sruns/Sovers
    Team = session[[sess_start]][day,1]
    # print(Swickets)
    # print(Sruns)
    # print(Sovers)
    # print(SRR)
  }
  #Add recursion in case of innings break in the middle of the session
  if(anyNA(session[[sess_end]][day,3])==TRUE){
    Swickets = -1; #print(Swickets); #print('bob2')
    # Team2 = 'end';
  }
  
  if(Swickets < 0){  # index = c(1) # For counter 
    # print(session[[4]])
    #list00 =  session_data(index, sess_start, 4)
    #index = index +1; print(index)
    #Swickets1 = list00[[1]]; Sruns1 = list00[[2]]; Sovers1 = list00[[3]]; SRR1 = list00[[4]]
    if(sess_start == 0){
      Swickets1 = as.numeric(session[[4]][innings,3])-as.numeric(session[[3]][day-1,3])
      Sruns1 = as.numeric(session[[4]][innings,2])-as.numeric(session[[3]][day-1,2])
      Sovers1 = as.numeric(session[[4]][innings,4])-as.numeric(session[[3]][day-1,4])
      SRR1 = Sruns1/Sovers1
      Team1 = session[[3]][day-1,1]
      print(Sruns1)
      innings = innings +1; #print(index);
      Swickets2 = as.numeric(session[[sess_end]][day,2])
      Sruns2 = as.numeric(session[[sess_end]][day,3])
      Sovers2 =  as.numeric(session[[sess_end]][day,4])
      SRR2 = Sruns2/Sovers2;
      Team2 = session[[sess_end]][day,1]
    }else {
      Swickets1 = as.numeric(session[[4]][innings,3])-as.numeric(session[[sess_start]][day,3])
      Sruns1 = as.numeric(session[[4]][innings,2])-as.numeric(session[[sess_start]][day,2])
      Sovers1 = as.numeric(session[[4]][innings,4])-as.numeric(session[[sess_start]][day,4])
      SRR1 = Sruns1/Sovers1
      Team1 = session[[sess_start]][day,1]
      # session[[4]] = session[[4]][-c(1),]
      # print(session[[4]])
      innings = innings +1; #print(index);
      Swickets2 = as.numeric(session[[sess_end]][day,2])
      Sruns2 = as.numeric(session[[sess_end]][day,3])
      Sovers2 =  as.numeric(session[[sess_end]][day,4])
      SRR2 = Sruns2/Sovers2;
      Team2 = session[[sess_end]][day,1]
    }
    # if(anyNA(Team2)==TRUE){
    #   #Swickets = -1; # print(Swickets)
    #   Team2 = "end";
    #}
    newList = list(Team1, Swickets1,Sruns1, Sovers1, SRR1, innings, Team2, Swickets2, Sruns2, Sovers2, SRR2)
    #return(newList0)
    # list01 =  session_data(day, 4, sess_end)
  }
  else {newList = list(Team, Swickets, Sruns, Sovers, SRR, innings)} 
  return(newList)
}


daytest = session_data(days+1,0,1, length(testinn[[1]]))
if(anyNA(daytest[[1]])==FALSE)
{
  days = days + 1;  
}
# days = 5;
#Session By Session Points Allocation
HList = rep(NULL); AList = rep(NULL)
Hrecord = rep(NULL); Arecord = rep(NULL)
Hpoints1 = 0; Apoints1 = 0; 
inn1 = 1; # start = 0; end = 1;
for(i in 1:days){
  for(j in 0:2){
    list1 = session_data(i,j,j+1,inn1)
    if((list1[[4]]==0 || list1[[5]] >= 4 && list1[[2]] >=4 )) # Stokes and Pant
    {
      bat = 1; batrec = 'T';
      bowl = 1; bowlrec = 'T';
    } else if( (list1[[2]] <= 1) || (list1[[5]] >= 3.5 && list1[[2]]<=3) || (list1[[2]]<=1 && list1[[5]]<=2)) 
    { bat = 2; batrec = 'W';
    bowl = 0; bowlrec = 'L'; 
    print(list1[[1]])
    }
    else if((list1[[2]] >= 4) || (list1[[5]] <= 2) || (list1[[2]]>= 4 && list1[[5]]>= 3.5) ){
      bat = 0; batrec = 'L'; 
      bowl = 2; bowlrec = 'W'; 
    }
    else{
      bat = 1; batrec = 'T';
      bowl = 1; bowlrec = 'T';
    }
    if(grepl(list1[[1]], home, fixed=TRUE)==TRUE)
    {
      Hpoints1 = Hpoints1 + bat; HList = append(HList, Hpoints1); Hrecord = append(Hrecord, batrec);
      Apoints1 = Apoints1 + bowl; AList = append(AList, Apoints1); Arecord = append(Arecord, bowlrec);
    }
    else{
      Apoints1 = Apoints1 + bat; AList = append(AList, Apoints1); Arecord = append(Arecord, batrec);
      Hpoints1 = Hpoints1 + bowl; HList = append(HList, Hpoints1); Hrecord = append(Hrecord, bowlrec);
    }
    #print(list)
    inn1 = list1[[6]];
  }
}

#days = 5; # Match finishes 2 days in to day 5. 
#If Match finishes before Tea on Day 5, winning team gets the rest of the session points
if( (length(HList<15)) && (winner==home) ) {
  HBonus1 = (2*(15-length(HList))); ABonus1 = 0;
} else if( (length(HList<15)) && (winner==away) ){
  ABonus1 =  (2*(15-length(HList))); HBonus1 = 0; 
} else {
  HBonus1 = 0;
  ABonus1 = 0;
}

#Final Points 
HTotal = HPoints + Hpoints1 + HBonus1
ATotal = APoints + Apoints1 + ABonus1


#Final Presentation - Home Team
hometab = c(home, HPoints, Hpoints1, HBonus1, HTotal)
tab1 = as.data.frame(matrix(hometab, ncol=5, byrow=TRUE))
names(tab1) = c('Team', 'Home/Away', 'Session', 'Bonus', 'Total')


#Final Presentation - Away Team
awaytab = c(away, APoints, Apoints1, ABonus1, ATotal)
tab2 = as.data.frame(matrix(awaytab, ncol=5, byrow=TRUE))
names(tab2) = c('Team', 'Home/Away', 'Session', 'Bonus', 'Total')
tab1[2,] = tab2[1,]


#Final Presentation - Detail
print(HTotal); print(ATotal)
print(HList); print(AList);
print(Hrecord); print(Arecord);
if(winner == home){
  print(table(Hrecord))
} else {
  print(table(Arecord))
}
print(tab1)



# l = list(df1, df2)
# mapply(write.table, x = l, file = c("df1.txt", "df2.txt"))

