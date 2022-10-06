# Data-Analysis-Project-Alternative-World-Test-Championship-Algorithm
Implement the proposed alternative algorithm to calculate the World Test Championship rankings

The blog article that goes along with this code is [linked here.](https://brokencricketdreams.com/2021/06/17/alternative-world-test-championship-points-table-australia-should-be-in-the-world-test-championship-final-i-have-the-data-to-prove-it/)

The article presents the results, explains the problems with the original ranking method, and illustrates the algorithm in detail.
Here is the data collection & extraction process and what problems were faced on the way.

## Data Collection Process
### Motivation

Initially, we did this the old school way.

For the first 33 Test matches, we literally perused through the commentary and Match Notes section of the scorecard and manually decided which team won each session. Talk about tedious…

This was difficult for two reason: (1) It was hard to keep up after every Test match, and more importantly, (2) it was completely subjective.

In order to standardize the process of determining who won each session and remove any bias we had after watching the match, I decided to code our algorithm in R and re-do the process from scratch.

### How Did We Get Our Data?
Before we could start implementing our proposal, we had to first get the data.

Our main data source was ESPN Cricinfo’s Match Results list for ICC World Test Championship, 2019-2021. As an input, I fed each scorecard individually into the program. The next step was to figure out how to get session-by-session data.

If you scroll to the bottom of the scorecard, there is a Match Notes section, which summarizes important moments at each interval of the match. The idea was to have our program read through these Match Notes and after preprocessing and removing the unnecessary characters, return data at “Lunch, “Tea”, “Innings Break”, and “End of Day.”

The important features to record at every interval were as follows: (1) Team Batting, (2) Runs, (3) Wickets, & (4) Overs. This data was stored in tables so all the data for lunch, tea, innings break, and end of day for all five days (or however long the Test match lasted) could be easily accessed.

Once the data was all nice and clean, things got a bit easier. At this point, we could compute the run-rate in each session and check if there was a switch of innings (all-out or declaration). Using this data, we could allocate points based on the proposal above.

We repeated this process for all the 58 matches and added up the points. Finally due to COVID*, we divided the total number attained by the total possible.

*Due to COVID-19 interruptions equal number of H/A games was not possible, so percentage was used.

### Issues Faced
Initially I thought, reading data from a scorecard would be an easy task, right? Wrong. I was surprised by the inconsistency in some of the records.

For example, when a day is rained out, sometimes they will put: “Rain – 0/0, Lunch – 0/0, End of Day – 0/0.” Almost always, in a rained-out game, some of the sessions were missing which made it difficult to automate the program efficiently. Day/Night matches were especially hilarious. Instead of “Tea” & “Dinner”, in some games “Lunch” and Tea” were written. In others, it was a combination of all four!

A more subtle issue was when innings break occurred at the same time as an interval. In some occasions, Innings Break” and corresponding score was avoided, which caused our data table to have some missing values.

Anyway, you get the point. There were several other little issues, but I do not want to sound like a broken record. What this process influenced me to do confirm after every scorecard was read that all the data was stored correctly in the program.

Quality check.
