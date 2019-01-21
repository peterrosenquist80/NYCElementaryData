setwd("/Users/peterrosenquist/Downloads/data-science-for-good")
#load data
schools <- read.csv("2016 School Explorer.csv", stringsAsFactors = F)
#subset to schools being tested 
fifth_grade_tested <- schools[schools$Grade.High=='05' & schools$Grade.5.Math...All.Students.Tested > 0,]
#function for character correction
factorToPerc <- function(column){
  column <- strsplit(column, split = "%")
  column <- as.numeric(column)/100
  return(column)
}
#apply factorToPerc
fifth_grade_tested[,19:26] <- sapply(fifth_grade_tested[,19:26], factorToPerc)
fifth_grade_tested[,c(27,29,31,33,35,37)] <- sapply(fifth_grade_tested[,c(27,29,31,33,35,37)], factorToPerc)
#add dummy for student achievement
fifth_grade_tested <- cbind(fifth_grade_tested, 
                            SAdummy = ifelse(fifth_grade_tested$Student.Achievement.Rating == "Exceeding Target" |
                                               fifth_grade_tested$Student.Achievement.Rating == "Meeting Target", 1, 0))
#add fraction of students with 4s
fifth_grade_tested <- cbind(fifth_grade_tested, 
                            math4 = fifth_grade_tested$Grade.5.Math.4s...All.Students/
                              fifth_grade_tested$Grade.5.Math...All.Students.Tested,
                            ela4 = fifth_grade_tested$Grade.5.ELA.4s...All.Students/
                              fifth_grade_tested$Grade.5.ELA...All.Students.Tested)
#change to numeric
fifth_grade_tested[,c(17,40,41)] <- sapply(fifth_grade_tested[,c(17,40,41)], as.numeric)
#subset data
fifth_grade_tested <- fifth_grade_tested[, c(162:164,17, 19:41)]
#save complete data set
saveRDS(fifth_grade_tested, file = "fifth_grade_tested.rds")
