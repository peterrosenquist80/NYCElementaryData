library(ggplot2)
library(gridExtra)
library(class)
#load
fifth_grade_tested <- readRDS(file = "fifth_grade_tested.rds")
#fit
lm.fit.1 <- lm(Average.Math.Proficiency~Percent.Black+Percent.Hispanic+
               Economic.Need.Index+SAdummy+Percent.of.Students.Chronically.Absent,
             data = fifth_grade_tested)
summary(lm.fit)
#look for nonlinear relationships
attach(fifth_grade_tested)
p1 <- ggplot()+geom_point(aes(Percent.Black, Average.Math.Proficiency))
p2 <- ggplot()+geom_point(aes(Percent.Hispanic, Average.Math.Proficiency))
p3 <- ggplot()+geom_point(aes(Economic.Need.Index, Average.Math.Proficiency))
p4 <- ggplot()+geom_point(aes(Percent.of.Students.Chronically.Absent, Average.Math.Proficiency))
grid.arrange(p1, p2, p3, p4, nrow = 2)
#maybe percent.black and chronically.absent
#function for comparing degree of models
degreeTest <- function(x, y){
  lm.fit <- lm(y~x)
  d2.fit <- lm(y~poly(x, 2))
  print(summary(lm.fit))
  print(summary(d2.fit))
}
degreeTest(Average.Math.Proficiency, Percent.Black)
degreeTest(Average.Math.Proficiency, Percent.of.Students.Chronically.Absent)
#strong evidence for quadratic relationship with chronically.absent
#less so with percent.black
#go ahead and check the other two
degreeTest(Average.Math.Proficiency, Percent.Hispanic)
degreeTest(Average.Math.Proficiency, Economic.Need.Index)
#evidence for quadratic relationship with percent.hispanic
#not for economic.need.index
lm.fit.2 <- lm(Average.Math.Proficiency~Percent.Black+poly(Percent.Hispanic, 2)+
                 Economic.Need.Index+SAdummy+poly(Percent.of.Students.Chronically.Absent, 2),
               data = fifth_grade_tested)
summary(lm.fit.2)