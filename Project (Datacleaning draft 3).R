library(data.table)
library(corrplot)

setwd("C:\\Users\\Nitro5\\Desktop\\R Team Project")

ibm.dt <- data.table(read.csv("IBM HR Data 2.csv ", na.strings = c("")))

######################
### Data Cleaning  ###
######################

# Remove data with Attrition == 'Termination' as Attrition only involves voluntary resignation or death
c.ibm.dt <- ibm.dt[Attrition != 'Termination']
c.ibm.dt$Attrition <- factor(c.ibm.dt$Attrition)

# Checking entries with duplicate application ID (there are 14)
dup <- ibm.dt[, .N, by = Application.ID][N == 2]
c.ibm.dt <- unique(c.ibm.dt)

# factor the relevant columns
c.ibm.dt$Education <- factor(c.ibm.dt$Education, levels = c(1,2,3,4,5), labels = c('Below College', 'College', 'Bachelor', 'Master', 'Doctor'))
c.ibm.dt$EnvironmentSatisfaction <- factor(c.ibm.dt$EnvironmentSatisfaction)
c.ibm.dt$JobSatisfaction <- factor(c.ibm.dt$JobSatisfaction)
c.ibm.dt$JobInvolvement <- factor(c.ibm.dt$JobInvolvement)
c.ibm.dt$JobLevel <- factor(c.ibm.dt$JobLevel)
c.ibm.dt$PerformanceRating <- factor(c.ibm.dt$PerformanceRating)
c.ibm.dt$RelationshipSatisfaction <- factor(c.ibm.dt$RelationshipSatisfaction)
c.ibm.dt$StockOptionLevel <- factor(c.ibm.dt$StockOptionLevel)
c.ibm.dt$WorkLifeBalance <- factor(c.ibm.dt$WorkLifeBalance)

# Check for other dubious entries in factor variables
lapply(c.ibm.dt[, sapply(c.ibm.dt, is.factor), with = F], unique)

# remove all data where EducationField or Employee.Source is "Test" (fictitious)
c.ibm.dt <- c.ibm.dt[!(EducationField %in% "Test" | Employee.Source %in% "Test")]

# Check for non-numeric entries in numeric ID columns (Application.ID, EmployeeNumber)
c.ibm.dt[!grep("^[0-9]+", Application.ID)]
c.ibm.dt[!grep("^[0-9]+", EmployeeNumber)]

# Note that there are 2 entries where Application.ID are "TESTING" or "Test" (suggests fictitious data so remove)
# Likely that <NA> and "?????" in Application IDs are not NULL (i.e. dont delete)
c.ibm.dt <- c.ibm.dt[!(Application.ID %in% "TESTING" | Application.ID %in% "Test")]
c.ibm.dt[Application.ID == "?????", Application.ID := NA]

# Remove all entries where employee numbers look like Test (keep only numeric values)
c.ibm.dt <- c.ibm.dt[grep("^[0-9]+", EmployeeNumber)]

# Remove useless columns and ID column (keep only Application number)
c.ibm.dt[, c("EmployeeCount", "EmployeeNumber", "Over18", "StandardHours") := NULL]

# Check for other duplicated data (exclude application ID, Employee.Source as criteria)
# it is unlikely that 2 person has exactly the same data except for application ID and Employee.Source
##summary(duplicated(c.ibm.dt[, !c("Application.ID", "Employee.Source"), with = F]))   # 15089 duplicated data
c.ibm.dt <- c.ibm.dt[!duplicated(c.ibm.dt[, !c("Application.ID", "Employee.Source"), with = F])]

# Create a new variable call Edu.Job.Fit (Whether the person education fit the department he/she is working in)
c.ibm.dt[, .N, by=.(EducationField, Department)][order(EducationField)]
c.ibm.dt[, Edu.Job.Fit := 'Neutral'][, Edu.Job.Fit := factor(Edu.Job.Fit, levels = c('Neutral', 'Good', 'Poor'))]

c.ibm.dt[EducationField == 'Human Resources'& Department == 'Human Resources', Edu.Job.Fit := 'Good']
c.ibm.dt[EducationField == 'Human Resources'& Department == 'Sales', Edu.Job.Fit := 'Poor']

c.ibm.dt[EducationField == 'Life Sciences' & Department == 'Research & Development', Edu.Job.Fit := 'Good']
c.ibm.dt[EducationField == 'Life Sciences' & Department == 'Human Resources', Edu.Job.Fit := 'Poor']

c.ibm.dt[EducationField == 'Marketing' & Department == 'Sales', Edu.Job.Fit := 'Good']
c.ibm.dt[EducationField == 'Marketing' & Department == 'Human Resources', Edu.Job.Fit := 'Poor']

c.ibm.dt[EducationField == 'Medical' & Department == 'Research & Development', Edu.Job.Fit := 'Good']
c.ibm.dt[EducationField == 'Medical' & Department == 'Human Resources', Edu.Job.Fit := 'Poor']

c.ibm.dt[EducationField == 'Technical Degree' & Department == 'Research & Development', Edu.Job.Fit := 'Good']
c.ibm.dt[EducationField == 'Technical Degree' & Department == 'Human Resources', Edu.Job.Fit := 'Poor']

c.ibm.dt[is.na(EducationField) | is.na(Department), Edu.Job.Fit := NA]

# Corr plot of all the variables
##corrplot(cor(c.ibm.dt[, !sapply(c.ibm.dt, is.factor), with = F], use="pairwise.complete.obs"), type = "upper")

# Choosing between HourlyRate, DailyRate, MonthlyRate, MonthlyIncome
# MonthlyIncome is highly correlated to TotalWorkingYears (as we would expect)
# HourlyRate, DailyRate and MonthlyRate has no correlation with any variables (drop these variables)
c.ibm.dt[, c('HourlyRate', 'DailyRate', 'MonthlyRate') := NULL]

# Save Cleaned Data Set
write.csv(c.ibm.dt, file = "IBM HR Data (Cleaned).csv", row.names = FALSE)

rm(c.ibm.dt, dup, ibm.dt)