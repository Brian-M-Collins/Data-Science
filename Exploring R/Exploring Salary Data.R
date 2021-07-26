Name <- c("James", "Claire", "Amanda", "Nick", "David", "Annie", "Ali",  "Joe", "Peter", "Annette")
strBirth_Date <- c("15-Jun-1979", "4-Oct-1972", "17-Dec-1980", "26-Jul-1984", "11-Jan-1959", "05-Nov-1986", "18-Feb-1977", "26-Sep-1969", "1-May-1973", "22-Apr-1984")
Birth_Date <- as.Date(strBirth_Date, "%d-%b-%Y")
Birth_Date <- format(Birth_Date, "%Y-%m-%d")
strStart_date <- c("1-Jul-2001", "4-Oct-1998", "17-Aug-2010", "15-Sep-2016", "1-May-1990", "15-Oct-2018", "12-Jun-1995", "15-Sep-1987", "27-Dec-1999", "15-Aug-2014")
Start_Date <- as.Date(strStart_date, "%d-%b-%Y")
Start_Date <- format(Start_Date, "%Y-%m-%d")
Job <- c("Reader", "Professor", "Lecturer", "Lecturer", "Reader", "Lecturer", "Reader", "Professor", "Reader", "Lecturer")
No_of_Car <- c(2,3,0,1,1,1,2,2,2,1)
No_of_Children <- c(2,3,1,0,1,1,2,2,3,1)
Home_Ownership <- c("Yes", "Yes", "No", "Yes", "Yes", "No", "Yes", "Yes", "No", "No")
Staff_Information <- data.frame("Name"=Name, "Birth_Date"=Birth_Date, "Start_Date"=Start_Date, "Job"=Job, "No_of_Car"=No_of_Car, "No_of_Children"=No_of_Children, "Home_Ownership"=Home_Ownership)

Salary_Data <- data.frame("Name"=Name)
Salary_Data$Basic_Salary <- 60000
Salary_Data$Basic_Salary[Staff_Information$Job == "Reader"] <- 50000
Salary_Data$Basic_Salary[Staff_Information$Job == "Lecturer"] <- 40000

Today <- Sys.Date()
format(Today, format="%Y-%m-%d")

Temp <- data.frame("Name"=Name)
Temp$Years_Experience <- as.numeric(difftime(Today, Staff_Information$Start_Date, units = "weeks"))/52.25
Temp$Age <- as.numeric(difftime(Today, Staff_Information$Birth_Date, units = "weeks"))/52.25
Temp$Experience_Weight <- ifelse(Temp$Years_Experience > 5, 1.2, 1)

for (i in 1:nrow(Salary_Data)){
  if(Temp$Age[i] < 40){
    Temp$Age_Weight[i] = 1
  }else if(Temp$Age[i] > 50){
    Temp$Age_Weight[i] = 1.2
  }else{
    Temp$Age_Weight[i] = 1.1
  }
}

Salary_Data$Experience_Bonus <- (Salary_Data$Basic_Salary*Temp$Experience_Weight)-Salary_Data$Basic_Salary
Salary_Data$Age_Bonus <- (Salary_Data$Basic_Salary*Temp$Age_Weight)-Salary_Data$Basic_Salary
Salary_Data$Car_Allowance <- Staff_Information$No_of_Car * 45
Salary_Data$Child_Allowance <- Staff_Information$No_of_Children * 80
Salary_Data$Accomodation_Allowance <- ifelse(Staff_Information$Home_Ownership == "Yes", 150, 200)
Salary_Data$Gross_Salary <- Salary_Data$Basic_Salary+Salary_Data$Experience_Bonus+Salary_Data$Age_Bonus+Salary_Data$Car_Allowance+Salary_Data$Child_Allowance+Salary_Data$Accomodation_Allowance

for (i in 1:nrow(Salary_Data)){
  if(Salary_Data$Gross_Salary[i] < 30000){
    Salary_Data$Tax_Band[i] = 0.20
  } else if(Salary_Data$Gross_Salary[i] > 60000){
    Salary_Data$Tax_Band[i] = 0.35
  } else if(Salary_Data$Gross_Salary[i] %in% 30000:45000){
    Salary_Data$Tax_Band[i] = 0.25
  } else {
    Salary_Data$Tax_Band[i] = 0.30
  }
}

Salary_Data$Taxable_Income <- Salary_Data$Gross_Salary * Salary_Data$Tax_Band

Salary_Data$Net_Income <- Salary_Data$Gross_Salary-Salary_Data$Taxable_Income

