require(dplyr)
#### EX1

ex1 <- read.csv("ex1.csv", stringsAsFactors = T)

ex1 %>% 
  group_by(Group.Name) %>% 
  summarize(no_rows = length(Group.Name))  %>%
  arrange(desc(no_rows))

#### EX2

ex2 <- read.csv("ex2.csv", stringsAsFactors = T)

ex2 %>%
  group_by(Group.Segment) %>%
  summarise(sum_of_rev = sum(Revenue))

#### EX3

ex3 <- read.csv("ex3.csv", stringsAsFactors = T)

ex3 %>%
  group_by(Group.Segment) %>%
  summarise(no_rows = length(Group.Segment),
            sum_member_months = sum(Member.Months),
            sum_revenue = sum(Revenue),
            sum_expenses = sum(Expenses)) %>%
  mutate(rev_pmpm = sum_revenue / sum_member_months,
         exp_pmpm = sum_expenses / sum_member_months)

#### EX4

ex4 <- read.csv("ex4.csv", stringsAsFactors = T)

ex4 %>%
  mutate(net.income.loss = Revenue - Expenses) %>%
  group_by(Group.Segment,Subdivision) %>%
  summarise(sum_revenue = sum(Revenue),
            sum_expenses = sum(Expenses),
            net.income = sum(net.income.loss))

#### EX5
## Option 1
ex5 <- read.csv("ex5.csv", stringsAsFactors = T)
ex5 <- filter(ex5, Group.Segment=="Big")
ex5.a <- aggregate(Expenses ~ Service.Month + Paid.Month, ex5, sum)

my.Date <- function(x){
  format(as.Date(x, origin="1899-12-30"), "%m-%y") ## Origin from Excel format
}

ex5.a$Service.Month <- my.Date(ex5.a$Service.Month)
ex5.a$Paid.Month <- my.Date(ex5.a$Paid.Month)
ex5.a

## Option 2, with reshape2
require(reshape2)
ex5$Service.Month <- my.Date(ex5$Service.Month)
ex5$Paid.Month <- my.Date(ex5$Paid.Month)
dcast(ex5, Service.Month ~ Paid.Month, sum, value.var = "Expenses", margins = T)

#### EX6
ex6 <- read.csv("ex6.csv", stringsAsFactors = T)

ex6.s <- ex6 %>%
  group_by(Group.Segment, Subdivision) %>%
  summarise(sum_revenue = sum(Revenue),
            sum_expenses = sum(Expenses))
## 1st table
filter(ex6.s, Group.Segment=="Big")

## 2nd table
ex6.s %>%
  filter(Group.Segment=="Medium") %>%
  arrange(desc(Subdivision)) %>%
  select(Group.Segment, Subdivision, sum_expenses, sum_revenue)

#### EX7
ex7 <- read.csv("ex7.csv", stringsAsFactors = T)

ex7 %>%
  filter(Group.Name != "Ramone's Castle") %>%
  group_by(Group.Segment) %>%
  summarise(sum_revenue = sum(Revenue),
            sum_expenses = sum(Expenses))

#### EX8 is just a repeat of of EX2
