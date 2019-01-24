library(arules)
library(arulesViz)

dataset = read.transactions('SAPRequest_Transaction.txt', sep=',', rm.duplicates = FALSE)
summary(dataset)

itemFrequencyPlot(dataset, sup=0.04)

# In order to set the support, look at product that's request 3 times a day, which is 21 (3*7) times a week, 
# divide this by total number of transactions (421) = 0.05

rules = apriori(data=dataset, parameter = list(support = 0.04, confidence=0.2))

# remove reverse/inverted/duplicate rules
gi <- generatingItemsets(rules)
inspect(gi)
duplicate <- which(duplicated(gi))
rules <- rules[-duplicate]

# show association
inspect(sort(rules, by = 'lift'))




#When user select Budget Reporting, we are 69% confident that they will select General Finance Reports


# what are the roles select together with Approver
rules_approver <- apriori(data=dataset, parameter = list(support = 0.01, confidence=0.2), 
                          appearance = list(default="rhs", lhs="Approver"))

inspect(rules_approver)

# what are the roles select together with General Finance Reports
rules_gen_rep <- apriori(data=dataset, parameter = list(support = 0.01, confidence=0.2), 
                          appearance = list(default="rhs", lhs="General Finance Reports"))

inspect(rules_gen_rep)


#plot graph
plot(rules, method='graph')
plot(rules, method='graph', engine='interactive')
plot(rules, method='graph', control=list(type='items'))



