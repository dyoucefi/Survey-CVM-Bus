
merged.survey.j=merged.survey.j%>%filter(!is.na(group)) 

merged.survey.j$decision_all=as.character(merged.survey.j$decision_all)
li_amount_walk=c( "1"= "0","2"= "5,000¥","3"= "10,000¥","4"= "15,000¥","5"= "20,000¥","6"= "50,000¥","7"= "100,000¥","8"= "Refuse all")
li_amount_bus=c("1"= "3,000¥","2"= "5,000¥","3"= "10,000¥","4"= "15,000¥","5"= "20,000¥", "6"= "50,000¥","7"= "100,000¥","8"= "Refuse all")


ggplot(merged.survey.j%>%filter(group=="Walking group with no bus question"),aes(x=decision_all))+geom_bar(fill="#3393FF")+  labs(x="Survey answers",y="Number of observations")+scale_x_discrete(labels=li_amount_walk)+theme_minimal()
ggsave("/Users/utokyoresearch/Desktop/jun_histo_walk_group.png")



ggplot(merged.survey.j%>%filter(group=="Bus-commuting group"),aes(x=decision_all))+geom_bar(fill="#3393FF")+  labs(x="Survey answers",y="Number of observations")+scale_x_discrete(labels=li_amount_bus)+theme_minimal()
ggsave("/Users/utokyoresearch/Desktop/jun_histo_bus_group.png")



