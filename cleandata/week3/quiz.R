a%>%  
  filter(ACR==3, AGS==6)%>%
  select(SERIALNO)%>%
  print

dd%>%
  arrange(desc(rankingGDP))%>%
  select(Long.Name.x)%>%
  slice(13)%>%
  print

dd %>% 
  group_by(Income.Group) %>% 
  summarise(meanRankingGDP = mean(rankingGDP, na.rm = TRUE))

dd%>%
  mutate(quantileGDP=cut(rankingGDP, breaks = quantile(rankingGDP, probs = seq(0, 1, 0.2), na.rm = TRUE)))%>%
  filter(Income.Group == "Lower middle income")%>%
  group_by(Income.Group, quantileGDP) %>%
  summarise(N = length(quantileGDP))%>%
  print