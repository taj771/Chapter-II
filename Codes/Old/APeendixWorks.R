c <- t%>%
  #filter(Year=="2019")%>%
  filter(Irrigation_Level==700)



min(c$ave_val)
max(c$ave_val)


mean(c$ave_val)




c <- t_combined %>%
  filter(!is.na(Year) & !is.na(Irrigation_Level) & !is.na(val_mm))





mean(t$val_mm, na.rm = TRUE)


c <- read_csv("AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126.csv")%>%
  filter(Date >= as.Date("2030-05-01") & Date <= as.Date("2030-10-30"))






t <- df%>%
  filter(Year==2020)

tt <- t%>%
  filter(Irrigation_Level==200)



mean(tt$ave_val, na.rm = T)


tt <- t%>%
  filter(Irrigation_Level==200)


mean(df2$economic_value)

c <- canola_marginal%>%
  select(year,`Total_Precipitation(mm)`)%>%
  distinct(year,.keep_all = T)


ttt <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")



weather <- read.csv("./AquaCropOPSyData/ClimateData/daymet_data_with_et0.csv")%>%
  filter(Year > 2017)%>%
  select(Year,Month,Day,Precipitation,yday)%>%
  group_by(Year,Month,Day)%>%
  mutate(aveprcp = mean(Precipitation))%>%
  distinct(Year,Month,Day, .keep_all = T)%>%
  mutate(
    full_date = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")
  )%>%
  filter(format(full_date, "%m-%d") >= "05-01" & format(full_date, "%m-%d") <= "10-31")%>%
  mutate(Month_Date = format(full_date, "%b %d"))



weather <- weather %>%
  mutate(Year = as.factor(Year)) # Convert Year to a factor



df2018 <- weather%>%
  filter(Year %in% c(2020, 2022))



ggplot(df2018, aes(x = yday, y = Precipitation, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_smooth(method = "loess", aes(group = Year), color = "black", linetype = "dashed") +  # Adding smooth line
  labs(
    title = "Daily Precipitation by Year",
    x = "Day of the Year",
    y = "Precipitation (mm)"
  ) +
  scale_fill_viridis_d() + # Optional: color palette for better visuals
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels vertically
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.line = element_line(size = 0.4, color = "black")
  )+
  scale_x_continuous(
    breaks = seq(1, 300, by = 3),  # Custom breaks every 30 days
  )+
  scale_y_continuous(limits = c(0,28),
    breaks = seq(-1,30, by=2)
  )




df2023 <- weather%>%
  filter(Year==2023)



ggplot(df2018, aes(x = yday, y = Precipitation, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_smooth(method = "loess", aes(group = Year), color = "black", linetype = "dashed") +  # Adding smooth line
  labs(
    title = "Daily Precipitation by Year",
    x = "Day of the Year",
    y = "Precipitation (mm)"
  ) +
  scale_fill_viridis_d() + # Optional: color palette for better visuals
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels vertically
  )+
  scale_x_continuous(
    breaks = seq(1, 290, by = 5),  # Custom breaks every 30 days
  )




  