df1 <- beam1 %>% slice(2, 4, 40:2086) %>% select(-1)
df1 <- df1[ , colSums(is.na(df1)) == 0] #remove Na colums

readings <- na.omit(unique(unlist(df1[1, ])))

# FOR LOOP (Replace == 1 with == i)

for (i in readings) {
  
df2 <- as.vector(df1[1, ] == i)
df3 <- as.data.frame(df1[,df2]) %>% slice(2:max(row(df1)))
exposer <- (unique(unlist(df3[1, ])))
names(df3) <- df3[1,]
df3 <- df3 %>% slice(2:max(row(df1)))

kev <- seq(0.02 , by = 0.02, length.out = max(row(df3)))

df3$kev <- kev

if(length(exposer) == 2){

df3$`1` <- as.integer(df3$`1`)
df3$`2` <- as.integer(df3$`2`)



plot <- ggplot(df3, aes(x=kev, y = `1`, colour = 'Exposer 1')) +
  geom_line()+
  geom_line(aes(y=`2`, colour = 'Exposer 2')) +
  ggtitle(i)


print(plot)}else{
  df3$`1` <- as.integer(df3$`1`)
  df3$`2` <- as.integer(df3$`2`)
  df3$`3` <- as.integer(df3$`3`)
  
  
  
  plot <- ggplot(df3, aes(x=kev, y = `1`, colour = 'Exposer 1')) +
    geom_line()+
    geom_line(aes(y=`2`, colour = 'Exposer 2')) +
    geom_line(aes(y=`3`, colour = 'Exposer 3')) +
    ggtitle(i)
    #xlim(0, 10)
  
  
  print(plot)
}

}
