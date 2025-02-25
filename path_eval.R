
path_df = read_csv("C:/users/jack.murphy/downloads/path_compare.csv") %>% 
  mutate(Date = mdy(Date),
         row = row_number(),
         path = as.numeric(`Club Path`),
         AoA = as.numeric(`Attack Ang.`),
         FtP = as.numeric(`Face To Path`),
         swing_dir = as.numeric(`Swing Dir.`)) %>% 
  select(Date, Shot, row, path, AoA, FtP, swing_dir) %>% rename(date = Date, shot = Shot)


path_df %>% 
  mutate(face = FtP - path) %>% 
  mutate(path_change = path - lag(path, 1)) %>%
  mutate(ftp_change = FtP - lag(FtP, 1)) %>%
  mutate(face_change = face - lag(face, 1)) %>%
  filter(!is.na(path_change)) %>%
  filter(!is.na(ftp_change)) %>%
  group_by(date) %>% 
  select(date, row, path, path_change, FtP, ftp_change, face, face_change) %>% 
  
  tidyr::gather(key = variable, value = value, -c(date, row)) %>%
  
  ggplot() + 
  geom_line(aes(x=row, y=value, color = as.factor(date))) +
  facet_wrap(~variable, scales = "free") +
  geom_hline(yintercept = 0)