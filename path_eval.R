path_df <- read_csv("C:/users/jack.murphy/downloads/path_compare.csv") %>%
  mutate(
    Date = mdy(Date),
    row = row_number(),
    path = as.numeric(`Club Path`),
    AoA = as.numeric(`Attack Ang.`),
    FtP = as.numeric(`Face To Path`),
    swing_dir = as.numeric(`Swing Dir.`)
  ) %>%
  select(Date, Shot, row, path, AoA, FtP, swing_dir) %>%
  rename(date = Date, shot = Shot) %>%
  mutate(face = FtP - path) %>%
  mutate(path_change = path - lag(path, 1)) %>%
  mutate(ftp_change = FtP - lag(FtP, 1)) %>%
  mutate(face_change = face - lag(face, 1)) %>%
  mutate(dir_change = swing_dir - lag(swing_dir, 1)) %>%
  mutate(AoA_change = AoA - lag(AoA, 1)) %>%
  filter(!is.na(path_change)) %>%
  filter(!is.na(ftp_change))

time_df <- path_df %>%
  group_by(date) %>%
  select(
    date, row,
    AoA, AoA_change,
    path, path_change,
    FtP, ftp_change,
    face, face_change,
    swing_dir, dir_change
  ) %>%
  tidyr::gather(key = var, value = value, -c(date, row)) %>%
  mutate(var = factor(var, levels = c(
    "AoA", "AoA_change",
    "path", "path_change",
    "face", "face_change",
    "FtP", "ftp_change",
    "swing_dir", "dir_change"
  )))


time_df %>%
  ggplot() +
  geom_line(aes(x = row, y = value, color = as.factor(date))) +
  facet_wrap(~var, scales = "free") +
  geom_hline(yintercept = 0)

agg_df <- path_df %>%
  select(date, shot, row, !contains("_change")) %>%
  tidyr::gather(key = var, value = value, -c(date, row, shot)) %>%
  group_by(date, var) %>%
  summarise(
    avg = mean(value),
    st_dev = sd(value)
  ) %>%
  mutate(var = factor(var, levels = c("AoA", "swing_dir", "path", "face", "FtP"))) %>%
  tidyr::gather(key = stat, value = value, -c(date, var))

agg_df %>%
  ggplot() +
  geom_col(aes(x = var, y = value, fill = as.factor(date)), position = "dodge") +
  facet_wrap(~stat, scales = "free")

agg_df %>%
  filter(stat == "st_dev" & var == "face") %>%
  mutate(
    max = max(.$value),
    ratio = value / max
  )

agg_df %>%
  filter(stat == "st_dev" & var == "face") %>%
  mutate(
    max = max(.$value),
    ratio = value / max
  )
