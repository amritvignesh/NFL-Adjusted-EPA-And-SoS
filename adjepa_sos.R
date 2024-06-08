library(nflfastR)
library(dplyr)
library(caret)
library(glmnet)
library(gt)
library(gtExtras)
library(nflplotR)

pbp <- load_pbp(2023) %>%
  filter(season_type == "REG", !is.na(posteam), (rush == 1 | pass == 1)) %>%
  mutate(is_home = as.factor(ifelse(posteam == home_team, 1, 0)), posteam = as.factor(posteam), defteam = as.factor(defteam)) %>%
  select(is_home, posteam, defteam, epa)

dummy <- dummyVars(" ~ .", data = pbp)
pbp_dummy <- data.frame(predict(dummy, newdata = pbp))

preds <- as.matrix(pbp_dummy[,-ncol(pbp_dummy)])
target <- pbp_dummy[,ncol(pbp_dummy)]

set.seed(123)

cv_ridge <- cv.glmnet(preds, target, alpha = 0, intercept = TRUE)
lambda <- cv_ridge$lambda.1se
ridge_model <- glmnet(preds, target, alpha = 0, lambda = 0, intercept = TRUE)

ridge_coefs <- data.frame(as.matrix(coef(ridge_model)))
ridge_coefs$name <- rownames(ridge_coefs)
ridge_coefs <- ridge_coefs %>% rename(coef = s0)
ridge_intercept <- ridge_coefs$coef[which(ridge_coefs$name == "(Intercept)")]
ridge_coefs <- ridge_coefs %>% filter(name != "(Intercept)") %>% mutate(coef = coef + ridge_intercept)

adj_off <- ridge_coefs %>%
  filter(grepl("posteam", name)) %>%
  mutate(name = sub("posteam.", "", name)) %>%
  rename(adj_off_epa = coef) %>%
  mutate(adj_off_epa = round(adj_off_epa, 3)) %>%
  select(name, adj_off_epa) %>%
  arrange(name)

rownames(adj_off) <- NULL

adj_def <- ridge_coefs %>%
  filter(grepl("defteam", name)) %>%
  mutate(name = sub("defteam.", "", name)) %>%
  rename(adj_def_epa = coef) %>%
  mutate(adj_def_epa = round(adj_def_epa, 3)) %>%
  select(name, adj_def_epa) %>%
  arrange(name)

rownames(adj_def) <- NULL

off <- pbp %>%
  group_by(name = posteam) %>%
  summarize(off_plays = n(), off_epa = round(mean(epa, na.rm = TRUE), 3))

def <- pbp %>%
  group_by(name = defteam) %>%
  summarize(def_plays = n(), def_epa = round(mean(epa, na.rm = TRUE), 3))

offense <- left_join(adj_off, off, by = "name") %>%
  mutate(off_adjustment = adj_off_epa - off_epa) %>%
  arrange(-adj_off_epa)

defense <- left_join(adj_def, def, by = "name") %>%
  mutate(def_adjustment = adj_def_epa - def_epa) %>%
  arrange(adj_def_epa)

sos <- left_join(offense, defense, by = "name") %>%
  mutate(logo = name, wordmark = name, total_adjustment = (off_plays * off_adjustment - def_plays * def_adjustment)/(off_plays + def_plays), total_adjustment = round(total_adjustment, 3)) %>%
  select(logo, wordmark, off_plays, off_adj = off_adjustment, def_plays, def_adj = def_adjustment, adj = total_adjustment) %>%
  arrange(-adj)

offense <- offense %>% mutate(logo = name, wordmark = name) %>% select(logo, wordmark, plays = off_plays, epa = off_epa, adj_epa = adj_off_epa, adj = off_adjustment)
defense <- defense %>% mutate(logo = name, wordmark = name) %>% select(logo, wordmark, plays = def_plays, epa = def_epa, adj_epa = adj_def_epa, adj = def_adjustment)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>nflverse</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

indiv_gt_func <- function(df, title_str, subtitle_str, reverse_bool) {
  table <- df %>% gt() %>%
    gt_nfl_logos(columns = gt::starts_with("logo")) %>%
    gt_nfl_wordmarks(columns = gt::starts_with("wordmark")) %>%
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    gt_hulk_col_numeric(plays) %>%
    gt_hulk_col_numeric(c(everything(), -logo, -wordmark, -plays), reverse = reverse_bool) %>%
    cols_label(
      logo = md(""),
      wordmark = md("**Team**"),
      plays = md("**Plays**"),
      epa = md("**EPA/Play**"),
      adj_epa = md("**Adj EPA/Play**"),
      adj = md("**Adjustment**")
    ) %>%
    tab_header(
      title = paste0("2023 NFL Adjusted ", title_str, " EPA/Play Leaders"),
      subtitle = md(paste0("*Adjusted Based On **Home-Field Advantage** & **", subtitle_str, " Faced***"))
    ) %>%
    tab_source_note(html(caption)) %>%
    opt_align_table_header(align = "center") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = everything()
      )
    )
  return(table)
}


gtsave(indiv_gt_func(offense, "Offensive", "Defense", FALSE), "offense.png", vwidth=1000, vheight=2500, zoom=1)
gtsave(indiv_gt_func(defense, "Defensive", "Offense", FALSE), "defense.png", vwidth=1000, vheight=2500, zoom=1)

sos_table <- sos %>% gt() %>%
  gt_nfl_logos(columns = gt::starts_with("logo")) %>%
  gt_nfl_wordmarks(columns = gt::starts_with("wordmark")) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  gt_hulk_col_numeric(c(everything(), -logo, -wordmark, -def_adj)) %>%
  gt_hulk_col_numeric(def_adj, reverse = TRUE) %>%
  cols_label(
    logo = md(""),
    wordmark = md("**Team**"),
    off_plays = md("**Off. Plays**"),
    off_adj = md("**Off. Adj.**"),
    def_plays = md("**Def. Plays**"),
    def_adj = md("**Def. Adj.**"),
    adj = md("**Total Adj.**")
  ) %>%
  tab_header(
    title = paste0("2023 NFL Strength of Schedule Rankings"),
    subtitle = md("*Based on a **Weighted Average** of **Offensive and Defensive Adjustments** for **EPA/Play***")
  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything()
    )
  )

gtsave(sos_table, "sos_table.png", vwidth = 1500, vheight = 1000)