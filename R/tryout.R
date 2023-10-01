devtools::install_github("albertostefanelli/cjsimPWR")
library(cjsimPWR)


df_power <- power_sim(
  n_attributes = 3,
  n_levels = c(2, 3, 5),
  n_tasks = 5,
  units = c(600),
  true_coef = list(0.2,
                   c(-0.1, 0.1),
                   c(-0.03, -0.1, -0.1, 0.1)),
  sigma.u_k = 0.05,
  sim_runs = 100,
  seed = 2334
)

df_power |> print(n=40)

df_power_interaction <- power_sim(
  n_attributes = 3,
  n_levels = c(2, 3, 6),
  n_tasks = 4,
  group_name = c("Democrats", "Independents", "Republicans", "Non-identifiers"),
  units = c(500, 200, 500, 100),
  true_coef = list("Democrats" = list(0.2, c(-0.1, 0.1), c(-0.1, -0.1, -0.1, 0.1, -0.03)),
                   "Independents" = list(0.1, c(-0.2, 0.05),  c(-0.1, 0.1, 0.1, 0.3, 0.01)),
                   "Republicans" = list(0.1, c(-0.1, -0.0005),  c(-0.1, 0.2, -0.1, 0.1, -0.01)),
                   "Non-identifiers" = list(0.1, c(-0.1, -0.05),  c(-0.1, 0.2, -0.1, 0.1, -0.01))
  ),
  sigma.u_k = 0.05,
  sim_runs = 100,
  seed = 12
)

df_power_interaction |> print(n=40)





