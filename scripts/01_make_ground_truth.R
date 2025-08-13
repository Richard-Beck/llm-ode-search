Rseed <- 41
setwd("~/projects/llm-ode-search/")
source("R/ground_truth_simulator.R")
spec <- jsonlite::fromJSON("data/ground_truth_specs/droop.json", simplifyVector=FALSE)
df <- simulate_from_spec(spec, seed=Rseed)
spec$sampling$dt <- 8
spec$observe<-spec$observe[c("X","S")]
df_sam <- simulate_from_spec(spec, seed=Rseed)
target_data_df <- df_sam %>%
  group_by(condition, time, variable, model) %>%
  summarise(value = mean(value), .groups = "drop")
write_csv(target_data_df, "results/csv/droop.csv")
packet_json <- summarize_for_llm(df_sam, spec_inputs = spec$inputs)
writeLines(packet_json, "data/LLM_data_packets/droop.json")
p <- ggplot(df,aes(x=time,y=value))+
  facet_grid(cols=vars(condition),rows = vars(variable),scales="free")+
  geom_line()+
  geom_point(data=df_sam,color="red")
p

spec <- jsonlite::fromJSON("data/ground_truth_specs/edelstein.json", simplifyVector=FALSE)
df <- simulate_from_spec(spec, seed=Rseed)
spec$sampling$dt <- 4
spec$observe<-spec$observe[c("Rstar")]
df_sam <- simulate_from_spec(spec, seed=Rseed)
target_data_df <- df_sam %>%
  group_by(condition, time, variable, model) %>%
  summarise(value = mean(value), .groups = "drop")
write_csv(target_data_df, "results/csv/edelstein.csv")
packet_json <- summarize_for_llm(df_sam, spec_inputs = spec$inputs)
writeLines(packet_json, "data/LLM_data_packets/edelstein.json")
p <- ggplot(df,aes(x=time,y=value))+
  facet_grid(cols=vars(condition),rows = vars(variable),scales="free")+
  geom_line()+
  geom_point(data=df_sam,color="red")
p

spec <- jsonlite::fromJSON("data/ground_truth_specs/beddington.json", simplifyVector=FALSE)
df <- simulate_from_spec(spec, seed=Rseed)
spec$sampling$dt <- 8
df_sam <- simulate_from_spec(spec, seed=Rseed)
target_data_df <- df_sam %>%
  group_by(condition, time, variable, model) %>%
  summarise(value = mean(value), .groups = "drop")
write_csv(target_data_df, "results/csv/beddington.csv")
packet_json <- summarize_for_llm(df_sam, spec_inputs = spec$inputs)
writeLines(packet_json, "data/LLM_data_packets/beddington.json")
p <- ggplot(df,aes(x=time,y=value))+
  facet_grid(cols=vars(condition),rows = vars(variable),scales="free")+
  geom_line()+
  geom_point(data=df_sam,color="red")
p