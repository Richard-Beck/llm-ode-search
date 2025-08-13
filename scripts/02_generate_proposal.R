source("R/llm_caller.R")
models <- list.files("data/LLM_data_packets/")
m <- models[3]
sys <- "You are an ODE-specification engine. Return ONE valid JSON object and nothing else (no code fences, no comments, no Markdown). All numbers must be finite JSON numbers (no NaN/Inf). Do not include any keys other than those allowed by the schema the user provides."

user_tpl <- readChar("prompts/init_template_v1", file.info("prompts/init_template_v1")$size) # or paste the template above into a string

packet_path <- paste0("data/LLM_data_packets/",m)   # array format
model_hints <- ""
prompt <- glue::glue(user_tpl, packet_json = read_packet_text(packet_path), model_hints = model_hints,
                     .open = "{{",
                     .close = "}}")

out <- cerebras_call(
  packet_path = packet_path,         # not strictly needed now; we already inlined packet_json
  prompt_template = prompt,      # we already rendered the full prompt
  model = "qwen-3-coder-480b",
  #model="qwen-3-235b-a22b-instruct-2507",
  template_vars = list(prompt = prompt),
  system_preamble = sys,
  out_path = paste0("data/llm_proposals/",m) 
)


s0 <- fromJSON(paste0("data/ground_truth_specs/",m), simplifyVector = FALSE)
r0 <- simulate_from_spec(s0)
s <- fromJSON(paste0("data/llm_proposals/",m), simplifyVector = FALSE)
res <- simulate_from_spec(s)
library(ggplot2)
p <- ggplot(r0,aes(x=time,y=value,group=replicate))+
  facet_grid(cols=vars(condition),rows = vars(variable),scales="free")+
  geom_line()+
  geom_point(data=res,color="red")
p
