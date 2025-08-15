llmSearch <- function(target_data,outDir,maxIter=5,LLM_ID = "qwen-3-coder-480b",inputs=list(),
                      init_template_path = "prompts/init_template_v1",
                      refine_template_path = "prompts/refine_template_v1",model_hints=""){
  options(device.ask.default = FALSE)
  source("R/llm_caller.R")
  source("R/optimization.R")
  source("R/create_summary_object.R")
  
  schema <- read_file("prompts/json_rules")
  
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  target_data_path <- file.path(outDir,"target_data.csv")
  target_data_packet_path <- file.path(outDir,"target_data_packet.json")
  
  llm_proposal_path <- file.path(outDir,"llm_proposals")
  summary_path <- file.path(outDir,"summaries")
  plot_path <- file.path(outDir,"plots")
  
  write_csv(target_data,target_data_path)
  packet_json <- summarize_for_llm(target_data, spec_inputs = inputs)
  writeLines(packet_json, target_data_packet_path)
  
  init_prompt <- compile_prompt(init_template_path,
                                packet_json=packet_json,
                                model_hints=model_hints,schema=schema)
  
  outpath <- file.path(llm_proposal_path,"prop_0.json")
  dir.create(dirname(outpath), showWarnings = FALSE, recursive = TRUE)
  llm_result <- call_llm(
    user_prompt = init_prompt,
    out_path = outpath,
    model = LLM_ID,
    temperature = 0.1,
    seed = 42)
  
  
  spec <- jsonlite::fromJSON(outpath, simplifyVector=FALSE)
  spec$noise <- NULL
  spec$replicates <- 1
  init_value <- optimize_model(spec,target_data,method="none")
  opt <- optimize_model(spec, target_data, method = "optim")
  spec_opt <- modify_spec_parms(opt$par,spec)
  
  df_ini <- simulate_from_spec(make_spec_transparent(spec))
  df_opt <- simulate_from_spec(make_spec_transparent(spec_opt))
  
  df_ini$id <- "init"
  df_opt$id <- "opt"
  
  df <- rbind(df_ini,df_opt)
  print("summarizing...")
  model_summary <- create_summary_object(
    llm_spec = spec,
    opt_results = opt,
    initial_objective_value = init_value,
    target_data_df = target_data,
    optimized_fit_df = df_opt
  )
  
  summary_output_path <- file.path(summary_path,"summary_0.json")
  save_summary_as_json(model_summary, summary_output_path)
  print("plotting...")
  plot_filepath <- file.path(plot_path,"fit_0.png")
  lut <- names(spec$observe)  
  names(lut) <- unlist(spec$observe)
  df$variable[df$variable%in%names(lut)] <- lut[df$variable[df$variable%in%names(lut)]]
  p <- ggplot(df,aes(x=time,y=value))+
    facet_grid(cols=vars(condition),rows = vars(variable),scales="free")+
    geom_line(aes(color=id))+
    geom_point(data=target_data,color="red")
  print(p)
  ggsave(plot_filepath,p)
  
  for(i in 1:maxIter){ 
    print(paste0("iteration: ",i))
    model_summary_json <- do.call(paste,lapply(0:(i-1),function(j){
      summary_output_path <- file.path(summary_path,paste0("summary_",j,".json"))
      model_summary_j <- read_file(summary_output_path)
      paste0("\nPROPOSAL ATTEMPT #",j,"\n",model_summary_j)
    }))
    
    prompt <- compile_prompt(refine_template_path,
                             packet_json=packet_json,
                             model_summary_json=model_summary_json,
                             model_hints=model_hints,schema=schema)
    
    outpath <- file.path(llm_proposal_path,paste0("prop_",i,".json"))
    dir.create(dirname(outpath), showWarnings = FALSE, recursive = TRUE)
    llm_result <- call_llm(
      user_prompt = prompt,
      out_path = outpath,
      model = LLM_ID,
      temperature = 0.1,
      seed = 42)
    
    spec <- jsonlite::fromJSON(outpath, simplifyVector=FALSE)
    spec$noise <- NULL
    spec$replicates <- 1
    init_value <- optimize_model(spec,target_data,method="none")
    opt <- optimize_model(spec, target_data, method = "optim")
    spec_opt <- modify_spec_parms(opt$par,spec)
    
    df_ini <- simulate_from_spec(make_spec_transparent(spec))
    df_opt <- simulate_from_spec(make_spec_transparent(spec_opt))
    
    df_ini$id <- "init"
    df_opt$id <- "opt"
    
    df <- rbind(df_ini,df_opt)
    
    model_summary <- create_summary_object(
      llm_spec = spec,
      opt_results = opt,
      initial_objective_value = init_value,
      target_data_df = target_data,
      optimized_fit_df = df_opt
    )
    
    summary_output_path <- file.path(summary_path,paste0("summary_",i,".json"))
    save_summary_as_json(model_summary, summary_output_path)
    
    plot_filepath <- file.path(plot_path,paste0("fit_",i,".png"))
    lut <- names(spec$observe)  
    names(lut) <- unlist(spec$observe)
    df$variable[df$variable%in%names(lut)] <- lut[df$variable[df$variable%in%names(lut)]]
    p <- ggplot(df,aes(x=time,y=value))+
      facet_grid(cols=vars(condition),rows = vars(variable),scales="free")+
      geom_line(aes(color=id))+
      geom_point(data=target_data,color="red")
    print(p)
    ggsave(plot_filepath,p)
  }
  
  
}


llmSearchMassonis <- function(target_data,outDir,maxIter=5,LLM_ID = "qwen-3-coder-480b",inputs=list(),
                      init_template_path = "prompts/init_template_v1",
                      refine_template_path = "prompts/refine_template_v1",model_hints="model is fully identifiable (no hidden states). Use only the states and state names observed in the data! Model has no hidden external input (no forcing function)."){
  options(device.ask.default = FALSE)
  source("R/llm_caller.R")
  source("R/massonis_utils.R")
  #source("R/create_summary_object.R")
  
  schema <- read_file("prompts/json_rules")
  
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  target_data_path <- file.path(outDir,"target_data.csv")
  target_data_packet_path <- file.path(outDir,"target_data_packet.json")
  
  llm_proposal_path <- file.path(outDir,"llm_proposals")
  summary_path <- file.path(outDir,"summaries")
  dir.create(summary_path, showWarnings = FALSE, recursive = TRUE)
  plot_path <- file.path(outDir,"plots")
  
  write_csv(target_data,target_data_path)
  packet_json <- summarize_for_llm_agg(target_data)
  writeLines(packet_json, target_data_packet_path)
  
  init_prompt <- compile_prompt(init_template_path,
                                packet_json=packet_json,
                                model_hints=model_hints,schema=schema)
  
  outpath <- file.path(llm_proposal_path,"prop_0.json")
  dir.create(dirname(outpath), showWarnings = FALSE, recursive = TRUE)
  llm_result <- call_llm(
    user_prompt = init_prompt,
    out_path = outpath,
    model = LLM_ID,
    temperature = 0.1,
    seed = 42)
  
  
  raw_output <- readr::read_file(outpath)
  if (stringr::str_detect(raw_output, "</think>")) {
    # The new format is present; strip the reasoning and extract the JSON
    message("Detected new output format. Stripping reasoning and parsing JSON...")
    
    json_string <- stringr::str_extract(raw_output, "(?s)</think>\\s*(.*)") |>
      stringr::str_remove("(?s)</think>\\s*")
    
    spec <- jsonlite::fromJSON(json_string)
    
  } else {
    # The old format is present; try to load the file directly as JSON
    message("New format not detected. Attempting to parse the entire file as JSON...")
    
    # This is your original code
    spec <- jsonlite::fromJSON(outpath)
    
  }
  spec$noise <- NULL
  spec$replicates <- 1
  
  cand <- llm_json_to_candidate(spec)
  theta0 <- theta0_from_llm(spec)
  res <- score_candidate_fit(target_data_path, cand, theta0, spar=NULL, scale="sd")
  print(res$optim_results)
  res$prior_spec <- spec
  
  model_summary <- res_to_string(res)
  
  summary_output_path <- file.path(summary_path,"summary_0.txt")
  write_file(model_summary,summary_output_path)
  
  
  for(i in 1:maxIter){ 
    print(paste0("iteration: ",i))
    model_summary <- do.call(paste,lapply(0:(i-1),function(j){
      summary_output_path <- file.path(summary_path,paste0("summary_",j,".txt"))
      model_summary_j <- read_file(summary_output_path)
      paste0("\nPROPOSAL ATTEMPT #",j,"\n",model_summary_j)
    }))
    
    prompt <- compile_prompt(refine_template_path,
                             packet_json=packet_json,
                             model_summary_json=model_summary,
                             model_hints=model_hints,schema=schema)
    
    outpath <- file.path(llm_proposal_path,paste0("prop_",i,".json"))
    llm_result <- call_llm(
      user_prompt = prompt,
      out_path = outpath,
      model = LLM_ID,
      temperature = 0.1,
      seed = 42)
    
    raw_output <- readr::read_file(outpath)
    if (stringr::str_detect(raw_output, "</think>")) {
      # The new format is present; strip the reasoning and extract the JSON
      message("Detected new output format. Stripping reasoning and parsing JSON...")
      
      json_string <- stringr::str_extract(raw_output, "(?s)</think>\\s*(.*)") |>
        stringr::str_remove("(?s)</think>\\s*")
      
      spec <- jsonlite::fromJSON(json_string)
      
    } else {
      # The old format is present; try to load the file directly as JSON
      message("New format not detected. Attempting to parse the entire file as JSON...")
      
      # This is your original code
      spec <- jsonlite::fromJSON(outpath)
      
    }
    spec$noise <- NULL
    spec$replicates <- 1
    
    cand <- llm_json_to_candidate(spec)
    theta0 <- theta0_from_llm(spec)
    res <- score_candidate_fit(target_data_path, cand, theta0, spar=NULL, scale="sd")
    print(res$optim_results)
    
    res$prior_spec <- spec
    
    model_summary <- res_to_string(res)
    
    
    summary_output_path <- file.path(summary_path,paste0("summary_",i,".txt"))
    write_file(model_summary,summary_output_path)
    
  }
  
  
}