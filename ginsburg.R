# This file contains functions to send models to ginsburg
# Prereq: apt-get install libssh-dev; install.packages("ssh"); install.packages("testit")

library(ssh)
library(testit)
library(stringr)

# Function to parse ssh output
sshout <- function(out) unlist(strsplit(rawToChar(out$stdout), "\n"))

# Function that uploads files to Ginsburg
send_file <- function(session, file, fld){
    scp_upload(session, file, to = fld)
    file.remove(file)
}

open_session <- function(user){
  pwd <- Sys.getenv("ginsburg_pwd")
  
  if (pwd == "") {
    print("No password saved. Input your password")
    pwd = readline()
    Sys.setenv(ginsburg_pwd = pwd)
  }
  
  
  # Open session
  session <- ssh_connect(paste0(user, "@burg.rcs.columbia.edu"),
                         passwd = Sys.getenv("ginsburg_pwd"))
  
  return(session)
  
}

# Create and save r script to run model
save_r_script <- function(r_library,
    data_file,
    formula,
    prior,
    chains,
    iter,
    warmup,
    cores,
    control,
    model_name,
    seed,
    save_output,
    criteria,
    confirm,
    kfold_threads = NULL){

    # Make sure we only save model if we want
    model_file <- ifelse(save_output, str_glue("'{model_name}'"), "NULL")
    
    # Compute threads
    threads <- floor(cores / chains)

    # Create r script to run model
    script <- c(
        str_glue(".libPaths('{r_library}')"),
        str_glue('unixtools::set.tempdir(normalizePath("../../tmp"))'),
        "print(tempdir())",
        "library(brms)",
        "library(cmdstanr)",
        "st <- Sys.time()",
        str_glue("load('./{data_file}')"),
        str_glue("(m <- brm({formula},
            data = data,
            prior = {prior},
            chains = {chains},
            iter = {iter},
            warmup = {warmup},
            cores = {chains},
            backend = 'cmdstanr',
            threads = threading({threads}),
            control = {control},
            file = {model_file},
            save_pars = save_pars(all = TRUE),
            silent=F,
            seed = {seed}))")
    )

    # Add criteria if needed
    if (!is.null(criteria)){
      
      if (criteria == "kfold") {
        kfold_threads <- floor(cores / 10)
        script <- c(script,
                    "options(future.globals.maxSize=10000*1024^2)",
                    str_glue("m <- add_criterion(m, criterion = 'kfold', 
                             threads = threading({cores}))"))
      }
      else {
        script <- c(script,
                    "options(future.globals.maxSize=5000*1024^2)",
                    str_glue("add_criterion(m, '{criteria}', cores = {cores}, 
                     moment_match = T, reloo = T)"))
      }
    }
    
    # Print time to run
    script <- c(script,
                "print(Sys.time() - st)")

    # Save .R file locally
    script_file_name <- str_glue("launch_{model_name}.R")
    writeLines(script, script_file_name)

    if (confirm){
        print("Going to run following script:\n")
        writeLines(script)
        invisible(readline(prompt="Press [enter] to continue"))
    }

    return(script_file_name)
}

# Create and save sh script to launch job
save_sh_script <- function(lab,
    model_name,
    cores,
    wall_time,
    memory,
    r_file_name) {
    script <- c(
        "#!/bin/sh",
        str_glue("#SBATCH --account={lab}"),
        str_glue("#SBATCH --job-name={model_name}"),
        str_glue("#SBATCH -c {cores}"),
        str_glue("#SBATCH --time={wall_time}"),
        "#SBATCH --output=R-%x.%j.out",
        "#SBATCH --error=R-%x.%j.err"
    )

    # Add memory requirement if one exists
    if (!is.null(memory)){
        script <- c(
            script, 
            str_glue("#SBATCH --mem-per-cpu={memory}")
        )
    }

    script <- c(
        script, 
        "module load R",
        str_glue("Rscript {r_file_name}")
    )

    # Save .R file locally
    script_file_name <- str_glue("submit_{model_name}.sh")
    writeLines(script, script_file_name)

    return(script_file_name)
}

# Master function that runs all
launch_model <- function(
    formula = "y ~ x",
    data = data.frame(x = rnorm(1000), y = rnorm(1000)),
    prior = "prior(normal(0,1), class = 'b')",
    model_name = "m1_0",
    save_output = FALSE,
    iter = 2000,
    warmup = floor(iter/2),
    chains = 3,
    seed = 1,
    cores = 3,
    control = "list()",
    memory = NULL,
    wall_time = "0-0:30",
    project = "test",
    lab = "dslab",
    user = "ya2402",
    r_library = "/burg/dslab/users/ya2402/R_lib",
    saved_models_fld = "../saved_models/",
    criteria = NULL,
    confirm = TRUE,
    force = F
) {
    # Test no spaces in project folder name
    assert("No spaces allowed in project name", !grepl(" ", project))

    # Paths
    ufld <- paste0("/burg/", lab, "/users/", user) # User folder on Ginsburg
    fld <- paste0(ufld, "/autorun_models/", project) # Project folder to work in on Gisnburg
    data_file <- paste0(model_name, "_data.rda")
    local_model_file <- file.path(saved_models_fld, paste0(model_name, ".rds"))

    # Don't run if local saved model exists, mimicking brm behaviour
    if(file.exists(local_model_file) & !force){
        print(paste0("Found local saved model ", model_name, ", not runninng."))
        return()
    }
    
    # Prepare R script
    r_script <- save_r_script(r_library,
                              data_file,
                              formula,
                              prior,
                              chains,
                              iter,
                              warmup,
                              cores,
                              control,
                              model_name,
                              seed,
                              save_output,
                              criteria,
                              confirm)
    

    # Open session
    session <- open_session(user)

    # Test that r_library folder exists
    tryCatch(
        ssh_exec_internal(session, str_glue("ls {r_library}")),
        error = function(cond) {
            message(str_glue("r libraries folder doesn't seem to ",
                "exist on Ginsburg: {r_library}"))
            message("Here's the original error message:")
            message(cond)
        }
    )

    # Create a projects folder if needed
    ssh_exec_wait(session, paste("mkdir -p", fld))

    # Send rscript to server
    send_file(session, r_script, fld)
    
    # Send data to server
    save(data, file = data_file)
    send_file(session, data_file, fld)

    # Send sh to server
    sh_script <- save_sh_script(
        lab,
        model_name,
        cores,
        wall_time,
        memory,
        r_script
    )
    send_file(session, sh_script, fld)

    # Launch task on server
    out <- ssh_exec_internal(session, command = c(
        str_glue("cd {fld}"),
        str_glue("sbatch {sh_script}")
        )
    )

    ssh_disconnect(session)

    # Return jobid
    return(gsub("\\D", "", rawToChar(out$stdout)))
}

check_status <- function(
    jobid,
    project = "test",
    lab = "dslab",
    user = "ya2402"
) {

    ## Move to seperate function
    # Test no spaces in project folder name
    assert("No spaces allowed in project name", !grepl(" ", project))

    # Paths
    ufld <- paste0("/burg/", lab, "/users/", user) # User folder on Ginsburg
    fld <- paste0(ufld, "/autorun_models/", project) # Project folder to work in on Gisnburg

    # Open session
    session <- open_session(user)

    # Print queue
    print("Running jobs:")
    ssh_exec_wait(session, command = c(
        str_glue("squeue -u {user}")
        ))

    # Print last line of file
    print("Current end of job output:")
    ssh_exec_wait(session, command = c(
        str_glue("cd {fld}"),
        str_glue("tail -2 slurm-{jobid}.out")
        ))

    ssh_disconnect(session)
}

fetch_results <- function(
    model_name = "m1_0",
    project = "test",
    lab = "dslab",
    user = "ya2402",
    saved_models_fld = "../saved_models/",
    force = F
) {
    library(testit)
    library(ssh)

    ## Move to seperate function
    # Test no spaces in project folder name
    assert("No spaces allowed in project name", !grepl(" ", project))

    # Paths
    ufld <- paste0("/burg/", lab, "/users/", user) # User folder on Ginsburg
    fld <- paste0(ufld, "/autorun_models/", project) # Project folder to work in on Gisnburg
    model_file <- paste0(model_name, ".rds")  # Saved model file on Ginsburg
    local_model_file <- file.path(saved_models_fld, model_file)

    if (!file.exists(local_model_file) | force){
        # Open session
        session <- open_session(user)

        # Download files
        scp_download(session, 
            file.path(fld, model_file), 
            to = saved_models_fld,
            verbose = T)

        ssh_disconnect(session)
    }


    return(brm(file = local_model_file))
}

# Add criterion to existing model
gins_add_criterion <- function(
    criteria = NULL,
    model_name = "m1_0",
    cores = 3,
    memory = NULL,
    wall_time = "0-0:30",
    project = "test",
    lab = "dslab",
    user = "ya2402",
    r_library = "~",
    saved_models_fld = "../saved_models/"
) {
    # Test no spaces in project folder name
    assert("No spaces allowed in project name", !grepl(" ", project))

    # Paths
    ufld <- paste0("/burg/", lab, "/users/", user) # User folder on Ginsburg
    fld <- paste0(ufld, "/autorun_models/", project) # Project folder to work in on Gisnburg
    data_file <- paste0(model_name, "_data.rda")
    local_model_file <- file.path(saved_models_fld, paste0(model_name, ".rds"))

    # Open session
    session <- open_session(user)

    # Test that r_library folder exists
    tryCatch(
        ssh_exec_internal(session, str_glue("ls {r_library}")),
        error = function(cond) {
            message(str_glue("r libraries folder doesn't seem to ",
                "exist on Ginsburg: {r_library}"))
            message("Here's the original error message:")
            message(cond)
        }
    )

    # Send saved model file to server if not found

    # Create r script to add criterion
    rscript <- c(
        str_glue(".libPaths('{r_library}')"),
        "library(brms)",
        str_glue("(m <- brm(
            file = '{model_name}'))"),
        str_glue("add_criterion(m, '{criteria}', 
            cores = {cores},
            overwrite = TRUE,
            moment_match = TRUE)")
    )

    # Save .R file locally
    rscript_file_name <- str_glue("add_criteria_{model_name}.R")
    writeLines(rscript, rscript_file_name)

    # Send to Ginsburg
    send_file(session, rscript_file_name, fld)

    # Send sh to server
    sh_script <- save_sh_script(
        lab,
        str_glue("add_crit_{model_name}"),
        cores,
        wall_time,
        memory,
        rscript_file_name
    )
    send_file(session, sh_script, fld)

    # Launch task on server
    out <- ssh_exec_internal(session, command = c(
        str_glue("cd {fld}"),
        str_glue("sbatch {sh_script}")
        )
    )

    ssh_disconnect(session)

    # Return jobid
    return(gsub("\\D", "", rawToChar(out$stdout)))
}
