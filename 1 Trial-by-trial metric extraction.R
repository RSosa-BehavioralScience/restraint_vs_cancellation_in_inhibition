#Set the R console language to English to make troubleshooting and help seeking more
#straightforward
Sys.setenv(LANG = "en")

#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)

#Import .csv file named "Global raw database"
#This line will generate a pop-up window for manually selecting the file!
events <- read.csv (file.choose(),fileEncoding="UTF-8-BOM") 

#Code column names in English
colnames(events) <- c("n", "rat", "timestamp", "label", "session", "condition")

#Remove unused column
events <- select(events, -c(n))

#Code event labels in English
translate_labels <- list(
  "Inicio de sesion" = "session starts",
  "Entrada comedero" = "entry",
  "Salida comedero" = "exit",
  "8s para ensayo" = "pre trial",
  "Inicio ensayo Ax-" = "inh trial begins",
  "Fin ensayo Ax-" = "inh trial ends",
  "Inicio ensayo A+" = "exc trial begins",
  "Entrega pellet" = "food delivery"
)
events$label <- ifelse(events$label %in% names(translate_labels), translate_labels[events$label], events$label)
events$label <- as.character(events$label)

#Change subject names
old_names <- c("CDX_DRN_1", "G1_DRN_1", "G7_DRN_1", "G8_DRN_1", "H2_DRN_1", "H7_DRN_1", "H8_DRN_1", 
               "H9_DRN_1", "MM24_DRN", "MM29_DRN", "MM7_DRN__1", "R1_DRN_1", "R2_DRN_1", "R3_DRN_1",
               "R4_DRN_1", "R8_DRN_1")

new_names <- c("CDX", "G1", "G7", "G8", "H2", "H7", "H8", "H9", "MM24", "MM29", "MM7", "R1", "R21", 
               "R3", "R4", "R8")

name_mapping <- setNames(new_names, old_names)

events$rat <- name_mapping[events$rat]

#Subjects' previous experience
experience_table <- data.frame(
  Subject = c("CDX", "G1", "G7", "G8", "H2", "H7", "H8", "H9",
              "R1", "R21", "R3", "R4", "R8",
              "MM24", "MM29", "MM7"),
  Previous_Experience = c(rep("Pharmacological trials and open field testing", 8),
                          rep("Flavor-sucrose conditioning", 5),
                          rep("Administered pharmacological agents during operant conditioning", 3))
)

print(experience_table)

#Classify food deliveries according to their sequential order
j <- 1
for (i in 1:nrow(events)){
  if (events$label[i]=="food delivery"){
    if (j==1){
      events$label[i] <- "food delivery 1"
      j <- 2
    } else {
      events$label[i] <- "food delivery 2"
      j <- 1}
  }
}
#Add the appropriate label to the last event recorded during a daily session
for (i in 1:nrow(events)){
  if (events$label[i] == "session starts" & events$timestamp[i] > 0){
    events$label[i] <- "session finishes"
  }
}

#Generate a progress bar object to monitor the duration of computationally intense routines
pb <- txtProgressBar(min = 1,
                     max = nrow(events),
                     style = 3,
                     width = 100,
                     char = "=")


#Create a column to indicate whether rats were inside or outside of the food hub for each event
#Consider getting cup of coffee, maybe
for (i in 1:nrow(events)){
  setTxtProgressBar(pb, i)
  if (events$label[i]=="session starts"){
    events$status[i] <- NA
  }
  else {if (events$label[i]=="entry"){
    events$status[i] <- "in"
  }
  else {if (events$label[i]=="exit"){
    events$status[i] <- "out"
  } else {events$status[i] <- events$status[i-1]}}}
}
close(pb)

#Add some missing status labels 
for (i in nrow(events):1){
  if (events$label[i]!="session finishes"){
    if (is.na(events$status[i])){
      if (events$label[i+1]!="session starts"){
        if (events$status[i+1]=="in"){
          events$status[i] <- "out"
        } else {events$status[i] <- "in"}
      }
    }
  }
}

#Create a data frame to store information segmented by individual trials
trials <- data.frame()

#Generate empty vectors that will constitute the columns in the 'trials' data frame
trial_type <- NA
response_type <- NA 
first_response <- NA
latency <- NA
withdrawal_time <- NA
time_to_withdraw <- NA
duration_t <- NA

#Gather data to calculate metrics and organize the 'trials' data frame
for (i in 1:nrow(events)){
  setTxtProgressBar(pb, i)
  if (events$label[i]=="session starts"){
    trial_beginning <- 0
    gen_trial_counter <- 0
    exc_trial_counter <- 0
    inh_trial_counter <- 0
  }
  if (events$label[i]=="exc trial begins"){
    gen_trial_counter <- gen_trial_counter + 1
    exc_trial_counter <- exc_trial_counter + 1
    specific_trial_no <- exc_trial_counter
    trial_type <- "exc"
    inter_trial_interval <- events$timestamp[i] - trial_beginning
    trial_beginning <- events$timestamp[i]
    if (events$status[i]=="in"){
      response_type <- "already in"
    } else {response_type <- "no response yet"}
  }
  if (events$label[i]=="inh trial begins"){
    gen_trial_counter <- gen_trial_counter + 1
    inh_trial_counter <- inh_trial_counter + 1
    specific_trial_no <- inh_trial_counter
    trial_type <- "inh"
    inter_trial_interval <- events$timestamp[i] - trial_beginning
    trial_beginning <- events$timestamp[i]
    if (events$status[i]=="in"){
      response_type <- "already in"
    } else {response_type <- "no response yet"} 
  } 
  if (!is.na(trial_type=="exc") | !is.na(trial_type=="inh")){
    if (events$label[i]=="entry"){
      if (response_type=="no response yet"){
        first_response <- events$timestamp[i]
        latency <- first_response - trial_beginning
        response_type <- "w response"
      }
    }
  }
  if (!is.na(response_type == "w response") | !is.na(response_type == "already in")){
    if (events$label[i]=="exit"){
      withdrawal_time <- events$timestamp[i]
    }
  }
  if (events$label[i] == "food delivery 1" | events$label[i] == "inh trial ends"){
    trial_ending <- events$timestamp[i]
    if (!is.na(response_type)){
      if (response_type == "no response yet"){
        response_type <- "wo response"
        position_at_trial_ending <- NA
        duration <- NA
        duration_q <- NA
      } else {
        if (events$status[i]=="out"){
          position_at_trial_ending <- "withdrawn"
          if (response_type == "w response"){
            time_to_withdraw <- withdrawal_time - first_response
            duration_t <- trial_ending - first_response
            duration_q <- time_to_withdraw / duration_t
          }
          if (response_type == "already in"){
            time_to_withdraw <- withdrawal_time - trial_beginning
            duration_t <- trial_ending - trial_beginning
            duration_q <- time_to_withdraw / duration_t
          }
        } else {
          position_at_trial_ending <- "finished inside"
          withdrawal_time <- NA
          if (response_type == "w response"){
            time_to_withdraw <- NA
            duration_t <- trial_ending - first_response
            duration_q <- 1
          }
          if (response_type == "already in"){
            time_to_withdraw <- NA
            duration_t <- trial_ending - trial_beginning
            duration_q <- 1
          }
          
        }
      }
    }
    trial_info <- data.frame(rat = events$rat[i],
                             session = events$session[i],
                             condition = events$condition[i],
                             trial = gen_trial_counter,
                             trial_type = trial_type,
                             specific_trial_no = specific_trial_no, 
                             trial_beginning = trial_beginning,
                             first_response = first_response,
                             latency = latency,
                             response_type = response_type,
                             position_at_trial_ending = position_at_trial_ending,
                             withdrawal_time = withdrawal_time,
                             trial_ending = trial_ending,
                             time_to_withdraw = time_to_withdraw,
                             duration_t = duration_t,
                             duration_q = duration_q,
                             inter_trial_interval = inter_trial_interval)
    trials <- rbind(trials, trial_info)
    trial_type <- NA
    first_response <- NA
    latency <- NA
    response_type <- NA 
    position_at_trial_ending <- NA
    withdrawal_time <- NA
    trial_ending <- NA
    time_to_withdraw <- NA
    duration_t <- NA
    duration_q <- NA
    inter_trial_interval <- NA
  }
}
close(pb)

#Save the newly created data frames to your computer as a .csv file (optional)
write.csv(events, "events.csv", row.names=FALSE)
write.csv(trials, "trials.csv", row.names=FALSE)

#Now, you may proceed with script 1.1 or save the current environment to continue later.
