subject_name = c("kid","danehko","lucio")
temperature = c(98.1,98.6,102)
flu_status = c(FALSE, FALSE, TRUE)
gender = factor(c("MALE","MALE","MALE"))
blood = factor(c("AB","O","A"),levels = c("A","B","AB","O"))
symptoms = factor(c("SEVERE","MILD","MODERATE"), levels = c("MILD","MODERATE","SEVERE"),ordered = TRUE)
pt_data = data.frame(subject_name, temperature, flu_status, gender, blood, symptoms, stringsAsFactors = FALSE)