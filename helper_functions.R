### Helper functions to uppercase the column

uppercased<-function(df, col_name) {
  
  df[[col_name]] <- toupper(df[[col_name]])
  df[[col_name]]<-as.factor(df[[col_name]])
  df
}

read_processed_csv<-function(file_path) {
  
  df<-read.csv(file_path, encoding = "UTF-8",quote="`", sep = ";", stringsAsFactors = TRUE)  
  df$X<-NULL 
  df$"Unnamed..0"<-NULL
  df
} 

### Helper function to import the Python generated data

read_processed_csv<-function(file_path) {
  
  df<-read.csv(file_path, encoding = "UTF-8",quote="`", sep = ";", stringsAsFactors = TRUE)  
  df$X<-NULL 
  df$"Unnamed..0"<-NULL
  df
} 
