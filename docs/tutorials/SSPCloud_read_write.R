#Importing relevant libraries

library(dplyr)
library(ggplot2)
library(data.table)
library(aws.s3)
library(ARTofR)

#####
#By creating a service in my personal account, the R session has automatically the credentials to access the personal storage, or bucket ("mes fichiers").
#Note that one can access any files in the folder named "diffusion" in an other user bucket.
#####

#Creating a dataframe
df = cars

#Writing the dataset in my bucket
s3write_using(x = df, #the object to write
              FUN = data.table::fwrite, #the function used to write
              #options of the FUN
              na = "", #example of option for fwrite
              #writing arguments
              object = "df_cars.csv", #the path and name of the file to write
              bucket = "vuillota", #The name of the bucket (identifiant IDEP in "Mon compte")
              opts = list("region" = "") #Purely technical option, but source of error if "region" = "" not specified
              )


#Reading the dataset in my bucket
##The arguments are the same than in s3write_using(), except for "x".
df_read = s3read_using(FUN = data.table::fread,
                       encoding = "UTF-8",
                       #Reading arguments
                       object = "df_cars.csv", #the path and name of the file to read
                       bucket = "vuillota", #The name of the bucket (identifiant IDEP in "Mon compte")
                       opts = list("region" = "") #Purely technical option, but source of error if "region" = "" not specified
)


#Create a plot with ggplot2::ggplot()

fig_cars = ggplot(data = df,
             aes(x = speed, y = dist)) %>%
  + geom_point() %>%
  + geom_line() %>%
  + theme_minimal()
fig_cars

#Saving the plot in the bucket
##As far as I know, it is not possible to use ggplot2::ggsave() directly to save in a bucket. 
##Instead, one can save the plot in the temporary folder associated with the R session, and move it to the bucket

###Creating a sub-folder in the temporary folder
tmp = paste(tempdir(), "fig", sep = "/")

###save it in the temporary folder 
ggsave(paste(tmp, "fig_cars.png", sep = "/"),
       plot = fig_cars,
       device = "png",
       height = 6, width = 9)

###List of files to save in the temp folder
files = list.files(tmp, full.names = TRUE)

###In a loop, add each file in the bucket (same bucket folder for every file in the tmp folder)
###Here, I add the figures in the "fig" folder. If this "fig" folder does not exist, it is created automatically.
for(f in files) 
{
  cat("Uploading file", paste0("'", f, "'"), "\n")
  aws.s3::put_object(file = f,
                     bucket = "vuillota/fig", 
                     region = "", 
                     show_progress = TRUE)
}
