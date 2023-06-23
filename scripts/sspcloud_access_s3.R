#Script

install.packages("paws", repos = "https://cloud.R-project.org")

Sys.setenv("AWS_ACCESS_KEY_ID" = "1XJ0ZSBL7EWRIU83IR13",
           "AWS_SECRET_ACCESS_KEY" = "J+lgx6lc0Ok8RYOmN9gF6Ld9wZsXrOEeW+zQHrGT",
           "AWS_DEFAULT_REGION" = "us-east-1",
           "AWS_SESSION_TOKEN" = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiIxWEowWlNCTDdFV1JJVTgzSVIxMyIsImFsbG93ZWQtb3JpZ2lucyI6WyIqIl0sImF1ZCI6WyJtaW5pby1kYXRhbm9kZSIsIm9ueXhpYSIsImFjY291bnQiXSwiYXV0aF90aW1lIjoxNjg3NDM1MTMyLCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6InZ1aWxsb3RhQGFmZC5mciIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJleHAiOjE2ODc1MjE1NDMsImZhbWlseV9uYW1lIjoiVnVpbGxvdCIsImdpdmVuX25hbWUiOiJBbnRvaW5lIiwiZ3JvdXBzIjpbXSwiaWF0IjoxNjg3NDM1MTM0LCJpc3MiOiJodHRwczovL2F1dGgubGFiLnNzcGNsb3VkLmZyL2F1dGgvcmVhbG1zL3NzcGNsb3VkIiwianRpIjoiYjgyMzkzZDQtMjg1Ny00NWViLTkyYTMtNGI1NTZkZjVlODQyIiwibmFtZSI6IkFudG9pbmUgVnVpbGxvdCIsIm5vbmNlIjoiMzEyZmE3ZDUtM2IxOC00OGJjLThiMjUtNmY4OTc3NjYxM2M2IiwicG9saWN5Ijoic3Rzb25seSIsInByZWZlcnJlZF91c2VybmFtZSI6InZ1aWxsb3RhIiwicmVhbG1fYWNjZXNzIjp7InJvbGVzIjpbIm9mZmxpbmVfYWNjZXNzIiwidW1hX2F1dGhvcml6YXRpb24iLCJkZWZhdWx0LXJvbGVzLXNzcGNsb3VkIl19LCJyZXNvdXJjZV9hY2Nlc3MiOnsiYWNjb3VudCI6eyJyb2xlcyI6WyJtYW5hZ2UtYWNjb3VudCIsIm1hbmFnZS1hY2NvdW50LWxpbmtzIiwidmlldy1wcm9maWxlIl19fSwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBncm91cHMgZW1haWwiLCJzZXNzaW9uX3N0YXRlIjoiMGNkMDZhYjQtNGU2NC00ZDNlLTliOWQtNGYwZDE4NmViOTJmIiwic2lkIjoiMGNkMDZhYjQtNGU2NC00ZDNlLTliOWQtNGYwZDE4NmViOTJmIiwic3ViIjoiOTI3N2Y3MzMtNTBmMy00MjM4LWI1YzctNjRjMmU4YTZiNDI0IiwidHlwIjoiQmVhcmVyIn0.Ao5W3QbfVDAN1GsTddCRHBooCT89TcOsBR1HbPdKBfcI2mWQKgXbyGB08MDPC1gc534Zyt3ve1kdNKyUMIxUNg",
           "AWS_S3_ENDPOINT"= "minio.lab.sspcloud.fr")

library("paws")
minio <- paws::s3(config = list(
  credentials = list(
    creds = list(
      access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
      secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      session_token = Sys.getenv("AWS_SESSION_TOKEN")
    )),
  endpoint = paste0("https://", Sys.getenv("AWS_S3_ENDPOINT")),
  region = Sys.getenv("AWS_DEFAULT_REGION")))

minio$list_buckets()
