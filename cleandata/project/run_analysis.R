pkg <- c("dplyr", "tidyr")
lapply(pkg, require, character.only=T)

to_dplyr <- function(f) {
    f <- read.table(f)
    tbl_df(f)
}

path <- getwd()
data_path <- file.path(path, "cleandata/project/UCI HAR Dataset")
subject_train <- read.table(file.path(data_path, "train", "subject_train.txt"))
subject_test <- read.table(file.path(data_path, "test", "subject_test.txt"))
y_train <- read.table(file.path(data_path, "train", "y_train.txt"))
y_test <- read.table(file.path(data_path, "test", "y_test.txt"))
x_train <- to_dplyr(file.path(data_path, "train", "X_train.txt"))
x_test <- to_dplyr(file.path(data_path, "test", "X_test.txt"))
features <- to_dplyr(file.path(data_path, "features.txt"))
act_names <- to_dplyr(file.path(data_path, "activity_labels.txt"))


act_names<-act_names %>%
  rename(activity_num=V1, activity_name=V2)
features<-features %>%
  rename(feature_num=V1, feature_name=V2)%>%
  filter(feature_name=grepl("mean\\(\\)|std\\(\\)", feature_name))%>%
  mutate(feature_num=paste0("V", feature_num))
subjects <- rbind(subject_train, subject_test)
y <- rbind(y_train, y_test)
x <- rbind_list(x_train, x_test)
dt<-x %>%
  select(one_of(features$feature_num)) %>%
  mutate(subjects=as.double(subjects$V1), activity_num=as.double(y$V1))
dt<-left_join(dt, act_names, by="activity_num")
dt<-dt %>%
  group_by(subjects, activity_num, activity_name)%>%
  gather()