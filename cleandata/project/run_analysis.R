pkg <- c("dplyr", "tidyr", "magrittr")
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
dt <- x %>%
  select(one_of(features$feature_num)) %>%
  mutate(subjects=as.double(subjects$V1), activity_num=as.double(y$V1))
dt <- left_join(dt, act_names, by="activity_num")
dt <- dt %>%
  arrange(subjects, activity_num, activity_name)%>%
  group_by(subjects, activity_num, activity_name)%>%
  gather(feature_num, val, -c(subjects, activity_num, activity_name))
dt <- left_join(dt, features, by="feature_num")


b <- matrix(seq(1, 2), nrow=2)
b1 <- matrix(seq(1, 3), nrow=3)
dt <- dt %>%
  mutate(feat_domain=factor(matrix(c(grepl("^t", feature_name), grepl("^f", feature_name)), ncol=nrow(b)) %*% b, labels=c("Time", "Freq")))%>%
  mutate(feat_instrument=factor(matrix(c(grepl("Acc", feature_name), grepl("Gyro", feature_name)), ncol=nrow(b)) %*% b, labels=c("Accelerometer", "Gyroscope")))%>%
  mutate(feat_accleration=factor(matrix(c(grepl("BodyAcc", feature_name), grepl("GravityAcc", feature_name)), ncol=nrow(b)) %*% b, labels=c(NA, "Body", "Gravity")))%>%
  mutate(feat_variable=factor(matrix(c(grepl("mean()", feature_name), grepl("std()", feature_name)), ncol=nrow(b)) %*% b, labels=c("Mean", "SD")))%>%
  mutate(feat_jerk=factor(grepl("Jerk", feature_name), labels=c(NA, "Jerk")))%>%
  mutate(feat_magnitude=factor(grepl("Mag", feature_name), labels=c(NA, "Magnitude")))%>%
  mutate(feat_axis=factor(matrix(c(grepl("-X", feature_name), grepl("-Y", feature_name), grepl("-Z", feature_name)), ncol=nrow(b1)) %*% b1, labels=c(NA, "X", "Y", "Z")))%>%
  select(-feature_name)

tidy_dt <- dt %>%
  group_by(subjects, activity_name, feat_domain, feat_instrument, feat_accleration, feat_variable, feat_jerk, feat_magnitude, feat_axis)%>%
  summarise(average=mean(val), count=n())