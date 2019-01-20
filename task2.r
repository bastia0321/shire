library(data.table)
library(scales)
library(dplyr)
library(ggplot2)
library(tidyquant)


transaction_data <- read.csv('transaction data updated.csv', stringsAsFactors = FALSE)
DT1 <- data.table(transaction_data)
DT1[, transaction_date := as.Date(transaction_date)]
###check which patients have 2+ switches
DT1_level <- DT1[, .(levels = length(unique(product_name))), by = patient_id]
patient_candidate <- DT1_level[DT1_level[levels > 2, which = TRUE], patient_id]
setkey(DT1, patient_id)

########################################
#Q2: Visualization of patients' journey#
########################################
Patient1000045 <- DT1[J(patient_candidate[1])]
Patient1000068 <- DT1[J(patient_candidate[2])]
Patient1000112 <- DT1[J(patient_candidate[3])]
#### patient 1000045
segment_point <- c(1, Patient1000045[source.of.buisness == 'Switch', which = TRUE], nrow(Patient1000045)+1)
segment_num <- length(segment_point) - 1
segment  <- data.frame(x = Patient1000045[segment_point[1:segment_num], transaction_date], 
                       xend = Patient1000045[segment_point[-1]-1, transaction_date],
                       y = Patient1000045[segment_point[1:segment_num], product_name],
                       yend = Patient1000045[segment_point[-1]-1, product_name])
# create scatter stratified by color w.r.t source of buisness
p1 <- ggplot(Patient1000045, aes(x = transaction_date, y = product_name, color = source.of.buisness))
p1 <- p1 + geom_point(size = 3) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab('Transaction Date') + 
  ylab('Product Name') +
  scale_x_date(date_labels = "%Y-%m-%d", date_minor_breaks = "1 day", date_breaks = '2 weeks')
p1 + geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = segment, colour = 'black') +
  guides(colour = guide_legend(title='source of buisness'))
ggsave('Q2P1.pdf', units = "in", width = 12, height = 5)

# if we zoom in
p1 <- ggplot(Patient1000045, aes(x = transaction_date, y = product_name, color = source.of.buisness))
p1 <- p1 + geom_point(size = 3) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab('Transaction Date') + 
  ylab('Product Name') +
  scale_x_date(date_labels = "%Y-%m-%d", date_minor_breaks = "1 day", date_breaks = '2 weeks')+
  coord_x_date(xlim = c("2016-10-20", "2016-10-24"))
p1 + geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = segment, colour = 'black') +
  guides(colour = guide_legend(title='source of buisness'))
ggsave('Q2P1zoom.pdf', units = "in", width = 12, height = 5)

#### patient 1000068
segment_point <- c(1, Patient1000068[source.of.buisness == 'Switch', which = TRUE], nrow(Patient1000068)+1)
segment_num <- length(segment_point) - 1
segment  <- data.frame(x = Patient1000068[segment_point[1:segment_num], transaction_date], 
                       xend = Patient1000068[segment_point[-1]-1, transaction_date],
                       y = Patient1000068[segment_point[1:segment_num], product_name],
                       yend = Patient1000068[segment_point[-1]-1, product_name])
# create scatter stratified by color w.r.t source of buisness
p2 <- ggplot(Patient1000068, aes(x = transaction_date, y = product_name, color = source.of.buisness))
p2 <- p2 + geom_point(size = 3, position = position_jitter(w = 0.3, h = 0)) + guides(colour = guide_legend(title='source of buisness')) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab('Transaction Date') + 
  ylab('Product Name') +
  scale_x_date(date_labels = "%Y-%m-%d", date_minor_breaks = "1 day", date_breaks = '1 month')
p2 + geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = segment, colour = 'black')
ggsave('Q2P2.pdf', units = "in", width = 12, height = 5)
#### patient  1000112
segment_point <- c(1, Patient1000112[source.of.buisness == 'Switch', which = TRUE], nrow(Patient1000112)+1)
segment_num <- length(segment_point) - 1
segment  <- data.frame(x = Patient1000112[segment_point[1:segment_num], transaction_date], 
                       xend = Patient1000112[segment_point[-1]-1, transaction_date],
                       y = Patient1000112[segment_point[1:segment_num], product_name],
                       yend = Patient1000112[segment_point[-1]-1, product_name])
# create scatter stratified by color w.r.t source of buisness
p3 <- ggplot(Patient1000112, aes(x = transaction_date, y = product_name, color = source.of.buisness))
p3 <- p3 + geom_point(size = 3) + guides(colour = guide_legend(title='source of buisness')) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab('Transaction Date') + 
  ylab('Product Name') +
  scale_x_date(date_labels = "%Y-%m-%d", date_minor_breaks = "1 day", date_breaks = '2 weeks')
p3 + geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = segment, colour = 'black') +
     geom_point(data = Patient1000112[c(26, 32)], aes(x = transaction_date, y = product_name, color = source.of.buisness), size =3)
ggsave('Q2P3.pdf', units = "in", width = 12, height = 5)
###########################################
#Q5: Visualization of combination therapies#
###########################################

ct_patient1 <- DT1[J(patient_candidate[2])]
ct_patient2 <- DT1[J(patient_candidate[12])]

#### visualize combination therapies of patient 1000068
# creat adjusted source of business
ct_patient1[, adjusted_source := source.of.buisness]
# change the first switch within combined therapies as `add on` and the following switches as `Continuation`
ct_patient1[45, adjusted_source := 'add on']
ct_patient1[46:.N, adjusted_source := 'Continuation']
# calculate the position of combination therapies journey
add_on = ct_patient1[adjusted_source == 'add on', which = TRUE]
add_on_journey = ct_patient1[add_on:.N][product_name == ct_patient1[add_on, product_name], which = TRUE] + add_on - 1
extension_journey = ct_patient1[(add_on - 1):.N][product_name == ct_patient1[add_on - 1, product_name], which = TRUE] + add_on - 2
ct_segment <- data.frame(x = ct_patient1[c(add_on_journey[1:2], extension_journey[1:3]), transaction_date],
                         xend = ct_patient1[c(add_on_journey[2:3], extension_journey[2:4]), transaction_date],
                         y = ct_patient1[c(add_on_journey[1:2], extension_journey[1:3]), product_name],
                         yend = ct_patient1[c(add_on_journey[2:3], extension_journey[2:4]), product_name])
# calculate the position of journey before combination therapies
segment_point <- c(1, ct_patient1[1:(add_on)][adjusted_source == 'Switch', which = TRUE], add_on)
segment_num <- length(segment_point) - 1
segment  <- data.frame(x = ct_patient1[segment_point[1:segment_num], transaction_date], 
                       xend = ct_patient1[segment_point[-1]-1, transaction_date],
                       y = ct_patient1[segment_point[1:segment_num], product_name],
                       yend = ct_patient1[segment_point[-1]-1, product_name])

add_on_fork <- data.frame(x = ct_patient1[add_on, transaction_date], xend = ct_patient1[add_on, transaction_date],
                          y = ct_patient1[add_on, product_name], yend = ct_patient1[add_on - 1, product_name])
pct1 <- ggplot(ct_patient1, aes(x = transaction_date, y = product_name, color = adjusted_source))
pct1 <- pct1 + geom_point(size = 3) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab('Transaction Date') + 
  ylab('Product Name') +
  scale_x_date(date_labels = "%Y-%m-%d", date_minor_breaks = "1 day", date_breaks = '1 month')
pct1 + geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = segment, colour = 'black') +
       geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = ct_segment, colour = 'black') +
       geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = add_on_fork, colour = 'blue') +
       guides(colour = guide_legend(title='behavior'))
ggsave('Q5P1.pdf', units = "in", width = 12, height = 5)

#### visualize combination therapies of patient 1000501
ct_patient2[, adjusted_source := source.of.buisness]
ct_patient2[3, adjusted_source := 'add on']
ct_patient2[4:94, adjusted_source := 'Continuation']
# calculate combination therapies and other journerys
add_on = ct_patient2[adjusted_source == 'add on', which = TRUE]
ct_segment <- data.frame(x = ct_patient2[c(1,3), transaction_date],
                         xend = ct_patient2[c(102,93), transaction_date],
                         y = ct_patient2[c(1,3), product_name],
                         yend = ct_patient2[c(102,93), product_name])
segment_point <- c(ct_patient2[103:.N][adjusted_source == 'Switch', which = TRUE] + 102, nrow(ct_patient2) + 1)
segment_num <- length(segment_point) - 1
segment  <- data.frame(x = ct_patient2[segment_point[1:segment_num], transaction_date], 
                       xend = ct_patient2[segment_point[-1]-1, transaction_date],
                       y = ct_patient2[segment_point[1:segment_num], product_name],
                       yend = ct_patient2[segment_point[-1]-1, product_name])
add_on_fork <- data.frame(x = ct_patient2[add_on, transaction_date], xend = ct_patient2[add_on, transaction_date],
                          y = ct_patient2[add_on, product_name], yend = ct_patient2[add_on + 1, product_name])
pct2 <- ggplot(data = ct_patient2 , aes(x = transaction_date, y = product_name, color = adjusted_source))
pct2 <- pct2 + geom_point(size = 3) + guides(colour = guide_legend(title='behavior')) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab('Transaction Date') + 
  ylab('Product Name') +
  scale_x_date(date_labels = "%Y-%m-%d", date_minor_breaks = "1 day", date_breaks = '1 month')
pct2 + geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = ct_segment, colour = 'black') +
       geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = segment, colour = 'black') +
       geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = add_on_fork, colour = 'blue') +
       geom_point(data = ct_patient2[103], aes(x = transaction_date, y = product_name, color = adjusted_source), size =3)
ggsave('Q5P2.pdf', units = "in", width = 12, height = 5)