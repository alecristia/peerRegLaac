library(childesr)
parts=get_participants()
table(parts$role)
d_transcripts <- get_transcripts()
alldat=get_speaker_statistics()
merge(alldat,d_transcripts,all.x=T,by="transcript_id")->alldat
alldat$type=NA
alldat$type[alldat$speaker_role %in% c("Brother","Sister","Sibling","Child","Playmate","Boy","Girl","Cousin")] <-"peer"
alldat$type[alldat$speaker_role %in% c("Mother","Adult","Babysitter","Aunt","Caretaker",
                                      "Father","Family_Friend","Grandfather","Grandmother",
                                      "Housekeeper","Nurse","Uncle","Teenager")] <-"adult"
table(alldat$speaker_role)
table(alldat$type)

sums=aggregate(num_utterances ~type + filename, data= alldat,sum)

summary(sums)
mysums=data.frame(sums)
summary(mysums)
adults=subset(mysums,type=="adult")
peers=subset(mysums,type=="peer")
adults$nutt_ad=adults$num_utterances
peers$nutt_peer=peers$num_utterances

adpe=merge(adults[,c("filename","nutt_ad")],peers[,c("filename","nutt_peer")],all=T)
adpe$tot=adpe$nutt_ad+adpe$nutt_peer
summary(adpe)
adpe$prop_peer=adpe$nutt_peer/adpe$tot
hist(adpe$prop_peer)
adpe$filename[adpe$prop_peer>.0001]->files_to_include
