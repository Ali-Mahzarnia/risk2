
library(readxl)
library(dplyr)

path_master='/Users/ali/Desktop/Sep23/risk/code/new_data_grid-export.csv'
data=read.csv(path_master )
data = as.data.frame(data)
data = as.data.frame(t(na.omit(t(data))))

datatemp=data%>%dplyr::select(PTID , Sex, Age, Clinical.Dx, APOE)#subselect

datatemp$Clinical.Dx[datatemp$Clinical.Dx=="" |   datatemp$Clinical.Dx=="Normal cognition" | datatemp$Clinical.Dx=="Impaired - not MCI"  ]  = 1
datatemp$Clinical.Dx[datatemp$Clinical.Dx=="MCI" | datatemp$Clinical.Dx=="Dementia"]  = 2
datatemp$Clinical.Dx = as.numeric(datatemp$Clinical.Dx)

# datatemp$APOE[datatemp$APOE == "2/3" | datatemp$APOE == "3/3"] = 3
# datatemp$APOE[datatemp$APOE == "2/4" | datatemp$APOE == "3/4" |  datatemp$APOE == "4/4"] = 4
# datatemp$APOE[datatemp$APOE == "" ] = 0
# datatemp$APOE= as.numeric(datatemp$APOE)

#nchar(datatemp[111,1])
datatemp=na.omit(datatemp)
#datatemp[nchar(datatemp$DWI)==1,]=matrix(NA,1,dim(datatemp)[2])
#datatemp=na.omit(datatemp)
#datatemp[substr(datatemp$DWI,1,1)!="N",]=matrix(NA,1,dim(datatemp)[2])
#datatemp=na.omit(datatemp) ## ommit all na and zero character dwi and died durring
#datatemp$DWI=as.numeric(substr(datatemp$DWI,2,6)) # make dwi numeric
#datatemp=datatemp[datatemp$Genotype!="HN",]

####plains
path_connec="/Users/ali/Desktop/Sep23/risk/code/ADRC_connectome/"
file_list=list.files(path_connec)
plain_index = grep("plain", file_list)
sft_node_index = grep("sift_node", file_list)
sft_index = grep("sift.csv", file_list)
dst_index = grep("distances.csv", file_list)
mean_FA_index = grep("mean_FA", file_list)

file_list = file_list[plain_index]
temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
#temp_conn=temp_conn[,2: dim(temp_conn)[2]]
connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
connectivity =connectivity
dim(connectivity)

notfound=0
##read connec
for (i in 1:dim(connectivity)[3]) {
  
  temp_index=which(datatemp$PTID[i]==(substr(file_list,1,8)))
  if (length(temp_index)>0) 
  { #print(temp_index)
    temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
    #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
    #colnames(temp_connec)=NA
    connectivity[,,i]=as.matrix(temp_connec)
    #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
    #connectivity[,,i]=as.matrix(temp_connec)
  }
  else
    notfound=c(notfound, datatemp$PTID[i])
  
}

notfound=notfound[2:length(notfound)]
not_found_index=which( datatemp$PTID  %in%  notfound )

datatemp=datatemp[-not_found_index,]
connectivity=connectivity[,,-not_found_index]
sum(is.na(connectivity))
aaa = which(is.na(connectivity), arr.ind = T)
#connectivity=connectivity[,,-not_found_index]
#sum(is.na(connectivity))

 save(connectivity, file="connectivity_plain.rda")


# 
# 
# 








response=datatemp

#
# RNA=read.delim('/Users/ali/Desktop/may23/risk/code/humanwithsymbol.txt',sep="\t", header = T)
# RNA=t(RNA)
# RNA_rows=rownames(RNA)
# 
# #RNA_rows= as.numeric(gsub("\\D", "", RNA_rows))
# #RNA_rows=RNA_rows[2:(length(RNA_rows)-1)]
# 
# RNA_data=RNA[2:(dim(RNA)[1]-1),]
# colnames(RNA_data)=RNA[dim(RNA)[1],]
# rownames(RNA_data)=RNA_rows[2:(length(RNA_rows)-1)]
# #rownames(RNA_data)=NULL
# sum(is.na(RNA_data))
# #RNA_data[!is.na(RNA_data)]=NA
# sum(is.na(RNA_data))
# 
# 
# #exist_response_ID=sub("\\s*\\:.*$", "",  response$Subject)
# #response$Subject=as.numeric(gsub("\\D", "", exist_response_ID ))
# #as.numeric(rownames(RNA_data))
# 
# 
# found=0
# notfound=0
# for (jj in 1:dim(RNA_data)[1]) {
# 
#   index=which( row.names( RNA_data)[jj]== response$ Subject )
#   if (length(index)>0){
#         found=c(found,index)
#   }
# 
#   else notfound=c(notfound,jj)
# 
#   }
# found=found[2:length(found)]
# 
# response=response[found,]
# dim(response)
# 
# # connectivity=connectivity[,,found]
# dim(connectivity)
# 
# notfound=notfound[2:length(notfound)]
# RNA_data=RNA_data[-notfound,]
# 
# dim(RNA_data)
# 
# rownames(RNA_data)=NULL


# 
# save(RNA_data, file="RNA_data.rda")
# save(connectivity, file="connectivity_plain.rda")
# 
save(response, file="response.rda")
# 
# 

# 
# ###### sift
# file_list=list.files(path_connec)
# file_list = file_list[sft_index]
# temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
# #temp_conn=temp_conn[,2: dim(temp_conn)[2]]
# connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
# connectivity =connectivity
# dim(connectivity)
# 
# notfound=0
# ##read connec
# for (i in 1:dim(connectivity)[3]) {
#   
#   temp_index=which(datatemp$MRI_Exam[i]==as.numeric(substr(file_list,2,6)))
#   if (length(temp_index)>0) 
#   { #print(temp_index)
#     temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
#     #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
#     #colnames(temp_connec)=NA
#     connectivity[,,i]=as.matrix(temp_connec)
#     #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
#     #connectivity[,,i]=as.matrix(temp_connec)
#   }
#   else
#     notfound=c(notfound, datatemp$MRI_Exam[i])
#   
# }
# #notfound=notfound[2:length(notfound)]
# #not_found_index=which( datatemp$DWI  %in%  notfound )
# 
# #datatemp=datatemp[-not_found_index,]
# #connectivity=connectivity[,,-not_found_index]
# #sum(is.na(connectivity))
# 
# #connectivity=connectivity[,,-not_found_index]
# sum(is.na(connectivity))
# dim(connectivity)
# # connectivity=connectivity[,,found]
# dim(connectivity)
# #response=datatemp
# #save(response, file="response.rda")
# save(connectivity, file="connectivity_sift.rda")
# 
# 
# 
# 
# ###### sift_node
# file_list=list.files(path_connec)
# file_list = file_list[sft_node_index]
# temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
# #temp_conn=temp_conn[,2: dim(temp_conn)[2]]
# connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
# connectivity =connectivity
# dim(connectivity)
# 
# notfound=0
# ##read connec
# for (i in 1:dim(connectivity)[3]) {
#   
#   temp_index=which(datatemp$MRI_Exam[i]==as.numeric(substr(file_list,2,6)))
#   if (length(temp_index)>0) 
#   { #print(temp_index)
#     temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
#     #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
#     #colnames(temp_connec)=NA
#     connectivity[,,i]=as.matrix(temp_connec)
#     #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
#     #connectivity[,,i]=as.matrix(temp_connec)
#   }
#   else
#     notfound=c(notfound, datatemp$MRI_Exam[i])
#   
# }
# #notfound=notfound[2:length(notfound)]
# #not_found_index=which( datatemp$DWI  %in%  notfound )
# 
# #datatemp=datatemp[-not_found_index,]
# #connectivity=connectivity[,,-not_found_index]
# #sum(is.na(connectivity))
# 
# #connectivity=connectivity[,,-not_found_index]
# sum(is.na(connectivity))
# dim(connectivity)
# # connectivity=connectivity[,,found]
# dim(connectivity)
# #response=datatemp
# #save(response, file="response.rda")
# save(connectivity, file="connectivity_sift_node.rda")
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# ###### dst_index
# file_list=list.files(path_connec)
# file_list = file_list[dst_index]
# temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
# #temp_conn=temp_conn[,2: dim(temp_conn)[2]]
# connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
# connectivity =connectivity
# dim(connectivity)
# 
# notfound=0
# ##read connec
# for (i in 1:dim(connectivity)[3]) {
# 
#   temp_index=which(datatemp$DWI[i]==as.numeric(substr(file_list,2,6)))
#   if (length(temp_index)>0)
#   {
#     temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
#     #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
#     #colnames(temp_connec)=NA
#     connectivity[,,i]=as.matrix(temp_connec)
#     #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
#     #connectivity[,,i]=as.matrix(temp_connec)
#   }
#   else
#     notfound=c(notfound, datatemp$DWI[i])
# 
# }
# 
# #notfound=notfound[2:length(notfound)]
# #not_found_index=which( datatemp$DWI  %in%  notfound )
# 
# #datatemp=datatemp[-not_found_index,]
# #connectivity=connectivity[,,-not_found_index]
# #sum(is.na(connectivity))
# 
# #connectivity=connectivity[,,-not_found_index]
# sum(is.na(connectivity))
# dim(connectivity)
# # connectivity=connectivity[,,found]
# dim(connectivity)
# #response=datatemp
# #save(response, file="response.rda")
# save(connectivity, file="connectivity_dst.rda")
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# ###### mean_FA_index
# file_list=list.files(path_connec)
# file_list = file_list[mean_FA_index]
# temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
# #temp_conn=temp_conn[,2: dim(temp_conn)[2]]
# connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
# connectivity =connectivity
# dim(connectivity)
# 
# notfound=0
# ##read connec
# for (i in 1:dim(connectivity)[3]) {
# 
#   temp_index=which(datatemp$DWI[i]==as.numeric(substr(file_list,2,6)))
#   if (length(temp_index)>0)
#   {
#     temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
#     #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
#     #colnames(temp_connec)=NA
#     connectivity[,,i]=as.matrix(temp_connec)
#     cat ( sum( is.na(as.matrix(temp_connec)  )  ) , "\n")
#     #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
#     #connectivity[,,i]=as.matrix(temp_connec)
#   }
#   else
#     notfound=c(notfound, datatemp$DWI[i])
# 
# }
# 
# #notfound=notfound[2:length(notfound)]
# #not_found_index=which( datatemp$DWI  %in%  notfound )
# 
# #datatemp=datatemp[-not_found_index,]
# #connectivity=connectivity[,,-not_found_index]
# #sum(is.na(connectivity))
# 
# #connectivity=connectivity[,,-not_found_index]
# sum(is.na(connectivity))
# dim(connectivity)
# # connectivity=connectivity[,,found]
# connectivity [is.na(connectivity)] = 0
# dim(connectivity)
# #response=datatemp
# #save(response, file="response.rda")
# save(connectivity, file="connectivity_mean_FA_index.rda")
# # 
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
