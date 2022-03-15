districts <- 
 
 for(i in 1:dim(db_data_sal())[1]){
      query <- paste0("REPLACE INTO districts VALUES ( '", paste0(row_id, i),"',",
                      i,',',
                      "'", active_user(), "',",
                      db_data_sal()[i,1] , ',',
                      db_data_sal()[i,2] ,',',
                      db_data_sal()[i,3] ,',',
                      db_data_sal()[i,4] ,',',
                      db_data_sal()[i,5] ,',',
                      db_data_sal()[i,6] ,',',
                      db_data_sal()[i,7] ,',',
                      db_data_sal()[i,8] ,',',
                      db_data_sal()[i,9] ,',',
                      db_data_sal()[i,10] ,',',
                      db_data_sal()[i,11] ,',',
                      db_data_sal()[i,12] ,',',
                      "'salary');"  )
      sqlQuery(query)
    }