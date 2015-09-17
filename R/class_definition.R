

# interactive helpers -----------------------------------------------------

run_query <- function(){
  qry_res <<- sqlQuery(conn,qry)
  #add some more help by reconnecting
}


# class that contains RODBC class -----------------------------------------


odbc_conn <- function(x) UseMethod("odbc_conn")
odbc_conn.RODBC <- function(x){
  structure(list('RODBC'=x),class="odbc_conn")
}
odbc_conn.default <- function(x){
  print("input must be of class RODBC")
}

meta_table <- function(conn,x,y,z=NA){
  if(!is.na(z)){
    names(y) <- z    
  }else{
    names(y) <- y
  }
  structure(list('rodbc_conn'=conn,'tableName'=x,'fields'=y),class="meta_table")  
}

qry_select <- function(x,...) UseMethod("qry_general")
qry_select.meta_table <- function(x,fields='all',alias=T,sans=NULL,run_qry=F,...){
  #validation of arguments
#   if(!fields %in% c('all','defined','sans')){
#     stop('fields argument not defined values of: all, defined')
#   }
  if(!is.logical(alias)){
    stop('alias argument must be logical')
  }
  if(!is.null(sans) & fields %in% c('all','defined')){
    warning('Select ALL chosen with sans fields; Select ALL overides')
  }
  if( !(fields %in% c('all','defined','sans')) ){
    cols <- sqlColumns(x$RODBC,x$tableName)$COLUMN_NAME
    if(!any(fields %in% cols)){
      stop('None of values in fields argument matches columns')
    }
  }
  
  if(fields=='all'){
    if(alias){   
      cols <- sqlColumns(x$RODBC,x$tableName)$COLUMN_NAME
      names(cols) <- cols #default alias to original name
      for(idx in 1:length(x$fields)){
        if(any(cols==x$fields[i])){
          names(cols)[which(cols==x$fields[i])] <- names(x$fields)[i]
        }else{
          warning('column defined in meta_table class not found')
        }
      }
      qry <- paste("SELECT",paste0(cols," AS ",names(cols),collapse=", "),"FROM",x$tableName)
    }else{
      qry <- paste("SELECT * FROM ",x$tableName)
    }
  }
  
  if(fields=='defined'){
    if(alias){
      qry <- paste("SELECT",paste0(x$fields," AS ",names(x$fields),collapse=","),"FROM",x$tableName)      
    }else{
      qry <- paste("SELECT",paste0(x$fields,collapse=","),"FROM",x$tableName)
    }
  }
  
  if(fields=='sans'){  
    if(is.null(sans)){
      qry <- paste("SELECT * FROM ",x$tableName)
    }else{
      cols <- sqlColumns(x$RODBC,x$tableName)$COLUMN_NAME
      cols_to_qry <- cols[!cols %in% sans]
      qry <- paste("SELECT",paste0(cols_to_qry,collapse=", "),"FROM",x$tableName)
    }
  }
  
  if(run_qry){
    return(sqlQuery(x$rodbc_conn,qry))
  }else{
    return(qry)
  }
  
}
qry_select.default <- function(x,...){
  print("x must be meta_table class")
}


#generic class that has structure separating select, from, where clauses
#maybe a whole table for select/where






