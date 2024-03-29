% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_transaction_to_master.R
\name{transform_transaction_to_master}
\alias{transform_transaction_to_master}
\title{transform transaction -> master}
\usage{
transform_transaction_to_master(dat, ROW_NUMBER = "ROW_NUMBER",
  ID = "ID", collapse = ":", STATIC_NUM = NULL, STATIC_CHAR = NULL,
  DYNAMIC_NUM = NULL, DYNAMIC_CHAR = NULL)
}
\arguments{
\item{dat}{transaction data frame}

\item{ROW_NUMBER}{column name for row number in the create data frame}

\item{ID}{identifier name}

\item{collapse}{separation character (defalt ":")}

\item{STATIC_NUM}{list of column name which shows STATIC NUMBER attribute}

\item{STATIC_CHAR}{list of column name which shows STATIC CHARACTER attribute}

\item{DYNAMIC_NUM}{list of column name which shows DYNAMIC NUMBER attribute}

\item{DYNAMIC_CHAR}{list of column name which shows DYNAMIC CHARACTER attribute}
}
\description{
transform transaction -> master
}
