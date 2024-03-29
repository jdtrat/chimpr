% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lists.R
\name{chmp_post_list}
\alias{chmp_post_list}
\alias{chmp_post_list_}
\title{Add Members to a List}
\usage{
chmp_post_list(
  conn,
  list_id = NULL,
  email_address = NULL,
  status = NULL,
  skip_merge_validation = TRUE,
  email_type = NULL,
  merge_fields = NULL,
  interests = NULL,
  language = NULL,
  vip = NULL,
  location = NULL,
  marketing_permissions = NULL,
  ip_signup = NULL,
  timestamp_signup = NULL,
  ip_opt = NULL,
  timestamp_opt = NULL,
  tags = NULL,
  key = NULL,
  parse = TRUE,
  invisibly = TRUE,
  ...
)

chmp_post_list_(
  conn,
  list_id = NULL,
  email_address = NULL,
  status = NULL,
  skip_merge_validation = TRUE,
  email_type = NULL,
  merge_fields = NULL,
  interests = NULL,
  language = NULL,
  vip = NULL,
  location = NULL,
  marketing_permissions = NULL,
  ip_signup = NULL,
  timestamp_signup = NULL,
  ip_opt = NULL,
  timestamp_opt = NULL,
  tags = NULL,
  key = NULL,
  ...
)
}
\arguments{
\item{conn}{a connection object. see \link{ChmpClient}}

\item{list_id}{\strong{Required:} The unique ID for the list}

\item{email_address}{\strong{Required:} Email address for a subscriber.}

\item{status}{\strong{Required:} Subscriber's current status. Possible values:
"subscribed", "unsubscribed", "cleaned", "pending", or "transactional".}

\item{skip_merge_validation}{If skip_merge_validation is true, member data
will be accepted without merge field values, even if the merge field is
usually required. The Mailchimp API defaults this to FALSE, but chimpr
authors prefer a more permissive default of TRUE.}

\item{email_type}{Type of email this member asked to get ('html' or 'text').}

\item{merge_fields}{A dictionary of merge fields \href{https://mailchimp.com/help/getting-started-with-merge-tags/}{audience fields} where
the keys are the merge tags. For example, {"FNAME":"Freddie"}}

\item{interests}{The key of this object's properties is the ID of the
interest in question.}

\item{language}{If set/detected, the \href{https://mailchimp.com/help/view-and-edit-contact-languages/}{subscriber's language}}

\item{vip}{\href{https://mailchimp.com/help/view-and-edit-contact-languages/}{VIP status} for
subscriber.}

\item{location}{Subscriber location information. See API for details.}

\item{marketing_permissions}{The marketing permissions for the subscriber.
See API for details.}

\item{ip_signup}{IP address the subscriber signed up from.}

\item{timestamp_signup}{The date and time the subscriber signed up for the
list in ISO 8601 format.}

\item{ip_opt}{The IP address the subscriber used to confirm their opt-in
status.}

\item{timestamp_opt}{The date and time the subcscriber confirmed their opt-in
status in ISO 8601 format.}

\item{tags}{The tags that are associated with a member}

\item{key}{A Mailchimp API key. See
\url{https://developer.mailchimp.com/documentation/mailchimp/}
to get a key}

\item{parse}{(logical) Whether to parse to list (\code{FALSE}) or
data.frame (\code{TRUE}). Default: \code{TRUE}}

\item{invisibly}{Logical: \code{TRUE} by default. If TRUE, the HTTP request
will be returned invisibly; \code{FALSE} and it will be returned explicitly.}

\item{...}{Curl options passed to \code{\link[crul]{HttpClient}}}
}
\description{
Add Members to a List
}
\examples{
\dontrun{
(conn <- ChmpClient$new())

chmp_post_list(conn,
               list_id = "your-list-id",
               email_address = "person@website.com",
               status = "subscribed")

}

}
