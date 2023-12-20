## Function to convert NULL to NA for parsing
nulltona <- function(x)
{
    if(is.null(x)) return(NA)
    return(x)
}

#' Get a List of supported Chains
#' @import rvest
#' @import jsonlite
#' @import stats
#' @import utils
#' @return List of Supported chain Indexes
#' @export
#' @examples
#' \dontrun{
#' chainList()
#' }
chainList <- function()
{
    chains <- suppressWarnings(as.vector(na.omit(as.numeric(html_text(html_nodes(read_html("https://grants-stack-indexer.gitcoin.co/data/"),xpath='//*[@id="files"]/li/a/span[1]'),trim=TRUE)))))
    chains <- chains[chains %in% c(1,10,137,250,42161,421613,424)]
    return(chains)
}

#' Get Rounds DataFrame for a given chain index
#' @param cidx Chain Index out of 1, 10, 137, 250, 42161, 421613, 424
#' @return Rounds DataFrame
#' @examples
#' \dontrun{
#' chainRoundDF(42161)
#' }
chainRoundDF <- function(cidx)
{
    tdata <- fromJSON(paste0("https://grants-stack-indexer.gitcoin.co/data/",cidx,"/rounds.json"),flatten=TRUE)
    rdata <- data.frame(
                            chainId = cidx,
                            roundId = tdata$id,
                            roundName = tdata$metadata.name,
                            roundType = nulltona(tdata$metadata.roundType),
                            roundProgramContractAddress = tdata$metadata.programContractAddress,
                            roundToken = tdata$token,
                            roundAmountUSD = tdata$amountUSD,
                            roundMatchAmt = tdata$matchAmount,
                            roundMatchAmtUSD = tdata$matchAmountUSD,
                            roundUniqueContributors = tdata$uniqueContributors,
                            roundVotes = tdata$votes,
                            roundMetaPtr = tdata$metaPtr,
                            roundStartTime = tdata$roundStartTime,
                            roundEndTime = tdata$roundEndTime,
                            roundCreatedAtBlock = tdata$createdAtBlock,
                            roundUpdatedAtBlock = tdata$updatedAtBlock,
                            roundEligibilityDescription = tdata$metadata.eligibility.description,
                            roundEligibilityRquirements = sapply(tdata$metadata.eligibility.requirements,function(x) paste0(unlist(x),collapse="\n")),
                            roundApplicationMetaPtr = tdata$applicationMetaPtr,
                            roundApplicationsStartTime = tdata$applicationsStartTime,
                            roundApplicationsEndTime = tdata$applicationsEndTime
                        )
    return(rdata)
}

#' Get Projects DataFrame for a given chain index
#' @param cidx Chain Index out of 1, 10, 137, 250, 42161, 421613, 424
#' @return Project DataFrame
#' @examples
#' \dontrun{
#' chainProjectDF(42161)
#' }
chainProjectDF <- function(cidx)
{
    tdata <- fromJSON(paste0("https://grants-stack-indexer.gitcoin.co/data/",cidx,"/projects.json"),flatten=TRUE)
    pdata <- data.frame(
                            chainId = cidx,
                            projectId = tdata$id,
                            projectMetaPtr = tdata$metaPtr,
                            projectOwner = sapply(tdata$owners,paste0,collapse=";"),
                            projectCreatedAtBlock = tdata$createdAtBlock,
                            projectTitle = nulltona(tdata$metadata.title),
                            projectDescription = nulltona(tdata$metadata.description),
                            projectWebsite = nulltona(tdata$metadata.website),
                            projectTwitter = nulltona(tdata$metadata.projectTwitter),
                            projectLogo = nulltona(tdata$metadata.logoImg),
                            projectBanner = nulltona(tdata$metadata.bannerImg),
                            projectUserGithub = nulltona(tdata$metadata.userGithub),
                            projectProjectGithub = nulltona(tdata$metadata.projectGithub)
                        )

    return(pdata)
}

#' Project Contribution Data for a given Round and Chain Index
#' @param cidx Chain Index out of 1, 10, 137, 250, 42161, 421613, 424
#' @param ridx RoundId
#' @return Project Contribution Data
#' @export
#' @examples
#' \dontrun{
#' chainRoundProjectData(42161,"0x302Dbc8eB3bf73565A1205648B61b23CB3f72Ff7")
#' }
chainRoundProjectData <- function(cidx,ridx)
{
    tdata <- fromJSON(paste0("https://grants-stack-indexer.gitcoin.co/data/",cidx,"/rounds/",ridx,"/applications.json"),flatten=TRUE)
    if(length(tdata)==0) return(data.frame())
    adata <- data.frame(
                            chainId = cidx,
                            roundId = ridx,
                            projectId = tdata$projectId,
                            projectStatus = tdata$status,
                            projectAmountUSD = tdata$amountUSD,
                            projectVotes = tdata$votes,
                            projectUniqueContributors = tdata$uniqueContributors,
                            projectCreatedAtBlock = tdata$uniqueContributors,
                            projectStatusUpdatedAtBlock = tdata$statusUpdatedAtBlock,
                            projectReceipient = nulltona(tdata$metadata.application.recipient),
                            projectTitle = nulltona(tdata$metadata.application.project.title),
                            projectDescription = nulltona(tdata$metadata.application.project.description),
                            projectWebsite = nulltona(tdata$metadata.application.project.website),
                            projectLogo = nulltona(tdata$metadata.application.project.logoImg),
                            projectBanner = nulltona(tdata$metadata.application.project.bannerImg),
                            projectUserGithub = nulltona(tdata$metadata.application.project.userGithub),
                            projectGithub = nulltona(tdata$metadata.application.project.projectGithub),
                            projectTwitter = nulltona(tdata$metadata.application.project.projectTwitter),
                            projectMeta = nulltona(tdata$metadata.application.project.metaPtr.pointe)
                        )
    return(adata)
}

#' Voter Contribution Data for a given Round and Chain Index
#' @param cidx Chain Index out of 1, 10, 137, 250, 42161, 421613, 424
#' @param ridx Round Id
#' @return Voter Contribution Data
#' @export
#' @examples
#' \dontrun{
#' chainRoundVotesData(42161,"0x302Dbc8eB3bf73565A1205648B61b23CB3f72Ff7")
#' }
chainRoundVotesData <- function(cidx,ridx)
{
    tdata <- fromJSON(paste0("https://grants-stack-indexer.gitcoin.co/data/",cidx,"/rounds/",ridx,"/votes.json"))
    if(length(tdata)==0) return(data.frame())
    vdata <- data.frame(
                            chainId = cidx,
                            roundId = ridx,
                            projectId = tdata$projectId,
                            voteId = tdata$id,
                            voteTxHash = tdata$transaction,
                            voteBlockNum = tdata$blockNumber,
                            voter = tdata$voter,
                            grantAddress = tdata$grantAddress,
                            token = tdata$token,
                            amount = tdata$amount,
                            amountUSD = tdata$amountUSD
                        )
    return(vdata)
}

#' Get Rounds DataFrame across all chains
#' @return Round DataFrame across All Chains
#' @export
#' @examples
#' \dontrun{
#' roundDF()
#' }
roundDF <- function() do.call(rbind,lapply(chainList(),chainRoundDF))

#' Get Project DataFrame across all chains
#' @return Project DataFrame across All Chains
#' @export
#' @examples
#' \dontrun{
#' projectDF()
#' }
projectDF <- function() do.call(rbind,lapply(chainList(),chainProjectDF))


#' Project Contribution Data for a give chain
#' @param cidx Chain Index out of 1, 10, 137, 250, 42161, 421613, 424
#' @return Project Contribution Data
#' @export
#' @examples
#' \dontrun{
#' chainProjectData(42161)
#' }
chainProjectData <- function(cidx)
{
    roundsDF <- chainRoundDF(cidx)
    projectdata <- list()
    pb <- txtProgressBar(min = 0, max = nrow(roundsDF), style = 3)
    for(idx in 1:nrow(roundsDF))
    {
        projectdata[[idx]]  <- chainRoundProjectData(roundsDF$chainId[idx],roundsDF$roundId[idx])
        setTxtProgressBar(pb, idx)
    }
    close(pb)
    do.call(rbind,projectdata)
}

#' Vote Contribution Data for a give chain
#' @param cidx Chain Index out of 1, 10, 137, 250, 42161, 421613, 424
#' @return Vote Contribution Data
#' @export
#' @examples
#' \dontrun{
#' chainVoteData(42161)
#' }
chainVoteData <- function(cidx)
{
    roundsDF <- chainRoundDF(cidx)
    votedata <- list()
    pb <- txtProgressBar(min = 0, max = nrow(roundsDF), style = 3)
    for(idx in 1:nrow(roundsDF))
    {
        votedata[[idx]]  <- chainRoundVotesData(roundsDF$chainId[idx],roundsDF$roundId[idx])
        setTxtProgressBar(pb, idx)
    }
    close(pb)
    do.call(rbind,votedata)
}

#' Project Contribution Data across all chains
#' @return Project Contribution Data
#' @export
#' @examples
#' \dontrun{
#' allProjectData()
#' }
allProjectData <- function()
{
    roundsDF <- roundDF()
    projectdata <- list()
    pb <- txtProgressBar(min = 0, max = nrow(roundsDF), style = 3)
    for(idx in 1:nrow(roundsDF))
    {
        projectdata[[idx]]  <- chainRoundProjectData(roundsDF$chainId[idx],roundsDF$roundId[idx])
        setTxtProgressBar(pb, idx)
    }
    close(pb)
    do.call(rbind,projectdata)
}

#' Vote Contribution Data for a give chain
#' @return Vote Contribution Data
#' @export
#' @examples
#' \dontrun{
#' allVoteData()
#' }
allVoteData <- function()
{
    roundsDF <- roundDF()
    votedata <- list()
    pb <- txtProgressBar(min = 0, max = nrow(roundsDF), style = 3)
    for(idx in 1:nrow(roundsDF))
    {
        votedata[[idx]]  <- chainRoundVotesData(roundsDF$chainId[idx],roundsDF$roundId[idx])
        setTxtProgressBar(pb, idx)
    }
    close(pb)
    do.call(rbind,votedata)
}