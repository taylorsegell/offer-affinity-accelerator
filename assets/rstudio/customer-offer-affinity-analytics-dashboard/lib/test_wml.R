source("lib/icp4d-api.R")

hostname = Sys.getenv('CP4D_HOSTNAME')
username = Sys.getenv('CP4D_USERNAME')
password = Sys.getenv('CP4D_PASSWORD')

if(nchar(hostname)>0 && nchar(username)>0 && nchar(password)>0) {
  
  # select pipeline deployment
  deploymentOptions = collectDeployments(hostname, username, password, 'offer_affinity_scoring_pipeline_function_deployment_tag')
  scoring_url = deploymentOptions$deployments[[1]]$scoring_url
  print(paste("Using deployment", names(deploymentOptions$deployments)[[1]]))
  print(deploymentOptions$deployments[[1]])
  
  # create payload
  payload = list(
    values = '2018-09-30',
    cust_id = 1218
  )
  
  # score
  scoringResponse = scoreModelDeployment(scoring_url, payload, deploymentOptions$token)
  print(scoringResponse)
  
} else {
  
  print('Missing some variables.')
  print(paste('CP4D_HOSTNAME =', hostname))
  print(paste('CP4D_USERNAME =', username))
  print(paste('CP4D_PASSWORD =', password))
  
}
