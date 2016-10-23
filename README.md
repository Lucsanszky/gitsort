#Overview

HTTP API to retrieve a given user's top five public repositories, owned by the user, 
sorted by size in descending order.

The API endpoint is reachable at: http://ec2-52-52-131-232.us-west-1.compute.amazonaws.com:3333/:username
It expects a GET request. E.g.: http://ec2-52-52-131-232.us-west-1.compute.amazonaws.com:3333/lucsanszky

#Assumptions

Forked repositories are user owned repositories too. 
https://developer.github.com/v3/repos/#list-user-repositories

#Deployment Description

The webservice is hosted on an Amazon AWS EC2 instance. 
The project is built locally on the EC2 instance by Stack 
and the resulting binary is executed in a GNU screen process.
