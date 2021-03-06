#!/bin/sh
FUSER=$1
PASSWORD=$2
PROFILE_DATA='{ "email": "maverickone@gmail.com", "lastName": "Donthireddy", "workPhone": "212-478-0707", "username": "murali", "mobilePhone": "646-258-7065", "firstName": "Murali", "homePhone": "609-790-4750", "password": "murali12" }'
echo $PROFILE_DATA
echo
echo "{\"username\":\"$USER\",\"password\":\"$PASSWORD\"}"
TOKEN=$(curl -X POST http://localhost:8080/login -H "Content-Type: application/json" -d "{\"username\":\"$USER\",\"password\":\"$PASSWORD\"}" |jq -r .token)
echo Got token $TOKEN
curl -X PUT http://localhost:8080/api/profile  -H "Authorization: Bearer $TOKEN" -H "Content-Type: application/json" -d "$PROFILE_DATA" |jq
curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/profile/murali |jq
#curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/user/murali |jq -c '.[]'
# curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/players |jq -c '.[]'
curl -X GET -H "Authorization: Bearer $TOKEN" http://localhost:8080/api/chuck
echo
curl -X GET http://localhost:8080/api/rajni
echo
