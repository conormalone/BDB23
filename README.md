# BDB23
NFL Big Data Bowl 2023
Finding the best Communicating O-Lines
get in play probability of a sack/pressure, find the best teams at reducing that in play

to do:
DONE get all pass rushers, passer and blockers for all plays
DONE get same for ball in play frames
divide plays into test-train 75:25
take random sample of frames within each play
prepare graph where distance is edge, qb, OL, DL are node-types, binary pressure is graph feat
second model glm or random forest
features: all player dist to qb, rusher to blocker, time

train


Visuals
time series of how team's sack risk changes per frame, hopefully showing some teams that reduce
animation of 1 play