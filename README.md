# BDB23
NFL Big Data Bowl 2023
Finding the best Communicating O-Lines
get in play probability of a sack/pressure, find the best teams at reducing that in play

to do:
REDO Model is there a sack within 15 frames

redo test train split
500 sacks
take 80% of sacks: 400
take 15 frames from each: 6000
take 1 frame at random from remaining 6000 0 plays

redo train/test/val, leave all_data the same

measure change over a play, or plays where rate reduces most.
see if you can improve the model

Visuals
do some exploratory vis of progresion per play/game/team
time series of how team's sack risk changes per frame, hopefully showing some teams that reduce
animation of 1 play



DONE tie in comb_and_frame and predictions
DONE get all pass rushers, passer and blockers for all plays
DONE get same for ball in play frames
DONE divide plays into test-train 75:25
DONE take random sample of frames within each play
DONE prepare graph where distance is edge, qb, OL, DL are node-types, binary pressure is graph feat
second model glm or random forest
DONE features: all player dist to qb, rusher to blocker, time

DONE train


