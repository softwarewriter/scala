Implementation of solver for Frog Puzzle 
(https://play.google.com/store/apps/details?id=com.giyomu.frogtactics)

Not implemented yet:
- Recursive? solution search


Nice:
1) excellent warning for missing case in match
[Warn] /home/local1/jergan/development/github/scala/frogpuzzle/src/main/scala/no/jergan/frogpuzzle/State.scala:16: match may not be exhaustive.
It would fail on the following inputs: (Some(JUMP), FACE_DOWN), (Some(JUMP), FACE_LEFT), (Some(JUMP), FACE_RIGHT), (Some(JUMP), FACE_UP)
one warning found
2) Pattern matching works great

Not so nice:
1) Not able to do case class hierarchies (breaks equals relation?). Need better solution here.
