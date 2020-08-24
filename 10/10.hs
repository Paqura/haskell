robot (name, attack, hp) = \message -> message (name, attack, hp)

superRobot = robot ("Bob", 10, 100)
puperRobot = robot ("John", 11, 100)

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, h) = h

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setHp aRobot newHp = aRobot (\(n, a, h) -> robot (n, a, newHp))

printRobotInfo aRobot = aRobot (\(n, a, h) ->
  "Name: " ++ n ++ ", Attack: " ++ show (a) ++ ", " ++ "HP: " ++ show(h))

damage aRobot damageVolume = aRobot (\(n, a, h) -> robot (n, a, h - damageVolume))

fight aRobot defender = damage defender attack
  where attack = if getHp aRobot > 10
        then getAttack aRobot
        else 0

round1Puper = fight superRobot puperRobot
result = getHp round1Puper

getRobotsHps arrayOfRobots getFn = map getFn arrayOfRobots

hps = getRobotsHps [superRobot, puperRobot] getHp
