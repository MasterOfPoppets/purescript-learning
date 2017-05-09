const { isValidDoorJS } = require('./output/Main/index')

const testDoor = {
  sashHeight: 2150,
  middleRail: 100
}

console.log(isValidDoorJS(testDoor))
