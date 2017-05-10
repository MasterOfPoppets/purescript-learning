const { isValidDoorJS } = require('./output/Main/index')

const testDoor = {
  bottomRail: 100,
  middle: {
    panels: [{ x: 100, y: 220 }, { x: 200, y: 300 }],
    rail: 100
  },
  sashHeight: 2150,
  topRail: 'Standard'
}

console.log(isValidDoorJS(testDoor))
