const Data_Maybe = require('./output/Data.Maybe/index')
const {
  isValidDoorJS,
  getMiddleRailHeight,
  door,
  TwoHundredAndFifty
} = require('./output/Main/index')

const testDoor = {
  bob: 250,
  middleRail: 'OneHundred'
}

console.log(isValidDoorJS(testDoor))