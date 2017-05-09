/* @flow */

type Except<a> = a | Error

type TopRail = 'Standard' | 'Compact'

type BottomRail = number

type FriezeRailHeight = number

type Panel = {
  x: number,
  y: number
}

type Freize = {
  panel: [Panel],
  friezeRailHeight: FriezeRailHeight
}

type MiddleRail = 'OneHundred' | 'TwoHundred'

type Middle = {
  panel: [Panel],
  middleRail: MiddleRail
}

type EntranceDoor = {
  topRail?: TopRail,
  frieze?: Freize,
  bottomRail?: BottomRail,
  middle?: Middle,
  mainPanels: [Panel],
  sashHeight: number
}

type getTopRailHeightT = TopRail => Except<number>

const getTopRailHeight: getTopRailHeightT = topRail => {
  switch (topRail) {
    case 'Standard':
      return 150
    case 'Compact':
      return 150
    default:
      (topRail: empty)
      return new Error(`${topRail} is not a valid TopRail type`)
  }
}
