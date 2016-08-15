import Lib       from '../lib/lib'

import findindex from 'array.prototype.findindex'
findindex.shim()

function games(state={}, action) {
    switch (action.type) {
        case 'INITIALIZE_GAMES': {
            return { gameList: action.gameList }
        }
        case 'DELETE_GAME': {
            const ret = Lib.shallowCopy(state)
            ret.gameList = Lib.shallowCopy(state.gameList)

            const delIdx = ret.gameList.findIndex(game => {
                return game.id === action.gameId
            })
            if (delIdx >= 0) {
                ret.gameList.splice(delIdx, 1)
            }
            return ret
        }
        default:
            return state
    }
}

export default games
