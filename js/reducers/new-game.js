import Lib              from '../lib/lib'
import * as NewGameActs from '../actions/new-game'

function newGame(state, action) {
    switch (action.type) {
        case NewGameActs.INITIALIZE_NEW_GAME: {
            return JSON.parse(JSON.stringify(action.game))
        }
        case NewGameActs.CHANGE_GAME_NAME: {
            const ret = Lib.assign({}, state)
            ret.game = Lib.assign({}, state.game, {name: action.value})
            return ret
        }
        case NewGameActs.CHANGE_GAME_PLACE: {
            const ret = Lib.assign({}, state)
            ret.game = Lib.assign({}, state.game, {place: action.value})
            return ret
        }
        case NewGameActs.CHANGE_TEMA_A_NAME: {
            const ret = Lib.assign({}, state)
            ret.game = Lib.assign({}, state.game, {teamAName: action.value})
            return ret
        }
        case NewGameActs.CHANGE_TEMA_B_NAME: {
            const ret = Lib.assign({}, state)
            ret.game = Lib.assign({}, state.game, {teamBName: action.value})
            return ret
        }
        default:
            return state
    }
}

export default newGame
