import lib from '../lib/lib'

function newGame(state, action) {
  console.log(action)
    switch (action.type) {
        case 'INITIALIZE_NEW_GAME': {
            return { newGame: action.game }
        }
        case 'CHANGE_GAME_NAME': {
            const ret = lib.copyJson(state)
            ret.newGame.game.name = action.value
            return ret
        }
        case 'CHANGE_GAME_PLACE': {
            const ret = lib.copyJson(state)
            ret.newGame.game.place = action.value
            return ret
        }
        case 'CHANGE_TEMA_A_NAME': {
            const ret = lib.copyJson(state)
            ret.newGame.game.teamAName = action.value
            console.log(ret)
            return ret
        }
        case 'CHANGE_TEMA_B_NAME': {
            const ret = lib.copyJson(state)
            ret.newGame.game.teamBName = action.value
            console.log(ret)
            return ret
        }
        case 'CHANGE_WIZARD_PANE': {
            const ret = lib.copyJson(state)
            ret.newGame.wizardState.activeKey = action.paneIndex
            return ret
        }
        default:
            return state
    }
}

export default newGame
