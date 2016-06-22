import lib from '../lib/lib'

function newGame(state, action) {
    switch (action.type) {
        case 'INITIALIZE_NEW_GAME': {
            return JSON.parse(JSON.stringify(action.game))
        }
        case 'CHANGE_GAME_NAME': {
            const ret = Object.assign({}, state)
            ret.game = Object.assign({}, state.game, {name: action.value})
            return ret
        }
        case 'CHANGE_GAME_PLACE': {
            const ret = Object.assign({}, state)
            ret.game = Object.assign({}, state.game, {place: action.value})
            return ret
        }
        case 'CHANGE_TEMA_A_NAME': {
            const ret = Object.assign({}, state)
            ret.game = Object.assign({}, state.game, {teamAName: action.value})
            return ret
        }
        case 'CHANGE_TEMA_B_NAME': {
            const ret = Object.assign({}, state)
            ret.game = Object.assign({}, state.game, {teamBName: action.value})
            return ret
        }
        case 'CHANGE_WIZARD_PANE': {
            const ret = Object.assign({}, state)
            ret.wizardState = Object.assign({}, state.wizardState, {activeKey: action.paneIndex})
            return ret
        }
        default:
            return state
    }
}

export default newGame
