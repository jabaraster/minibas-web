import Lib from '../lib/lib'

function cloneScore(state, quarterIndex) {
    const newState = Object.assign({}, state)
    newState.game  = Object.assign({}, state.game)

    const game = newState.game
    game.score = [].concat(game.score)

    const score = game.score
    score[quarterIndex] = Object.assign({}, score[quarterIndex])

    return newState
}

function game(state, action) {
    switch (action.type) {
        case 'INITIALIZE_GAME': {
            return action.game
        }
        case 'CHANGE_GAME_PROPERTY': {
            const ret = Object.assign({}, state)
            ret.game = Object.assign({}, state.game)
            ret.game.property = Object.assign({}, action.property)
            return ret
        }
        case 'CHANGE_TEAM_POINT': {
            const quarterIndex = action.quarterIndex
            const ret = cloneScore(state, quarterIndex)
            const prop = 'team'+action.teamAorB+'Point'
            ret.game.score[quarterIndex][prop] = action.value
            return ret
        }
        case 'CHANGE_LOCK': {
            const quarterIndex = action.quarterIndex
            const ret = cloneScore(state, quarterIndex)
            ret.game.score[quarterIndex].lock = action.lock
            return ret
        }
        case 'UI_CHANGE_EDIT_DIALOG_OPEN': {
            const ret = Object.assign({}, state)
            ret.uiState = Object.assign({}, state.uiState,
                            {editDialogOpen: action.editDialogOpen})
            return ret
        }
        case 'UI_CHANGE_MENU_OPEN': {
            const ret = Object.assign({}, state)
            ret.uiState = Object.assign({}, state.uiState,
                            {menuOpen: action.menuOpen})
            return ret
        }
        default:
            return state
    }
}

export default game
