import Lib from '../lib/lib'

const copyScore = (state, quarterIndex) => {
    const ret = Lib.shallowCopy(state)
    ret.game = Lib.shallowCopy(state.game)
    ret.game.scoreList = Lib.shallowCopy(state.game.scoreList)
    ret.game.scoreList[quarterIndex] = Lib.shallowCopy(state.game.scoreList[quarterIndex])
    return ret
}
const copyUiState = (state) => {
    const ret = Lib.shallowCopy(state)
    ret.uiState = Lib.shallowCopy(state.uiState)
    return ret
}

function game(state, action) {
    switch (action.type) {
        case 'INITIALIZE_GAME': {
            const ret = Lib.shallowCopy(state)
            ret.game = action.game
            return ret
        }
        case 'CHANGE_GAME_PROPERTY': {
            const ret = Lib.shallowCopy(state)
            Lib.assign(ret, action.game)
            return ret
        }
        case 'CHANGE_TEAM_POINT': {
            const quarterIndex = action.quarterIndex
            const ret = copyScore(state, quarterIndex)
            const prop = 'team'+action.teamAorB+'Point'
            ret.game.scoreList[quarterIndex][prop] = action.value
            return ret
        }
        case 'CHANGE_LOCK': {
            const quarterIndex = action.quarterIndex
            const ret = copyScore(state, quarterIndex)
            ret.game.scoreList[quarterIndex].lock = action.lock
            return ret
        }
        case 'UI_CHANGE_EDIT_DIALOG_OPEN': {
            const ret = copyUiState(state)
            ret.uiState.editDialogOpen = action.editDialogOpen
            return ret
        }
        case 'UI_CHANGE_MENU_OPEN': {
            const ret = copyUiState(state)
            ret.uiState.menuOpen = action.menuOpen
            return ret
        }
        default:
            return state
    }
}

export default game
