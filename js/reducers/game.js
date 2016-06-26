function game(state, action) {
    switch (action.type) {
        case 'INITIALIZE_GAME': {
            return action.game
        }
        case 'CHANGE_TEAM_POINT': {
            const ret = Object.assign({}, state)
            ret.score = [].concat(ret.score)

            const quarterIndex = action.quarterIndex
            ret.score[quarterIndex] = Object.assign({}, ret.score[quarterIndex])
            ret.score[quarterIndex]['team'+action.teamAorB+'Point'] = action.value

            return ret
        }
        case 'CHANGE_LOCK': {
            const ret = Object.assign({}, state)
            ret.score = [].concat(ret.score)

            const quarterIndex = action.quarterIndex
            ret.score[quarterIndex] = Object.assign({}, ret.score[quarterIndex])
            ret.score[quarterIndex].lock = action.lock

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
