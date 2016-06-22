function game(state, action) {
    switch (action.type) {
        case 'INITIALIZE_GAME': {
            return action.game
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
