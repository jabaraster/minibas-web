function games(state={}, action) {
    switch (action.type) {
        case 'INITIALIZE_GAMES': {
            return action.games
        }
        case 'EDIT_GAME': {
            const newGames = [].concat(state)
            const target = newGames[action.gameIndex]
            target.edit = !target.edit
            return newGames
        }
        case 'SET_TEAM_A_NAME': {
            const newGames = [].concat(state)
            const target = newGames[action.gameIndex]
            target.teamA.name = action.name
            return newGames
        }
        case 'SET_TEAM_B_NAME': {
            const newGames = [].concat(state)
            const target = newGames[action.gameIndex]
            target.teamB.name = action.name
            return newGames
        }
        default:
            return state
    }
}

export default games
