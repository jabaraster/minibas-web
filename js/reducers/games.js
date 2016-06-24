function games(state={}, action) {
    switch (action.type) {
        case 'INITIALIZE_GAMES': {
            return { games: action.games }
        }
        case 'DELETE_GAME': {
            const newGames = [].concat(state.games)
            const delIdx = newGames.findIndex(game => {
                return game.game.id === action.gameId
            })
            if (delIdx >= 0) {
                newGames.splice(delIdx, 1)
            }
            return  { games: newGames }
        }
        default:
            return state
    }
}

export default games
