function games(state={}, action) {
    switch (action.type) {
        case 'INITIALIZE_GAMES': {
            return { gameList: action.gameList }
        }
        case 'DELETE_GAME': {
            const newGames = [].concat(state.data)
            const delIdx = newGames.findIndex(data => {
                return data.game.property.id === action.gameId
            })
            if (delIdx >= 0) {
                newGames.splice(delIdx, 1)
            }
            return  { gameList: newGames }
        }
        default:
            return state
    }
}

export default games
