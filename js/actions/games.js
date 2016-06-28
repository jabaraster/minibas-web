export function initializeGames(data) {
    return { type: 'INITIALIZE_GAMES', data }
}

export function deleteGame(gameId) {
    return { type: 'DELETE_GAME', gameId }
}
