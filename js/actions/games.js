export function initializeGames(gameList) {
    return { type: 'INITIALIZE_GAMES', gameList }
}

export function deleteGame(gameId) {
    return { type: 'DELETE_GAME', gameId }
}
