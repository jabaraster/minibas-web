export function initializeGames(games) {
    return { type: 'INITIALIZE_GAMES', games }
}

export function deleteGame(gameId) {
    return { type: 'DELETE_GAME', gameId }
}
