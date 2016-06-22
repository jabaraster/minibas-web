export function editGame(gameIndex) {
    return { type: 'EDIT_GAME', gameIndex: gameIndex }
}

export function setTeamName(type, name, gameIndex) {
    return { type, name, gameIndex }
}

export function initializeGames(games) {
    return { type: 'INITIALIZE_GAMES', games }
}
