export function initialzeGame(game) {
    return { type: 'INITIALIZE_GAME', game }
}

export function changeTeamPoint(quarterIndex, teamAorB, value) {
    return { type: 'CHANGE_TEAM_POINT', quarterIndex, teamAorB, value }
}

export function uiChangeMenuOpen(menuOpen) {
    return { type: 'UI_CHANGE_MENU_OPEN', menuOpen }
}
