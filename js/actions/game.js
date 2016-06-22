export function initialzeGame(game) {
    return { type: 'INITIALIZE_GAME', game }
}

export function uiChangeMenuOpen(menuOpen) {
    return { type: 'UI_CHANGE_MENU_OPEN', menuOpen }
}
