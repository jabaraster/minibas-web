export function initialzeGame(game) {
    return { type: 'INITIALIZE_GAME', game }
}

export function changeGame(game) {
    return { type: 'CHANGE_GAME', game }
}

export function changeTeamPoint(quarterIndex, teamAorB, value) {
    return { type: 'CHANGE_TEAM_POINT', quarterIndex, teamAorB, value }
}

export function changeLock(quarterIndex, lock) {
    return { type: 'CHANGE_LOCK', quarterIndex, lock }
}

export function uiChangeMenuOpen(menuOpen) {
    return { type: 'UI_CHANGE_MENU_OPEN', menuOpen }
}

export function uiChangeEditDialogOpen(editDialogOpen) {
    return { type: 'UI_CHANGE_EDIT_DIALOG_OPEN', editDialogOpen }
}
