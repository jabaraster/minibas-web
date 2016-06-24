
export function initializeNewGame(game) {
    return { type: 'INITIALIZE_NEW_GAME', game }
}

export function changeGameName(value) {
    return { type: 'CHANGE_GAME_NAME', value }
}

export function changeGamePlace(value) {
    return { type: 'CHANGE_GAME_PLACE', value }
}

export function changeTeamAName(value) {
    return { type: 'CHANGE_TEMA_A_NAME', value }
}

export function changeTeamBName(value) {
    return { type: 'CHANGE_TEMA_B_NAME', value }
}

export function changeWizardPane(paneIndex) {
    return { type: 'CHANGE_WIZARD_PANE', paneIndex }
}

