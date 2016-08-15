
export const INITIALIZE_NEW_GAME = 'new-game_INITIALIZE_NEW_GAME'
export const initializeNewGame = game => {
    return { type: INITIALIZE_NEW_GAME, game }
}

export const CHANGE_LEAGUE_NAME = 'new-game_CHANGE_LEAGUE_NAME'
export const changeLeagueName = value => {
    return { type: CHANGE_LEAGUE_NAME, value }
}

export const CHANGE_GAME_NAME = 'new-game_CHANGE_GAME_NAME'
export const changeGameName = value => {
    return { type: CHANGE_GAME_NAME, value }
}

export const CHANGE_GAME_PLACE = 'new-game_CHANGE_GAME_PLACE'
export const changeGamePlace = value => {
    return { type: CHANGE_GAME_PLACE, value }
}

export const CHANGE_TEAM_A_NAME = 'new-game_CHANGE_TEAM_A_NAME'
export const changeTeamAName = value => {
    return { type: CHANGE_TEAM_A_NAME, value }
}

export const CHANGE_TEAM_B_NAME = 'new-game_CHANGE_TEAM_B_NAME'
export const changeTeamBName = value => {
    return { type: CHANGE_TEAM_B_NAME, value }
}
