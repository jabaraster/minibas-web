import Lib              from '../lib/lib'
import * as NewGameActs from '../actions/new-game'
import * as LeagueActs  from '../actions/league'
import * as TeamActs    from '../actions/team'

const copyGame = state => {
    const ret = Lib.shallowCopy(state)
    ret.game = Lib.shallowCopy(state.game)
    return ret
}

function newGame(state, action) {
    switch (action.type) {
        case NewGameActs.CHANGE_LEAGUE_NAME: {
                console.log(action)
            const ret = copyGame(state)
            ret.game.leagueName = action.value
            return ret
        }
        case NewGameActs.CHANGE_GAME_NAME: {
            const ret = copyGame(state)
            ret.game.gameName = action.value
            return ret
        }
        case NewGameActs.CHANGE_GAME_PLACE: {
            const ret = copyGame(state)
            ret.game.gamePlace = action.value
            return ret
        }
        case NewGameActs.CHANGE_TEAM_A_NAME: {
            const ret = copyGame(state)
            ret.game.teamAName = action.value
            return ret
        }
        case NewGameActs.CHANGE_TEAM_B_NAME: {
            const ret = copyGame(state)
            ret.game.teamBName = action.value
            return ret
        }
        case LeagueActs.FETCHED_LEAGUE_LIST: {
            const ret = Lib.shallowCopy(state)
            ret.leagueList = action.leagueList
            return ret
        }
        case TeamActs.FETCHED_TEAM_LIST: {
            const ret = Lib.shallowCopy(state)
            ret.teamList = action.teamList
            return ret
        }
        default:
            return state
    }
}

export default newGame
