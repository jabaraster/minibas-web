import Ajaxer from '../lib/ajaxer'
import Lib    from '../lib/lib'

export const startFetchLeagueList = () => {
    return dispatch => {
        Ajaxer.get(Lib.href('league-index-href')).end((err, res) => {
            if (Ajaxer.evalError(err)) return
            dispatch(fetchedLeagueList(res.body))
        })
    }
}

export const FETCHED_LEAGUE_LIST = 'league_FETCHED_LEAGUE_LIST'
export const fetchedLeagueList = leagueList => {
    return { type: FETCHED_LEAGUE_LIST, leagueList }
}
