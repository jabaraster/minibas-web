import Ajaxer from '../lib/ajaxer'
import Lib    from '../lib/lib'

export const startFetchTeamList = () => {
    return dispatch => {
        Ajaxer.get(Lib.href('team-index-href')).end((err, res) => {
            if (Ajaxer.evalError(err)) return
            dispatch(fetchedTeamList(res.body))
        })
    }
}

export const FETCHED_TEAM_LIST = 'team_FETCHED_TEAM_LIST'
export const fetchedTeamList = teamList => {
    return { type: FETCHED_TEAM_LIST, teamList }
}
