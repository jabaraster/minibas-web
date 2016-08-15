import React            from 'react'
import { createStore }  from 'redux'
import { Provider }     from 'react-redux'
import { render }       from 'react-dom'

import Store            from './lib/store'
import reducer          from './reducers/new-game'

import Ajaxer           from './lib/ajaxer'
import Lib              from './lib/lib'
import * as NewGameActs from './actions/new-game'
import * as LeagueActs  from './actions/league';
import * as TeamActs    from './actions/team';
import RootComponent    from './containers/NewGame'

$(() => {

const initialState = {
    game: {
        leagueName: '',
        gameName: '',
        gamePlace: '',
        teamAName: '',
        teamBName: '',
    },
    leagueList: [],
    teamList: [],
}
const store = Store.createStore(reducer, initialState)

render(
    <Provider store={store}>
      <RootComponent />
    </Provider>,
    document.getElementById('content')
)

store.dispatch(LeagueActs.startFetchLeagueList())
store.dispatch(TeamActs.startFetchTeamList())

Lib.hideInitialLoadingIcon()
Lib.setUpWindowUnloadAlert()

})
