import React           from 'react'
import { createStore } from 'redux'
import { Provider }    from 'react-redux'
import { render }      from 'react-dom'
import app             from './reducers'
import Games           from './containers/Games'

const initialState = {
    games: [
      { name: 'ゲーム1', edit: false, teamA: { name: 'チームA', point: 0}, teamB: { name: 'チームB', point: 0 } },
      { name: 'ゲーム2', edit: false, teamA: { name: 'チームA', point: 0}, teamB: { name: 'チームB', point: 0 } },
    ]
}
const store = createStore(app, initialState)

render(
    <Provider store={store}>
      <Games />
    </Provider>,
    document.getElementById('page')
)
