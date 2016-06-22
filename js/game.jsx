import React           from 'react'
import { createStore } from 'redux'
import { Provider }    from 'react-redux'
import { render }      from 'react-dom'
import * as actions    from './actions/game'
import app             from './reducers/game'
import Ajaxer          from './lib/ajaxer'
import lib             from './lib/lib'
import RootComponent   from './containers/Game'

const store = createStore(app, {})

$(() => {
    Ajaxer.get($('#game-href').text()).end((err, res) => {
        if (Ajaxer.evalError(err)) return
        const state = res.body
        state.uiState = {
            menuOpen: false,
        }
        store.dispatch(actions.initialzeGame(state))
        render(
            <Provider store={store}>
              <RootComponent />
            </Provider>,
            document.getElementById('content')
        )
    })
})
