import React           from 'react'
import { createStore } from 'redux'
import { Provider }    from 'react-redux'
import { render }      from 'react-dom'

import Store           from './lib/store'
import reducer         from './reducers/new-game'

import Ajaxer          from './lib/ajaxer'
import Lib             from './lib/lib'
import * as actions    from './actions/new-game'
import RootComponent   from './containers/NewGame'

$(() => {

const initialState = {}
const store = Store.createStore(reducer, initialState)

Ajaxer.get(Lib.href('empty-game-href')).end((err, res) => {
    if (Ajaxer.evalError(err)) return
    const state = res.body
    store.dispatch(actions.initializeNewGame(state))
    render(
        <Provider store={store}>
          <RootComponent />
        </Provider>,
        document.getElementById('content')
    )
    Lib.hideInitialLoadingIcon()
    Lib.setUpWindowUnloadAlert()
})

})
