import React           from 'react'
import { createStore } from 'redux'
import { Provider }    from 'react-redux'
import { render }      from 'react-dom'
import * as actions    from './actions/game'
import reducer         from './reducers/game'
import Lib             from './lib/lib'
import Ajaxer          from './lib/ajaxer'
import Store           from './lib/store'
import RootComponent   from './containers/Game'

$(() => {

const store = Store.createStore(reducer, {})

Ajaxer.get(Lib.href('game-href')).end((err, res) => {
    if (Ajaxer.evalError(err)) return
            console.log(res)
    const state = res.body
    state.uiState = {
        menuOpen: false,
        editDialogOpen: false,
    }
    store.dispatch(actions.initialzeGame(state))
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
