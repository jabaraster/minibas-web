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

const initialState = {
    game: {},
    uiState: {
        menuOpen: false,
        editDialogOpen: false,
    }
}
const store = Store.createStore(reducer, initialState)

Ajaxer.get(Lib.href('game-href')).end((err, res) => {
    if (Ajaxer.evalError(err)) return
    store.dispatch(actions.initialzeGame(res.body))
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
