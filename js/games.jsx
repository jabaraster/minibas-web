import React           from 'react'
import { createStore } from 'redux'
import { Provider }    from 'react-redux'
import { render }      from 'react-dom'
import * as actions    from './actions/games'
import reducer         from './reducers/games'
import Lib             from './lib/lib'
import Ajaxer          from './lib/ajaxer'
import Store           from './lib/store'
import RootComponent   from './containers/Games'

$(() => {

const initialState = {
    gameList: []
}
const store = Store.createStore(reducer, initialState)

Ajaxer.get(Lib.href('game-index-href')).end((err, res) => {
    if (Ajaxer.evalError(err)) return
    const gameList = res.body
    if (!gameList.length) {
        swal({
            title: 'ミニバスアプリへようこそ！',
            text: 'まずは試合情報を作成しましょう！',
            type: 'success'
        },
        () => {
            location.href = Lib.href('new-game-href')
        })
        return
    }
    store.dispatch(actions.initializeGames(gameList))
    render(
        <Provider store={store}>
          <RootComponent />
        </Provider>,
        document.getElementById('content')
    )
    Lib.hideInitialLoadingIcon()
})

})
