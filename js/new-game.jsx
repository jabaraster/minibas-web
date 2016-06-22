import React           from 'react'
import { createStore } from 'redux'
import { Provider }    from 'react-redux'
import { render }      from 'react-dom'
import newGame             from './reducers/new-game'
import Ajaxer          from './lib/ajaxer'
import lib             from './lib/lib'
import * as actions    from './actions/new-game'
import swal            from 'sweetalert'
import RootComponent   from './containers/NewGame'
import { combineReducers } from 'redux'

const store = createStore(newGame)

$(() => {
    Ajaxer.get(lib.href('empty-game-href')).end((err, res) => {
        if (err) {
            swal({title:'通信エラー',text:err,type:'error'})
            return
        }
        const state = res.body
        state.wizardState = {
          activeKey: 0,
          panes: [
            { label: '試合名と場所' },
            { label: 'チーム名'     },
          ]}
        store.dispatch(actions.initializeNewGame(res.body))
        render(
            <Provider store={store}>
              <RootComponent />
            </Provider>,
            document.getElementById('content')
        )
    })
})
