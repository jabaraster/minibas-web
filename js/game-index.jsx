import React           from 'react'
import { createStore } from 'redux'
import { Provider }    from 'react-redux'
import { render }      from 'react-dom'
import * as actions    from './actions/games'
import app             from './reducers/games'
import lib             from './lib/lib'
import swal            from 'sweetalert'
import Ajaxer          from './lib/ajaxer'
import RootComponent   from './containers/Games'

const initialState = {
    games: []
}
const store = createStore(app, initialState)

$(() => {
    Ajaxer.get(lib.href('game-index-href')).end((err, res) => {
        if (err) {
            swal({title:'通信エラー',text:err,type:'error'})
            return
        }
        render(
            <Provider store={store}>
              <RootComponent />
            </Provider>,
            document.getElementById('content')
        )
    })
})
