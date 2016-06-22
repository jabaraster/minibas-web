import React           from 'react'
import { createStore } from 'redux'
import { Provider }    from 'react-redux'
import { render }      from 'react-dom'
import app             from './reducers'
import Ajaxer          from './lib/ajaxer'
import lib             from './lib/lib'
import swal            from 'sweetalert'

const store = createStore(app, {})

$(() => {
    Ajaxer.get(lib.href('empty-game-href')).end((err, ref) => {
        if (err) {
            swal({title:'通信エラー',text:err,type:'error'})
            return
        }
        store.dispatch(actions.initializeGames(res.body))
        render(
            <Provider store={store}>
              <RootComponent />
            </Provider>,
            document.getElementById('content')
        )
    })
})
