
import React         from 'react'
import { createStore } from 'redux'
import { render }      from 'react-dom'
import { Provider }    from 'react-redux'
import { connect }   from 'react-redux'
import FormGroup     from 'react-bootstrap/lib/FormGroup'
import FormControl   from 'react-bootstrap/lib/FormControl'

function changeInput(value) {
    return { type: 'CHANGE_INPUT', value }
}

function reducer(state, action) {
  console.log(state)
    switch (action.type) {
        case 'CHANGE_INPUT': {
            const ret = JSON.parse(JSON.stringify(state))
            ret.newGame.game.name = action.value
            console.log(ret)
            return ret
        }
        default:
            return state
    }
}

function mapStateToProps(state) {
    return state
}
const Page = connect(mapStateToProps)(({newGame,dispatch}) => {
    const onChange = (e) => {
        dispatch(changeInput(e.target.value))
    }
    return (
        <div>
          <h1>{'value: '+newGame.game.name}</h1>
          <FormGroup>
            <FormControl type="text"
              value={newGame.game.name}
              onChange={e => onChange(e)}
            />
          </FormGroup>
          <input type="text"
              value={newGame.game.name}
              onChange={e => onChange(e)}
          />
        </div>
    )
})

const initialState = {
    newGame: {
        game: { name: '' },
        score: []
    }
}
const store = createStore(reducer, initialState)

render(
    <Provider store={store}>
      <Page />
    </Provider>,
    document.getElementById('content')
)
