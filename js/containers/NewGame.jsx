import React         from 'react'
import { connect }   from 'react-redux'

const NewGame = () => {
    return (
        <h1>New Game</h1>
    )
}

function mapStateToProps(state) {
    return state
}

export default connect(mapStateToProps)(NewGame)
