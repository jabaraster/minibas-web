import React         from 'react'
import { connect }   from 'react-redux'
import ButtonToolbar from 'react-bootstrap/lib/ButtonToolbar'
import Glyphicon     from 'react-bootstrap/lib/Glyphicon'
import lib           from '../lib/lib'

const Games = ({games, dispatch}) => {
    const tagGame = (game, idx) => {
        return (
            <li key={'game_'+idx}>
              {game.name}
            </li>
        )
    }
    return (
        <div className="games">
          <ButtonToolbar>
            <a className="btn btn-primary" href={lib.href('new-game-href')}>
              <Glyphicon glyph="plus" />
            </a>
          </ButtonToolbar>
          <ul>
            {games.map(tagGame)}
          </ul>
        </div>
    )
}

function mapStateToProps(state) {
    return { games: state.games }
}

export default connect(mapStateToProps)(Games)
