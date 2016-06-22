import React         from 'react'
import { connect }   from 'react-redux'
import ButtonToolbar from 'react-bootstrap/lib/ButtonToolbar'
import Button        from 'react-bootstrap/lib/Button'
import Glyphicon     from 'react-bootstrap/lib/Glyphicon'
import lib           from '../lib/lib'

const Games = ({games, dispatch}) => {
    const tagGame = (game, idx) => {
        return (
            <tr key={'game_'+idx}>
              <td>{game.name}</td>
              <td>
                <Button>
                  <Glyphicon glyph="pencil" />
                </Button>
                <Button>
                  <Glyphicon glyph="trash" />
                </Button>
              </td>
            </tr>
        )
    }
    return (
        <div className="games">
          <ButtonToolbar>
            <a className="btn btn-primary" href={lib.href('new-game-href')}>
              <Glyphicon glyph="plus" />
            </a>
          </ButtonToolbar>
          <table className="table table-striped">
            <thead></thead>
            <tbody>
              {games.map(tagGame)}
            </tbody>
          </table>
        </div>
    )
}

function mapStateToProps(state) {
    return { games: state.games }
}

export default connect(mapStateToProps)(Games)
