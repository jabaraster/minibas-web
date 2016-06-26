import React         from 'react'
import { connect }   from 'react-redux'
import ButtonToolbar from 'react-bootstrap/lib/ButtonToolbar'
import Button        from 'react-bootstrap/lib/Button'
import Glyphicon     from 'react-bootstrap/lib/Glyphicon'
import Ajaxer        from '../lib/ajaxer'
import Lib           from '../lib/lib'
import swal          from 'sweetalert'
import * as actions  from '../actions/games'

const Games = ({games, dispatch}) => {
    const editGame = (editUrl) => {
        location.href = editUrl
    }
    const deleteGame = (gameId) => {
        Lib.doubleConfirm('削除しますか？',
            '本当に削除しますか？削除すると元に戻せません！',
            () => {
                Ajaxer.del('/games/' + gameId).end((err, res) => {
                    if (Ajaxer.evalError(err)) return
                    swal({
                        title: '削除しました！',
                        text: '',
                        type: 'success',
                        showConfirmButton: false,
                        timer: 1000,
                    })
                    dispatch(actions.deleteGame(gameId))
                })
        })
    }
    const tagGame = ({game, editUrl}, idx) => {
        return (
            <tr key={'game_'+idx}>
              <td className="game-name">
                <a className="game-name" href={editUrl}>{game.name}</a>
              </td>
              <td>
                <Button bsStyle="primary" onClick={() => { editGame(editUrl) }}>
                  <Glyphicon glyph="pencil" />
                </Button>
                <Button bsStyle="danger" onClick={() => { deleteGame(game.id) }}>
                  <Glyphicon glyph="trash" />
                </Button>
              </td>
            </tr>
        )
    }
    return (
        <div className="games">
          <ButtonToolbar>
            <a className="btn btn-primary" href={Lib.href('new-game-href')}>
              <Glyphicon glyph="plus" />
            </a>
          </ButtonToolbar>
          <table className="table">
            <thead></thead>
            <tbody>
              {games.map(tagGame)}
            </tbody>
          </table>
        </div>
    )
}

function mapStateToProps(state) {
    return state
}

export default connect(mapStateToProps)(Games)
