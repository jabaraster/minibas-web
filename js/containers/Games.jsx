import React       from 'react'
import { connect } from 'react-redux'

import Button       from 'react-bootstrap/lib/Button'
import Glyphicon    from 'react-bootstrap/lib/Glyphicon'

import FormGroup    from 'react-bootstrap/lib/FormGroup'
import ControlLabel from 'react-bootstrap/lib/ControlLabel'
import FormControl  from 'react-bootstrap/lib/FormControl'

import Team         from './Team'

import * as actions from '../actions'

const Games = ({games, dispatch}) => {
    const tagGame = (game, idx) => {
        const onEditClick = (gameIndex) => {
            dispatch(actions.editGame(gameIndex))
        }
        const onChange = (e, teamAorB) => {
            dispatch(actions.setTeamName('SET_TEAM_'+teamAorB+'_NAME', e.target.value, idx))
        }
        return (
            <div key={'game_'+idx} className="game">
              <Button bsStyle="primary" onClick={() => onEditClick(idx)}>
                <Glyphicon glyph="pencil" />
              </Button>
              {game.edit ?
                 ( <span className="editing">編集中</span> ) :
                 null }
              <h1>{game.name}</h1>

              {game.edit ?
                 (<FormGroup>
                    <FormControl type="text"
                                 onChange={e => onChange(e, "A")}
                                 value={game.teamA.name}/>
                    <FormControl.Feedback />
                  </FormGroup>) :
                 (<h2>{game.teamA.name}</h2>)}

               {game.edit ?
                 (<FormGroup>
                    <FormControl type="text"
                                 onChange={e => onChange(e, "B")}
                                 value={game.teamB.name}/>
                    <FormControl.Feedback />
                  </FormGroup>) :
                 (<h2>{game.teamB.name}</h2>)}

               <input type="range" />

              <Button bsStyle="primary">
                <Glyphicon glyph="plus" />
              </Button>
            </div>
        )
    }
    return (
        <div className="games">
          {games.map(tagGame)}
        </div>
    )
}

function mapStateToProps(state) {
    return { games: state.games }
}

export default connect(mapStateToProps)(Games)
