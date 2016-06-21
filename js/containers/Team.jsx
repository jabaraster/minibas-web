import React       from 'react'
import { connect } from 'react-redux'

import FormGroup    from 'react-bootstrap/lib/FormGroup'
import ControlLabel from 'react-bootstrap/lib/ControlLabel'
import FormControl  from 'react-bootstrap/lib/FormControl'

import * as actions from '../actions'

const Team = ({teamAorB, game, gameIndex, dispatch}) => {
    console.log(game)
    const validate = () => {
        return team.name ? 'success' : 'error'
    }
    const onChange = (e, gameIndex) => {
        dispatch(actions.setTeamName('SET_TEAM_'+teamAorB+'_NAME', e.target.value, gameIndex))
    }
    return (
        <h2>{game.teamA.name}</h2>
    )
//    return (
//        <FormGroup validationState={validate()}>
//          <ControlLabel>{team.name}</ControlLabel>
//          <FormControl type="text"
//                       onChange={e => onChange(e, gameIndex)}
//                       value={team.name}/>
//          <FormControl.Feedback />
//        </FormGroup>
//    )
}


function mapStateToProps(state, props) {
    return props
    return {
        team: state.games[props.gameIndex]['team'+props.teamAorB],
        gameIndex: props.gameIndex,
    }
}

export default connect(mapStateToProps)(Team)
