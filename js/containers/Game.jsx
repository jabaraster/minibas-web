import React          from 'react'
import { connect }    from 'react-redux'
import Button         from 'react-bootstrap/lib/Button'
import Glyphicon      from 'react-bootstrap/lib/Glyphicon'
import FormControl    from 'react-bootstrap/lib/FormControl'
import classnames     from 'classnames';
import Clearfix       from 'react-bootstrap/lib/Clearfix'
import * as actions   from '../actions/game'
import Lib            from '../lib/lib'

const QuarterScore = connect((_,props) => { return props })
  (({data,quarterIndex,connector,dispatch}) => {
    const selectAll = e => { e.target.select() }
    const onPointChange = (teamAorB, value) => {
        dispatch(actions.changeTeamPoint(quarterIndex, teamAorB, value - 0))
    }
    return (
        <div className="quarter-score">
          <FormControl type="number"
            min={0}
            value={data.teamAPoint}
            onFocus={selectAll}
            onChange={e => { onPointChange('A', e.target.value) }}
          />
          <span className="connector">{connector}</span>
          <FormControl type="number"
            min={0}
            value={data.teamBPoint}
            onFocus={selectAll}
            onChange={e => { onPointChange('B', e.target.value) }}
          />
        </div>
    )
})

const Game = ({game,score,uiState,dispatch}) => {
    const onMenuClick = () => {
        dispatch(actions.uiChangeMenuOpen(!uiState.menuOpen))
    }
    const menuClass = classnames({
                        'menu': true,
                        'menu-open': uiState.menuOpen,
                        'menu-close': !uiState.menuOpen,
                      })
    const contentClass = classnames({
                        'main-content': true,
                        'main-content-menu-open': uiState.menuOpen,
                         })
    return (
        <Clearfix>
          <div className={menuClass}>
            <Button bsSize="large" onClick={onMenuClick}>
              <Glyphicon glyph="remove" />
            </Button>
            <a href={Lib.href('game-index-ui-href')}>
              <Glyphicon glyph="home" />
              ホーム
            </a>
          </div>
          <div className={contentClass}>
            <Button bsSize="large" onClick={onMenuClick}>
              <Glyphicon glyph="list" />
            </Button>
            <h1>{game.name}</h1>
            <div className="score">
              <div className="team team-a">
                {game.teamAName}
              </div>
              <div className="quarter-scores">
                <QuarterScore data={score[0]} quarterIndex={0} connector="-" />
                <QuarterScore data={score[1]} quarterIndex={1} connector="-" />
                <QuarterScore data={score[2]} quarterIndex={2} connector="-" />
                <QuarterScore data={score[3]} quarterIndex={3} connector="-" />
                <QuarterScore data={score[4]} quarterIndex={4} connector="延長" />
              </div>
              <div className="team team-b">
                {game.teamBName}
              </div>
            </div>
          </div>
        </Clearfix>
    )
}

function mapStateToProps(state) {
    return state
}

export default connect(mapStateToProps)(Game)
