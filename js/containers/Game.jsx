import React          from 'react'
import { connect }    from 'react-redux'
import Button         from 'react-bootstrap/lib/Button'
import Glyphicon      from 'react-bootstrap/lib/Glyphicon'
import classnames     from 'classnames';
import Clearfix       from 'react-bootstrap/lib/Clearfix'
import * as actions   from '../actions/game'

const Game = ({game,uiState,dispatch}) => {
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
            menu
          </div>
          <div className={contentClass}>
            <Button onClick={onMenuClick}>
              <Glyphicon glyph="list" />
            </Button>
            <div className="score">
              <div className="quarter first"></div>
              <div className="quarter second"></div>
              <div className="quarter third"></div>
              <div className="quarter fourth"></div>
            </div>
          </div>
        </Clearfix>
    )
}

function mapStateToProps(state) {
  console.log(state)
    return state
}

export default connect(mapStateToProps)(Game)
