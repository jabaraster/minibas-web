import React          from 'react'
import { connect }    from 'react-redux'
import ButtonToolbar  from 'react-bootstrap/lib/ButtonToolbar'
import Button         from 'react-bootstrap/lib/Button'
import Glyphicon      from 'react-bootstrap/lib/Glyphicon'
import ControlLabel   from 'react-bootstrap/lib/ControlLabel'
import FormGroup      from 'react-bootstrap/lib/FormGroup'
import FormControl    from 'react-bootstrap/lib/FormControl'
import Modal          from 'react-bootstrap/lib/Modal'
import classnames     from 'classnames';
import Clearfix       from 'react-bootstrap/lib/Clearfix'
import * as actions   from '../actions/game'
import Lib            from '../lib/lib'
import App            from '../lib/app'
import Ajaxer         from '../lib/ajaxer'

const QuarterScore = connect(Lib.returnProps)
  (({score,quarterIndex,connector,dispatch}) => {
    const selectAll = e => {
        e.target.select()
    }
    const onPointChange = (teamAorB, value) => {
        dispatch(actions.changeTeamPoint(quarterIndex, teamAorB, value - 0))
    }
    const onLockChange = () => {
        Ajaxer.patch(score.urlBase)
            .send({lock: !score.lock})
            .end((err, res) => {
                if (err) {
                    swal({
                        title: '通信エラー',
                        text: err,
                        type: 'error',
                    })
                    return
                }
                dispatch(actions.changeLock(quarterIndex, !score.lock))
            })
    }
    return (
        <div className="quarter-score">
          {score.lock ?
           <span className="form-control">{score.teamAPoint}</span>
           :
           <FormControl type="number"
             min={0}
             value={score.teamAPoint}
             onFocus={selectAll}
             onChange={e => { onPointChange('A', e.target.value) }}
           />}
          <span className="connector">{connector}</span>
          {score.lock ?
           <span className="form-control">{score.teamBPoint}</span>
           :
           <FormControl type="number"
             min={0}
             value={score.teamBPoint}
             onFocus={selectAll}
             onChange={e => { onPointChange('B', e.target.value) }}
           />}
          <a className="lock-control" href="#" onClick={onLockChange}>
            <Glyphicon glyph={score.lock ? 'pencil' : 'lock'} />
          </a>
        </div>
    )
})

const EditDialog = connect(Lib.returnProps)
  (({game,uiState,dispatch}) => {
    const save = () => {
        swal({
            title: '保存中...',
            text: '',
            type: 'info',
            showCancelButton: false,
            showConfirmButton: false,
        })
        Ajaxer.post(game.urlBase)
            .send(game)
            .end((err,res) => {
            if (Ajaxer.evalError(err)) return
            swal.close()
            dispatch(actions.changeGameProperty(property))
            dispatch(actions.uiChangeEditDialogOpen(false))
            dispatch(actions.uiChangeMenuOpen(false))
        })
    }
    const onCancel = () => {
        dispatch(actions.uiChangeEditDialogOpen(false))
    }
    const setValue = (propName, value) => {
        game[propName] = value
        dispatch(actions.changeGameProperty(game))
    }
    return (
       <Modal show={uiState.editDialogOpen}>
         <Modal.Body>
           <FormGroup>
             <ControlLabel>試合の名前</ControlLabel>
             <FormControl type="text"
                 value={game.name}
                 onChange={e => { setValue('name', e.target.value) }}
             />
           </FormGroup>
           <FormGroup>
             <ControlLabel>試合の場所</ControlLabel>
             <FormControl type="text"
                 value={game.place}
                 onChange={e => { setValue('place', e.target.value) }}
             />
           </FormGroup>
           <FormGroup>
             <ControlLabel>チームAの名前</ControlLabel>
             <FormControl type="text"
                 value={game.teamAName}
                 onChange={e => { setValue('teamAName', e.target.value) }}
             />
           </FormGroup>
           <FormGroup>
             <ControlLabel>チームBの名前</ControlLabel>
             <FormControl type="text"
                 value={game.teamBName}
                 onChange={e => { setValue('teamBName', e.target.value) }}
             />
           </FormGroup>
         </Modal.Body>
         <Modal.Footer>
           <Button onClick={onCancel}>キャンセル</Button>
           <Button onClick={save} bsStyle="success">OK</Button>
         </Modal.Footer>
       </Modal>
    )
})

const Game = ({game,uiState,dispatch}) => {
    const onMenuClick = () => {
        dispatch(actions.uiChangeMenuOpen(!uiState.menuOpen))
    }
    const save = () => {
        swal({
            title: '保存中...',
            text: '',
            type: 'info',
            showCancelButton: false,
            showConfirmButton: false,
        })
        Ajaxer.post(game.urlBase)
            .send(game)
            .end((err,res) => {
            if (Ajaxer.evalError(err)) return
            swal.close()
        })
    }
    const onEditClick = () => {
        dispatch(actions.uiChangeEditDialogOpen(!uiState.editDialogOpen))
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
    const total = App.total(game.scoreList)
    return (
        <Clearfix>
          <EditDialog game={game} uiState={uiState} />
          <div className={menuClass}>
            <Button bsSize="large" onClick={onMenuClick}>
              <Glyphicon glyph="remove" />
            </Button>
            <a href="#" onClick={onEditClick}>
              <Glyphicon glyph="pencil" />
              ゲーム情報の編集
            </a>
            <a href={Lib.href('game-index-ui-href')}>
              <Glyphicon glyph="home" />
              ホームに戻る
            </a>
          </div>
          <div className={contentClass}>
            <ButtonToolbar>
              <Button bsSize="large" onClick={onMenuClick}>
                <Glyphicon glyph="list" />
              </Button>
              <Button bsStyle="success" bsSize="large" onClick={save}>
                <Glyphicon glyph="cloud-upload" />
              </Button>
            </ButtonToolbar>
            <h1>{game.leagueName}</h1>
            <h1>{game.name}</h1>
            <div className="score">
              <div className="team team-a">
                {game.teamAName}
              </div>
              <div className="quarter-scores">
                <QuarterScore score={game.scoreList[0]} quarterIndex={0} connector="-" />
                <QuarterScore score={game.scoreList[1]} quarterIndex={1} connector="-" />
                <QuarterScore score={game.scoreList[2]} quarterIndex={2} connector="-" />
                <QuarterScore score={game.scoreList[3]} quarterIndex={3} connector="-" />
                <QuarterScore score={game.scoreList[4]} quarterIndex={4} connector="延長" />
                <div className="quarter-score total">
                  <span className="total-point team-a form-control">{total.teamAPoint}</span>
                  <span className="connector">計</span>
                  <span className="total-point team-b form-control">{total.teamBPoint}</span>
                  <a className="lock-control">
                    <Glyphicon glyph="ok"/>
                  </a>
                </div>
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
