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
import Ajaxer         from '../lib/ajaxer'

const QuarterScore = connect(Lib.returnProps)
  (({data,quarterIndex,url,connector,dispatch}) => {
    const selectAll = e => {
        e.target.select()
    }
    const onPointChange = (teamAorB, value) => {
        dispatch(actions.changeTeamPoint(quarterIndex, teamAorB, value - 0))
    }
    const onLockChange = () => {
        Ajaxer.patch(url)
            .send({lock: !data.lock})
            .end((err, res) => {
                if (err) {
                    swal({
                        title: '通信エラー',
                        text: err,
                        type: 'error',
                    })
                    return
                }
                dispatch(actions.changeLock(quarterIndex, !data.lock))
            })
    }
    return (
        <div className="quarter-score">
          {data.lock ?
           <span className="form-control">{data.teamAPoint}</span>
           :
           <FormControl type="number"
             min={0}
             value={data.teamAPoint}
             onFocus={selectAll}
             onChange={e => { onPointChange('A', e.target.value) }}
           />}
          <span className="connector">{connector}</span>
          {data.lock ?
           <span className="form-control">{data.teamBPoint}</span>
           :
           <FormControl type="number"
             min={0}
             value={data.teamBPoint}
             onFocus={selectAll}
             onChange={e => { onPointChange('B', e.target.value) }}
           />}
          <a className="lock-control" href="#" onClick={onLockChange}>
            <Glyphicon glyph={data.lock ? 'pencil' : 'lock'} />
          </a>
        </div>
    )
})

const EditDialog = connect(Lib.returnProps)
  (({property,score,url,uiState,dispatch}) => {
    const save = () => {
        swal({
            title: '保存中...',
            text: '',
            type: 'info',
            showCancelButton: false,
            showConfirmButton: false,
        })
        const game = {property,score}
        Ajaxer.post(url)
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
        property[propName] = value
        dispatch(actions.changeGameProperty(property))
    }
    return (
       <Modal show={uiState.editDialogOpen}>
         <Modal.Body>
           <FormGroup>
             <ControlLabel>試合の名前</ControlLabel>
             <FormControl type="text"
                 value={property.name}
                 onChange={e => { setValue('name', e.target.value) }}
             />
           </FormGroup>
           <FormGroup>
             <ControlLabel>試合の場所</ControlLabel>
             <FormControl type="text"
                 value={property.place}
                 onChange={e => { setValue('place', e.target.value) }}
             />
           </FormGroup>
           <FormGroup>
             <ControlLabel>チームAの名前</ControlLabel>
             <FormControl type="text"
                 value={property.teamAName}
                 onChange={e => { setValue('teamAName', e.target.value) }}
             />
           </FormGroup>
           <FormGroup>
             <ControlLabel>チームBの名前</ControlLabel>
             <FormControl type="text"
                 value={property.teamBName}
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

const Game = ({game,urls,uiState,dispatch}) => {
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
        Ajaxer.post(urls.game)
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
    return (
        <Clearfix>
          <EditDialog property={game.property} score={game.score} url={urls.game} uiState={uiState} />
          <div className={menuClass}>
            <Button bsSize="large" onClick={onMenuClick}>
              <Glyphicon glyph="remove" />
            </Button>
            <a href="#" onClick={onEditClick}>
              <Glyphicon glyph="pencil" />
              編集
            </a>
            <a href={Lib.href('game-index-ui-href')}>
              <Glyphicon glyph="home" />
              ホーム
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
            <h1>{game.name}</h1>
            <div className="score">
              <div className="team team-a">
                {game.property.teamAName}
              </div>
              <div className="quarter-scores">
                <QuarterScore data={game.score[0]} url={urls.quarter[0]} quarterIndex={0} connector="-" />
                <QuarterScore data={game.score[1]} url={urls.quarter[1]} quarterIndex={1} connector="-" />
                <QuarterScore data={game.score[2]} url={urls.quarter[2]} quarterIndex={2} connector="-" />
                <QuarterScore data={game.score[3]} url={urls.quarter[3]} quarterIndex={3} connector="-" />
                <QuarterScore data={game.score[4]} url={urls.quarter[4]} quarterIndex={4} connector="延長" />
              </div>
              <div className="team team-b">
                {game.property.teamBName}
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
