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

const EditDialog = connect(Lib.returnProps)(({game,score,url,uiState,dispatch}) => {
    const save = () => {
        swal({
            title: '保存中...',
            text: '',
            type: 'info',
            showCancelButton: false,
            showConfirmButton: false,
        })
        Ajaxer.post(url)
            .send({game,score,editUrl:'',url:'',scoreUrls:[]})
            .end((err,res) => {
            if (Ajaxer.evalError(err)) return
            swal.close()
            dispatch(actions.changeGame(game))
            dispatch(actions.uiChangeEditDialogOpen(false))
            dispatch(actions.uiChangeMenuOpen(false))
        })
    }
    const onCancel = () => {
        dispatch(actions.uiChangeEditDialogOpen(false))
    }
    const setValue = (propName, value) => {
        game[propName] = value
        dispatch(actions.changeGame(game))
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

const Game = ({game,score,url,scoreUrls,uiState,dispatch}) => {
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
        Ajaxer.post(url)
            .send({game,score,editUrl:'',url:'',scoreUrls:[]})
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
          <EditDialog game={game} score={score} url={url} uiState={uiState} />
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
                {game.teamAName}
              </div>
              <div className="quarter-scores">
                <QuarterScore data={score[0]} url={scoreUrls[0]} quarterIndex={0} connector="-" />
                <QuarterScore data={score[1]} url={scoreUrls[1]} quarterIndex={1} connector="-" />
                <QuarterScore data={score[2]} url={scoreUrls[2]} quarterIndex={2} connector="-" />
                <QuarterScore data={score[3]} url={scoreUrls[3]} quarterIndex={3} connector="-" />
                <QuarterScore data={score[4]} url={scoreUrls[4]} quarterIndex={4} connector="延長" />
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
