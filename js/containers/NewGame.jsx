import React         from 'react'
import { connect }   from 'react-redux'
import ButtonToolbar from 'react-bootstrap/lib/ButtonToolbar'
import Button        from 'react-bootstrap/lib/Button'
import Glyphicon     from 'react-bootstrap/lib/Glyphicon'
import FormGroup     from 'react-bootstrap/lib/FormGroup'
import FormControl   from 'react-bootstrap/lib/FormControl'
import ControlLabel  from 'react-bootstrap/lib/ControlLabel'
import Nav           from 'react-bootstrap/lib/Nav'
import NavItem       from 'react-bootstrap/lib/NavItem'
import Ajaxer        from '../lib/ajaxer'
import lib           from '../lib/lib'
import * as actions  from '../actions/new-game'

const WizardMenu = connect(state => { return state.wizardState })
    (({activeKey,panes,dispatch}) => {
    const onSelect = (paneIndex) => {
        dispatch(actions.changeWizardPane(paneIndex))
    }
    const tagItem = (pane,idx) => {
        return (
            <NavItem key={'navitem'+idx} eventKey={idx} >{pane.label}</NavItem>
        )
    }
    return (
        <Nav bsStyle="pills" activeKey={activeKey} onSelect={onSelect}>
          {panes.map(tagItem)}
        </Nav>
    )
})

const FirstPane = connect((_, props) => { return props })(({game,dispatch}) => {
    const onPlaceChange = (e) => {
        dispatch(actions.changeGamePlace(e.target.value))
    }
    const onNameChange = (e) => {
        dispatch(actions.changeGameName(e.target.value))
    }
    return (
        <div>
          <FormGroup>
            <ControlLabel>試合名を入力して下さい</ControlLabel>
            <FormControl
               type="text"
               value={game.name}
               onChange={e => onNameChange(e)}
            />
          </FormGroup>
          <FormGroup>
            <ControlLabel>場所を入力して下さい</ControlLabel>
            <FormControl
               type="text"
               value={game.place}
               onChange={e => onPlaceChange(e)}
            />
          </FormGroup>
        </div>
    )
})

const SecondPane = connect((_, props) => { return props })(({game,dispatch}) => {
    const onTeamANameChange = (e) => {
        dispatch(actions.changeTeamAName(e.target.value))
    }
    const onTeamBNameChange = (e) => {
        dispatch(actions.changeTeamBName(e.target.value))
    }
    return (
        <div className="second-pane">
          <FormGroup>
            <ControlLabel>チーム<span className="emphasis">A</span>の名前を入力して下さい</ControlLabel>
            <FormControl
               type="text"
               value={game.teamAName}
               onChange={e => onTeamANameChange(e)}
            />
          </FormGroup>
          <FormGroup>
            <ControlLabel>チーム<span className="emphasis">B</span>の名前を入力して下さい</ControlLabel>
            <FormControl
               type="text"
               value={game.teamBName}
               onChange={e => onTeamBNameChange(e)}
            />
          </FormGroup>
        </div>
    )
})

const NewGame = ({game, score, wizardState, dispatch}) => {
    const movePreviousPane = () => {
        if (wizardState.activeKey === 0) return;
        dispatch(actions.changeWizardPane(wizardState.activeKey - 1))
    }
    const moveNextPane = () => {
        if (wizardState.activeKey >= wizardState.panes.length - 1) return;
        dispatch(actions.changeWizardPane(wizardState.activeKey + 1))
    }
    const save = () => {
        swal({
            title: '',
            text: '保存しますか？',
            type: 'info',
            showCancelButton: true,
            closeOnConfirm: false,
            showLoaderOnConfirm: true,
        },
        function(isConfirm) {
            if (!isConfirm) return
            Ajaxer.put(lib.href('game-index-href')).
                send({game,score}).
                end(function(err, res) {
                    if (Ajaxer.evalError(err)) return
                    swal('保存完了！', '試合を表示します。', 'success')
                })
        })
    }
    const tagPane = (wizardState) => {
      switch (wizardState.activeKey) {
        case 0: return (
                  <FirstPane game={game} dispatch={dispatch} />
                )
        case 1: return (
                  <SecondPane game={game} dispatch={dispatch} />
                )
        default: throw 'invalid activeKey -> ' + wizardState.activeKey
      }
    }
    return (
        <div className="new-game">
          <ButtonToolbar>
            <a className="btn btn-default" href={lib.href('game-index-ui-href')}>
              <Glyphicon glyph="arrow-left" />
              キャンセル
            </a>
          </ButtonToolbar>

          <WizardMenu />
          {tagPane(wizardState)}

          {wizardState.activeKey === 0 ?
            null :
            <ButtonToolbar className="previous">
              <a className="btn btn-info btn-lg" href="#" onClick={movePreviousPane}>
                <Glyphicon glyph="arrow-left" />
                前へ
              </a>
            </ButtonToolbar>}
          {wizardState.activeKey < wizardState.panes.length - 1 ?
             <ButtonToolbar className="next">
               <a className="btn btn-info btn-lg btn-next" href="#" onClick={moveNextPane}>
                 <Glyphicon glyph="arrow-right" />
                 次へ
               </a>
             </ButtonToolbar> :
             null }

          <ButtonToolbar className="save">
            <Button bsStyle="success" bsSize="large" onClick={save} block>
              <Glyphicon glyph="ok" />
              保存
            </Button>
          </ButtonToolbar>

        </div>
    )
}

function mapStateToProps(state) {
    return state
}

export default connect(mapStateToProps)(NewGame)
