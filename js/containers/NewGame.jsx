import React         from 'react'
import { connect }   from 'react-redux'
import { ButtonToolbar } from 'react-bootstrap/'
import Button        from 'react-bootstrap/lib/Button'
import Glyphicon     from 'react-bootstrap/lib/Glyphicon'
import FormGroup     from 'react-bootstrap/lib/FormGroup'
import FormControl   from 'react-bootstrap/lib/FormControl'
import ControlLabel  from 'react-bootstrap/lib/ControlLabel'
import Ajaxer        from '../lib/ajaxer'
import Lib           from '../lib/lib'
import * as actions  from '../actions/new-game'

const NewGame = ({game, score, dispatch}) => {
    const onPlaceChange = (e) => {
        dispatch(actions.changeGamePlace(e.target.value))
    }
    const onNameChange = (e) => {
        dispatch(actions.changeGameName(e.target.value))
    }
    const onTeamANameChange = (e) => {
        dispatch(actions.changeTeamAName(e.target.value))
    }
    const onTeamBNameChange = (e) => {
        dispatch(actions.changeTeamBName(e.target.value))
    }
    const now = () => {
        const ret = new Date()
        return [ret.getFullYear(), ret.getMonth()+1, ret.getDate()].join('/')
             + ' ' + [ret.getHours(), ret.getMinutes(), ret.getSeconds()].join(':')
    }
    const setIfNull = (obj, propName, value) => {
        const val = obj[propName]
        if (!val.trim()) obj[propName] = value
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
        (isConfirm) => {
            if (!isConfirm) return
            setIfNull(game.property, 'name', now() + 'の試合')
            setIfNull(game.property, 'teamAName', 'チームA')
            setIfNull(game.property, 'teamBName', 'チームB')

            Ajaxer.put(Lib.href('game-index-href')).
                send(game).
                end((err, res) => {
                    if (Ajaxer.evalError(err)) return
                    swal({
                        title: '保存完了！',
                        text: '試合を表示します。',
                        type: 'success'
                    },() => {
                        location.href = res.header.location
                    })
                })
        });
    }
    return (
        <div className="new-game">
          <ButtonToolbar>
            <a className="btn btn-default" href={Lib.href('game-index-ui-href')}>
              <Glyphicon glyph="arrow-left" />
              キャンセル
            </a>
          </ButtonToolbar>

          <div>
            <FormGroup>
              <ControlLabel>試合名を入力して下さい</ControlLabel>
              <FormControl
                 type="text"
                 value={game.name}
                 placeholder='(省略化)'
                 onChange={e => onNameChange(e)}
              />
            </FormGroup>
            <FormGroup>
              <ControlLabel>場所を入力して下さい</ControlLabel>
              <FormControl
                 type="text"
                 value={game.place}
                 placeholder='(省略化)'
                 onChange={e => onPlaceChange(e)}
              />
            </FormGroup>
            <FormGroup>
              <ControlLabel>チーム<span className="emphasis">A</span>の名前を入力して下さい</ControlLabel>
              <FormControl
                 type="text"
                 value={game.teamAName}
                 placeholder='(省略化)'
                 onChange={e => onTeamANameChange(e)}
              />
            </FormGroup>
            <FormGroup>
              <ControlLabel>チーム<span className="emphasis">B</span>の名前を入力して下さい</ControlLabel>
              <FormControl
                 type="text"
                 value={game.teamBName}
                 placeholder='(省略化)'
                 onChange={e => onTeamBNameChange(e)}
              />
            </FormGroup>
          </div>

          <ButtonToolbar className="save">
            <Button bsStyle="success" bsSize="large" onClick={save}block>
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
