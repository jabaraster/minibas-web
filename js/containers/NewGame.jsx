import React         from 'react'
import { connect }   from 'react-redux'
import Ajaxer        from '../lib/ajaxer'
import Lib           from '../lib/lib'
import * as actions  from '../actions/new-game'

import { ButtonToolbar, Button, Glyphicon,
         FormGroup, InputGroup, FormControl,
         ControlLabel, DropdownButton, MenuItem } from 'react-bootstrap/'

const NewGame = ({game, score, dispatch}) => {
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
              <ControlLabel>リーグを選択するか、入力して下さい</ControlLabel>
              <InputGroup>
                <FormControl
                   type="text"
                   value={game.name}
                   placeholder='(省略化)'
                   onChange={e => actions.changeGameName(e.target.value)}
                />
                <DropdownButton pullRight componentClass={InputGroup.Button}>
                  <MenuItem>aaa</MenuItem>
                  <MenuItem>bbb</MenuItem>
                </DropdownButton>
              </InputGroup>
            </FormGroup>
            <FormGroup>
              <ControlLabel>試合名を入力して下さい</ControlLabel>
              <FormControl
                 type="text"
                 value={game.name}
                 placeholder='(省略化)'
                 onChange={e => actions.changeGameName(e.target.value)}
              />
            </FormGroup>
            <FormGroup>
              <ControlLabel>場所を入力して下さい</ControlLabel>
              <FormControl
                 type="text"
                 value={game.place}
                 placeholder='(省略化)'
                 onChange={e => actions.changeGamePlace(e.target.value)}
              />
            </FormGroup>
            <FormGroup>
              <ControlLabel>チーム<span className="emphasis">A</span>の名前を入力して下さい</ControlLabel>
              <FormControl
                 type="text"
                 value={game.teamAName}
                 placeholder='(省略化)'
                 onChange={e => actions.changeTeamAName(e.target.value)}
              />
            </FormGroup>
            <FormGroup>
              <ControlLabel>チーム<span className="emphasis">B</span>の名前を入力して下さい</ControlLabel>
              <FormControl
                 type="text"
                 value={game.teamBName}
                 placeholder='(省略化)'
                 onChange={e => actions.changeTeamBName(e.target.value)}
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
