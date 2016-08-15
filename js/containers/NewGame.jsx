import React, { PropTypes } from 'react'
import { connect }          from 'react-redux'
import Ajaxer               from '../lib/ajaxer'
import Lib                  from '../lib/lib'
import * as NewGameActs     from '../actions/new-game'
import TeamList             from '../containers/TeamList'
import LeagueList           from '../containers/LeagueList'

import { ButtonToolbar, Button, Glyphicon,
         FormGroup, InputGroup, FormControl,
         ControlLabel, DropdownButton, MenuItem } from 'react-bootstrap/'

const NewGame = ({game, leagueList, dispatch}) => {
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
                    console.log(game)
            setIfNull(game, 'leagueName', '')
            setIfNull(game, 'gameName', now() + 'の試合')
            setIfNull(game, 'gamePlace', '')
            setIfNull(game, 'teamAName', 'チームA')
            setIfNull(game, 'teamBName', 'チームB')

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
            <a className="btn btn-default clickable"
              href={Lib.href('game-index-ui-href')}>
              <span className="ripple__effect is-blue"/>
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
                   value={game.leagueName}
                   placeholder='(省略化)'
                   onChange={e => dispatch(NewGameActs.changeLeagueName(e.target.value))}
                />
                <LeagueList
                  onSelect={l => dispatch(NewGameActs.changeLeagueName(l.name))}/>
              </InputGroup>
            </FormGroup>
            <FormGroup>
              <ControlLabel>試合名を入力して下さい</ControlLabel>
              <FormControl
                 type="text"
                 value={game.gameName}
                 placeholder='(省略化)'
                 onChange={e => dispatch(NewGameActs.changeGameName(e.target.value))}
              />
            </FormGroup>
            <FormGroup>
              <ControlLabel>場所を入力して下さい</ControlLabel>
              <FormControl
                 type="text"
                 value={game.gamePlace}
                 placeholder='(省略化)'
                 onChange={e => dispatch(NewGameActs.changeGamePlace(e.target.value))}
              />
            </FormGroup>
            <FormGroup>
              <ControlLabel>チーム<span className="emphasis">A</span>を入力、あるいは入力して下さい</ControlLabel>
              <InputGroup>
                <FormControl
                   type="text"
                   value={game.teamAName}
                   placeholder='(省略化)'
                   onChange={e => dispatch(NewGameActs.changeTeamAName(e.target.value))}
                />
                <TeamList menuKeyToken="teamA"
                  onSelect={team => dispatch(NewGameActs.changeTeamAName(team.name))}/>
              </InputGroup>
            </FormGroup>
            <FormGroup>
              <ControlLabel>チーム<span className="emphasis">B</span>を入力、あるいは入力して下さい</ControlLabel>
              <InputGroup>
                <FormControl
                   type="text"
                   value={game.teamBName}
                   placeholder='(省略化)'
                   onChange={e => dispatch(NewGameActs.changeTeamBName(e.target.value))}
                />
                <TeamList menuKeyToken="teamB"
                  onSelect={team => dispatch(NewGameActs.changeTeamBName(team.name))}/>
              </InputGroup>
            </FormGroup>
          </div>

          <ButtonToolbar className="save">
            <Button bsStyle="success" bsSize="large"
              className="clickable"
              onClick={save}block>
              <span className="ripple__effect is-blue"/>
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

NewGame.propTypes = {
    game: PropTypes.object.isRequired,
    leagueList: PropTypes.array.isRequired,
    dispatch: PropTypes.func.isRequired,
}

export default connect(mapStateToProps)(NewGame)
