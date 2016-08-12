import React, { PropTypes } from 'react'
import { DropdownButton, MenuItem, InputGroup } from 'react-bootstrap/'

const LeagueListView = ({leagueList,onSelect}) => {
    const onLeagueSelect = eventKey => {
        const idx = eventKey
        onSelect(leagueList[idx], idx)
    }
    return (
        <DropdownButton pullRight
          title=""
          id="league-list-dropdown"
          componentClass={InputGroup.Button}
          onSelect={onLeagueSelect}
        >
          {leagueList.map((league, idx) => {
            return <MenuItem key={`league-dropdown-menu_${idx}`} eventKey={idx}>{league.name}</MenuItem>
          })}
        </DropdownButton>
    )
}

LeagueListView.propTypes = {
    leagueList: PropTypes.array.isRequired,
    onSelect: PropTypes.func.isRequired,
}

export default LeagueListView
