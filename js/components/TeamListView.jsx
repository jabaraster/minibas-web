import React, { PropTypes } from 'react'
import { DropdownButton, MenuItem, InputGroup } from 'react-bootstrap/'

const TeamListView = ({menuKeyToken,teamList,onSelect}) => {
    const onTeamSelect = eventKey => {
        const idx = eventKey
        onSelect(teamList[idx], idx)
    }
    return (
        <DropdownButton pullRight
          title=""
          id={menuKeyToken}
          componentClass={InputGroup.Button}
          onSelect={onTeamSelect}
        >
          {teamList.map((team, idx) => {
            return <MenuItem key={`team_${menuKeyToken}_${idx}`} eventKey={idx}>{team.name}</MenuItem>
          })}
        </DropdownButton>
    )
}

TeamListView.propTypes = {
    menuKeyToken: PropTypes.string.isRequired,
    teamList: PropTypes.array.isRequired,
    onSelect: PropTypes.func.isRequired,
}

export default TeamListView
