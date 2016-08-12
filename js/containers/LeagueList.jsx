import React       from 'react'
import { connect } from 'react-redux'
import Lib         from '../lib/lib'
import View        from '../components/LeagueListView'

const mapStateToProps = state => {
    return { leagueList: state.leagueList }
}

const mapDispatchToProps = (_, props) => {
    return {
        onSelect: props.onSelect,
    }
}

export default connect(mapStateToProps, mapDispatchToProps)(View)
