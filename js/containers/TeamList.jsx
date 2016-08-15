import React       from 'react'
import { connect } from 'react-redux'
import Lib         from '../lib/lib'
import View        from '../components/TeamListView'

const mapStateToProps = (state, props) => {
    const ret = Lib.shallowCopy(props)
    ret.teamList = state.teamList
    return ret
}

const mapDispatchToProps = (_, props) => {
    return {
        onSelect: props.onSelect,
    }
}

export default connect(mapStateToProps, mapDispatchToProps)(View)
