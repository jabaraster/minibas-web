import * as Redux      from 'redux'
import thunkMiddleware from 'redux-thunk'
import createLogger    from 'redux-logger'

export default {
    createStore(reducer, initialState) {
        if (appMode === 'development') {
            const loggerMiddleware = createLogger()
            const createStoreWithMiddleware = Redux.applyMiddleware(
                thunkMiddleware,
                loggerMiddleware
            )(Redux.createStore)
            return createStoreWithMiddleware(reducer, initialState)
        } else {
            const createStoreWithMiddleware = Redux.applyMiddleware(
                thunkMiddleware
            )(Redux.createStore)
            return createStoreWithMiddleware(reducer, initialState)
        }
    },
}
