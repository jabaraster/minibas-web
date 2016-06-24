
import request from 'superagent'
import swal    from 'sweetalert'

export default {
    post(pUrl) {
        return request.post(pUrl)
               .set(csrfHeaderName, csrfToken)
               .type('json')
               .accept('json')
               ;
    },
    get(pUrl) {
        return request.get(pUrl)
               .set(csrfHeaderName, csrfToken)
               .type('json')
               .accept('json')
               ;
    },
    put(pUrl) {
        return request.put(pUrl)
               .set(csrfHeaderName, csrfToken)
               .type('json')
               .accept('json')
               ;
    },
    del(pUrl) {
        return request.del(pUrl)
               .set(csrfHeaderName, csrfToken)
               .type('json')
               .accept('json')
               ;
    },
    evalError(pError) {
        if (pError) {
            swal({
                title: '通信エラー',
                text: ['status: ', pError.status, '<br/>', JSON.stringify(pError.response.body)].join(''),
                html: true,
                type: 'error',
            });
            return true;
        }
        return false;
    },
}
