
import request from 'superagent'
import swal    from 'sweetalert'

function exec(pUrl, methodName) {
   return request[methodName](pUrl)
          .set(csrfHeaderName, csrfToken)
          .type('json')
          .accept('json')
          ;
}

export default {
    post(pUrl) {
        return exec(pUrl, 'post');
    },
    get(pUrl) {
        return exec(pUrl, 'get');
    },
    put(pUrl) {
        return exec(pUrl, 'put');
    },
    del(pUrl) {
        return exec(pUrl, 'del');
    },
    patch(pUrl) {
        return exec(pUrl, 'patch');
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
