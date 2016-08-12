import swal from 'sweetalert'
import Lib  from './lib'

const timer = 800

export default {
    setUpWindowUnloadAlert() {
        $(window).on('beforeunload', () => {
            return ''
        })
    },
    success(message) {
        swal({
            title: '',
            text: message,
            type: 'success',
            timer: timer,
            showConfirmButton: false,
            closeOnCancel: false, // この指定がないと、sweetalertのダイアログが完全に表示しきる前にダイアログ外をクリックした際にcancelボタンを押したことになるばかりか、ダイアログが閉じた時のハンドラが動作しないため、非常に困ったことになる. sweetalertのいただけない挙動.
        })
    },
    successWithCloseHandler(message, closeHandler) {
        swal({
            title: '',
            text: message,
            type: 'success',
            timer: timer,
            showConfirmButton: false,
            closeOnCancel: false,
        },
        () => {
            Lib.call(closeHandler)
        })
    },
    error(message) {
        swal('', message, 'error')
    },
    hideLoading() {
        $('#loader').fadeOut()
    },
    doubleConfirm(pFirstText, pSecondText, pOkOperation) {
        swal({
            title: '',
            text: pFirstText,
            type: 'warning',
            showCancelButton: true,
            cancelButtonText: 'はい',
            confirmButtonText: 'いいえ',
            confirmButtonColor: 'rgb(193,193,193)',
            closeOnCancel: false,
            closeOnConfirm: true,
        }, (isConfirm) => {
            if (isConfirm) return;
            // cancelとOKを反転させる. ボタン連打に対処するため.
            swal({
                title: '',
                text: pSecondText,
                type: 'warning',
                showCancelButton: true,
                cancelButtonText: 'いいえ',
                confirmButtonText: 'はい',
                confirmButtonColor: 'rgb(200,143,143)',
                closeOnCancel: true,
                closeOnConfirm: false,
                showLoaderOnConfirm: true
            }, (isConfirm) => {
                if (!isConfirm) return;
                pOkOperation();
            });
        });
    },
}
