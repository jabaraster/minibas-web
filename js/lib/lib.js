export default {
    href(pTagId) {
        const ret = $('#' + pTagId).attr('href');
        if (!ret) throw ('not found tag! tag id is \'' + pTagId + '\'')
        return ret
    },
    copyJson(obj) {
        if (!obj) return obj
        return JSON.parse(JSON.stringify(obj))
    },
    hideInitialLoadingIcon() {
        setTimeout(() => {
            $('#screen-all').fadeOut(500/*ms*/, function() { $(this).remove() })
        }, 0)
    },
    setUpWindowUnloadAlert() {
        $(window).on('beforeunload', () => {
            return ''
        })
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
    firstCharLowerCase(p) {
        if (p === '') return ''
        return p.charAt(0).toLowerCase() + p.substr(1)
    },
    returnProps(_, props) {
        return props
    },
}
