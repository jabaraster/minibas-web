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
}
