
const parseDateCore = d => {
    return d.substr(0, 16).replace(/-/g, '/').replace('T', ' ')
}

export default {
    assign(dest, src1, src2) {
        for (let p in src1) {
            dest[p] = src1[p]
        }
        if (src2) {
            for (let p in src2) {
                dest[p] = src2[p]
            }
        }
        return dest
    },
    call(func) {
        if (typeof func === 'function') func()
    },
    href(pTagId) {
        const ret = $('#' + pTagId).attr('href');
        if (!ret) throw ('not found tag! tag id is \'' + pTagId + '\'')
        return ret
    },
    shallowCopy(obj) {
        if (Array.isArray(obj)) {
            return [].concat(obj)
        }
        return this.assign({}, obj)
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
    deepCopy(obj) {
        return JSON.parse(JSON.stringify(obj))
    },
    firstCharLowerCase(p) {
        if (p === '') return ''
        return p.charAt(0).toLowerCase() + p.substr(1)
    },
    comma(s) {
        return String(s).replace( /(\d)(?=(\d\d\d)+(?!\d))/g, '$1,')
    },
    returnState(state) {
        return state
    },
    returnProps(_, props) {
        return props
    },
    toIdHash(hasIdObjectList) {
        const ret = {}
        hasIdObjectList.forEach(o => {
            ret[o.id] = o
        })
        return ret
    },
    toNumber(s) {
        return new String(s) - 0
    },
    swapArrayElement(ary, idx1, idx2) {
        const temp = ary[idx1]
        ary[idx1] = ary[idx2]
        ary[idx2] = temp
    },
    parseDate(d) {
        return parseDateCore(d)
    },
    sendGAEvent(eventCategory, eventAction) {
        if (!ga) {
            if (console.warn) console.warn(`ga is undefined. can't send evetn -> eventCategory: ${eventCategory}, eventAction: ${eventAction}`)
            return
        }
        ga('send', 'event', eventCategory, eventAction)
    },
}
