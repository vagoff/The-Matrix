function jsNewObj() {
    return {}
}

function jsIsEmpty(a) {
    for (p in a)
        return false
    return true
}

function jsInsert(k,v,m) {
    m[k] = v
    return m
}

function jsLookup(m,k) {
    return m[k]
}

functionjsRemove(k,m) {
    delete m[k]
    return m
}
