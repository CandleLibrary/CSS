export default function(cacher){
    let cache = null;
    const destroy = cacher.prototype.destroy;
    const init = cacher.prototype.init;

    cacher.prototype.destroy = function(...args){

        if(destroy)
            destroy.call(this, ...args);

        this.next_cached = cache;
        cache = this;
    }

    return function(...args){
            let r
        if(cache){
            r = cache;
            cache = cache.next_cached;
            r.next_cached = null;
            init.call(r,...args);
        }else{
            r = new cacher(...args);
            r.next_cached = null;
            r.CACHED = true;
        }
        return r;
    };
};
