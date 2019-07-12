const observer_mixin_symbol = Symbol("observer_mixin_symbol");

const observer_mixin = function(calling_name, prototype){

	const observer_identifier = Symbol("observer_array_reference");

	prototype[observer_mixin_symbol] = observer_identifier;

	//Properly destructs this particular observer on the object instance.
	prototype.destroyObserverFunctionality = function(){

		
	}

	//Adds an observer to the object instance. Applies a property to the observer that references the object instance.
	//Creates new observers array if one does not already exist.
	prototype.addObserver = function(observer) {
		let observers = this[observer_identifier];

        if(observer[observer_identifier] == this)
            return
        
        if(observer[observer_identifier])
           observer[observer_identifier].removeObserver(observer)
        
        if(!observers)
            observers = this[observer_identifier] = [];
        
        observers.push(observer);
        
        observer[observer_identifier] = this;
    }

    //Removes an observer from this object instance. 
    prototype.removeObserver = function(observer) {
    	const observers = this[observer_identifier];

        for (let i = 0, l = observers.length; i < l; i++)
            if (observers[i] == observer) return (observer[observer_identifier] = null, observers.splice(i, 1));
    }
    

    prototype.updateObservers = function(){
    	const observers = this[observer_identifier];

    	if(observers)
    		observers.forEach(obj=>obj[calling_name](this));
    }
}

observer_mixin.destroy = function(observer_mixin_instance){
	
	const symbol = observer_mixin_instance.prototype[observer_mixin_symbol];
	
	if(symbol){
		if(this[symbol])
			this[symbol].forEach(this.removeObserver.bind(this));

		this[symbol] = null;
	}
}

export default observer_mixin;
