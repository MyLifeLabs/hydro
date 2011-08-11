// Definitions for built-in protocols

module Ice {
    interface Locator;
    interface LocatorRegistry;

    ["hydro:mapping:identity"] struct Identity {
	string name;
	string category;
    };

    exception AdapterNotFoundException {};
    exception ObjectNotFoundException {};
    exception ServerNotFoundException {};
    exception InvalidReplicaGroupIdException {};
    exception AdapterAlreadyActiveException {};

    interface Locator {
	["nonmutating"] Object* findObjectById(Ice::Identity id)
	    throws ObjectNotFoundException;

	["nonmutating"] Object* findAdapterById(string id)
	    throws AdapterNotFoundException;

        ["nonmutating"] LocatorRegistry* getRegistry();
    };

    interface LocatorRegistry {
        idempotent void setAdapterDirectProxy(string id, Object* proxy)
	    throws AdapterNotFoundException, 
	    AdapterAlreadyActiveException;

	idempotent void setReplicatedAdapterDirectProxy(string adapterId,
							string replicaGroupId, 
							Object* p)
	    throws AdapterNotFoundException, 
	    AdapterAlreadyActiveException, 
	    InvalidReplicaGroupIdException;

	// setServerProcessProxy: missing
    };

};
