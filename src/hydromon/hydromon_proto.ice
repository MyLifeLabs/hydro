/* $Id$ -*- c -*- */

module Hydro {

    exception TooManyMonitoredObjects {};
    /* Too many objects have already been registered for being monitored.
     */

    exception Error { string msg; };
    /* Some error occurred */ 
    
    struct monitoredObject {
	Ice::Object *monobject;    /* The object being monitored */
	string       operation;    /* This operation is used to ping the object */
	bool         idem;         /* Whether operation is idempotent */
	long         endTimestamp; /* The monitoring is guaranteed to continue until this time */
	string       shmName;      /* The shared memory segment */
	int          shmIndex;     /* The index in the shared memory segment */
	int          deadPings;    /* Number of consecutive unreplied pings */
    };

    sequence<monitoredObject> monitoredObjectSeq;

    interface Mon {
	monitoredObject
	    requestMonitoring(Ice::Object *monobject,
			      string operation,
			      bool idem,
			      long reqEndTimestamp
			      )
	    throws TooManyMonitoredObjects, Error;
	/* Requests that the [monobject] is monitored by pinging it constantly.
           A ping is an invocation of [operation] of the default facet.
           The operation is called without arguments, and it must not return
           any value.

           On success, the [requestMonitoring] function returns a control
           structure [monitoredObject]. This can be a newly created structure,
           or one that is shared with other monitoring requests. This
           especially means that [endTimestamp] may be bigger than requested.

           If a new structure has been allocated, it is immediately tried
           to ping the object. The result is returned in [deadPings]. If
           the structure already exists, the result of the last ping is
           returned. [deadPings] counts the number of failing pings, so a
           value of 0 is the best.

           The monitor continues to ping the object even after the
           [requestMonitoring] function has returned. The results can be
           retrieved by opening the POSIX shared memory segment [shmFile] with
	   shm_open(), and by reading the byte [shmIndex]. This byte returns
           [min(deadPings,127)]. When [endTimestamp] is reached,
           the byte may be reused for other purposes.

           For uniformity of the monitoring, the ping frequency and the
           ping timeout cannot be individually configured.
	*/

	monitoredObjectSeq
	    list()
	    throws Error;
	/* Lists all current monitorings */
	
    };

};
