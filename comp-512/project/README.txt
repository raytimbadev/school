COMP 512 Distributed Systems: Deliverable 1a
-
Team: Alexandre Laporte
	  Jacob Errington

Distributed system using SOAP webservices:
Note: This port assumes that if a server or
middleware layer is running it is the only instance on that machine and
thus all services are served on port 8080.
Additionally one must change the server locations described in build.xml to
the relevant ip's. 
Instructions:
------------
	Server 1:
		run ant server
	Server 2:
		run ant server
	Server 3: 
		run ant server
	Middleware:
		With the appropriate ip's specified in build.xml
		run ant middleware
	Client
		With the middleware ip specified in build.xml
		run ant client

The client interface is unchanged and behavior should reflect that
of the provided source with the caviat of being distributed.
